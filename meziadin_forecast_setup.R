# Code to process (1) Meziadin sockeye run by age data and (2) escapement and total return data
# for Meziadin-specific sockeye run forecast using ForecastR
library(tidyverse)

# Read in Meziadin/Nass TE/TR data:
mez_nass_TR_TE <- read_csv("data/nass vs mez TR TE new.csv") %>%
  filter(type == "Total Return") %>%
  drop_na("Meziadin") %>%
  rename(runyear = year)

# data to 2023:
#filter to include just the missing years (2018-2023)

datanew <- read.csv("data/TRTC-Results--Nass-Sockeye_kke.csv") %>%
  filter(CU_Name == "Meziadin") %>%
  subset(select = -c(CU, SpeciesId, T_Idx_E, ExpFactor1, ExpFactor2, AdjSum, ObsE,
                     ExpFactor3, TE, CDN.Harvest, CDN.ER, Q1, Q2, Q3, ER.Indicator,
                     ER.Indicator.Lookup, ER.Source, StatArea, TRTC, Total.Harvest,
                     Total.ER, CU_Name)) %>%
  filter(Year %in% c("2018", "2019", "2020", "2021", "2022", "2023")) %>%
  rename(runyear = Year, `Meziadin_Total Return` = Total.Run)%>%
  mutate(`Meziadin_Total Return` = as.numeric(gsub(",","",`Meziadin_Total Return`))) %>%
  mutate(p.age3 = 0.046,
         p.age4 = 0.29,
         p.age5 = 0.512,
         p.age6 = 0.154,
         Total = sum(p.age3, p.age4, p.age5, p.age6), 
         "3" = p.age3*`Meziadin_Total Return`,
         "4" = p.age4*`Meziadin_Total Return`,
         "5" = p.age5*`Meziadin_Total Return`,
         "6" = p.age6*`Meziadin_Total Return`)

#Wide format to have total run and escapement as columns
TR_TE_wide <- pivot_wider(mez_nass_TR_TE, names_from = type, values_from = c(Meziadin, Nass))

# Read in Meziadin by age data:
mez_age <- read_csv("data/Mez scale data - Andy.csv") %>%
  rename(runyear = Year)

# Merge the two dataframes 
all_mezdata <- merge(TR_TE_wide, mez_age, by = "runyear") %>%
  # filter to only Meziadin data, remove uninformative columns
  subset(select = -c(`Nass_Total Return`, RunAge7, AgeComp, mez.p, non.mez)) %>%
  # add sockeye escapement for each age class (total * percent age)
  # add sockeye total return number for each age class
  mutate("3" = RunAge3*`Meziadin_Total Return`,
         "4" = RunAge4*`Meziadin_Total Return`,
         "5" = RunAge5*`Meziadin_Total Return`,
         "6" = RunAge6*`Meziadin_Total Return`) %>%
  # clarify column name for percentages
  rename(p.age3 = RunAge3, p.age4 = RunAge4, p.age5 = RunAge5, p.age6 = RunAge6)

# Merge old and new data
all_mezdata <- bind_rows(all_mezdata, datanew)

# format for plotting
all_mezdata_long <- all_mezdata %>%
  pivot_longer(cols = "3":"6", names_to = "tr_ageclass", values_to = "tr_count") %>%
  rename(`Age Class` = tr_ageclass)

prop_ages <- all_mezdata %>%
  pivot_longer(cols = "p.age3":"p.age6", names_to = "Age class", values_to = "proportion_age")


# Plot total run by year with age classes:
ggplot(all_mezdata_long, aes(x = runyear, y = tr_count, fill = `Age Class`)) +
  geom_col() +
  xlab("Year") +
  ylab("Sockeye Total Run by year and age class") +
  theme_minimal()

# Proportion of age classes by year
ggplot(prop_ages, aes(x = runyear, y = proportion_age, fill = `Age class`))+
  geom_col() +
  theme_minimal() +
  xlab("Run Year") +
  ylab("Proportion")+
  scale_x_continuous(breaks = round(seq(min(prop_ages$runyear), max(prop_ages$runyear), 
                                        by = 2),1)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))


# getting mean ages for 2018-2023.
# 1. filter older data to last 5 years (2013 - 2018).
# 2. Calculate mean of each age class over last 5 years
# 3. In dataset (data_new, above), add cols with each mean proportion, and estimated return of each age class
# by multiplying proportion by total run.

# mean_ages <- all_mezdata_long %>%
#   filter(runyear %in% c("2013", "2014", "2015", "2016", "2017")) %>%
#   mutate(mean_age3 = mean(p.age3), mean_age4 = mean(p.age4), mean_age5 = mean(p.age5),
#          mean_age6 = mean(p.age6))


# FORMATTING FOR FORECASTR ------------------------------------------------


# Format identical to ForecastR example - removing escapement and just include total return data,
# add columns for stock, species etc

mez_forecastr <- all_mezdata_long %>%
  subset(select = -c(p.age3, p.age4, p.age5, p.age6, Total, 
                     `Meziadin_Total Return`)) %>%
  rename(Run_Year = runyear, Age_Class = `Age Class`, Average_Terminal_Run = tr_count) %>%
  #filter(Age_Class != "3") %>% # remove age class 3 (for now..) because lots of zeros for this age class
  mutate(Brood_Year = Run_Year - as.numeric(Age_Class),
         Stock_Name = "Meziadin",
         Stock_Species = "Sockeye",
         Stock_Abundance = "Terminal Run",
         Forecasting_Year = "2024") %>%
  select(Stock_Name, Stock_Species, Stock_Abundance, Forecasting_Year, Run_Year, Brood_Year,
         Age_Class, Average_Terminal_Run) %>%
  mutate(Average_Terminal_Run = ifelse(is.na(Average_Terminal_Run), 0, Average_Terminal_Run))


# remove values from columns 1-4, except for the first row
mez_forecastr <- mez_forecastr %>%
  mutate(
    Stock_Name = ifelse(row_number() == 1, Stock_Name, ""),
    Stock_Species = ifelse(row_number() == 1, Stock_Species, ""),
    Stock_Abundance = ifelse(row_number() == 1, Stock_Abundance, ""),
    Forecasting_Year = ifelse(row_number() == 1, Forecasting_Year, "")
  )

write.csv(mez_forecastr, "~/coastland/nass-data-summaries/data/mez_forecastr.csv", row.names = FALSE)

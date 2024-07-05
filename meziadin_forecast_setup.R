# Code to process (1) Meziadin sockeye run by age data and (2) escapement and total return data
# for Meziadin-specific sockeye run forecast using ForecastR
library(tidyverse)

# Read in Meziadin/Nass TE/TR data:
mez_nass_TR_TE <- read_csv("data/nass vs mez TR TE new.csv") %>%
  rename(runyear = year)

#Wide format to have total run and escapement as columns
TR_TE_wide <- pivot_wider(mez_nass_TR_TE, names_from = type, values_from = c(Meziadin, Nass))


# Read in Meziadin by age data:
mez_age <- read_csv("data/Mez scale data - Andy.csv") %>%
  rename(runyear = Year)

# Merge the two dataframes 
all_mezdata <- merge(TR_TE_wide, mez_age, by = "runyear", all = TRUE) %>%
  # filter to only Meziadin data, remove uninformative columns
  subset(select = -c(`Nass_Escapement`, `Nass_Total Return`, RunAge7, AgeComp, mez.p, non.mez)) %>%
  # add sockeye escapement for each age class (total * percent age) 
  mutate(run_age3_esc = RunAge3*Meziadin_Escapement,
         run_age4_esc = RunAge4*Meziadin_Escapement,
         run_age5_esc = RunAge5*Meziadin_Escapement,
         run_age6_esc = RunAge6*Meziadin_Escapement) %>%
  # add sockeye total return number for each age class
  mutate("3" = RunAge3*`Meziadin_Total Return`,
         "4" = RunAge4*`Meziadin_Total Return`,
         "5" = RunAge5*`Meziadin_Total Return`,
         "6" = RunAge6*`Meziadin_Total Return`) %>%
  # clarify column name for percentages
  rename(p.age3 = RunAge3, p.age4 = RunAge4, p.age5 = RunAge5, p.age6 = RunAge6)

# format for plotting
all_mezdata_long <- all_mezdata %>%
  #pivot_longer(cols = run_age3_esc:run_age6_esc, names_to = "esc_ageclass", values_to = "esc_count") %>%
  pivot_longer(cols = "3":"6", names_to = "tr_ageclass", values_to = "tr_count")
# Look at a plot of escapement by age by year
#ggplot(all_mezdata_long, aes(x = runyear, y = esc_count, fill = esc_ageclass)) +
#  geom_col() +
#  xlab("Year") +
#  ylab("Sockeye Escapement by year and age class")
# And with total run:
ggplot(all_mezdata_long, aes(x = runyear, y = tr_count, fill = tr_ageclass)) +
  geom_col() +
  xlab("Year") +
  ylab("Sockeye Total Run by year and age class")

# Format identical to ForecastR example - removing escapement and just include total return data,
# add columns for stock, species etc

mez_forecastr <- all_mezdata_long %>%
  subset(select = -c(p.age3, p.age4, p.age5, p.age6, Total, 
                     Meziadin_Escapement, `Meziadin_Total Return`)) %>%
  rename(Run_Year = runyear, Age_Class = tr_ageclass, Average_Terminal_Run = tr_count) %>%
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

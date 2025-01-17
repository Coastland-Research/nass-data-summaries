# Code to process (1) Meziadin sockeye run by age data and (2) escapement and total return data
# for Meziadin-specific sockeye run forecast using ForecastR
library(tidyverse)
library(scales)

# Read in Meziadin/Nass TE/TR data:
mez_nass_TR_TE <- read_csv("data/nass vs mez TR TE new.csv") %>%
  filter(type == "Total Return") %>%
  drop_na("Meziadin") %>%
  rename(runyear = year)

# Wide format to have total run and escapement as columns
TR_TE_wide <- pivot_wider(mez_nass_TR_TE, names_from = type, values_from = c(Meziadin, Nass)) %>%
  subset(select = -c(`Nass_Total Return`, mez.p, non.mez))

# Read new data (to 2023)
datanew <- read.csv("data/TRTC-Results--Nass-Sockeye_kke.csv") %>%
  filter(CU_Name == "Meziadin") %>%
  subset(select = -c(CU, SpeciesId, T_Idx_E, ExpFactor1, ExpFactor2, AdjSum, ObsE,
                     ExpFactor3, TE, CDN.Harvest, CDN.ER, Q1, Q2, Q3, ER.Indicator,
                     ER.Indicator.Lookup, ER.Source, StatArea, TRTC, Total.Harvest,
                     Total.ER, CU_Name)) %>%
  filter(Year %in% c("2018", "2019", "2020", "2021", "2022", "2023")) %>%
  rename(runyear = Year, `Meziadin_Total Return` = Total.Run)%>%
  mutate(`Meziadin_Total Return` = as.numeric(gsub(",","",`Meziadin_Total Return`)))

# ADD 2024 DATA TO NEW DATA:
# Meziadin total return is the escapement (fishwheel count) plus harvest before the fishwheel
# (about 49%). So fishwheel + (fishwheel*.49)
runyear <- c(2024)
mez_return <- c(735996.1) 
# calculated as Esc / (1-ER) using Meziadin fishwheel count for Esc and 0.49 for ER 
data24 <- data.frame(runyear, mez_return) %>%
  rename(`Meziadin_Total Return` = mez_return)

datanew <- bind_rows(datanew, data24)

# Merge old and new data (without ages):
all_mezdata <- bind_rows(TR_TE_wide, datanew)

# Read in predicted returns
predictions <- read.csv("data/mez_predictions.csv")

predictions <- full_join(all_mezdata, predictions, by="runyear") %>%
  filter(!is.na(predicted.return)) %>%
  mutate(predicted.return = as.numeric(predicted.return),
         p25 = as.numeric(p25),
         p75 = as.numeric(p75)) 

# Plot Meziadin total returns by year (no age breakdown)
ggplot(all_mezdata, aes(x = runyear, y = `Meziadin_Total Return`)) +
  geom_col(fill = "seagreen")+
  theme_minimal()+
  labs(x = "Run Year", y = "Total Return to Meziadin")+
  scale_x_continuous(breaks = round(seq(min(all_mezdata$runyear), max(all_mezdata$runyear), 
                                        by = 2),1))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))

### Add a line showing the pre-season forecasts
# Create a csv file for each year by changing forecast year and re-saving.
# Run each csv through forecastR
# Save sibling reg  point forecast, p25 and p75
# create a data frame of the forecasts
# bind with total return data - name something like ("predicted return")
# Plot by adding aes with line for predicted return, error bars for p25 and p75

###
ggplot(predictions, aes(x = runyear)) +
  geom_col(aes(y = `Meziadin_Total Return`, fill = "Actual Return"), color = "seagreen") +
  geom_point(aes(y = predicted.return, color = "Predicted Return")) +
  geom_errorbar(
    aes(ymin = p25, ymax = p75, color = "Predicted Return"),
    width = 0.2
  ) +
  theme_minimal() +
  labs(
    x = "Run Year",
    y = "Total Return to Meziadin"
  ) +
  scale_x_continuous(breaks = round(seq(min(predictions$runyear), max(predictions$runyear), by = 1), 1)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_fill_manual(
    values = c("Actual Return" = "seagreen"),
    name = NULL  # Remove "fill" label
  ) +
  scale_color_manual(
    values = c("Predicted Return" = "black"),
    name = NULL  # Remove "colour" label
  ) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 0.5),
    axis.text.y = element_text(angle = 60),
    legend.position = "top"  # Move legend to the top
  )

###

# Read in Meziadin age data -----------------------------------------------

mez_age <- read_csv("data/Mez scale data - Andy.csv") %>%
  rename(runyear = Year)

# new data to 2022
mez_age_2 <- read_csv("data/Mez ages for Andy.csv") %>%
  filter(Year %in% c("2020", "2021", "2022")) %>%
  rename(runyear = Year) %>%
  #add row for 2023 and 2024 with age percentages using the mean from the last 5y (2018-2022, and 2019-2023)
  add_row(runyear = 2023, AgeComp = "MeziadinAnnual", RunAge3 = 0.1288244, RunAge4 = 0.4137336, RunAge5 = 0.3906876, RunAge6 = 0.06675444,
          RunAge7 = 0, Total = 1.00) %>%
  add_row(runyear = 2024, AgeComp = "MeziadinAnnual", RunAge3 = 0.1345893, RunAge4 = 0.4084802, RunAge5 = 0.3928252, RunAge6 = 0.06410532,
           RunAge7 = 0, Total = 1.00)

mez_age <- rbind(mez_age, mez_age_2)

# Merge the two dataframes 
age_mezdata <- merge(all_mezdata, mez_age, by = "runyear") %>%
  subset(select = -c(RunAge7, AgeComp)) %>%
  # add sockeye escapement for each age class (total * percent age)
  # add sockeye total return number for each age class
  mutate("3" = RunAge3*`Meziadin_Total Return`,
         "4" = RunAge4*`Meziadin_Total Return`,
         "5" = RunAge5*`Meziadin_Total Return`,
         "6" = RunAge6*`Meziadin_Total Return`) %>%
  # column name for percentages
  rename(p.age3 = RunAge3, p.age4 = RunAge4, p.age5 = RunAge5, p.age6 = RunAge6)

# Format for plotting
age_mezdata_long <- age_mezdata %>%
  pivot_longer(cols = "3":"6", names_to = "tr_ageclass", values_to = "tr_count") %>%
  rename(`Age Class` = tr_ageclass)

prop_ages <- age_mezdata %>%
  pivot_longer(cols = "p.age3":"p.age6", names_to = "Age class", values_to = "proportion_age")

# Plot total run by year with age classes:
ggplot(age_mezdata_long, aes(x = runyear, y = tr_count, fill = `Age Class`)) +
  geom_col() +
  xlab("Year") +
  ylab("Sockeye Total Run by year and age class") +
  theme_minimal()+
  scale_x_continuous(breaks = round(seq(min(prop_ages$runyear), max(prop_ages$runyear), 
                                        by = 2),1)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))

# Proportion of age classes by year
ggplot(prop_ages, aes(x = runyear, y = proportion_age, fill = `Age class`))+
  geom_col() +
  theme_minimal() +
  xlab("Run Year") +
  ylab("Proportion")+
  scale_x_continuous(breaks = round(seq(min(prop_ages$runyear), max(prop_ages$runyear), 
                                        by = 2),1)) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))


# FORMATTING FOR FORECASTR ------------------------------------------------


# Format identical to ForecastR example - removing escapement and just include total return data,
# add columns for stock, species etc

mez_forecastr <- age_mezdata_long %>%
  subset(select = -c(p.age3, p.age4, p.age5, p.age6, Total, 
                     `Meziadin_Total Return`)) %>%
  rename(Run_Year = runyear, Age_Class = `Age Class`, Average_Terminal_Run = tr_count) %>%
  #filter(Age_Class != "3") %>% # remove age class 3 (for now..) because lots of zeros for this age class
  mutate(Brood_Year = Run_Year - as.numeric(Age_Class),
         Stock_Name = "Meziadin",
         Stock_Species = "Sockeye",
         Stock_Abundance = "Terminal Run",
         Forecasting_Year = "2025") %>%
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
  ) %>%
  mutate(Average_Terminal_Run = replace(Average_Terminal_Run, Average_Terminal_Run==0, 1))

write.csv(mez_forecastr, "~/coastland/nass-data-summaries/data/mez_forecastr.csv", row.names = FALSE)


# 2024 run prediction -----------------------------------------------------
# File for 2024 forecasting year (to compare forecast with actual 2024 run):

# mez_24 <- age_mezdata_long %>%
#   subset(select = -c(p.age3, p.age4, p.age5, p.age6, Total, 
#                      `Meziadin_Total Return`)) %>%
#   rename(Run_Year = runyear, Age_Class = `Age Class`, Average_Terminal_Run = tr_count) %>%
#   filter(Run_Year != "2024") %>% 
#   mutate(Brood_Year = Run_Year - as.numeric(Age_Class),
#          Stock_Name = "Meziadin",
#          Stock_Species = "Sockeye",
#          Stock_Abundance = "Terminal Run",
#          Forecasting_Year = "2024") %>%
#   select(Stock_Name, Stock_Species, Stock_Abundance, Forecasting_Year, Run_Year, Brood_Year,
#          Age_Class, Average_Terminal_Run) %>%
#   mutate(Average_Terminal_Run = ifelse(is.na(Average_Terminal_Run), 0, Average_Terminal_Run))
# 
# mez_24 <- mez_24 %>%
#   mutate(
#     Stock_Name = ifelse(row_number() == 1, Stock_Name, ""),
#     Stock_Species = ifelse(row_number() == 1, Stock_Species, ""),
#     Stock_Abundance = ifelse(row_number() == 1, Stock_Abundance, ""),
#     Forecasting_Year = ifelse(row_number() == 1, Forecasting_Year, "")
#   ) %>%
#   mutate(Average_Terminal_Run = replace(Average_Terminal_Run, Average_Terminal_Run==0, 1))
# 
# # write csv
# write.csv(mez_24, "~/coastland/nass-data-summaries/data/mez_24.csv", row.names = FALSE)

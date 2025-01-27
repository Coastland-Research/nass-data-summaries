library(tidyverse)
library(scales)
library(gridExtra)


# Read in Meziadin/Nass TE/TR data ----------------------------------------

mez_nass_TR_TE <- read_csv("data/nass vs mez TR TE new.csv") %>%
  filter(type == "Total Return") %>%
  drop_na("Nass") %>%
  rename(runyear = year)

nass_TR_TE <- pivot_wider(mez_nass_TR_TE, names_from = type, values_from = c(Meziadin, Nass)) %>%
  subset(select = -c(`Meziadin_Total Return`, mez.p, non.mez))

# 2018-2023 data. Need to add returns from all streams for each year to get total Nass return
datanew <- read.csv("data/TRTC-Results--Nass-Sockeye_kke.csv") %>%
  subset(select = -c(CU, SpeciesId, T_Idx_E, ExpFactor1, ExpFactor2, AdjSum, ObsE,
                      ExpFactor3, TE, CDN.Harvest, CDN.ER, Q1, Q2, Q3, ER.Indicator,
                      ER.Indicator.Lookup, ER.Source, StatArea, TRTC, Total.Harvest,
                      Total.ER)) %>%
  filter(Year %in% c("2018", "2019", "2020", "2021", "2022", "2023")) %>%
  mutate(Total.Run = as.numeric(gsub(",","", Total.Run))) %>%
  group_by(Year) %>%
  summarise(`Nass_Total Return` = sum(Total.Run, na.rm = TRUE)) %>%
  rename(runyear = Year)

# 2024 data from DFO 2024 post-season review
runyear <- c(2024)
nass_return <- c(614751) 
# run size estimate to Gitwinksihlkw fishwheels
data24 <- data.frame(runyear, nass_return) %>%
  rename(`Nass_Total Return` = nass_return)

datanew <- bind_rows(datanew, data24)

# Combine old and new datasets
nass_TR <- bind_rows(nass_TR_TE, datanew)

# Plot Nass total returns by year (no age breakdown)
ggplot(nass_TR, aes(x = runyear, y = `Nass_Total Return`)) +
  geom_col(fill = "seagreen")+
  theme_minimal()+
  labs(x = "Run Year", y = "Total Return to Nass")+
  scale_x_continuous(breaks = round(seq(min(nass_TR$runyear), max(nass_TR$runyear), 
                                        by = 2),1))+
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5))

# Format for forecastR

nass_TR <- nass_TR %>%
  rename(Run_Year = runyear, Average_Terminal_Run = tr_count) %>%
  #filter(Age_Class != "3") %>% # remove age class 3 (for now..) because lots of zeros for this age class
  mutate(Brood_Year = Run_Year - as.numeric(Age_Class),
         Stock_Name = "Meziadin",
         Stock_Species = "Sockeye",
         Stock_Abundance = "Terminal Run",
         Forecasting_Year = "2025") %>%
  select(Stock_Name, Stock_Species, Stock_Abundance, Forecasting_Year, Run_Year, Brood_Year,
         Age_Class, Average_Terminal_Run) %>%
  mutate(Average_Terminal_Run = ifelse(is.na(Average_Terminal_Run), 0, Average_Terminal_Run))


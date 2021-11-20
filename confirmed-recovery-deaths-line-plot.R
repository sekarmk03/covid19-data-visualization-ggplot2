library(readr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)

dfcovid <- read_csv("covid19_data_cleaned.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# mengganti na dengan 0 atau ""
dfcovid$ProvinceState[is.na(dfcovid$ProvinceState)] <- ""
dfcovid$Long[is.na(dfcovid$Long)] <- 0
dfcovid$Lat[is.na(dfcovid$Lat)] <- 0
dfcovid$Confirmed[is.na(dfcovid$Confirmed)] <- 0

# membuat atribut baru
# penjumlahan nilai atribut yg ada berdasarkan Date
confirmed <- as.data.frame(dfcovid %>% group_by(Date) %>%
          summarise(Confirmed = sum(Confirmed)))
recovered <- as.data.frame(dfcovid %>% group_by(Date) %>%
          summarise(Recovered = sum(Recovered)))
deaths <- dfcovid %>% group_by(Date) %>%
        summarise(Deaths = sum(Deaths))

# membuat data frame menggunakan mapping
dfcovid2 <- do.call(rbind, Map(data.frame, Date=confirmed$Date, Confirmed=confirmed$Confirmed,
                               Recovered=recovered$Recovered, Deaths=deaths$Deaths))

ggplot() +
  # membuat line 1,2,3
  geom_line(data=dfcovid2, aes(x=Date, y=Deaths), col="red", size=1) +
  geom_line(data=dfcovid2, aes(x=Date, y=Recovered), col="green", size=1) +
  geom_line(data=dfcovid2, aes(x=Date, y=Confirmed), col="blue", size=1) +
  # membuat theme
  theme_classic() +
  # menambah judul
  ggtitle("Worldwide Covid 19 Cases") +
  # menambah label sumbu y
  labs(y="Amount")

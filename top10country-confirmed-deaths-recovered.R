library(readr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(viridis)

dfcovid <- read_csv("covid19_data_cleaned.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# mengganti na dengan 0 atau ""
dfcovid$ProvinceState[is.na(dfcovid$ProvinceState)] <- ""
dfcovid$Long[is.na(dfcovid$Long)] <- 0
dfcovid$Lat[is.na(dfcovid$Lat)] <- 0
dfcovid$Confirmed[is.na(dfcovid$Confirmed)] <- 0

# membuat data total nilai (jumlah) beberapa atribut berdasarkan Country
top10 <- as.data.frame(dfcovid %>% group_by(Country) %>%
      summarise(Confirmed = sum(Confirmed), Recovered = sum(Recovered),
                Deaths = sum(Deaths), Active = sum(Active)))
top10

# mengurutkan data desc
top10 <- top10[order(-top10$Confirmed),]

ggplot(top10[1:10,], aes(x=reorder(Country, Deaths), y=Deaths, fill=Deaths)) +
  # agar tingginya bar sesuai y
  geom_bar(stat="identity") +
  # menukar posisi sumbu x dan y
  coord_flip()+
  # mengatur warna
  scale_fill_viridis_c(option = 'magma', direction = -1)+
  # mengatur tema ke minimal
  theme_minimal()+
  # menukar sumbu x dan y, membalik urutan bar
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  # membuat text keterangan
  labs(title = '10 Countries with the Most COVID-19 Deaths',
    x = 'Country', y = 'Total Deaths')



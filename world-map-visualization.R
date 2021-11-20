library(readr)
library(magrittr)
library(dplyr)
library(tidyverse)
library(plotly)
library(maps)
library(viridis)

dfcovid<- read_csv("covid19_data_cleaned.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))

# mengganti na dengan 0 atau ""
dfcovid$ProvinceState[is.na(dfcovid$ProvinceState)] <- ""
dfcovid$Long[is.na(dfcovid$Long)] <- 0
dfcovid$Lat[is.na(dfcovid$Lat)] <- 0
dfcovid$Confirmed[is.na(dfcovid$Confirmed)] <- 0

# menyimpan data map dunia
world <- map_data("world")

data <- dfcovid %>%
  # mengatur nilai country
  arrange(Country) %>%
  # membuat variable baru (name) dari data sebelumnya di dalam dataset
  mutate( name=factor(Country, unique(Country))) %>%
  # membuat variable baru (mytext)
  mutate( mytext=paste(
    "City: ", name, "\n", 
    "Confirmed: ", Confirmed, "\n",
    "Recovered: ", Recovered)
  )

mapData <- data %>%
  # membuat plot
  ggplot() +
  # membuat dataran
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="green", alpha=0.3) +
  # membuat bulatan
  geom_point(aes(x=Long, y=Lat, size=Confirmed, color=Confirmed, text=mytext, alpha=0.3) ) +
  # mengatur ukuran map
  scale_size_continuous(range=c(1,15)) +
  # mengatur warna
  scale_color_viridis(option="inferno", trans="log" ) +
  # mengatur transparansi
  scale_alpha_continuous(trans="log") +
  # membuat tema kartesius kosong
  theme_void() +
  # membuat proyeksi peta
  coord_map() +
  # menghilangkan legend
  theme(legend.position = "none")

mapPrint <- ggplotly(mapData, tooltip="text")
mapPrint






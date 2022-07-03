library(flexdashboard)
library(lubridate)
library(dplyr)
library(ggplot2) # pembuatan plot statis
library(plotly) # buat plot interaktif
library(glue) # utk setting tooltip
library(shiny)
library(shinydashboard)
library(ggthemes)
library(leaflet)
library(scales)

options(scipen = 9999)

covid <- read.csv("covid-variants.csv")

covid_clean <- covid %>% 
  mutate(date = ymd(date),
         variant = as.factor(variant),
         location = as.factor(location)) %>% 
  select(location, date, variant, num_sequences, perc_sequences) %>% 
  filter(variant != "non_who") %>% 
  group_by(location, date, variant) %>% 
  summarise(jumlah_kasus = sum(num_sequences), jumlah_persentase = sum(perc_sequences)) 


covid_variant <- covid %>% 
  select(location, variant, num_sequences) %>% 
  filter(variant != "non_who") %>% 
  group_by(variant) %>% 
  summarise(jumlah_kasus = sum(num_sequences)) %>% 
  arrange(-jumlah_kasus) %>% 
  head(5) %>% 
  mutate(
  label=glue("Variant: {variant}
                    Total Case: {jumlah_kasus}
                    "))

countries <- read.csv("countries.csv")

covid_location_geo <- covid_clean %>% 
  select(location, jumlah_kasus) %>%
  group_by(location) %>% 
  summarise(jumlah_kasus=sum(jumlah_kasus)) %>% 
  mutate(
  label=glue("Location: {location},
                    Total Case: {jumlah_kasus}"))

map_data <- covid_location_geo %>% 
  left_join(countries, by=c("location"="name"))

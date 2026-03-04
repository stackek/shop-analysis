merge_weather <- function(shop = 1, weather = "temperature", ready = T){
library(tidyverse)
if(shop == 1) df <- readxl::read_xlsx("receipts.xlsx")
if(shop == 2) df <- readxl::read_xlsx("bazhana-2025.xlsx")

df <- df%>%
  filter(Тип == "SELL")%>%
  select(where(~!all(is.na(.))))%>%
  select(-c(`Фіскальний номер ПРРО`, `Локальний номер ПРРО`, Посилання, `Сума з урахуванням заокруглення (грн)`, Тип, `ID чека`, `Фіскальний номер`, `Офлайн`))

colnames(df) = c("datetime",  "payment", "total")

df <- df%>%
  mutate(datetime = as_datetime(datetime, format = "%d.%m.%Y %H:%M:%S"))%>%
  mutate(total = as.numeric(total))%>%
  mutate(payment = str_replace(payment, "Карта|Картка|Безготівкова оплата", "Card"),
         payment = str_replace(payment, "Готівка", "Cash"))

if(ready == F){

library(httr)
library(jsonlite)

dates <- as.character(unique(date(df$datetime)))

# Borysp
if(shop == 1){
latitude <- 50.431054
longitude <- 30.653326
}

# Bazhana
if(shop == 2){
latitude <- 50.399833
longitude <- 30.650654
}

# Initialize a list to store results
weather_list <- list()

waiting <- 0

for(day in dates){
  # Open-Meteo API URL for historical hourly data
  waiting <- waiting + 1
  if(waiting %% 200 == 0) Sys.sleep(10)
  url <- paste0(
    "https://historical-forecast-api.open-meteo.com/v1/forecast?",
    "latitude=", latitude,
    "&longitude=", longitude,
    "&start_date=", day,
    "&end_date=", day,
    "&hourly=", paste(weather, collapse = ","),
    # "&bounding_box=49,14.12,54.85,24.15",
    "&timezone=EET"
  )
  print(day)
  
  resp <- GET(url)
  data <- fromJSON(content(resp, "text"), flatten = TRUE)
  
  if (!is.null(data$hourly)) {
    
    day_weather <- as.data.frame(data$hourly)
    # convert time column to POSIXct
    day_weather$time <- as.POSIXct(
      day_weather$time,
      format = "%Y-%m-%dT%H:%M",
      tz = "UTC"
    )
  }
  
    weather_list[[as.character(day)]] <- day_weather
  }

# Combine all days
weather_df <- bind_rows(weather_list)


weather_df <- weather_df %>%
  mutate(hour_time = floor_date(time, unit = "hour"))

if(shop == 1) save(weather_df, file = "rdata/weather_borysp.RData")
if(shop == 2) save(weather_df, file = "rdata/weather_bazhana.RData")
}
else {
  if(shop == 1) load("rdata/weather_borysp.RData")
  if(shop == 2) load("rdata/weather_bazhana.RData")
}

View(weather_df)

df <- df %>%
  mutate(planned_hour = floor_date(datetime, unit = "hour"))

df <- df%>%
  left_join(weather_df, by = c("planned_hour" = "hour_time"))%>%
  select(-planned_hour, -time)

if(shop == 1) save(df, file = "rdata/sales_borysp_weather.RData")
if(shop == 2) save(df, file = "rdata/sales_bazhana_weather.RData")
}

weather <- c(
  "temperature_2m",
  "visibility",
  "precipitation",
  "relative_humidity_2m",
  "surface_pressure"
)


merge_weather(1, weather, F)

load("rdata/sales_borysp_weather.RData")

df%>%
  group_by(date = as.Date(datetime))%>%
  summarise(total = sum(total))%>%
  plot(.$total)

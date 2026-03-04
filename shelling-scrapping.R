library(httr) # for sending HTTP requests to get webpages
library(rvest) # for parsing and extracting HTML content
library(xml2) # for wrangling XML/HTML

url <- "https://uk.wikipedia.org/wiki/%D0%9E%D0%B1%D1%81%D1%82%D1%80%D1%96%D0%BB%D0%B8_%D0%9A%D0%B8%D1%94%D0%B2%D0%B0"

headers <- c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")

response <- GET(url, add_headers(headers))

if (http_status(response)$category == "Success") {
  
  # Success! Extract data
  
} else {
  
  print("Failed to retrieve page. Status code:", http_status(response)$code)
  
}

webpage <- read_html(response)

table <- html_node(webpage, xpath="/html/body/div[3]/div/div[3]/main/div[3]/div[3]/div[1]/table[7]")
rows <- html_nodes(table, "tr")
data <- html_table(table)

write.csv(data, "shellings.csv")

# Assume 'data' is already loaded

library(dplyr)
library(lubridate)
library(stringr)

# Remove unnecessary columns
data_clean <- data %>%
  select(-c(1, 8, 9))

# Standardize column names
colnames(data_clean) <- c("DateTime", "Location", "Coordinates", "Weapon", "Deaths", "Injured")

data_clean <- data_clean%>%
  filter(grepl("Усього", DateTime)==F, fixed = TRUE)%>%
  filter(grepl("(24.02.2022–15.07.2024)", DateTime) == F, fixed = TRUE)

# Clean DateTime column
data_clean$DateTime <- str_replace_all(data_clean$DateTime, "02024-", "2024-")
data_clean$DateTime <- str_replace_all(data_clean$DateTime, "~", "")
data_clean$DateTime <- str_replace_all(data_clean$DateTime, "(?<=\\d{2}-\\d{2}-\\d{2})\\d+", "")
data_clean$DateTime <- str_replace_all(data_clean$DateTime, "^(2 січня 2024)", "2024-01-02")
data_clean$DateTime <- str_replace_all(data_clean$DateTime, "(02 січня 2024)", "02 00:00")

data_clean <- data_clean %>%
  mutate(
    ymd = as_date(str_extract(DateTime, "\\d{4}-\\d{2}-\\d{2}")),
    hour = as.numeric(str_remove(str_extract(DateTime, "(\\d{2}):"), ":")),
    min = as.numeric(str_remove(str_extract(DateTime, ":(\\d{2})"), ":")),
    datetime_clean = make_datetime(year = year(ymd),
                                   month = month(ymd),
                                   day = day(ymd),
                                   hour = hour,
                                   min = min),
    DateTime = datetime_clean,
    ymd = NULL,
    hour = NULL,
    min = NULL,
    datetime_clean = NULL
          )

library(tidyr)
data_clean <- data_clean%>%
  mutate(Deaths = as.numeric(str_extract(Deaths, "^\\d+")),
         Injured = as.numeric(str_extract(Injured, "^\\d+")),
         Deaths = replace_na(Deaths, 0),
         Injured = replace_na(Injured, 0))
    
data_clean <- data_clean%>%
  mutate(Weapon = ifelse(grepl("ракет", Weapon, ignore.case = T), "Rocket Missile", Weapon),
        Weapon = ifelse(grepl("дрон", Weapon, ignore.case = T), "Drone", Weapon))

# data_clean$DateTime[1:8] <- rep(data_clean$DateTime[8], 8)
data_clean$DateTime[25] <- data_clean$DateTime[24]

shellings <- data_clean

save(shellings, file = "rdata/shellings.RData")

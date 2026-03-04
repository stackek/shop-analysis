library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)

url <- "https://kyiv.digital/storage/air-alert/stats.html"

# Read HTML
pg <- read_html(url)

# Extract table rows
rows <- pg %>%
  html_elements("table tr")

# Parse each row: timestamp + status text
parsed <- map_df(rows, function(r) {
  cells <- r %>% html_elements("td") %>% html_text(trim = TRUE)
  tibble(
    timestamp = cells[1],
    status = cells[2]
  )
})

# We only need rows with explicit start/stop markers
df <- parsed %>%
  mutate(
    type = case_when(
      str_detect(status, "Повітряна тривога") ~ "START",
      str_detect(status, "Відбій тривоги") ~ "STOP",
      TRUE ~ NA
    )
  ) %>%
  filter(!is.na(type))

# Reverse to start pairing from the bottom
df_rev <- df %>% arrange(desc(row_number()))

# Pair START that has STOP after it (in reversed order)
pairs <- list()
current_start <- NULL

for (i in 1:nrow(df_rev)) {
  row <- df_rev[i, ]
  if (row$type == "START") {
    current_start <- row$timestamp
  } else if (row$type == "STOP" && !is.null(current_start)) {
    pairs <- append(pairs, list(
      tibble(start = current_start, stop = row$timestamp)
    ))
    current_start <- NULL
  }
}

# Combine into final dataframe, restore chronological order
alarms <- bind_rows(pairs) %>%
mutate(
    start = str_replace_all(start, "\\u00A0", " "),
    start = as.POSIXct(start, format = "%H:%M %d.%m.%Y", tz = "Europe/Kyiv"),
    stop = str_replace_all(stop, "\\u00A0", " "),
    stop = as.POSIXct(stop, format = "%H:%M %d.%m.%Y", tz = "Europe/Kyiv")
  )%>%
  mutate_all(.funs = ~update(., year = year(.) + 2000))

print(alarms)

save(alarms, file = "rdata/alarms.RData")


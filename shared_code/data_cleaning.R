###############
#
# Data Cleaning
#
###############


library(tidyverse)
library(lubridate)


raw = read_csv("data/hurrican703.csv") %>% 
  janitor::clean_names() %>% 
  dplyr::select(-month) %>% 
  mutate(
    time = gsub("[()]", "", time),
    time = parse_datetime(time, "%y-%m-%d %H:%M:%S")
  ) %>% 
  mutate(
    month = month(time)
  ) %>% 
  filter(hour(time) %in% c(0, 6, 12, 18),
         minute(time) == 0,
         second(time) == 0)

placeholder = data.frame(id = "placeholder", 
                         time = parse_datetime("00-01-01 00:00:00", "%y-%m-%d %H:%M:%S"), 
                         season = 2048,
                         nature = "PH",
                         latitude = 0, 
                         longitude = 0,
                         wind_kt = 0,
                         month = 0)

dt = bind_cols(rbind(raw, placeholder), 
               rbind(placeholder, raw), 
               .name_repair = "unique")  

dt = dt[2:(nrow(dt)-1),]

dt

dt = dt %>% 
  filter(id...1 == id...9,
         time...10 + 6*60*60 == time...4) %>% 
  mutate(
    d_lat = latitude...5 - latitude...13,
    d_log = longitude...6 - longitude...14,
    d_wkt = wind_kt...7 - wind_kt...15
  ) %>% 
  dplyr::select(id = id...1, wkt_new = wind_kt...7, wkt_cur = wind_kt...15, 
                d_lat, d_log, d_wkt, year = season...11, 
                month = month...16, type = nature...12)

hc = distinct(dt, id) %>% add_rownames("i")

hc

dt = dt %>% left_join(hc)

head(dt)

delete_id = dt %>% 
  group_by(i) %>% 
  summarize(n = n()) %>%
  filter(n <= 5)

dt = dt %>% 
  filter(!(i %in% delete_id$i))

set.seed(2022)
train_index = rownames_to_column(dt) %>% 
  group_by(i) %>% 
  sample_frac(0.8) %>% 
  pull(rowname) %>% 
  as.numeric()

train = dt[train_index,]
test = dt[-train_index,]

#head(dt)
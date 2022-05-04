###############
#
# Data Cleaning
#
###############


library(tidyverse)
library(lubridate)

correct.year = function(date) {
  date$year = date$year - 100
  return(date)
}

raw = read_csv("data/hurrican703.csv") %>% 
  janitor::clean_names() %>% 
  select(-nature, -season, -month) %>% 
  mutate(
    time = gsub("[()]", "", time),
    time = as.POSIXlt(parse_datetime(time, "%y-%m-%d %H:%M:%S")),
    time = as.POSIXct(correct.year(time))
  ) %>% 
  filter(hour(time) %in% c(0, 6, 12, 18),
         minute(time) == 0,
         second(time) == 0)

placeholder = data.frame(id = "placeholder", 
                         time = parse_datetime("00-01-01 00:00:00", "%y-%m-%d %H:%M:%S"), 
                         latitude = 0, 
                         longitude = 0,
                         wind_kt = 0)

dt = bind_cols(rbind(raw, placeholder), 
               rbind(placeholder, raw), 
               .name_repair = "unique")  

dt = dt[2:(nrow(dt)-1),]

dt = dt %>% 
  filter(id...1 == id...6,
         time...7 + 6*60*60 == time...2) %>% 
  mutate(
    d_lat = latitude...3 - latitude...8,
    d_log = longitude...4 - longitude...9,
    d_wkt = wind_kt...5 - wind_kt...10
  ) %>% 
  select(id = id...1, wkt_new = wind_kt...5, wkt_cur = wind_kt...10, d_lat, d_log, d_wkt)

hc = distinct(dt, id) %>% add_rownames("i")

dt = dt %>% left_join(hc)

#head(dt)
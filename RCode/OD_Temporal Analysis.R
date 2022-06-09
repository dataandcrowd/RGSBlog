library(tidyverse)
library(lubridate)
library(data.table)
library(scales)
#S2019_jan <- 
#  read_csv("OD2019/glasgow_20190101_20190630_ride_2019_1_total.csv") %>% 
#  mutate(month = "2019-01-01")

#S2019_feb <- 
#  read_csv("OD2019/glasgow_20190101_20190630_ride_2019_2_total.csv") %>% 
#  mutate(month = "2019-02-01")


# Batch Import with the data.table package
files <-list.files(path="OD", pattern="^glasgow_2019", full.names=TRUE)
S2019 <- rbindlist(lapply(files, fread), idcol = "month")
S2019[, month := factor(month, labels = basename(files))]
S2015 <- fread("OD/glasgow_2015_ride_od_rollup_month_total.csv")
S2016 <- fread("OD/glasgow_2016_ride_od_rollup_month_total.csv")
S2017 <- fread("OD/glasgow_2017_ride_od_rollup_month_total.csv")
S2018 <- fread("OD/glasgow_2018_ride_od_rollup_month_total.csv")



# We do summarise the work
S2019 %>% 
  as_tibble %>% 
  mutate(month = str_sub(month, 32,38),
         month = parse_date_time(month, orders = c("Y", "Ym")) %>% as_date) %>%  
  select(origin, destination, month, athcnt, actcnt, cmtcnt) %>% 
  group_by(origin, destination, month) %>% 
  summarise_all(.funs = "sum") -> S2019df

S2018 %>% 
  as_tibble %>% 
  mutate(month = paste0("2018_", month_n),
         month = parse_date_time(month, orders = c("Y", "Ym")) %>% as_date) %>% 
  select(origin, destination, month, athcnt, actcnt, cmtcnt) %>% 
  group_by(origin, destination, month) %>% 
  summarise_all(.funs = "sum") -> S2018df

S2017 %>% 
  as_tibble %>% 
  mutate(month = paste0("2017_", month_n),
         month = parse_date_time(month, orders = c("Y", "Ym")) %>% as_date) %>% 
  select(origin, destination, month, athcnt, actcnt, cmtcnt) %>% 
  group_by(origin, destination, month) %>% 
  summarise_all(.funs = "sum") -> S2017df

S2016 %>% 
  as_tibble %>% 
  mutate(month = paste0("2016_", month_n),
         month = parse_date_time(month, orders = c("Y", "Ym")) %>% as_date) %>% 
  select(origin, destination, month, athcnt, actcnt, cmtcnt) %>% 
  group_by(origin, destination, month) %>% 
  summarise_all(.funs = "sum") -> S2016df

S2015 %>% 
  as_tibble %>% 
  mutate(month = paste0("2015_", month_n),
         month = parse_date_time(month, orders = c("Y", "Ym")) %>% as_date) %>% 
  select(origin, destination, month, athcnt, actcnt, cmtcnt) %>% 
  group_by(origin, destination, month) %>% 
  summarise_all(.funs = "sum") -> S2015df

#
#bind_rows(S2015df, S2016df, S2017df, S2018df, S2019df) %>% 
#  write_csv("strava_year_bind.csv")

# Plot
bind_rows(S2015df, S2016df, S2017df, S2018df, S2019df) %>% 
  ungroup() %>% 
  select(-c(origin, destination)) %>% 
  group_by(month)%>% 
  summarise_all(.funs = "sum") %>% 
  pivot_longer(!month, names_to = "type", values_to = "count") -> Strava_df

Strava_df %>% 
  ggplot(aes(month, count, colour = type)) +
  geom_line() +
  labs(title = "Monthly Roll-up Counts of Active Travels using Strava Metro in Glasgow",
       #subtitle = "Glasgow, UK",
       y = "Counts",
       x = "",
       caption = "athcnt:Count of people that traveled between that Origin/Destination pair.\nactcnt:Count of trips made between that Origin/Destination pair.\ncmtcnt:Count of trips classified as commutes made between that OD pair.") + 
  theme_bw(base_size = 11.5) +
  scale_x_date(breaks = "3 months", minor_breaks = "1 month", labels=date_format("%B %Y")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom", legend.margin=margin(t=-15),
        plot.caption = element_text(hjust = 0, face = "italic")) -> Strava_df_plot
  
Strava_df_plot

ggsave("Strava_temporal.png", Strava_df_plot, width = 7, height = 4)

library(ReviseR)
library(lubridate)
library(tsbox)
library(sandwich)
library(dplyr)
library(car)
library(dlm)


data("sample_data")

scenario <- c("2004-01-01", "2024-01-01") 

dta <- sample_data %>% as_tibble()

df_gdp_realtime <- get_realtime_series(dta, pub_date, time) 
df_gdp_finrev <- get_latest_vintage_series(dta, pub_date, time)

df_merged <- merge(df_gdp_realtime, df_gdp_finrev, by = c("time", "id")) %>%
  mutate(revision = diff.y - diff.x)

gdp_growth <- df_merged %>%
  select(c(time, diff.x, diff.y, revision)) %>%
  rename(initial ="diff.x", final = "diff.y") %>%
  filter(time >= ymd(scenario[1]) & time < ymd(scenario[2])) 

res_gdp <- create_summary_stats(gdp_growth)

MZ_gdp <- MincerZarnowitz(dta, gdp_growth, 0.05)

# testing for public consumption
y1 <- gdp_growth$initial
x1 <- gdp_growth$final
err_pubcon <- KalmanNowcastPrecision(x1, y1, 0, 10, c(-2, 1, -2))

err_pubcon1 <- KalmanNowcastPrecision( y1, 0, 10, c(-2, 1, -2))
err_pubcon2 <- kalman_nowcast(y1)$nowcast

prec_score <- sqrt(mean(err_pubcon^2))/sqrt(mean((x1-y1)^2))

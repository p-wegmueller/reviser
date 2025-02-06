library(dplyr)
library(tsbox)
library(ggplot2)
library(reviser)
library(lubridate)

load("~/Documents/GitHub/reviser/inst/exdata/pk_rt.RData")
dta <- dta.real.csa %>% filter(id == "gdp") %>% mutate(id = "CH")
load("~/Documents/GitHub/reviser/inst/exdata/usa_rt.RData")
dta <- bind_rows(
  dta %>% filter(time >= as.Date("2000-01-01")), 
  tidy_data %>% filter(time >= as.Date("2000-01-01"))
  )


dta1 <- get_days_to_release(dta)
dta2 <- get_first_release(dta)
dta3 <- get_latest_release(dta)
dta4 <- get_nth_release(dta, n = 0:9)
dta44 <- get_fixed_release(dta, years = 3, month = "September")
dta5 <- get_releases_by_date(dta, as.Date("2007-10-01"))
dta6 <- get_revisions(dta, interval = 1)
dta11 <- get_revisions(dta4 %>% dplyr::select(-pub_date), interval = 1)
dta7 <- get_revisions(dta, nth_release = "latest") 
dta8 <- get_revisions(dta, nth_release = 1)


dta9 <- get_revision_analysis(
  dta %>% filter(year(pub_date) %in% c(2022:2023) ) , 
  get_nth_release(dta, n = "latest")
  )

dta13 <- get_revision_analysis(
  get_nth_release(dta, n = 0:4),
  get_nth_release(dta, n = "latest")
  )


dta10 <- reviser::get_first_efficient_release(
  get_nth_release(dta #%>% 
                    #filter(id == "CH"), 
                  ,n = 0:9),
  get_nth_release(dta #%>% 
                    #filter(id == "CH"), 
                  ,n = 16)
)
res<-summary(dta10)

nowcast <- kk_nowcast(dta10$CH$data, e = dta10$CH$e, trace=1)
nowcast$params
ncast <- nowcast$filtered_states$release_3_lag_0
actl <- tsbox::ts_wide(dta10$CH$data)$release_3[(dta10$CH$e+1):nrow(tsbox::ts_wide(dta10$CH$data))]

# RMSE predicted efficient release
rmse1 <- sqrt(mean((ncast - actl)^2, na.rm = T))

# RMSE first release
rmse2 <- sqrt(mean((tsbox::ts_wide(dta10$CH$data)$release_0[(dta10$CH$e+1):nrow(tsbox::ts_wide(dta10$CH$data))] - actl)^2, na.rm = T))

rmse1/rmse2

gdp <- reviser::gdp %>% ts_pc() %>% ts_span(start="2004-01-01")

dta12 <- reviser::get_first_efficient_release(
  get_nth_release(gdp, n = 0:150),
  get_nth_release(gdp, n = 16)
)
summary(dta12)

nowcast <- kk_nowcast(dta12$data, e=3, trace=1, h = 4)
nowcast$params

ncast <- nowcast$filtered_states$release_5_lag_0
actl <- ts_wide(dta12$data)$release_5[(dta12$e+1):nrow(ts_wide(dta12$data))]

# RMSE predicted efficient release
rmse1 <- sqrt(mean((ncast - actl)^2, na.rm = T))

# RMSE first release
rmse2 <- sqrt(mean((ts_wide(dta12$data)$release_0[(dta12$e+1):nrow(ts_wide(dta12$data))] - actl)^2, na.rm = T))

rmse1/rmse2




plot_vintages(dta2, dim_col = "id", title = "First GDP release")
plot_vintages(dta3, dim_col = "id", title = "Latest GDP release")

plot_vintages(dta4 %>% filter(id == "CH"), dim_col = "release", title = "Swiss GDP releases")
plot_vintages(dta4 %>% filter(id == "USA"), dim_col = "release", title = "US GDP releases")

plot_vintages(dta5 %>% select(id, value, time=pub_date, pub_date=time), dim_col = "id",
              title= "GDP growth estimates", subtitle = "For the 2007-10-01 value")

plot_vintages(dta6 %>% filter(id == "CH", pub_date == "2024-07-01"), type = "bar",
              title = "Revisions to Swiss GDP growth rates", 
              subtitle = "Latest release compared to previous quarter")

plot_vintages(dta6 %>% filter(id == "CH", time >= "2020-01-01" & time < "2022-01-01" & pub_date < "2022-01-01"), type = "line",
              title = "Revisions to Swiss GDP growth rates during Covid-19", 
              subtitle = "Compared to previous quarter")

plot_vintages(dta6 %>% filter(id == "USA", time >= "2020-01-01" & time < "2022-01-01" & pub_date < "2022-01-01"), type = "bar", 
              title = "Revisions to U.S. GDP growth rates during Covid-19",
              subtitle = "Compared to previous quarter")

plot_vintages(dta7 %>% filter(id == "CH", time >= "2020-01-01" & time < "2022-01-01"), type = "point", 
              title = "Revisions to Swiss GDP growth rates during Covid-19",
              subtitle = "Compared to latest release")

plot_vintages(dta8 %>% filter(id == "CH", pub_date == "2024-07-01"), type = "bar", 
              title = "Revisions to Swiss GDP growth rates",
              subtitle = "Latest compared to first release")







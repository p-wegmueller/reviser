library(dplyr)
library(tsbox)
library(ggplot2)
library(reviser)

gdp <- reviser::gdp

gdp_wide <- vintages_wide(gdp)
gdp_wide <- vintages_wide(gdp_wide)

gdp_long_pc <- vintages_long(gdp_wide, keep_na = FALSE) %>% tsbox::ts_pc() %>% tsbox::ts_span(start="1980-01-01")
gdp_long <- vintages_long(gdp_wide)


plot_vintages(gdp_long_pc %>% filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2009-10-01")), type = "line") +
  xlim(as.Date("2008-01-01"), as.Date("2010-01-01")) 

gdp_wide2 <- vintages_wide(gdp_long)

gdp_long_rev <- get_revisions(gdp_long_pc, nth_release = 2) 

gdp_vertical <- get_nth_release(gdp_long_pc, n = c(0:2,6))
plot_vintages(gdp_vertical, dim_col = "release", type = "line") +
  xlim(as.Date("2008-01-01"), as.Date("2013-01-01")) 

plot_vintages(gdp_long_rev %>% filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2013-10-01")), 
              type = "bar") +
  xlim(as.Date("2008-01-01"), as.Date("2010-01-01")) 

plot_vintages(gdp_long_rev, type = "boxplot")  + xlim(as.Date("2004-01-01"), as.Date("2006-01-01")) 

final_release <- get_nth_release(gdp_long_pc, n = 16)
df <- get_nth_release(gdp_long_pc, n = 0:6)
#summary <- get_revision_analysis(df, final_release)

eff_rel <- get_first_efficient_release(df, final_release)
summary(eff_rel)


df <- get_nth_release(tsbox::ts_span(tsbox::ts_pc(reviser::gdp), start = "1980-01-01"), n = 0:1)


kk_list <- kk_nowcast(df = df, e = 1, model = "KK", h = 5, trace=0)

kk_list$params

kk_list$kk_model_mat
ss <- kk_list$ss_model_mat

compare1 <- kk_list$filtered_states[, c("time","release_2_lag_0")]
mean((df_wide$final[3:179] - compare1$release_2_lag_0)^2, na.rm = T)
y1 <- kk_list$observations$release_0_lag_0
y1<- (gdp_long %>% vintages_wide())$`2024-10-01`
compare <- KalmanNowcastPrecision(y1, m0=0,C0= 0.5, initial_params = c(0, 0.5, 5))
compare
mse2 <- mean((df_wide$release_1[2:179] - compare[[1]][2:179])^2, na.rm=T)
mean((df_wide$final[3:179] - df_wide$release_0[3:179])^2, na.rm=T)
mean((df_wide$final[2:179] - df_wide$release_1[2:179])^2, na.rm=T)
mean((df_wide$final[2:179] - df_wide$release_3[2:179])^2, na.rm=T)
mean((df_wide$final[2:179] - df_wide$release_6[2:179])^2, na.rm=T)



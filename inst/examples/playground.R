library(dplyr)
library(tsbox)
gdp <- reviser::gdp

gdp_wide <- vintages_wide(gdp)
gdp_wide <- vintages_wide(gdp_wide)

gdp_long_pc <- vintages_long(gdp_wide, keep_na = F) %>% tsbox::ts_pc() %>% tsbox::ts_span(start="1980-01-01")
gdp_long <- vintages_long(gdp_wide)

plot_vintages(gdp_long_pc %>% filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2011-10-01")), type = "line") +
  xlim(as.Date("2008-01-01"), as.Date("2010-01-01")) 

gdp_wide2 <- vintages_wide(gdp_long)

gdp_long_rev <- get_revisions(gdp_long) 

df <- get_nth_release(gdp_long, n = 1:7)
final_release <- get_nth_release(gdp_long, n = 16) 
eff_rel <- get_first_efficient_release(df, final_release)
summary(eff_rel)
kk_list <- kk_nowcast(df = eff_rel$data, e = eff_rel$e, model = "Classical", estim = "MLE", h = 4, trace=2)

kk_list$auxiliary$fit$value

kk_list$params

kk_list$ss_model

compare1 <- kk_list$filtered_states$release_1_lag_0
mse1 <- mean((df$final[2:179] - compare1)^2, na.rm = T)
y1 <- kk_list$observations$release_0_lag_0
y1<- (gdp_long %>% vintages_wide())$`2024-10-01`
compare <- KalmanNowcastPrecision( y1, m0=0,C0= 0.5, initial_params = c(0, 0.5, 5))
compare
mse2 <- mean((df$release_1[2:179] - compare[[1]][2:179])^2, na.rm=T)
mean((df$final[2:179] - df$release_0[2:179])^2, na.rm=T)
mean((df$final[2:179] - df$release_1[2:179])^2, na.rm=T)
mean((df$final[2:179] - df$release_3[2:179])^2, na.rm=T)
mean((df$final[2:179] - df$release_6[2:179])^2, na.rm=T)



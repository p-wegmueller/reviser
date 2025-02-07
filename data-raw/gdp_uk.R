## code to prepare `gdp_uk` dataset goes here

gdp_uk <- readxl::read_excel("inst/exdata/real-expenditure-long-run.xlsx", 
                             sheet = 2,
                             skip=3)

gdp_uk <- gdp_uk %>%
  rename(time = 1) %>%
  dplyr::mutate(
    time = lubridate::floor_date(as.Date(time), "quarter")
    #time = zoo::as.Date(zoo::as.yearqtr(.[[1]], format = "%Y:Q%q")),
  )# %>%

colnames(gdp_uk) <- c(
  "time",
    as.character(as.Date(paste0("01",colnames(gdp_uk)[2:length(colnames(gdp_uk))]), format = "%d%b%Y"))
)

gdp_uk <- vintages_long(gdp_uk)

usethis::use_data(gdp_uk, overwrite = TRUE)

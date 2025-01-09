## code to prepare `gdp` dataset goes here

gdp <- readxl::read_excel("inst/exdata/ROUTPUTQvQd.xlsx")

gdp <- gdp %>%
  dplyr::mutate(
    time = zoo::as.Date(zoo::as.yearqtr(DATE, format = "%Y:Q%q")),
  ) %>%
  dplyr::select( -DATE) %>%
  dplyr::select( time, 58:238) %>%
  dplyr::mutate(dplyr::across(dplyr::where(is.character), as.numeric)) %>%
  tidyr::pivot_longer(
    cols = -c( time),
    names_to = "pub_date",
    values_to = "value"
  ) %>%
  dplyr::mutate(
    pub_date = zoo::as.Date(zoo::as.yearqtr(stringr::str_split(pub_date, "ROUTPUT", simplify=T)[,2], format = "%yQ%q")),
  ) %>%
  dplyr::filter(
    !is.na(value),
    time >= as.Date("1970-01-01")
    ) 

usethis::use_data(gdp, overwrite = TRUE)

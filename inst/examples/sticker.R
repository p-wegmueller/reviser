library(ggplot2)
library(hexSticker)
library(reviser)

file_name <- "logo.png"
file_path <- "man/figures/"
file_path <- paste0(file_path, file_name)
file_path


  
 ( plot_vintages(gdp_long_pc %>% filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2012-10-01")), type = "line") +
  xlim(as.Date("2008-01-01"), as.Date("2010-07-01")) +# + ylim(12500, 14500)
  theme_void() +
  theme_transparent() +
    theme(legend.position = "none") ) %>%
  hexSticker::sticker(
    package = "reviser",
    # Font size for package name.
    p_size = 20,
    #p_color = "cornflowerblue",
    s_x = 1,
    s_y = 1,
    s_width = 1.5,
    s_height = 1,
    # y position for package name.
    p_y = 1.5,
    # Color to fill hexagon.
    h_fill = "steelblue4",
    # Color for hexagon border.
    h_color	= "cornflowerblue",
    filename = file_path,
    url = ""
  )


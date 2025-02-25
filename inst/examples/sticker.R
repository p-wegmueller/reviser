library(ggplot2)
library(hexSticker)
library(reviser)
library(dplyr)

file_name <- "logo.png"
file_path <- "man/figures/"
file_path <- paste0(file_path, file_name)

(reviser::plot_vintages(
  gdp_long_pc %>%
    filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2012-10-01")),
  type = "line"
) +
  xlim(as.Date("2008-01-01"), as.Date("2010-07-01")) + # + ylim(12500, 14500)
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")) %>%
  hexSticker::sticker(
    package = "reviser",
    # Font size for package name.
    p_size = 20,
    #p_color = "cornflowerblue",
    p_family = "mono",
    s_x = 1,
    s_y = 1,
    s_width = 1.9,
    s_height = 1.3,
    # y position for package name.
    p_y = 1.5,
    # Color to fill hexagon.
    h_fill = "steelblue1",
    # Color for hexagon border.
    #h_color	= "cornflowerblue",
    h_color = "steelblue",
    filename = file_path,
    url = "github.com/p-wegmueller/ReviseR",
    u_size = 4
  )

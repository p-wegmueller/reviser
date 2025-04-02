library(ggplot2)
library(hexSticker)
library(reviser)
library(dplyr)
library(tsbox)

file_name <- "logo.png"
file_path <- "man/figures/"
file_path <- paste0(file_path, file_name)

# Example dataset
gdp_long_pc <- reviser::gdp %>%
  filter(id == "US") %>% 
  ts_pc() %>%
  na.omit()

# pt <- (reviser::plot_vintages(
#   gdp_long_pc %>%
#     filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2012-10-01")),
#   type = "line"
# ) +
#   xlim(as.Date("2008-01-01"), as.Date("2010-07-01")) + # + ylim(12500, 14500)
#   theme_void() +
#   theme_transparent() +
#   theme(legend.position = "none")) 
# 
# 
# p1 <- hexSticker::sticker(
#   pt,
#     package = "reviser",
#     # Font size for package name.
#     p_size = 20,
#     #p_color = "cornflowerblue",
#     p_family = "mono",
#     s_x = 1,
#     s_y = 1,
#     s_width = 1.9,
#     s_height = 1.3,
#     # y position for package name.
#     p_y = 1.5,
#     # Color to fill hexagon.
#     h_fill = "steelblue1",
#     # Color for hexagon border.
#     #h_color	= "cornflowerblue",
#     h_color = "steelblue",
#     filename = file_path,
#     url = "github.com/p-wegmueller/reviser",
#     u_size = 4
#   )
# 
# p1

# highlight_vintage <- gdp_long_pc %>% 
#   filter(pub_date == as.Date("2012-10-01"))
# 
# pt3 <- reviser::plot_vintages(
#   gdp_long_pc %>%
#     filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2012-10-01")),
#   type = "line"
# ) +
#   geom_line(data = highlight_vintage, aes(x = time, y = value), color = "navy", linewidth = 1.2) +
#   # geom_hline(yintercept=0, linetype="dotted", color = "black") + 
#   xlim(as.Date("2008-01-01"), as.Date("2010-07-01")) +
#   theme_void() +
#   theme_transparent() +
#   theme(legend.position = "none") 
# 
# p3 <- hexSticker::sticker(
#   pt3,
#   package = "reviser",
#   p_size = 22,
#   p_family = "Roboto Condensed",
#   p_fontface = "bold",
#   p_color = "navy",
#   p_y = 1.4,
#   s_x = 1,
#   s_y = 0.85,
#   s_width = 1.9,
#   s_height = 1.2,
#   h_fill = "aliceblue",
#   h_color = "navy",
#   filename = file_path,
#   url = "github.com/p-wegmueller/reviser",
#   u_size = 3,
#   u_color = "navy"
# )
# 
# p3
# 
highlight_vintage <- gdp_long_pc %>%
  filter(pub_date == as.Date("2012-10-01"))

provence_colors <- c("#D2691E", "#DDA15E", "#BC6C25", "#606C38", "#283618",
                     "#A0522D", "#B5651D", "#8E7DBE", "#778DA9", "#415A77",
                     "#7D5A50", "#6B705C", "#D9AE94", "#9B2226")

pt_provence <- reviser::plot_vintages(
  gdp_long_pc %>%
    filter(pub_date > as.Date("2009-01-01") & pub_date < as.Date("2012-10-01")),
  type = "line"
) +
  geom_line(
    data = highlight_vintage, aes(x = time, y = value),
    color = "#D2691E", linewidth = 1.2
  ) +
  scale_color_manual(values = provence_colors) +
  xlim(as.Date("2008-01-01"), as.Date("2010-07-01")) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")

hexSticker::sticker(
  pt_provence,
  package = "reviser",
  p_size = 22,
  p_family = "Roboto Condensed",
  p_fontface = "bold",
  p_color = "#5A3E2B",          # Earthy brown
  p_y = 1.4,
  s_x = 1,
  s_y = 0.85,
  s_width = 1.9,
  s_height = 1.2,
  h_fill = "#F8ECD1",           # Soft sandy beige
  h_color = "#A0522D",          # Terracotta
  filename = file_path,
  url = "github.com/p-wegmueller/reviser",
  u_size = 3,
  u_color = "#5A3E2B"
)

# p4
# 
# library(gridExtra)
# grid.arrange(p1, p3, p4, ncol = 3, respect = TRUE)

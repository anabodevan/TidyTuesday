                      #####################################
                      ###      tidy tuesday week 27     ###
                      ###   xkcd color survey results   ###
                      #####################################


# ---- SETUP 

library(tidyverse)
library(colorspace)
library(ggtext)
library(showtext)
                      
color_ranks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')

font_add_google("Source Sans Pro", "source")
showtext_auto()

# ---- WRANGLING 

# Function to convert hex to HSV and arrange by hue
create_rainbow_data <- function(data) {
  data |> 
    # Convert hex to RGB then to HSV
    mutate(
      rgb = map(hex, ~colorspace::hex2RGB(.x)@coords),
      r = map_dbl(rgb, ~.x[1]),
      g = map_dbl(rgb, ~.x[2]),
      b = map_dbl(rgb, ~.x[3])
    ) |> 
    # Convert to HSV for better color sorting
    mutate(
      hsv = pmap(list(r, g, b), ~rgb2hsv(c(..1, ..2, ..3))),
      hue = map_dbl(hsv, ~.x[1]),
      saturation = map_dbl(hsv, ~.x[2]),
      value = map_dbl(hsv, ~.x[3])
    ) |> 
    # Handle NaN hue (occurs with grays) by setting to 0
    mutate(hue = ifelse(is.nan(hue), 0, hue)) |> 
    # Sort by hue, then saturation, then value for smooth gradient
    arrange(hue, desc(saturation), desc(value)) |> 
    # Add position for plotting
    mutate(position = row_number())
}

# Create the rainbow data
rainbow_data <- create_rainbow_data(color_ranks)

plot <- rainbow_data |> 
  ggplot(aes(x = position, y = 1)) +
  # Main color strip
  geom_tile(aes(fill = hex), width = 1, height = 1) +
  # Custom fill scale
  scale_fill_identity() +
  # Clean axes
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # Theme
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      family = "source",
      size = 48,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 25)
    ),
    plot.subtitle = element_text(
      family = "source",
      size = 28,
      hjust = 0.5,
      color = "grey30",
      margin = margin(b = 35)
    ),
    plot.caption = element_text(
      family = "source",
      size = 20,
      hjust = 0.5,
      color = "grey50",
      margin = margin(t = 30)
    ),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  # Labels
  labs(
    title = "THE XKCD COLOR SPECTRUM",
    subtitle = "All 949 colors from the XKCD Color Survey arranged by hue, saturation, and brightness",
    caption = "Data: XKCD Color Survey | Tidy Tuesday Week 27 | Colors flow from red → orange → yellow → green → blue → purple"
  )

ggsave("rainbow.png", plot, width = 16, height = 8, dpi = 300, bg = "white")

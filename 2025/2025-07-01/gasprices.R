                ###############################
                ###   tidy tuesday week 26  ###
                ###   weekly u.s gas prices ###
                ###############################
                

# ---- SETUP 
                                
library(pacman)
pacman :: p_load(tidyverse, kableExtra, showtext, ggtext, glue, dplyr, ggtext)                

weekly_gas_prices <-
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-01/weekly_gas_prices.csv')

font_add_google("Roboto Slab", "slab")
showtext_auto()
showtext_opts(dpi = 300)

cores <- c("#4a4e69", "#9a8c98")
bg <- "#fefefe"

# ---- DATA 

df <-
  weekly_gas_prices %>% 
  mutate(fuel = factor(fuel, levels = c("gasoline", "diesel"))) %>% 
  group_by(year = year(date), fuel, grade, formulation) %>% 
  summarize(
    avg_price = mean(price)
  ) %>% 
  distinct()

ggplot(df, aes(x = year, y = avg_price, fill = fuel)) +
  geom_area(alpha = 0.9) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_vline(xintercept = 2014, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = 0) +
  
  geom_label(aes(x = 2007.5, y = 8.2, label = "2008 Recession"),
             hjust = 1, size = 2, fill = "transparent", label.size = NA, color = "gray30", family = "slab") +
  geom_label(aes(x = 2013.5, y = 8.2, label = "Arab Spring"),
             hjust = 1, size = 2, fill = "transparent", label.size = NA, color = "gray30", family = "slab") +
  geom_label(aes(x = 2019.5, y = 8.2, label = "COVID"),
             hjust = 1, size = 2, fill = "transparent", label.size = NA, color = "gray30", family = "slab") +

  
  scale_fill_manual(values = cores) +
  scale_color_manual(values = cores) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(limits = c(2000, NA), breaks = seq(2000, 2025, by = 5)) +

  
  labs(
    title = "Gas and Diesel Price Trends in the U.S",
    caption = "Source: EIA | anabodevan.github.io",
    x = NULL,
    y = "Avg. Price (USD)") + 
  
  theme_minimal(base_family = 'slab') +
  theme(
    plot.margin = margin(17.5, 15, 10, 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2),
    axis.text = element_text(size = 11, color = "gray20"),
    plot.title = element_text(size = 20, color = "gray10"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray40"),
    panel.background = element_rect(fill = bg, color = bg),
    plot.background = element_rect(fill = bg, color = bg),
    legend.position = 'top'
  )

ggsave("gasprices.png", width = 9, height = 5.1)

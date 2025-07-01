#####   TIDYTUESDAY 2025-06-17 - API SPECS   #
#####   WHICH API FORMATS ARE USED THE MOST? #
##############################################

##### PACKAGES 

library(pacman)

pacman :: p_load(tidyverse, scales, glue, MetBrewer, showtext, ggtext)

#### DATA 

# Load 

api_categories <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')

# Analysis 

# Count origins and create a new format NA == AWS 
api_origins %>% 
  distinct(name, .keep_all = TRUE) %>% 
  count(format, sort = TRUE)

api_origins <-
  api_origins %>% 
  mutate(
    format = if_else(is.na(format) & str_detect(name, 'aws'), 'aws', format)
  ) %>% 
  mutate(format = fct_lump(format, n = 4)) %>% 
  filter(!is.na(format)) %>% 
  count(name, format, sort = TRUE)

# Count categories and create Other 
api_categories %>% 
  count(name, sort = TRUE) %>% 
  filter(n > 1)

api_categories %>% 
  count(apisguru_category, sort = TRUE)

api_categories <-
  api_categories %>% 
  mutate(apisguru_category = fct_lump(apisguru_category, n = 5)) %>% 
  distinct(name, apisguru_category) %>% 
  filter(!is.na(apisguru_category))

# Join dfs 
data <- 
  api_origins %>% 
  left_join(api_categories, by = "name") %>% 
  count(format, apisguru_category) %>%
  filter(!is.na(apisguru_category))

#### AESTHETICS 

font_add_google("Source Sans Pro", "source")
showtext_auto()

#### PLOT 

order <-
  data %>% 
  group_by(format) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  pull(format) %>% 
  as.character()

format_labels <- c(
  'aws' = 'AWS',
  'swagger' = 'Swagger',
  'google' = 'Google',
  'openapi' = 'OpenAPI',
  'Other' = 'Other'
)

plot_data <-
  data %>% 
  mutate(
    format = factor(format, levels = order),
    category_clean = str_to_sentence(apisguru_category) |> str_replace_all('_', ' ')
  )

p <- plot_data %>% 
  ggplot(aes(x = n, y = fct_rev(format), fill = category_clean)) +
  
  # Stacked horizontal bars
  geom_col(width = 0.8, alpha = 0.9) +
  
  # Format labels on the left
  geom_text(
    data = plot_data %>% 
      group_by(format) %>% 
      summarise(total = sum(n), .groups = 'drop'),
    aes(x = -5, y = fct_rev(format), label = format_labels[format]),
    hjust = 1,
    family = 'source',
    size = 4,
    fontface = 'bold',
    color = "black",
    inherit.aes = FALSE
  ) +
  
  # Total count labels at the end of bars
  geom_text(
    data = plot_data %>% 
      group_by(format) %>% 
      summarise(total = sum(n), .groups = 'drop'),
    aes(x = total + 10, y = fct_rev(format), label = comma(total)),
    hjust = 0,
    family = 'source',
    size = 3.5,
    color = "black",
    fontface = 'bold',
    inherit.aes = FALSE
  ) +
  
  MetBrewer::scale_fill_met_d('Hiroshige') +
  
  # Clean scales
  scale_x_continuous(
    expand = expansion(mult = c(0.12, 0.05)),
    labels = comma_format()
  ) +
  
  # Legend styling
  guides(
    fill = guide_legend(
      override.aes = list(alpha = 1),
      nrow = 2,
      byrow = TRUE,
      title = NULL
    )
  ) +
  
  # Theme and styling
  theme_minimal(base_family = "source") +
  theme(
    # Remove y-axis elements since we have custom labels
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    # Style x-axis
    axis.text.x = element_text(size = 10, face = "plain"),
    axis.title.x = element_text(size = 12, face = "plain", margin = margin(t = 15)),
    
    # Panel styling
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    
    # Legend styling
    legend.position = 'bottom',
    legend.text = element_text(size = 11),
    legend.margin = margin(t = 25),
    legend.box.spacing = unit(0.3, "cm"),
    
    # Title styling
    plot.title = element_text(
      family = "source", 
      size = 18, 
      face = 'bold',
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      family = "source", 
      size = 9, 
      color = "grey20",
      margin = margin(t = 20)
    ),
    
    # Plot margins
    plot.margin = margin(25, 30, 25, 30)
  ) +
  
  labs(
    x = "Number of APIs",
    y = NULL,
    title = "API Format Usage by Category",
    caption = "TidyTuesday 2025W24 | Data: APIs.guru | Visualization: @anabodevan"
  )

# Display the plot
print(p)

ggsave("apiguru.png", plot = p, bg = "white")

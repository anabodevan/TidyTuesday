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

label <- c(
  'aws' = 'AWS    ',
  'swagger' = 'Swagger    ',
  'google' = 'Google    ',
  'openapi' = 'OpenAPI    ',
  'Other' = 'Other    '
)

data <-
  data %>% 
  group_by(format) %>% 
  mutate(cumsum = cumsum(n)) %>% 
  ungroup()

data %>% 
  ggplot(aes(x = n, y = factor(format, levels = rev(order)))) +
  geom_segment(
    aes(
      x = cumsum - n,
      xend = cumsum,
      color = str_to_sentence(apisguru_category) |> str_replace_all('_', ' ')
    ),
    linewidth = 5,
    lineend = 'round',
    key_glyph = 'point'
  ) +
  
  geom_text(
    data = tibble(format = order, n = 0),
    aes(label = label[format]),
    hjust = 1,
    family = 'source',
    size = 3,
    color = "black"
  ) +
  
  MetBrewer::scale_color_met_d('Hiroshige', direction = -1) +
  scale_x_continuous(expand = c(0, 0, 0, 0)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  coord_radial(start = 0, end = 1.7 * pi, theta = 'x', inner.radius = .20) +
  
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.position = 'bottom',
    legend.key.size = unit(0, 'line'),
    legend.key.spacing.y = unit(-.45, 'line'),
    legend.text = element_text(margin = margin(l = -0.15, unit = "cm")),
    plot.title.position = "panel",
    plot.title = element_text(family = "source", size = 16, face = 'bold'),
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Format and Usage of APIs",
    caption = "TidyTuesday 2025W24 | Ana Bodevan @anabodevan"
    )

ggsave("apiguru.png", width = 7, height = 5, dpi = 300")


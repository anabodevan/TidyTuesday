##### TIDYTUESDAY 2025-06-03 - PROJECT GUTENBERG ##### 
#####   THE MOST PROLIFIC AUTHORS PER LANGUAGE   #####
######################################################


##### PACKAGES 

library(pacman)

pacman :: p_load(tidytuesdayR, tidyverse, glue, scales, showtext, ggtext, MetBrewer)

#### DATA 

# Load 

tuesdata <- tidytuesdayR::tt_load('2025-06-03')

gutenberg_authors <- tuesdata$gutenberg_authors
gutenberg_languages <- tuesdata$gutenberg_languages
gutenberg_metadata <- tuesdata$gutenberg_metadata

# Explore 

gutenberg_authors %>% count(author, sort = TRUE)
gutenberg_languages %>% count(language, sort= TRUE)

# Manipulate

exclude <- c(NA, 'Various', 'Anonymous', 'Unknown')

data <-
  gutenberg_metadata %>% 
  select(gutenberg_author_id, language) %>% 
  left_join(gutenberg_authors, by = 'gutenberg_author_id') %>% 
  count(gutenberg_author_id, author, language, sort = TRUE) %>% 
  group_by(language) %>% 
  filter(!author %in% exclude)  %>%
  slice_max(n = 1, order_by = n, with_ties = FALSE) %>% 
  ungroup() %>% 
  arrange(-n) %>% 
  head(10)

data <- data %>%
  mutate(
    author_temp = author,
    author_clean = case_when(
      author_temp == "Lytton, Edward Bulwer Lytton, Baron" ~ "Edward Bulwer-Lytton",
      author_temp == "Sand, George" ~ "George Sand",
      author_temp == "Goethe, Johann Wolfgang von" ~ "Johann Wolfgang von Goethe",
      author_temp == "London, Jack" ~ "Jack London",
      author_temp == "Jókai, Mór" ~ "Mór Jókai",
      author_temp == "Pérez Galdós, Benito" ~ "Benito Pérez Galdós",
      author_temp == "Castelo Branco, Camilo" ~ "Camilo Castelo Branco",
      author_temp == "Barrili, Anton Giulio" ~ "Anton Giulio Barrili",
      author_temp == "Plato" ~ "Plato",
      author_temp == "Matull, Kurt" ~ "Kurt Matull",
      TRUE ~ author_temp
    )
  ) %>%
  select(-author_temp) %>% 
  mutate(language = fct_reorder(language, book_count))# Remove temporary column

#### PLOT

# Fonts 

font_add_google("Nunito", "Nunito", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 300)

# Plot 

p <- ggplot(data, aes(x = language, y = book_count)) +
  
  geom_col(aes(fill = language),
           width = .8,
           show.legend = FALSE) +
  
  geom_text(aes(label = author_clean, y = book_count),
            hjust = -0.02,
            vjust = 0.5,
            size = 2.5,
            color = "gray20",
            family = "Nunito",
            fontface = "bold") +
  geom_text(aes(label = book_count, y = book_count - 16),  
            hjust = 0,
            vjust = 0.5,
            size = 3,
            color = "gray10",
            family = "Nunito",
            fontface = "bold") +

  coord_flip() + 
  
  scale_fill_manual(values = met.brewer("Hiroshige", n = nrow(data))) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.35)),
    breaks = pretty_breaks()
  ) +
  
  labs(
    title = "The Most Featured Authors at Project Gutenberg",
    subtitle = "Most prolific authors by language",
    x = "Language",
    y = "Number of Books",
    caption = "Data: Project Gutenberg via TidyTuesday | Visualization: Ana Bodevan"
  ) +
  
  theme_minimal() +
  theme(
    text = element_text(family = "Nunito"),
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 8), 
                              color = "gray10", family = "Nunito"),
    plot.subtitle = element_text(size = 12, color = "gray50", margin = margin(b = 25),
                                 family = "Nunito"),
    axis.text.y = element_text(size = 10, color = "gray30", family = "Nunito"),
    axis.text.x = element_text(size = 8, color = "gray40", family = "Nunito"),
    axis.title = element_text(size = 10, face = "bold", color = "gray20", family = "Nunito"),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.3),
    plot.caption = element_text(size = 9, color = "gray60", hjust = 0, 
                                margin = margin(t = 20), family = "Nunito"),
    plot.margin = margin(20, 90, 20, 20)
  )
  
p

ggsave("gutenberg.png", p, width = 7, height = 5, dpi = 300)



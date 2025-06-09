#####   TIDYTUESDAY 2025-06-10 - U.S JUDGES  #
#####   NUMBER OF APPOINTMENTS PER PRESIDENT #
##############################################

##### PACKAGES 

library(pacman)

pacman :: p_load(tidytuesdayR, tidyverse, glue, scales, showtext, ggtext, MetBrewer)

#### DATA 

# Load 

tuesdata <- tidytuesdayR::tt_load('2025-06-10')
judges_app <- tuesdata$judges_appointments

# Analysis 

appointments_data <- judges_app %>% 
  filter(!president_name %in% c("Assignment", "Reassignment")) %>%
  count(president_name, sort = TRUE) %>%
  mutate(
    # Add historical eras 
    era = case_when(
      president_name %in% c("George Washington", "John Adams", "Thomas Jefferson", 
                            "James Madison", "James Monroe", "John Quincy Adams") ~ "Founding Era (1789-1829)",
      president_name %in% c("Andrew Jackson", "Martin Van Buren", "William H. Harrison", 
                            "John Tyler", "James K. Polk", "Zachary Taylor", "Millard Fillmore",
                            "Franklin Pierce", "James Buchanan") ~ "Antebellum Era (1829-1861)",
      president_name %in% c("Abraham Lincoln", "Andrew Johnson", "Ulysses Grant", 
                            "Rutherford B. Hayes", "James A. Garfield", "Chester A. Arthur") ~ "Civil War & Reconstruction (1861-1885)",
      president_name %in% c("Grover Cleveland", "Benjamin Harrison", "William McKinley", 
                            "Theodore Roosevelt", "William H. Taft", "Woodrow Wilson") ~ "Gilded Age & Progressive Era (1885-1921)",
      president_name %in% c("Warren G. Harding", "Calvin Coolidge", "Herbert Hoover", 
                            "Franklin D. Roosevelt", "Harry S Truman") ~ "Interwar & WWII Era (1921-1953)",
      president_name %in% c("Dwight D. Eisenhower", "John F. Kennedy", "Lyndon B. Johnson", 
                            "Richard M. Nixon", "Gerald Ford", "Jimmy Carter") ~ "Cold War Era (1953-1981)",
      president_name %in% c("Ronald Reagan", "George H.W. Bush", "William J. Clinton", 
                            "George W. Bush", "Barack Obama") ~ "Modern Era (1981-2017)",
      TRUE ~ "Other"
    ),
    # Create shortened names for better readability
    short_name = case_when(
      str_detect(president_name, "Franklin D. Roosevelt") ~ "F.D. Roosevelt",
      str_detect(president_name, "George W. Bush") ~ "G.W. Bush",
      str_detect(president_name, "George H.W. Bush") ~ "G.H.W. Bush",
      str_detect(president_name, "William J. Clinton") ~ "Clinton",
      str_detect(president_name, "Theodore Roosevelt") ~ "T. Roosevelt",
      str_detect(president_name, "Richard M. Nixon") ~ "Nixon",
      str_detect(president_name, "Dwight D. Eisenhower") ~ "Eisenhower",
      TRUE ~ str_remove(president_name, "\\s[A-Z]\\.$")  # Remove middle initials
    )
  ) # thx claude 

#### AESTHETICS

# Fonts 

font_add_google("Playfair Display", "playfair")
font_add_google("Source Sans Pro", "source")
showtext_auto()

# Colors 

era_colors <- met.brewer("Hiroshige", 7)
names(era_colors) <- c("Founding Era (1789-1829)", 
                       "Antebellum Era (1829-1861)",
                       "Civil War & Reconstruction (1861-1885)",
                       "Gilded Age & Progressive Era (1885-1921)",
                       "Interwar & WWII Era (1921-1953)",
                       "Cold War Era (1953-1981)",
                       "Modern Era (1981-2017)")

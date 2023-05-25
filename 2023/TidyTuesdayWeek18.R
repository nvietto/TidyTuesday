
# libraries

pacman::p_load(tidyverse, tidytuesdayR, ggtext)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

tuesdata <- tidytuesdayR::tt_load('2023-05-02')
tuesdata <- tidytuesdayR::tt_load(2023, week = 18)

plots <- tuesdata$plots
species <- tuesdata$species
surveys <- tuesdata$surveys

## data manage

df <- surveys %>% 
  left_join(species, by = "species") %>% 
  select(species, commonname, year, treatment, hfl, meanhfl) %>% 
  filter (species != "RF", species != "PH", species != "PI", species != "DS",
          species != "RO", species != "PL", species != "BA") %>% 
  filter(treatment != "removal") %>% 
  filter(year <= 2015 & year > 2000) %>% 
  group_by(commonname, year, treatment, hfl, meanhfl) %>% 
  drop_na() 

# plot 

Baseplot <- ggplot(data = df, 
       aes(x = year, y = hfl, fill = treatment, color = treatment)) +
  facet_wrap(~commonname) +
  geom_area(color = "darkblue", alpha = 0.6) +
  scale_fill_brewer(palette = "Paired") +
  geom_hline(aes(yintercept=meanhfl, color="red"), linetype="dashed") +
  theme_minimal()

Plot <- Baseplot +
  labs(caption = "Source: Portal Project | @ViettoNicholas ",
    title = paste0("Hindfoot growth differences between <span style='color:#91BAD5'> control </span> and <span style='color:#2E5984'>exclosure </span> groups within
               the Portal Project"),
      subtitle = "In comparison to the species' <span style='color:#ff0000'> mean </span> hindfoot length, the <span style='color:#91BAD5'> control </span> group's
                hindfoot length had much more growth than the <span style='color:#2E5984'>exclosure </span> groups across all species.") + 
  theme(text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 20,
                                                     face = "bold",
                                                     family = "PT Sans Caption",
                                                     lineheight = 1.3,
                                                     margin = margin(1, 0, 1, 0, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(family = "Cabin", size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(0, 0, 1, 0, "lines"),
                                                       halign = 0))

  Plot + theme(
    legend.position = "none",
    axis.title =element_blank())
    

  

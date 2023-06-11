
# Load Packages ------------------------------------------------------------

pacman::p_load(tidyverse, tidytuesdayR, ggtext, ggbump, ggthemes, ggtext, wesanderson)


# Get the Data ---------------------------------------------------------------


owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

dat <- owid_energy

# Data Wrangle ------------------------------------------------------------------

data <- dat %>% 
  select(country, year, solar_energy_per_capita) %>% 
  filter(country %in% c("United Kingdom", "France", "Germany", "Israel", "United States",
                     "China", "Japan")) %>%
  filter(year > 2016 ) %>% 
  pivot_longer(cols = 3) %>% 
 mutate(name = str_to_title(str_to_upper(name))) %>% 
  mutate(name = str_to_title(str_replace_all(name, "_", " "))) %>% 
  na.omit %>% 
  group_by(year) %>% 
  mutate(rank = rank(value, ties.method = "random")) %>% 
  ungroup() %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country)) %>%
  mutate(country = ifelse(country == "United States", "US", country))

# Plot ---------------------------------------------------------------

ggplot(data, aes(year, rank, color = country)) +
  geom_point(size = 5) +
  geom_text(data = data %>% 
              filter(year == min(year)),
            aes(x = year - .59, label = country), size = 5.5, hjust = 0.252) +
  geom_text(data = data
            %>% filter(year == max(year)),
            aes(x = year + .6, label = country), size = 5.5, hjust = 0.7) +
  geom_bump(size = 2.5, lineend = "round") +
  theme_economist() +
  theme(legend.position = "none",
        axis.text = element_text (face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_reverse(breaks = 1:100) +
  scale_x_continuous(breaks = data$year) +
  labs(caption = "Source: Our World Data | @ViettoNicholas ",
       title = paste0("Ranking of solar energy consumption per capita across China, France, the United Kingdom, Israel,
                      the United States, Japan, and Germany"),
       subtitle = "From 2017 to 2021, the <span style='color:#800080'> United Kingdom </span> moved from third to first in solar
       energy consumption (measured in kilowatt-hours). While <span style='color:#008080'> Israel </span> and <span style='color:#ff0000'> China </span> 
       both moved down three spots in rank.") + 
  theme(text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 20,
                                                     face = "bold",
                                                     family = "PT Sans Caption",
                                                     lineheight = 1.3,
                                                     margin = margin(1, 0, 1, 0, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(family = "Oswald", size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(0, 0, 1, 0, "lines"),
                                                       halign = 0))





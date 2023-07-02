# Load Packages and Data ------------------------------------------------------------

pacman::p_load(tidyverse, janitor, ggtext, ggrepel, ggthemes, maps, showtext)

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')


# Fonts -------------------------------------------------------------------------

font_add_google( "Fredericka the Great", "Chewy")
showtext_auto()

# Data Wrangle ------------------------------------------------------------------

map <- map_data("state") %>% 
  filter(region == "michigan")

markers <- historical_markers %>%
  filter(state_or_prov == "Michigan") %>% 
  select(latitude_minus_s, longitude_minus_w, title) %>%
  drop_na() %>% 
  filter(grepl("University", title)) %>% 
  mutate(title = ifelse(title == "University of Michigania", "University of Michigan", title))
  
markers1 <- markers[-c(6,9), ]
  
# Plot -------------------------------------------------------------------


ggplot() +
  geom_polygon(
    data = map, 
    aes(x = long , y = lat),
    fill = "honeydew3"
    ) + 
  theme_void() +
  geom_point(data = markers1,
             aes(x = longitude_minus_w , 
                 y = latitude_minus_s),
             alpha = 1.2) +
  geom_label_repel(data = markers1,
                   aes(x = longitude_minus_w,
                       y = latitude_minus_s,
                       label = title),
                   family = "Chewy",
                   seed = 227,
                   force = 75) +
  labs(caption = " #TidyTuesday | Source: Historical Marker Database | @ViettoNicholas ",
       title = paste0("Historic Universities in the Mitten"),
       subtitle = "Univerisities in Michigan that are designated as historical markers by the Historical
       Marker Database") + 
  theme(text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 30,
                                                     face = "bold",
                                                     family = "Fredericka the Great",
                                                     lineheight = 1.3,
                                                     margin = margin(1, 1, 1, 1, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(family = "Chewy", size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(0.5, 2 , 0.5, 2, "lines"),
                                                       halign = 0))




  















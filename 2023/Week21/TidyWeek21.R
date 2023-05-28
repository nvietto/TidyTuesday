# Load Packages ------------------------------------------------------------

pacman::p_load(tidyverse, tidytuesdayR, janitor, ggtext, ggthemes, osmdata, ggmap, sf)

# Get the Data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2023, week = 21)

squirrel_data <- tuesdata$squirrel_data


# Plot ---------------------------------------------------------------

cp <- c(-73.9850, 40.7630, -73.9450, 40.8021)

water <- cp %>%
  opq() %>%
  add_osm_feature(key = "water") %>% 
  osmdata_sf()

street <- cp %>% 
  opq() %>% 
  add_osm_feature(key ="highway") %>% 
  osmdata_sf()

map <- ggplot() +
  geom_sf(data = water$osm_polygons,
          inherit.aes = FALSE,
          color = "blue",
          fill = "blue") +
  geom_sf(data = street$osm_lines,
          inherit.aes = FALSE,
          #color = "#",
          alpha = 0.5,
          size = 0.7,
          linetype = "solid") +
  coord_sf(ylim = c(40.763321, 40.802099), 
           xlim = c(-73.99000, -73.941647),
           expand = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(color = "black"),
        axis.ticks.x = element_blank()) +
  theme_void() +
  labs(caption = "Source: Central Park Squirrel Census | @ViettoNicholas",
       title = "Where the Central Park Squirrels Spend Time ",
       subtitle = "The Squirrel Census is a multimedia science, design, and storytelling project
       focusing on the Eastern gray (Sciurus carolinensis). The dataset contains data from over 
       3,000 sightings") + 
  theme(text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 19,
                                                     face = "bold",
                                                     lineheight = 0.4,
                                                     margin = margin(t = 20, b = 20),
                                                     family = "Bookman"),
        plot.subtitle = ggtext::element_textbox_simple(family = "NimbusRom ", size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(t = 7, b = 10),
                                                       colour = "Black",
                                                       face = "italic",
                                                       halign = 0))
  
map + geom_point( data = squirrel_data, aes(x = X, y = Y), size = 0.5)




 
 
 

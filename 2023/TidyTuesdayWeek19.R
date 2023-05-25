


# Get the Data

tuesdata <- tidytuesdayR::tt_load('2023-05-09')
tuesdata <- tidytuesdayR::tt_load(2023, week = 19)

childcare_costs <- tuesdata$childcare_costs
counties <- tuesdata$counties

pacman:: p_load(tidyverse, ggtext, janitor, mapdata, maps, glue, wesanderson)



# Data manage

data <- childcare_costs %>% 
  group_by(county_fips_code) %>% 
  left_join(counties, by = "county_fips_code") %>% 
  rename(fips = county_fips_code)

df <- map_data("county") %>%
  mutate(polyname = paste(region, subregion, sep = ",")) %>% 
  inner_join(county.fips, by = "polyname") %>% 
  tibble() %>% 
  left_join(data, by = "fips", multiple = "all") %>% 
  filter(study_year == 2018)

# Note: could also join data frames by county name
# would have mutate country name in "data" with str_remove "county" from variable
# then (str_to_title) subregion to county in map data and left join

# Texts


main_title <- "United States Median Household Income by County in 2018 "

caption <- "Source: The National Database of Childcare Prices (NDCP) | @ViettoNicholas "


# Select Palette

pal <- wes_palette("Zissou1", 100, type = "continuous")

# Plot

plotA <- df %>% ggplot(aes(long, lat, group = group, fill = mhi_2018)) +
  geom_polygon() + 
scale_fill_gradientn(colours = pal) + 
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  coord_equal() +
  theme_void() +
  labs(title = main_title,
       caption = caption)

plotA + theme(plot.title = element_text (vjust = 8,
                                         color = "black",
                                             size = 20,
                                             face = "bold",
                                             family = "Timesn",
                                             lineheight = 1.3),
                                             
              plot.subtitle = ggtext::element_textbox_simple(family = "Cabin", size = rel(1.1), lineheight = 1.3,
                                                             margin = margin(0, 0, 1, 0, "lines"),
                                                             halign = 0),
    legend.key.height = unit(0.30, "cm"),
    legend.key.width = unit(0.7, "cm"),
    legend.text = element_text(size = 11, family = "Times"),
    legend.title = element_blank(),
    legend.position = c(0.01, 0.05), 
    legend.justification = c(0, 0),
    legend.key.size = unit(0.3, "cm"),
    legend.key = element_blank(),
    legend.background=element_blank(),
    axis.title =element_blank()) 
  



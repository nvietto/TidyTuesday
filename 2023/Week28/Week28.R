

# Load Packages and Data -------------------------------------------------------

pacman::p_load(tidyverse, janitor, ggtext, ggthemes, showtext)


glob <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')


# Fonts ------------------------------------------------------------------------

font_add_google("Cinzel" )
showtext_auto()


# Data Wrangle -----------------------------------------------------------------


data <-glob |>
  select(Year, Jan:Dec) |> 
  pivot_longer(cols = 2:13, names_to = "Month") |> 
  rename(Temp = value) |> 
  drop_na()
  
  
# Plot -------------------------------------------------------------------------

ggplot(data, aes(Year, Month, fill = Temp)) + 
         geom_tile() +
  scale_fill_distiller(palette = "RdBu", name = "Temperature deviation (°C)") +
  scale_x_continuous(breaks = seq(1880, 2020, 20),
                     expand = c(0, 0.6)) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5,
                               barwidth = 10,
                               barheight = 0.8))+
  labs(caption = " #TidyTuesday | Source: NASA GISS Surface Temperature Analysis (GISTEMP v4) | @ViettoNicholas",
       title = paste0("Global Surface Temperatures, 1880 - 2023"),
       subtitle = paste0("The GISS Surface Temperature Analysis version 4 (GISTEMP v4) is an estimate of
                         global surface temperature change.")) +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = c(0.855, 1.08),
        plot.margin = margin(1, 1, 1, 1, "cm"), 
        legend.key.width = unit(1.5, "cm"),
        legend.direction = "horizontal",
        text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 23,
                                                     face = "bold",
                                                     family = "Cinzel",
                                                     lineheight = 1.3,
                                                     margin = margin(b = 6)),
        plot.subtitle = ggtext::element_textbox_simple(family = "Cinzel",
                                                       size = rel(1.1), 
                                                       lineheight = 1.5,
                                                       margin = margin(t = 3, b = 32)))





# Load 📦 ---------------------------------------------------------------------

pacman::p_load(janitor, tidyverse, ggtext, showtext, sysfonts, fontawesome, ggimage)

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/planting_2021.csv')

# Fonts -----------------------------------------------------------------------

font_add_google("Chewy")
font_add_google("Inknut Antiqua")
font_add_google("Raleway")

title <-"Chewy"
subtitle <- "Inknut Antiqua"
vegetext <- "Raleway"

showtext_auto()

# Socials ---------------------------------------------------------------------

sysfonts::font_add(family = "Font Awesome 6 Brands",
regular = "/Users/nicholasvietto/Documents/TidyTuesday/fontawe/otfs/Font Awesome 6 Brands-Regular-400.otf")

github_icon <- "&#xf09b"
github_username <- "nvietto"
mastodon_icon <- "&#xf4f6"
masto_username <- "nvietto"
twitter_icon <- "&#xf099"
twitter_username <- "ViettoNicholas"

social_caption <- glue::glue(
  "<span style='color: #000000'> #TidyTuesday </span>
  <span style='color: #000000'>Data: Lisas Vegetable Garden Data </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #000000'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #000000'>{masto_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #000000'>{twitter_username}</span>")

# Wrangle ---------------------------------------------------------------------

plot <- data |> 
  select(vegetable, seeds = number_seeds_planted) |> 
  group_by(vegetable) |> 
  summarise(n = sum(seeds)) |> 
  filter(n > 100)

plot$image <- paste0("images/", plot$vegetable, ".png")


# Plot 📈 ---------------------------------------------------------------------

plot |> ggplot(aes(x = n, y = reorder(vegetable, + n), fill = n)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  geom_image(aes(image = image),
             size = 0.05, 
             asp = 1.5) +
  geom_label(aes(label = n),
             hjust = 2,
             color = "black", 
             fill = "white",
             size = 4, 
             label.padding = unit(0.3, "lines")) +
  theme_void() +
  theme(plot.title = element_text(family = title, 
                                  size = 32,
                                  hjust = 0.5, 
                                  face = "bold", 
                                  color = "#000000", 
                                  margin = margin(b = 6)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = subtitle,
                                     size = 10, 
                                     hjust = 0.5,
                                     color = "#000000",
                                     margin = margin(b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 9, 
                                    color = "#000000", 
                                    hjust = 0.7, 
                                    margin = margin(b = 5, t = 10)),
        legend.position = "none",
        axis.text.y = element_text(family = vegetext,
                                   size = 25, 
                                   color = "#000000", 
                                   hjust = 1, 
                                   margin = margin(l = 5)),
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA,
                                        fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, 
                                       fill = "#FFFFFF")) +
  labs(caption = social_caption,
       title = paste0("Lisa's Vegetable Garden"),
       subtitle = paste0("The top 8 vegetables planted in Lisa's Vegetable Garden in 2021.")) +
  theme(plot.caption = element_textbox_simple())









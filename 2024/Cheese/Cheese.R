# Load 📦 ---------------------------------------------------------------------

pacman::p_load(tidyverse, ggtext, fontawesome, showtext, sysfonts,  ggpattern)

cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

# Fonts -----------------------------------------------------------------------

font_add_google("Playball")
font_add_google("Voltaire")

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
  "<span style='color: #FFFF00'> #TidyTuesday </span>
  <span style='color: #FFFF00'>Data:Cheese.com </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #FFFF00'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #FFFF00'>{masto_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #FFFF00'>{twitter_username}</span>")


# Wrangle ---------------------------------------------------------------------

data <- cheeses |> 
  select(cheese, vegan) |> 
  group_by(vegan) |> 
  summarise(n = n_distinct(cheese)) |> 
  drop_na()
    

data$image <- paste0("images/", data$vegan, ".png")

# Plot 📈 ---------------------------------------------------------------------

ggplot(data, aes(vegan, n)) +
  geom_col_pattern(aes(pattern_filename = image),
    pattern = 'image',
    pattern_type = 'tile', 
    fill = 'white',
    colour = 'black', 
    pattern_filter = 'box', 
    pattern_scale = 0.5) +
  scale_pattern_filename_discrete(choices = unique(data$image)) +
  theme_void() +
  theme(
        legend.position = "none",
        axis.title.y = element_blank(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA,
                                        fill = "#000000"),
        plot.background = element_rect(color = NA, 
                                       fill = "#000000"),
        plot.title = element_text(family = "Playball",
                                  size = 30,
                                  hjust = 0.5, 
                                  face = "bold", 
                                  color = "#FFFFFF", 
                                  margin = margin(b = 6)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Voltaire",
                                     size = 15, 
                                     hjust = 0.5,
                                     color = "#FFFFFF",
                                     margin = margin(b = 10)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 7, 
                                    color = "#FFFF00", 
                                    hjust = 0.5,  
                                    vjust = 1,   
                                    margin = margin(t = 0.5))) +
  geom_label(aes(label = n),
             hjust = 1,
             vjust = -0.02,
             color = "black", 
             fill = "white",
             size = 3.5, 
             ) +
  labs(caption = social_caption,
       title = paste0("More Types of Vegan Cheese, Please"),
       subtitle = paste0("According to data from <span style='color:#FFFF00'> Cheese.com </span>, there are over 700 varieties of non-vegan cheese,
                         the vegan alternative presents only a limited selection of six types.")) +
  theme(plot.subtitle = element_textbox_simple(),
    plot.caption = element_textbox_simple())



# Load Data 📊---------------------------------------------------------------

spells <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-17/spells.csv')


# Load Packages 📦 -----------------------------------------------------------

pacman::p_load(tidyverse, janitor, ggtext, fontawesome, showtext, sysfonts)


# Fonts 🔠  ------------------------------------------------------------------

font_add_google("Playball")
font_add_google("Playfair Display")

showtext_auto()


# Wrangle 🤠 ----------------------------------------------------------------


data <- spells |> 
  dplyr::select(wizard, school, range) |> 
  dplyr::filter(wizard == TRUE) |> 
  mutate(range = parse_number(range)) |> 
  mutate(school = str_to_title(school)) |> 
  filter(!is.na(range)) |> 
  group_by(school) |> 
  mutate(range = mean(range))


# Socials 💻 ---------------------------------------------------------------------

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "/Users/nicholasvietto/Documents/TidyTuesday/fontawe/otfs/Font Awesome 6 Brands-Regular-400.otf")

github_icon <- "&#xf09b"
github_username <- "nvietto"
mastodon_icon <- "&#xf4f6"
masto_username <- "nvietto"
bluesky_icon <- "&#xe671"
bluesky_username <- "@nvietto.bsky.social"


social_caption <- glue::glue(
  "<span style='color: #000000'> #TidyTuesday </span>
  <span style='color: #000000'>Data: Dungeons and Dragons Spells</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #000000'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #000000'>{masto_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{bluesky_icon};</span>
  <span style='color: #000000'>{bluesky_username}</span>")


# Plot 📈 -----------------------------------------------------------------


data |> 
  ggplot(aes(school, range)) +
  geom_col(fill = alpha("orange", 0.5)) +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(),
    plot.title = element_text(family = "Playball",
                              size = 20,
                              hjust = 0.5, 
                              face = "bold", 
                              color = "black", 
                              margin = margin(b = 10)),
    plot.subtitle = element_text(family = "Playfair Display",
                                 size = 10, 
                                 hjust = 1,
                                 color = "black"),
    plot.caption = element_text(size = 8)) +
  labs(caption = social_caption,
       title = paste0("Dungeons and Dragons Schools of Spells - Wizard's Need Distance"),
       subtitle = paste0("Evocation spells have the greatest average casting distance among all schools of magic for Wizards."))  +
  theme(plot.subtitle = element_textbox_simple(),
        plot.caption = element_textbox_simple())
  
    
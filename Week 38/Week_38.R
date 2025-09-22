# Load 📦 ---------------------------------------------------------------------

pacman::p_load(tidyverse, ggtext, fontawesome, showtext, sysfonts, skimr, ggbeeswarm, ggrepel)

# Load Data 📊---------------------------------------------------------------

fide_ratings_september <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv")

df <- fide_ratings_september

# Fonts 🔠  ------------------------------------------------------------------


font_add_google("MedievalSharp")
font_add_google("Metamorphous")
font_add_google("Skranji")
font_add_google("Playfair Display")

showtext_auto()

# Socials 💻 ---------------------------------------------------------------------

sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = "/Users/nicholasvietto/Documents/TidyTuesday/fontawe/otfs/Font Awesome 6 Brands-Regular-400.otf"
)

github_icon <- "&#xf09b"
github_username <- "nvietto"
mastodon_icon <- "&#xf4f6"
masto_username <- "nvietto"
bluesky_icon <- "&#xe671"
bluesky_username <- "@nvietto.bsky.social"


social_caption <- glue::glue(
  "<span style='color: #FFFFFF'> #TidyTuesday </span>
  <span style='color: #FFFFFF'>Data: FIDE (the International Chess Federation)</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #FFFFFF'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #FFFFFF'>{masto_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{bluesky_icon};</span>
  <span style='color: #FFFFFF'>{bluesky_username}</span>"
)



# Wrangle 🤠 ----------------------------------------------------------------

skim(df)

plot <- df |>
  dplyr::select(foa, rating) |>
  mutate(
    foa = factor(foa,
      levels = c("AGM", "AIM", "AFM", "ACM")
    )
  ) |>
  drop_na()


dim(plot)



# Plot 📈 ---------------------------------------------------------------------

plot |>
  ggplot(aes(x = rating, y = foa, color = foa)) +
  geom_beeswarm(size = 0.3, priority = "ascending",  shape = 25) +
  xlim(700, 2400) +
  scale_y_discrete(
    labels = c(
      "AGM" = "Arena Grand Master",
      "AIM" = "Arena International Master",
      "AFM" = "Arena FIDE Master",
      "ACM" = "Arena Candidate Master"
    )
  ) +
  geom_vline(xintercept = 1000, linetype = "solid", color = "#FF851B") +
  geom_label_repel(
    data = data.frame(x = 1000, foa = "AIM", label = "Not a Noob Anymore"),
    aes(x = x, y = foa, label = label),
    inherit.aes = FALSE,
    nudge_x = -265,
    nudge_y = 0.30,
    arrow = arrow(length = unit(0.015, "npc")),
    min.segment.length = 0,
    color = "white",
    family = "Playfair Display",
    fontface = "bold",
    size = 3.5,
    fill = "#FF851B"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(color = NA, fill = "#000000"),
    plot.background = element_rect(color = NA, fill = "#000000"),
    panel.grid.major = element_line(color = "gray40", size = 0.4),
    plot.title = element_text(
      family = "MedievalSharp", size = 16, hjust = 0.5,
      face = "bold", color = "#FFFFFF", margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "Metamorphous", size = 10, hjust = 0.5,
      face = "italic", color = "#FFFFFF",
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      size = 8, color = "#FFFFFF", hjust = 0.5, vjust = 1,
      margin = margin(t = 5)
    ),
    axis.text.y = element_text(
      family = "Skranji", 
      size = 6.5, color = "#FFFFFF",
      hjust = 1
    ),
    axis.text.x = element_text(
      family = "Skranji", size = 8, color = "#FFFFFF"
    ),
    plot.margin = unit(c(1, 1, 1, 0.1), "cm")
  ) +
  labs(
    caption  = social_caption,
    title    = "How Your Chess Rating (Elo) Measures Up: \n A Comparison of the 1000 Elo Milestone and Arena Master Levels",
    subtitle = " Reaching 1000 ELO marks progress past the <span style='color:#FF851B'>noob phase</span>, but when compared to the September FIDE Ratings, mastery still lies ahead."
  ) +
  theme(
    plot.subtitle = element_textbox_simple(margin = margin(b = 15)), 
    plot.caption  = element_textbox_simple()
  )





# References 📖 -----------------------------------------------------------

# Nicola Rennie "Economic Diversity and Student Outcomes" (https://github.com/nrennie/tidytuesday/tree/main/2024/2024-09-10)
# R Graph Gallery (https://r-graph-gallery.com/beeswarm.html)
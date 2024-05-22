

# Load 


pacman::p_load(janitor, tidyverse, ggHoriPlot, ggtext, showtext, sysfonts, fontawesome)

emissions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-21/emissions.csv')


# Fonts and Socials


font_add_google("Black Ops One")
font_add_google("Mulish")
showtext_auto()

sysfonts::font_add(family = "Font Awesome 6 Brands",
                   regular = "/Users/nicholasvietto/Documents/TidyTuesday/fontawe/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_auto()

github_icon <- "&#xf09b"
github_username <- "nvietto"
mastodon_icon <- "&#xf4f6"
masto_username <- "nvietto"
twitter_icon <- "&#xf099"
twitter_username <- "ViettoNicholas"

social_caption <- glue::glue(
  "<span style='color: #FFFFFF'>Data: Carbon Majors </span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{github_icon};</span>
  <span style='color: #FFFFFF'>{github_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{mastodon_icon};</span>
  <span style='color: #FFFFFF'>{masto_username}</span>
  <span style='font-family:\"Font Awesome 6 Brands\";'>{twitter_icon};</span>
  <span style='color: #FFFFFF'>{twitter_username}</span>")


# Wrangle

data <- emissions |> 
  dplyr::select(year, commodity, total_emissions_MtCO2e) |> 
  filter(year >= 1960, !(commodity %in% c("Cement", "Lignite Coal", "Thermal Coal", "Sub-Bituminous Coal")))|> 
  rename(tot_em = total_emissions_MtCO2e) |> 
  group_by(year, commodity) |> 
  drop_na() 


# Plot

data |> 
ggplot(aes(year, tot_em)) +
  geom_col(aes(fill = commodity), color = "honeydew", linewidth = .1) +
  facet_grid(commodity~.) +
  theme(
    strip.text = element_text(size = 6.3, face = "bold", color = "black", family = "Times")
  ) +
  scale_fill_hcl(palette = 'Blues,', reverse = T) +
  labs(caption = social_caption,
       title = paste0("Carbon Majors Emissions Data"),
       subtitle = paste0("Carbon Majors is a database of historical production data from 122 of the world’s largest
                         oil, gas, coal, and cement producers. This data is used to quantify the direct operational emissions 
                         and emissions from the combustion of marketed products that can be attributed to these entities.
                         <span style='color:#87CEEB'>Since the 1960s, the total emissions traced to Anthracite, Bituminous, Metallurgical Coal, Natural Gas,
                          Oil and NGL has been steadily increasing.</span>")) +
  theme(
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.title = element_blank(), 
        legend.position = 'none',
        panel.background = element_rect(fill = "black", colour = "black"),  
        plot.background = element_rect(fill = "black", colour = "black"), 
        panel.grid = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "white",
                                                     size = 20,
                                                     face = "bold",
                                                     family = "Black Ops One",
                                                     lineheight = 1.3,
                                                     margin = margin(b = 14)),
        plot.subtitle = ggtext::element_textbox_simple(color = "white",
                                                       family = "Mulish",
                                                       size = rel(1), 
                                                       lineheight = 1.0,
                                                       margin = margin(t = 3, b = 32)),
        plot.caption = ggtext::element_textbox_simple(color = "white",
                                                      family = "Mulish"))


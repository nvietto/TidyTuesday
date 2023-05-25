
# Load Packages ------------------------------------------------------------

pacman::p_load(tidyverse, tidytuesdayR, ggtext, ggridges, ggthemes, geomtextpath, janitor, scales, glue)

# Get the Data ---------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2023, week = 20)

tornados <- tuesdata$tornados

# Data wrangling ------------------------------------------------------------

data <- tornados %>% 
  select(yr, mo, st, mag) %>% 
  filter(yr == 2022) %>% 
  group_by(mo) %>% 
  filter(st == "NE"| st == "KS"| st == "IA"| st == "TX"| st == "OK"|
         st == "SD"| st == "LA"| st == "MS"| st == "AR") 
 

data$st = ifelse(data$st == "NE", "Nebraska", data$st)
data$st = ifelse(data$st == "KS", "Kansas", data$st)
data$st = ifelse(data$st == "IA", "Iowa", data$st)
data$st = ifelse(data$st == "TX", "Texas", data$st)
data$st = ifelse(data$st == "OK", "Oklahoma", data$st)
data$st = ifelse(data$st == "SD", "South Dakota", data$st)
data$st = ifelse(data$st == "LA", "Louisiana", data$st)
data$st = ifelse(data$st == "MS", "Missouri", data$st)
data$st = ifelse(data$st == "AR", "Arkansas", data$st)
# Plot  ------------------------------------------------------------


BasePlot <- data %>% 
  ggplot(aes(x = mo, y = st, fill = mag, label = st, color = st)) + 
  geom_density_ridges_gradient(scale = 1.4) +
  geom_textdensity(size = 3, rich =TRUE, fontface = 2, hjust = 0.05, vjust = - 2.5) +
  scale_x_continuous(breaks = pretty_breaks()) +
  coord_cartesian(xlim = c(1, 12)) +
  theme_economist() +
  theme(axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_rect(color = "black"),
        axis.ticks.x = element_blank())
                                        
BasePlot +
labs(caption = "Source: National Weather Service Strom Prediction Center | @ViettoNicholas",
     title = "Magnitude of Tornados within the Tornado Alley in 2022",
     subtitle = "In April, the magnitude for all states was the highest, with South Dakota having the Strongest Tornados") + 
  theme(text = element_text(),
        plot.title =  ggtext::element_textbox_simple(color = "black",
                                                     size = 20,
                                                     face = "bold",
                                                     family = "PT Sans Caption",
                                                     lineheight = 1.2,
                                                     margin = margin(0.7, 0, 1, 0, "lines")),
        plot.subtitle = ggtext::element_textbox_simple(family = "Courier ", size = rel(1.1), lineheight = 1.3,
                                                       margin = margin(0, 1, 1, 0, "lines"),
                                                       halign = 0)) 




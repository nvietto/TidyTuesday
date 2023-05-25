
# load libraries 
pacman::p_load(tidyverse, scales, ggtext)

remotes::install_github("nrennie/LondonMarathon")

data(winners, package = "LondonMarathon")
data(london_marathon, package = "LondonMarathon")

London <- merge(winners, london_marathon, by = "Year")


# Data manaage 

london_plot <- London %>%
  filter(Year <= 2020 & Year > 2000) %>%
  mutate(Year = factor(Year))


# Plot code

options(scipen = 999)

LP<- ggplot(
  data = london_plot,
  mapping = aes(x = Year)
) + geom_point(aes(y = Applicants),
             color = "#B4464B") + 
  geom_point(aes(y = Accepted),
             color = "#82B446") +
  labs(x = "Year",
       y = "Applicants and Applicants Accepted",
       title = "Since 2001, the number of applicants for the London Marathon
          have increased, while the number accepted has remained relatively the same,
       except for a decrease in 2020") +
  theme_minimal()
 
LP +  
  labs(title = 
         paste0("Since 2001, the number of <span style='color:#B4464B'> applicants</span> for the London Marathon
         have <span style='font-size:28pt'>increased</span>, while the number <span style='color:#82B446'>accepted</span> has
                remained relatively the same, except for a  <span style='font-size:14pt'>decrease</span> in 2020")) + 
  theme(text = element_text(color = "#4682B4"),
           plot.title =  ggtext::element_textbox_simple(color = "black",
                                     size = 20,
                                     face = "bold",
                                     family = "PT Sans Caption",
                                     lineheight = 1.3,
                                     margin = margin(1, 0, 2, 0, "lines")))




library(tidyverse)
library(lubridate)
library(readxl)

# takes the information downloaded from mesonet, save as newscoop.xlsx

green_cd <- read_excel("./nwscoop.xlsx")

green_cd <- green_cd %>% 
  mutate(Av_Soil_T = (Soil_T - 32)*5/9) %>% 
  mutate(Av_Air_T = (Air_T - 32)*5/9)

class(green_cd$day)
green_cd$day <- as.Date(green_cd$day)

# Varaibles
# fdoy : fisrt day of the year
# ep : early planting
# np : normal planting
# pds: planting dates

# Site
loc <- "Greenfield (IA)"

# Dates
fdoy <- "2021-01-01"
ep <- 106
np <- 154
pds <- "Early Planting Date: April 16th\nNormal Planting Date: June 3rd"
plot_title <- "Average Soil & Air Temperatures"

green_cd %>% pivot_longer(c(Av_Soil_T, Av_Air_T), names_to = "Profile") %>% 
  ggplot(aes(x = day, y = value)) +
  # Planting window
  geom_rect(aes(xmin = as.Date.numeric(140, origin = fdoy), xmax = as.Date.numeric(155, origin = fdoy), 
                ymin = -Inf, ymax = Inf), fill = 'limegreen', alpha = 0.002) +
  annotate('text', x = as.Date.numeric(148, origin = fdoy), y = 34.5, 
           label = "Regular\nPlanting\nWindow (IA)", color = "darkgreen")  +

  # Average temperature
  geom_point(aes(color = Profile)) + 
  geom_line(aes(color = Profile)) +
  geom_line(y = 12, linetype = 3, size = 1.25) +
  annotate('text', x = as.Date.numeric(165, origin = fdoy), y = 10, 
           label = "Minimun\nPlanting\nTemperature") +
  
  # Early planting features
  geom_vline(xintercept = as.Date.numeric(ep, origin = fdoy), linetype = 1, size = 1) +
  annotate('text', x = as.Date.numeric((ep + 1), origin = fdoy), y = 35, 
           label = "Early\nplanting Date", hjust = 0) +
  
  # Normal planting features
  geom_vline(xintercept = as.Date.numeric(np, origin = fdoy), linetype = 1, size = 1) +
  annotate('text', x = as.Date.numeric((np + 1), origin = fdoy), y = 35, 
           label = "Normal\nPlanting Date", hjust = 0) +
  # Labels
  theme_minimal() +
  xlab(NULL) +
  ylab("Temperature [°C]") +
  labs(title = plot_title,
       subtitle = paste0(loc),
       caption = paste0(pds, "\nSource: https://mesonet.agron.iastate.edu/"))

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

head(green_cd)

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

# Features
plot_title <- "Average soil temperature"
level <- "10 cm/4 in"


# Soil temperature
soil_temp <-
ggplot(green_cd, aes(x = day)) +
  # Planting window
  geom_rect(aes(xmin = as.Date.numeric(140, origin = fdoy), xmax = as.Date.numeric(155, origin = fdoy), 
                ymin = -Inf, ymax = Inf), fill = 'limegreen', alpha = 0.002) +
  annotate('text', x = as.Date.numeric(148, origin = fdoy), y = 34, 
           label = "Regular\nPlanting\nWindow (IA)", color = "darkgreen") +
  
  # Average temperature
  geom_point(aes(y = Av_Soil_T), color = "red") + 
  geom_line(aes(y = Av_Soil_T), color = "red") +
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
  xlab(NULL) +
  ylab("Temperature [°C]") +
  labs(title = plot_title,
      subtitle = paste0(loc, " - ", level),
      caption = paste0(pds, "\nSource: https://mesonet.agron.iastate.edu/"))
  
soil_temp  


# Air temperature  
# Features
plot_title <- "Average air temperature"

air_temp <-
ggplot(green_cd, aes(x = day)) +
  
  # Planting Windows
  geom_rect(aes(xmin = as.Date.numeric(140, origin = fdoy), xmax = as.Date.numeric(155, origin = fdoy), 
                ymin = -Inf, ymax = Inf), fill = 'limegreen', alpha = 0.002) +
  annotate('text', x = as.Date.numeric(148, origin = fdoy), y = 34, 
           label = "Regular\nPlanting\nWindows (IA)", color = "darkgreen") +
  
  # Recoreded Temperature
  geom_point(aes(y = Av_Air_T), color = "red") + 
  geom_line(aes(y = Av_Air_T), color = "red") + 
  
  # Base Temperature
  geom_line(y = 10, linetype = 3, size = 1.25) +
  annotate('text', x = as.Date.numeric(165, origin = fdoy), y = 7, 
           label = "Base\nTemperature") +
  # Early planting
  geom_vline(xintercept = as.Date.numeric(ep, origin = fdoy), linetype = 1, size = 1) +
  annotate('text', x = as.Date.numeric((ep + 1), origin = fdoy), y = 35, 
           label = "Early\nplanting date", hjust = 0) +
  # Normal Planting
  geom_vline(xintercept = as.Date.numeric(np, origin = fdoy), linetype = 1, size = 1) +
  annotate('text', x = as.Date.numeric((np + 1), origin = fdoy), y = 35, 
           label = "Normal\nPlanting Date", hjust = 0) +
  xlab(NULL) +
  ylab("Temperature [°C]") +
  labs(title = plot_title, 
       subtitle = loc,
       caption = paste0(pds, "\nSource: https://mesonet.agron.iastate.edu/"))


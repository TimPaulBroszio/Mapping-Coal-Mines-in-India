# Coal Mines in India - Visualization 
# Author: Tim Paul Broszio, M.A., M.A.
# FernUniversität in Hagen
# Department of International Politics


# Setup----

Sys.Info()

sessionInfo()

Sys.setenv(language = "en")

rm(list=ls())

pac <- "pacman" %in% rownames(installed.packages())
if(!pac) install.packages("pacman")
library(pacman)
rm(pac)
p_load("tidyverse", "haven", "readxl", "ggmap", "sf", "sp",
       "cartography", "giscoR", "countrycode", "maptools", 
       "rgeos", "scales", "RColorBrewer", "rgdal", "maps", 
       "broom", "geojsonio", "forcats", "viridis")



# Import Data-----

## Coal Mines India

file.choose()

df <- read_xlsx("C:\\Sonstige Unterlagen\\R Projekte\\Datenvisualisierung\\India Coal Mining Industry\\Indian Coal Mines Dataset_January 2021-1.xlsx", 
                sheet = 2, col_names = TRUE)

# Data Structure

str(df)

df <- as.data.frame(df)

head(df)


## Choropleth Map with GGplot2
# https://r-graph-gallery.com/327-chloropleth-map-from-geojson-with-ggplot2.html

file.choose()


IND <- geojson_read("C:\\Sonstige Unterlagen\\R Projekte\\Datenvisualisierung\\India Coal Mining Industry\\gadm41_IND_1.json",  what = "sp")

IND1 <- broom::tidy(IND, region = "NAME_1")



## Manipulate Data----


# split id column between upper letters to get the complete name of indian states

IND1$id <- gsub('([[:upper:]])', ' \\1', IND1$id)

names(df)[2] <- "State"


# coal mine data

N <- df %>% 
  count(State)

names(N)[2] <- "Count"

N$Count <- as.numeric(N$Count)

# "merge" both data 

India <- IND1 %>%
  mutate(Mines = case_when(id == " Assam" ~ "3", 
                           id == " Tamil Nadu" ~ "3", 
                           id == " Uttar Pradesh" ~ "5",
                           id == " Rajasthan" ~ "7", 
                           id == " Gujarat" ~ "9", 
                           id == " Orissa" ~ "29",
                           id == " Chhattisgarh" ~ "52",
                           id == " Maharashtra" ~ "54",
                           id == " Madhya Pradesh" ~ "56",
                           id == " Telangana" ~ "57",
                           id == " West Bengal" ~ "70",
                           id == " Jharkhand" ~ "114",
                           TRUE ~ "NA"))


str(India)

India$Mines <- as.numeric(India$Mines)

save(India, file = "India.Rda")

## Visualization Frequency and Geodata-----


# Frequency

P1 <- ggplot(N, aes(x = reorder(State, -Count), Count, group = State)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() + 
  labs(title = "Coal Mines per Indian States", 
       x = "", 
       y = "", 
       caption = "Source: Pai, S; Zerriffi, H; Kaluarachchi, S, 2021; 
       Pai, S & Zerriffi, H. 2021") +
  theme_bw()


P1


# mapping geodata



P3 <- ggplot() +
  geom_polygon(data = India, aes(fill = Mines, x = long, y = lat, group = group)) +
  theme_void() +
  coord_map()


P3


# create a customized palette


pal1 <- c("#e7f8f7","#cef1ef", "#b6eae7", "#9de3df", "#85dcd7", "#6cd4ce", 
          "#54cdc6", "#3bc6be", "#23bfb6", "#0ab8ae", "#c5caca")



# customize mapping

P3 <- ggplot() +
  geom_polygon(data = India, aes(fill = factor(Mines), x = long, y = lat, group = group)) +
  theme_void() +
  scale_fill_manual(values = pal1, breaks = waiver(),
                    name = "Number of Mines", 
                    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), 
                                         label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "Coal Mine Concentration",
    subtitle = "Number of Coal Mines per Indian States",
    caption = "NA = No information available | Data: Pai, S; Zerriffi, H; Kaluarachchi, S, 2021; 
Pai, S & Zerriffi, H. 2021 | Creation: Tim Paul Broszio"
  ) +
  theme(
    text = element_text(family = "Halis" ,color = "#1e1450"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 19, hjust=0.01, color = "#1e1450", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 15, hjust=0.01, color = "#1e1450", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size= 9, color = "#1e1450", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.title = element_text(size = 8, face = "bold"), 
    legend.position = c(0.5, 0.04), 
    legend.key.width = unit(4, "cm"), 
    ) +
  coord_map(projection = "mercator")

P3



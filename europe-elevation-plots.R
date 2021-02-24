library(here)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(geofacet)

# Load and reshape data to long format
df <- read_csv(here("elevations-europe.csv"))
df <- df %>%
  pivot_longer(
    cols = c(lowest, highest),
    names_to = "position",
    values_to = "elevation"
  )

# Encode lowest-highest category numerically
df$pos <- 1
df$pos[df$position == "lowest"] <- 0

grid_preview("europe_countries_grid2")

# Plot an areal slope graph with sky and a custom theme
theme <- theme_void(base_size = 7) +
  theme(plot.tag = element_text(
    size = rel(3),
    lineheight = 1.15,
    hjust = 0,
    face = "bold"
  )) +
  theme(plot.caption = element_text(size = rel(0.75))) +
  theme(plot.margin = margin(.6, .6, .6, .6, "cm")) +
  theme(strip.text.x = element_text(face = "bold")) +
  theme(strip.text.x = element_text(margin = margin(0, 0, .03, 0, "cm"))) +
  theme(plot.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"))

r <- ggplot(df, aes(x = pos, y = max(df$elevation))) +
  geom_area(fill = "#99E9FF") + # sky
  geom_area(aes(x = pos, y = elevation), fill = "#E6B63C") + # ground
  facet_geo(~ country, grid = "europe_countries_grid2") +
  theme +
  labs(tag = "Minimum and maximum\nelevations in Europe",
       caption = "\n@rastrau, idea by @philshem") +
  theme(plot.tag.position = c(0.0, 0.045))

ggsave(here("europe-elevations.png"), r, width = 21.8, height = 19.3, units = "cm")


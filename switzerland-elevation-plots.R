library(here)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(geofacet)

# Load and reshape data to long format
df <- read_csv(here("elevations-switzerland.csv"))
df <- df %>%
  pivot_longer(
    cols = c(lowest, highest, mean),
    names_to = "position",
    values_to = "elevation"
  )

# Encode lowest-highest category numerically
df$pos <- 1
df$pos[df$position == "lowest"] <- 0
df$pos[df$position == "mean"] <- 0.5


# Fix an unwanted offset in the cantons grid
ch_cantons_grid2$row <- ch_cantons_grid2$row - 1

# First attempt: Plot a linear slope graph
p <- ggplot(df, aes(x = pos, y = elevation)) +
  geom_point() +
  geom_line() +
  facet_geo(~ canton, grid = "ch_cantons_grid2") +
  theme_minimal()

ggsave(here("attempt-1.png"), p, width = 25, height = 19.3, units = "cm")

# Second attempt:Plot an areal slope graph
q <- ggplot(df, aes(x = pos, y = elevation)) +
  geom_area() +
  facet_geo( ~ canton, grid = "ch_cantons_grid2") +
  theme_minimal()

ggsave(here("attempt-2.png"), q, width = 25, height = 19.3, units = "cm")

# Third, finale version: Plot an areal slope graph with sky and a custom theme
theme <- theme_void(base_size = 12) +
  theme(plot.tag = element_text(
    size = rel(1.5),
    lineheight = 1.15,
    hjust = 0,
    face = "bold"
  )) +
  theme(plot.caption = element_text(size = rel(0.75))) +
  theme(plot.margin = margin(.6, .6, .6, .6, "cm")) +
  theme(strip.text.x = element_text(face = "bold")) +
  theme(plot.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"))

r <-
  ggplot(filter(df, position != "mean"), aes(x = pos, y = max(df$elevation))) +
  geom_area(fill = "#99E9FF") + # sky
  geom_area(aes(x = pos, y = elevation), fill = "#E6B63C") + # ground
  facet_geo( ~ canton, grid = "ch_cantons_grid2") +
  theme +
  labs(tag = "Minimum and\nmaximum\nelevations",
       caption = "\n@rastrau, idea by @philshem") +
  theme(plot.tag.position = c(0, 0.935))

ggsave(here("switzerland-elevations.png"), r, width = 25, height = 19.3, 
       units = "cm")

r <- ggplot(df, aes(x = pos, y = max(df$elevation))) +
  geom_area(fill = "#99E9FF") + # sky
  geom_area(aes(x = pos, y = elevation), fill = "#E6B63C") + # ground
  facet_geo( ~ canton, grid = "ch_cantons_grid2") +
  theme +
  labs(tag = "Minimum, mean,\nand maximum\nelevations",
       caption = "\n@rastrau, idea by @philshem") +
  theme(plot.tag.position = c(0, 0.935))

ggsave(here("switzerland-elevations-with-mean.png"), r, width = 25, 
       height = 19.3, units = "cm")

   





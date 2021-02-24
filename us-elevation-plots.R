library(here)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(geofacet)

# Load and reshape data to long format
df <- read_csv(here("elevations-us.csv"))
df <- df %>%
  select(-c(lowest_ft, mean_ft, highest_ft)) %>%
  pivot_longer(
    cols = c(lowest_m, mean_m, highest_m),
    names_to = "position",
    values_to = "elevation"
  )

df$state_territory[df$state_territory == "District of Columbia"] <-
  "D.C."
df$state_territory <- str_replace_all(df$state_territory,
                                      "Northern Mariana Islands",
                                      "Mariana Isl.")
df$state_territory <- str_replace_all(df$state_territory, "North", "N.")
df$state_territory <- str_replace_all(df$state_territory, "South", "S.")
df$state_territory <- str_replace_all(df$state_territory, "Islands", "Isl.")
df$state_territory <- str_replace_all(df$state_territory, "Island", "Isl.")

# Encode lowest-highest category numerically
df$pos <- 1
df$pos[df$position == "mean_m"] <- 0.5
df$pos[df$position == "lowest_m"] <- 0


us_state_grid <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4,
          4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7,
          7, 7, 8, 8, 8, 9, 9, 9, 9),
  col = c(13, 14, 1, 13, 12, 11, 13, 12, 11, 10, 8, 7, 6, 5, 4, 3, 2, 9, 11,
          10, 8, 7, 6, 5, 4, 3, 2, 9, 10, 8, 7, 6, 5, 4, 3, 2, 9, 10, 8, 7, 6,
          5, 4, 3, 9, 6, 5, 9, 1, 10, 5, 1, 3, 13, 14),
  code = c("NH", "ME", "AK", "MA", "VT", "NY", "RI", "CT", "NJ", "PA", "MI",
           "WI", "MN", "ND", "MT", "ID", "WA", "OH", "DE", "MD", "IN", "IL",
           "IA", "SD", "WY", "UT", "OR", "WV", "DC", "KY", "TN", "MO", "NE",
           "CO", "NV", "CA", "VA", "NC", "AL", "MS", "AR", "KS", "NM", "AZ",
           "SC", "LA", "OK", "GA", "-", "FL", "TX", "-", "HI", "-", "-"),
  name = c("New Hampshire", "Maine", "Alaska", "Massachusetts", "Vermont",
           "New York", "Rhode Isl.", "Connecticut", "New Jersey",
           "Pennsylvania", "Michigan", "Wisconsin", "Minnesota", "N. Dakota",
           "Montana", "Idaho", "Washington", "Ohio", "Delaware", "Maryland",
           "Indiana", "Illinois", "Iowa", "S. Dakota", "Wyoming", "Utah",
           "Oregon", "West Virginia", "D.C.", "Kentucky", "Tennessee",
           "Missouri", "Nebraska", "Colorado", "Nevada", "California",
           "Virginia", "N. Carolina", "Alabama", "Mississippi", "Arkansas",
           "Kansas", "New Mexico", "Arizona", "S. Carolina", "Louisiana",
           "Oklahoma", "Georgia", "Mariana Isl.", "Florida", "Texas", "Guam",
           "Hawaii", "Puerto Rico", "Virgin Isl."),
  stringsAsFactors = FALSE
)

# First attempt:Plot a linear slope graph
p <- ggplot(df, aes(x = pos, y = elevation)) +
  geom_point() +
  geom_line() +
  facet_geo( ~ state_territory, grid = us_state_grid) +
  theme_minimal(base_size = 7.8)

ggsave(here("us-attempt-1.png"), p, width = 25, height = 19.3, units = "cm")

# Second attempt: Plot an areal slope graph
q <- ggplot(df, aes(x = pos, y = elevation)) +
  geom_area() +
  facet_geo( ~ state_territory, grid = us_state_grid) +
  theme_minimal(base_size = 7.8)

ggsave(here("us-attempt-2.png"), q, width = 25, height = 19.3, units = "cm")

# Third, final version: Plot an areal slope graph with sky and a custom theme
theme <- theme_void(base_size = 7) +
  theme(plot.tag = element_text(
    size = rel(3),
    lineheight = 1.15,
    hjust = 0,
    face = "bold"
  )) +
  theme(plot.caption = element_text(size = rel(0.75))) +
  theme(plot.margin = margin(.6, .6, .6, .6, "cm")) +
  #theme(strip.text.x = element_text(face = "bold")) +
  theme(strip.text.x = element_text(margin = margin(0, 0, .03, 0, "cm"))) +
  theme(plot.background = element_rect(fill = "#F8F8F8", colour = "#F8F8F8"))

r <- ggplot(filter(df, position != "mean_m"), aes(x = pos,
                                                  y = max(df$elevation,
                                                          na.rm = TRUE))) +
  geom_area(fill = "#99E9FF") + # sky
  geom_area(aes(x = pos, y = elevation), fill = "#E6B63C") + # ground
  facet_geo( ~ state_territory, grid = us_state_grid) +
  theme +
  labs(tag = "US minimum and maximum elevations",
       caption = "\n@rastrau, idea by @philshem") +
  theme(plot.tag.position = c(0.178, 0.984))

ggsave(here("us-elevations.png"), r, width = 25, height = 19.3, units = "cm")

r <- ggplot(df, aes(x = pos, y = max(df$elevation, na.rm = TRUE))) +
  geom_area(fill = "#99E9FF") + # sky
  geom_area(aes(x = pos, y = elevation), fill = "#E6B63C") + # ground
  facet_geo( ~ state_territory, grid = us_state_grid) +
  theme +
  labs(tag = "US minimum, mean, and maximum elevations",
       caption = "\n@rastrau, idea by @philshem") +
  theme(plot.tag.position = c(0.124, 0.984))

ggsave(here("us-elevations-with-mean.png"), r, width = 25, height = 19.3, 
       units = "cm")
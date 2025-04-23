library(tidyverse)
library(ggplot2)
library(janitor)
library(scales)
library(dplyr)
library(usmap)
data2024 <- YEAR_04_DATA_PUF |> clean_names()
view(data2024)

nonprofit_with_region <- YEAR_04_DATA_PUF |>
  group_by(CensusRegion9, ntmaj12) |>
  summarise(weighted_count = sum(year4wt, na.rm = TRUE), .groups = "drop") |>
  group_by(CensusRegion9) |>
  mutate(percent = 100 * weighted_count / sum(weighted_count)) |>
  ungroup()
view(nonprofit_with_region)

# defining our custom geo data frame 
custom_regions <- data.frame(
  state = c("WA", "OR", "CA", "HI", "AK",   # pacific 9
            "ID", "MT", "WY", "CO", "UT", "NV", "AZ", "NM",    # mountain 8
            "ND", "SD", "MN", "IA", "MO", "NE", "KS",          # west north central 4
            "TX", "OK", "AR", "LA",                            # west south central 7
            "KY", "TN", "AL", "MS",                            # east south central 6
            "WI", "MI", "OH", "IN", "IL",                      # east north central 3
            "NY", "PA", "NJ",                                  # middle atlantic 2
            "WV", "MD", "DC", "DE", "VA", "NC", "SC", "GA", "FL", # south atlantic 5
            "VT", "NH", "MA", "CT", "RI", "ME"),               # new england 1
  region = c(rep(9, 5),
             rep(8, 8),
             rep(4, 7),
             rep(7, 4),
             rep(6, 4),
             rep(3, 5),
             rep(2, 3),
             rep(5, 9),
             rep(1, 6))
)

# Filtering for one type
ntmaj12_type <- "PU"

type_by_region <- nonprofit_with_region |>
  filter(ntmaj12 == ntmaj12_type) |>
  select(CensusRegion9, percent) |>
  rename(region = CensusRegion9) |>
  mutate(region = as.numeric(region))

# joining with state-level data
map_data <- custom_regions |>
  left_join(type_by_region, by = "region")

# GOOD PLOT of % of PU nonprofits by census region
pu_map <- plot_usmap(data = map_data, values = "percent", regions = "states") +
  scale_fill_continuous(
    name = paste("Percent:", ntmaj12_type),
    low = "#e0f3db", high = "#0868ac", labels = percent_format(scale = 1)
  ) +
  labs(
    title = paste("Percent of", ntmaj12_type, "Nonprofits by Census Region"),
    subtitle = "Using 9-Region Census Division",
    fill = "% of Total"
  ) +
  theme_minimal()
########
#### now comparing all of the types 
region_summary_all <- nonprofit_with_region |>
  rename(region = CensusRegion9) |>
  mutate(region = as.numeric(region))

map_data_all <- custom_regions |>
  full_join(region_summary_all, by = "region", relationship = "many-to-many") |>
  mutate(
    percent_label = paste0(round(percent, 1), "%"),
    count_label = paste0("n=", round(weighted_count))
  )

##### GOOD MAP distribution of all types 
compare_map <- plot_usmap(data = map_data_all, values = "percent", regions = "states") +
  facet_wrap(~ntmaj12) +
  scale_fill_gradientn(
    colors = c("#edf8e9", "#bae4b3", "#74c476", "#238b45", "#00441b"),
    values = scales::rescale(c(0, 5, 10, 15, 20)),  # adjust breaks to your data range
    name = "Percent",
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Distribution of Nonprofit Types by Census Region",
    subtitle = "Faceted by Nonprofit Type (`ntmaj12`)",
    fill = "% in Region"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

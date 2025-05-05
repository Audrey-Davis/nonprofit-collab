library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
data24 <- read_csv("YEAR-04-DATA-PUF.csv")

### LOCAL FUNDING
local_nonseek_receive <- data24 |>
  filter(FndRaise_LocGvtGrnt_Seek == 0, FndRaise_LocGvtGrnt_Rcv == 1)

local_nonseek_receive <- local_nonseek_receive |>
  mutate(sector_group = recode(ntee1,
                               A = "I.", B = "II.", C = "III.", D = "III.",
                               E = "IV.", F = "IV.", G = "IV.", H = "IV.",
                               I = "V.", J = "V.", K = "V.", L = "V.",
                               M = "V.", N = "V.", O = "V.", P = "V.",
                               Q = "VI.", R = "VII.", S = "VII.", T = "VII.",
                               U = "VII.", V = "VII.", W = "VII.",
                               X = "VIII.", Y = "IX.", Z = "X."))

sector_labels <- c(
  "I." = "Arts, Culture, and Humanities",
  "II." = "Education",
  "III." = "Environment and Animals",
  "IV." = "Health",
  "V." = "Human Services",
  "VI." = "International, Foreign Affairs",
  "VII." = "Public, Societal Benefit",
  "VIII." = "Religion Related",
  "IX." = "Mutual/Membership Benefit",
  "X." = "Unknown, Unclassified"
)

local_props <- local_nonseek_receive %>%
  count(sector_group) %>%
  mutate(proportion = n / sum(n))

local_rec_noseek <- ggplot(local_nonseek_receive, aes(x = sector_group)) +
  geom_bar(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  labs(
    title = "Nonprofits That Received Local Gov Grants Without Seeking",
    x = "Sector",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### STATE FUNDING
state_nonseek_receive <- data24 |>
  filter(FndRaise_StateGvtGrnt_Seek == 0, FndRaise_StateGvtGrnt_Rcv == 1)

state_nonseek_receive <- state_nonseek_receive |>
  mutate(sector_group = recode(ntee1,
                               A = "I.", B = "II.", C = "III.", D = "III.",
                               E = "IV.", F = "IV.", G = "IV.", H = "IV.",
                               I = "V.", J = "V.", K = "V.", L = "V.",
                               M = "V.", N = "V.", O = "V.", P = "V.",
                               Q = "VI.", R = "VII.", S = "VII.", T = "VII.",
                               U = "VII.", V = "VII.", W = "VII.",
                               X = "VIII.", Y = "IX.", Z = "X."))
state_rec_noseek <- ggplot(state_nonseek_receive, aes(x = sector_group)) +
  geom_bar(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  labs(
    title = "Nonprofits That Received State Gov Grants Without Seeking",
    x = "Sector",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### FEDERAL funding 
fed_nonseek_receive <- data24 |>
  filter(FndRaise_FedGvtGrnt_Seek == 0, FndRaise_FedGvtGrnt_Rcv == 1)

fed_nonseek_receive <- fed_nonseek_receive |>
  mutate(sector_group = recode(ntee1,
                               A = "I.", B = "II.", C = "III.", D = "III.",
                               E = "IV.", F = "IV.", G = "IV.", H = "IV.",
                               I = "V.", J = "V.", K = "V.", L = "V.",
                               M = "V.", N = "V.", O = "V.", P = "V.",
                               Q = "VI.", R = "VII.", S = "VII.", T = "VII.",
                               U = "VII.", V = "VII.", W = "VII.",
                               X = "VIII.", Y = "IX.", Z = "X."))
fed_rec_noseek <- ggplot(fed_nonseek_receive, aes(x = sector_group)) +
  geom_bar(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  labs(
    title = "Nonprofits That Received Federal Gov Grants Without Seeking",
    x = "Sector",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### ALL COMBINED
local_nonseek_receive <- local_nonseek_receive |> mutate(level = "Local")
state_nonseek_receive <- state_nonseek_receive |> mutate(level = "State")
fed_nonseek_receive <- fed_nonseek_receive |> mutate(level = "Federal")

combined_nonseek_receive <- bind_rows(local_nonseek_receive, state_nonseek_receive, fed_nonseek_receive)

noseek_combined <- ggplot(combined_nonseek_receive, aes(x = sector_group, fill = level)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(labels = sector_labels) +
  labs(
    title = "Nonprofits That Received Gov Grants Without Seeking",
    x = "Sector",
    y = "Count",
    fill = "Gov Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### MAP
custom_regions <- data.frame(
  state = c("WA", "OR", "CA", "HI", "AK",
            "ID", "MT", "WY", "CO", "UT", "NV", "AZ", "NM",
            "ND", "SD", "MN", "IA", "MO", "NE", "KS",
            "TX", "OK", "AR", "LA",
            "KY", "TN", "AL", "MS",
            "WI", "MI", "OH", "IN", "IL",
            "NY", "PA", "NJ",
            "WV", "MD", "DC", "DE", "VA", "NC", "SC", "GA", "FL",
            "VT", "NH", "MA", "CT", "RI", "ME"),
  region = c(rep("9", 5),
             rep("8", 8),
             rep("4", 7),
             rep("7", 4),
             rep("6", 4),
             rep("3", 5),
             rep("2", 3),
             rep("5", 9),
             rep("1", 6))
)
region_labels <- c(
  "1" = "New England",
  "2" = "Middle Atlantic",
  "3" = "East North Central",
  "4" = "West North Central",
  "5" = "South Atlantic",
  "6" = "East South Central",
  "7" = "West South Central",
  "8" = "Mountain",
  "9" = "Pacific"
)

# Merge counts into the custom regions
custom_regions <- custom_regions %>%
  mutate(region = as.numeric(region)) # make sure numeric for join

local_map_data <- custom_regions %>%
  left_join(local_region_counts, by = c("region" = "CensusRegion9")) %>%
  mutate(n = replace_na(n, 0)) # replace NAs with 0

# LOCAL. GOOD MAP 
local_noseek_plot <- plot_ly(
  data = local_map_data,
  type = 'choropleth',
  locations = ~state,   # states two-letter abbreviation
  locationmode = 'USA-states',
  z = ~n,               # the counts
  text = ~paste(state, "<br>Nonprofits:", n),  # hover text
  colorscale = 'RdPu',
  colorbar = list(title = "Nonprofits"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Nonprofits Receiving Local Gov \nGrants Without Seeking (by Region)",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

############
#####
local_region_counts <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 0, FndRaise_LocGvtGrnt_Rcv == 1) %>%
  count(CensusRegion9) %>%
  mutate(funding_type = "Local")

state_region_counts <- data24 %>%
  filter(FndRaise_StateGvtGrnt_Seek == 0, FndRaise_StateGvtGrnt_Rcv == 1) %>%
  count(CensusRegion9) %>%
  mutate(funding_type = "State")

fed_region_counts <- data24 %>%
  filter(FndRaise_FedGvtGrnt_Seek == 0, FndRaise_FedGvtGrnt_Rcv == 1) %>%
  count(CensusRegion9) %>%
  mutate(funding_type = "Federal")

region_counts_all <- bind_rows(local_region_counts, state_region_counts, fed_region_counts)

### STATE MAP
custom_regions <- custom_regions %>%
  mutate(region = as.numeric(region))

state_region_counts_weighted <- data2024 |>
  group_by(census_region9) |>
  summarise(n = sum(year4wt, na.rm = TRUE))

state_map_data <- custom_regions %>%
  left_join(state_region_counts_weighted, by = c("region" = "census_region9")) %>%
  mutate(n = replace_na(n, 0))

state_noseek_plot <- plot_ly(
  data = state_map_data,
  type = 'choropleth',
  locations = ~state,   # states two-letter abbreviation
  locationmode = 'USA-states',
  z = ~n,               # the counts
  text = ~paste(state, "<br>Nonprofits:", n),  # hover text
  colorscale = 'RdPu',
  colorbar = list(title = "Nonprofits"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Nonprofits Receiving State Gov \nGrants Without Seeking (by Region)",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
###### FED MAP
fed_map_data <- custom_regions %>%
  left_join(fed_region_counts, by = c("region" = "CensusRegion9")) %>%
  mutate(n = replace_na(n, 0))

fed_noseek_plot <- plot_ly(
  data = fed_map_data,
  type = 'choropleth',
  locations = ~state,   # states two-letter abbreviation
  locationmode = 'USA-states',
  z = ~n,               # the counts
  text = ~paste(state, "<br>Nonprofits:", n),  # hover text
  colorscale = 'RdPu',
  colorbar = list(title = "Nonprofits"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Nonprofits Receiving Federal Gov \nGrants Without Seeking (by Region)",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
### These all have weights factored in 
###### PROPORTION MAPS 
# Local 
total_local_region_counts <- data24 %>%
  group_by(CensusRegion9) %>%
  summarise(total_n = sum(year4wt, na.rm = TRUE))

local_region_counts_pro <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 0, FndRaise_LocGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

local_region_props <- left_join(local_region_counts_pro, total_local_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

local_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(local_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

# PLOT
local_noseek_plot_pro <- plot_ly(
  data = local_map_data,
  type = 'choropleth',
  locations = ~state,
  locationmode = 'USA-states',
  z = ~proportion,
  text = ~paste(state, "<br>Proportion:", scales::percent(proportion)),
  colorscale = 'RdPu',
  colorbar = list(title = "Proportion"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Proportion of Nonprofits Receiving Local \nGov Grants Without Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

#### State 
total_state_region_counts <- data24 %>%
  group_by(CensusRegion9) %>%
  summarise(total_n = sum(year4wt, na.rm = TRUE))

state_region_counts_pro <- data24 %>%
  filter(FndRaise_StateGvtGrnt_Seek == 0, FndRaise_StateGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

state_region_props <- left_join(state_region_counts_pro, total_state_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

state_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(state_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

# PLOT
state_noseek_plot_pro <- plot_ly(
  data = state_map_data,
  type = 'choropleth',
  locations = ~state,
  locationmode = 'USA-states',
  z = ~proportion,
  text = ~paste(state, "<br>Proportion:", scales::percent(proportion)),
  colorscale = 'RdPu',
  colorbar = list(title = "Proportion"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Proportion of Nonprofits Receiving State \nGov Grants Without Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
#### Federal
total_fed_region_counts <- data24 %>%
  group_by(CensusRegion9) %>%
  summarise(total_n = sum(year4wt, na.rm = TRUE))

fed_region_counts_pro <- data24 %>%
  filter(FndRaise_FedGvtGrnt_Seek == 0, FndRaise_FedGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

fed_region_props <- left_join(fed_region_counts_pro, total_fed_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

fed_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(fed_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))
# PLOT
fed_noseek_plot_pro <- plot_ly(
  data = fed_map_data,
  type = 'choropleth',
  locations = ~state,
  locationmode = 'USA-states',
  z = ~proportion,
  text = ~paste(state, "<br>Proportion:", scales::percent(proportion)),
  colorscale = 'RdPu',
  colorbar = list(title = "Proportion"),
  marker = list(line = list(color = 'white', width = 1))
) %>%
  layout(
    title = "Proportion of Nonprofits Receiving Federal \nGov Grants Without Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
##
#### PROPORTION BAR GRAPHS
local_props <- local_nonseek_receive %>%
  group_by(sector_group) %>%
  summarise(weighted_n = sum(year4wt, na.rm = TRUE)) |>
  mutate(proportion = weighted_n / sum(weighted_n))

local_rec_noseek <- ggplot(local_props, aes(x = sector_group, y = proportion)) +
  geom_col(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Nonprofits Receiving Local Gov Grants Without Seeking",
    x = "Sector",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

state_props <- state_nonseek_receive %>%
  group_by(sector_group) %>%
  summarise(weighted_n = sum(year4wt, na.rm = TRUE)) |>
  mutate(proportion = weighted_n / sum(weighted_n))

state_rec_noseek <- ggplot(state_props, aes(x = sector_group, y = proportion)) +
  geom_col(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Nonprofits Receiving State Gov Grants Without Seeking",
    x = "Sector",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fed_props <- fed_nonseek_receive %>%
  group_by(sector_group) %>%
  summarise(weighted_n = sum(year4wt, na.rm = TRUE)) |>
  mutate(proportion = weighted_n / sum(weighted_n))

fed_rec_noseek <- ggplot(fed_props, aes(x = sector_group, y = proportion)) +
  geom_col(fill = "#2c7fb8") +
  scale_x_discrete(labels = sector_labels) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Nonprofits Receiving Federal Gov Grants Without Seeking",
    x = "Sector",
    y = "Proportion"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_props <- combined_nonseek_receive %>%
  group_by(level, sector_group) %>%
  summarise(weighted_n = sum(year4wt, na.rm = TRUE), .groups = 'drop') %>%
  group_by(level) %>%
  mutate(proportion = weighted_n / sum(weighted_n))

noseek_combined <- ggplot(combined_props, aes(x = sector_group, y = proportion, fill = level)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = sector_labels) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Nonprofits Receiving\n Gov Grants Without Seeking",
    x = "Sector",
    y = "Proportion",
    fill = "Gov Level"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



















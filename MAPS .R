library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
data24 <- read_csv("YEAR-04-DATA-PUF.csv")

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
## universal variables
custom_regions <- custom_regions |>
  mutate(region = as.numeric(region)) # make sure numeric for join

total_region_counts <- data24 %>%
  group_by(CensusRegion9) %>%
  summarise(total_n = sum(year4wt, na.rm = TRUE))

## Other Federated Giving Programs
other_region_counts_pro <- data24 %>%
  filter(FndRaise_OthrGvngPrgrm_Seek == 1, FndRaise_OthrGvngPrgrm_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

other_region_props <- left_join(other_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

other_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(other_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

other_seek_plot_pro <- plot_ly(
  data = other_map_data,
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
    title = "Proportion of Nonprofits Receiving Other \nFederated Giving Programs After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

## Combined Federal Campaign
combined_region_counts_pro <- data24 %>%
  filter(FndRaise_CombFedCmpgn_Seek == 1, FndRaise_CombFedCmpgn_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

combined_region_props <- left_join(combined_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

combined_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(combined_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

combined_seek_plot_pro <- plot_ly(
  data = combined_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nCombined Federal Campaigns After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# United Way
united_region_counts_pro <- data24 %>%
  filter(FndRaise_UntdWy_Seek == 1, FndRaise_UntdWy_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

united_region_props <- left_join(united_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

united_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(united_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

united_seek_plot_pro <- plot_ly(
  data = united_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nUnited Way After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Corporate Grants or Donations
corp_region_counts_pro <- data24 %>%
  filter(FndRaise_Corp_Found_Grnt_Seek == 1, FndRaise_Corp_Found_Grnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

corp_region_props <- left_join(corp_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

corp_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(corp_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

corp_seek_plot_pro <- plot_ly(
  data = corp_map_data,
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
    title = "Proportion of Nonprofits Receiving from Corporate \nGrants or Donations After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Donor Advised Funds 

daf_region_counts_pro <- data24 %>%
  filter(FndRaise_DAF_Seek == 1, FndRaise_DAF_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

daf_region_props <- left_join(daf_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

daf_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(daf_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

daf_seek_plot_pro <- plot_ly(
  data = daf_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nDonor Advised Funds After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Community Foundations 
cf_region_counts_pro <- data24 %>%
  filter(FndRaise_CFGrnt_Seek == 1, FndRaise_CFGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

cf_region_props <- left_join(cf_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

cf_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(cf_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

cf_seek_plot_pro <- plot_ly(
  data = cf_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nCommunity Foundations After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Private Foundations
pf_region_counts_pro <- data24 %>%
  filter(FndRaise_PFGrnt_Seek == 1, FndRaise_PFGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

pf_region_props <- left_join(pf_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

pf_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(pf_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

pf_seek_plot_pro <- plot_ly(
  data = pf_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nPrivate Foundations After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Local Contracts 
loc_region_counts_pro <- data24 %>%
  filter(FndRaise_LocGvtCntrct_Seek == 1, FndRaise_LocGvtCntrct_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

loc_region_props <- left_join(loc_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

loc_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(loc_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

loc_seek_plot_pro <- plot_ly(
  data = loc_map_data,
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
    title = "Proportion of Nonprofits Receiving from \nLocal Gov Grants After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
# State Contracts 
state_region_counts_pro <- data24 %>%
  filter(FndRaise_StateGvtCntrct_Seek == 1, FndRaise_StateGvtCntrct_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

state_region_props <- left_join(state_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

state_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(state_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

state_seek_plot_pro <- plot_ly(
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
    title = "Proportion of Nonprofits Receiving from \nState Gov Contracts After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )
# Federal Contracts 
fed_region_counts_pro <- data24 %>%
  filter(FndRaise_FedGvtCntrct_Seek == 1, FndRaise_FedGvtCntrct_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

fed_region_props <- left_join(fed_region_counts_pro, total_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

fed_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(fed_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

fed_seek_plot_pro <- plot_ly(
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
    title = "Proportion of Nonprofits Receiving from \nFederal Gov Contracts After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Local Grants 
local_region_counts_pro <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 1, FndRaise_LocGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

local_region_props <- left_join(local_region_counts_pro, total_local_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

local_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(local_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

local_seek_plot_pro <- plot_ly(
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
    title = "Proportion of Nonprofits Receiving Local \nGov Grants After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# State Grants 
stateg_region_counts_pro <- data24 %>%
  filter(FndRaise_StateGvtGrnt_Seek == 1, FndRaise_StateGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

stateg_region_props <- left_join(stateg_region_counts_pro, total_state_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

stateg_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(stateg_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

stateg_seek_plot_pro <- plot_ly(
  data = stateg_map_data,
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
    title = "Proportion of Nonprofits Receiving State \nGov Grants After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )

# Federal Grants 
fedg_region_counts_pro <- data24 %>%
  filter(FndRaise_FedGvtGrnt_Seek == 1, FndRaise_FedGvtGrnt_Rcv == 1) %>%
  group_by(CensusRegion9) %>%
  summarise(received_n = sum(year4wt, na.rm = TRUE))

fedg_region_props <- left_join(fedg_region_counts_pro, total_fed_region_counts, by = "CensusRegion9") %>%
  mutate(proportion = received_n / total_n)

fedg_map_data <- custom_regions %>%
  mutate(region = as.numeric(region)) %>%
  left_join(fedg_region_props, by = c("region" = "CensusRegion9")) %>%
  mutate(proportion = replace_na(proportion, 0))

fedg_seek_plot_pro <- plot_ly(
  data = fedg_map_data,
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
    title = "Proportion of Nonprofits Receiving Federal \nGov Grants After Seeking",
    geo = list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  )


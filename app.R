# Load required packages
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(dplyr)) install.packages('dplyr')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(plotly)) install.packages('plotly')
if(!require(survey)) install.packages('survey')
if(!require(shiny)) install.packages('shiny')
setwd("/Users/audreydavis/Desktop/data & society/Nonprofit Collab Work Page/grants_map/RegionStrata")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(survey)
library(shiny)

# Load your data
data24 <- read_csv("YEAR-04-DATA-PUF.csv")
YEAR_04_DATA_PUF <- data24 %>%
  mutate(SizeStrata = recode(SizeStrata,
                             "1" = "<$100,000",
                             "2" = "$100,000-$499,999",
                             "3" = "$500,000-$999,999",
                             "4" = "$1 million-$9,999,999",
                             "5" = "$10 million and above"))

# Custom regions
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
) %>% mutate(region = as.numeric(region))

# --- Helper to Create Region Plots ---
generate_region_plot <- function(data24, seek_col, rcv_col, total_region_counts, title_text) {
  region_counts <- data24 %>%
    filter(.data[[seek_col]] == 1, .data[[rcv_col]] == 1) %>%
    group_by(CensusRegion9) %>%
    summarise(received_n = sum(year4wt, na.rm = TRUE), .groups = 'drop')
  
  region_props <- left_join(region_counts, total_region_counts, by = "CensusRegion9") %>%
    mutate(proportion = received_n / total_n)
  
  map_data <- custom_regions %>%
    left_join(region_props, by = c("region" = "CensusRegion9")) %>%
    mutate(proportion = replace_na(proportion, 0))
  
  plot_ly(
    data = map_data,
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
      title = title_text,
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
    )
}

# --- Total Region Counts ---
total_region_counts <- data24 %>%
  group_by(CensusRegion9) %>%
  summarise(total_n = sum(year4wt, na.rm = TRUE), .groups = 'drop')

# --- Generate Region Plotly Objects ---
local_seek_plot_pro     <- generate_region_plot(data24, "FndRaise_LocGvtGrnt_Seek", "FndRaise_LocGvtGrnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Local Gov Grants")
stateg_seek_plot_pro    <- generate_region_plot(data24, "FndRaise_StateGvtGrnt_Seek", "FndRaise_StateGvtGrnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving State Gov Grants")
fedg_seek_plot_pro      <- generate_region_plot(data24, "FndRaise_FedGvtGrnt_Seek", "FndRaise_FedGvtGrnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Federal Gov Grants")
loc_seek_plot_pro       <- generate_region_plot(data24, "FndRaise_LocGvtCntrct_Seek", "FndRaise_LocGvtCntrct_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Local Gov Contracts")
state_seek_plot_pro     <- generate_region_plot(data24, "FndRaise_StateGvtCntrct_Seek", "FndRaise_StateGvtCntrct_Rcv", total_region_counts, "Proportion of Nonprofits Receiving State Gov Contracts")
fed_seek_plot_pro       <- generate_region_plot(data24, "FndRaise_FedGvtCntrct_Seek", "FndRaise_FedGvtCntrct_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Federal Gov Contracts")
pf_seek_plot_pro        <- generate_region_plot(data24, "FndRaise_PFGrnt_Seek", "FndRaise_PFGrnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Private Foundation Grants")
cf_seek_plot_pro        <- generate_region_plot(data24, "FndRaise_CFGrnt_Seek", "FndRaise_CFGrnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Community Foundation Grants")
daf_seek_plot_pro       <- generate_region_plot(data24, "FndRaise_DAF_Seek", "FndRaise_DAF_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Donor Advised Funds")
corp_seek_plot_pro      <- generate_region_plot(data24, "FndRaise_Corp_Found_Grnt_Seek", "FndRaise_Corp_Found_Grnt_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Corporate Grants")
united_seek_plot_pro    <- generate_region_plot(data24, "FndRaise_UntdWy_Seek", "FndRaise_UntdWy_Rcv", total_region_counts, "Proportion of Nonprofits Receiving United Way Funding")
combined_seek_plot_pro  <- generate_region_plot(data24, "FndRaise_CombFedCmpgn_Seek", "FndRaise_CombFedCmpgn_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Combined Federal Campaign Funds")
other_seek_plot_pro     <- generate_region_plot(data24, "FndRaise_OthrGvngPrgrm_Seek", "FndRaise_OthrGvngPrgrm_Rcv", total_region_counts, "Proportion of Nonprofits Receiving Other Federated Giving Programs")
# Local Government Grant
local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  proptable <- svyby(~FndRaise_LocGvtGrnt_Rcv, ~SizeStrata, Loc_Grnt_design_seek, svymean, na.rm = TRUE)
  proptable <- proptable %>% mutate(CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se, CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 * se)
  
  ggplot(proptable, aes(x = SizeStrata, y = FndRaise_LocGvtGrnt_Rcv)) +
    geom_col(fill = "#05bfff") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Local Government Grant (Received | Sought)", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
local_grants_stats <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  design_seek <- subset(design, FndRaise_LocGvtGrnt_Seek == 1)
  svychisq(~FndRaise_LocGvtGrnt_Rcv + SizeStrata, design_seek)
}

# State Government Grant
State_grant_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  design_seek <- subset(design, FndRaise_StateGvtGrnt_Seek == 1)
  proptable <- svyby(~FndRaise_StateGvtGrnt_Rcv, ~SizeStrata, design_seek, svymean, na.rm = TRUE)
  proptable <- proptable %>% mutate(CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se, CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se)
  
  ggplot(proptable, aes(x = SizeStrata, y = FndRaise_StateGvtGrnt_Rcv)) +
    geom_col(fill = "#05bfff") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "State Government Grant (Received | Sought)", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
State_grants_stats <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  design_seek <- subset(design, FndRaise_StateGvtGrnt_Seek == 1)
  svychisq(~FndRaise_StateGvtGrnt_Rcv + SizeStrata, design_seek)
}

# Federal Government Grant
Fed_grant_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  design_seek <- subset(design, FndRaise_FedGvtGrnt_Seek == 1)
  proptable <- svyby(~FndRaise_FedGvtGrnt_Rcv, ~SizeStrata, design_seek, svymean, na.rm = TRUE)
  proptable <- proptable %>% mutate(CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se, CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se)
  
  ggplot(proptable, aes(x = SizeStrata, y = FndRaise_FedGvtGrnt_Rcv)) +
    geom_col(fill = "#05bfff") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Federal Government Grant (Received | Sought)", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Fed_grant_stats <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  design_seek <- subset(design, FndRaise_FedGvtGrnt_Seek == 1)
  svychisq(~FndRaise_FedGvtGrnt_Rcv + SizeStrata, design_seek)
}

# Local Government Contract
Local_contract_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_LocGvtCntrct_Seek == 1)
  tab <- svyby(~FndRaise_LocGvtCntrct_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se, CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_LocGvtCntrct_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Local Government Contract", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Local_contract_stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_LocGvtCntrct_Seek == 1)
  svychisq(~FndRaise_LocGvtCntrct_Rcv + SizeStrata, seek)
}

# State Government Contract
State_contract_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_StateGvtCntrct_Seek == 1)
  tab <- svyby(~FndRaise_StateGvtCntrct_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se, CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_StateGvtCntrct_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "State Government Contract", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
State_contract_stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_StateGvtCntrct_Seek == 1)
  svychisq(~FndRaise_StateGvtCntrct_Rcv + SizeStrata, seek)
}

# Federal Government Contract
Fed_contracts_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_FedGvtCntrct_Seek == 1)
  tab <- svyby(~FndRaise_FedGvtCntrct_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se, CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_FedGvtCntrct_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Federal Government Contract", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Fed_contracts_stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_FedGvtCntrct_Seek == 1)
  svychisq(~FndRaise_FedGvtCntrct_Rcv + SizeStrata, seek)
}

# Private Foundation Grant
Priv_grnt_plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_PFGrnt_Seek == 1)
  tab <- svyby(~FndRaise_PFGrnt_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se, CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_PFGrnt_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Private Foundation Grant", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Priv_grnt_stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_PFGrnt_Seek == 1)
  svychisq(~FndRaise_PFGrnt_Rcv + SizeStrata, seek)
}

# Donor Advised Funds
Donor_Funds_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_DAF_Seek == 1)
  tab <- svyby(~FndRaise_DAF_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_DAF_Rcv - 1.96 * se, CI_High = FndRaise_DAF_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_DAF_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Donor Advised Funds", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Donor_Funds_Stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_DAF_Seek == 1)
  svychisq(~FndRaise_DAF_Rcv + SizeStrata, seek)
}

# Community Foundation
Community_Grnt_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_CFGrnt_Seek == 1)
  tab <- svyby(~FndRaise_CFGrnt_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se, CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_CFGrnt_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Community Foundation Grants", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Community_Grnt_stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_CFGrnt_Seek == 1)
  svychisq(~FndRaise_CFGrnt_Rcv + SizeStrata, seek)
}

# Corporate Grants and Donations
Corp_Funds_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_Corp_Found_Grnt_Seek == 1)
  tab <- svyby(~FndRaise_Corp_Found_Grnt_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se, CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_Corp_Found_Grnt_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Corporate Grants and Donations", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Corp_Funds_Stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_Corp_Found_Grnt_Seek == 1)
  svychisq(~FndRaise_Corp_Found_Grnt_Rcv + SizeStrata, seek)
}

# United Way
UW_fund_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_UntdWy_Seek == 1)
  tab <- svyby(~FndRaise_UntdWy_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se, CI_High = FndRaise_UntdWy_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_UntdWy_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "United Way", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
UW_fund_Stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_UntdWy_Seek == 1)
  svychisq(~FndRaise_UntdWy_Rcv + SizeStrata, seek)
}

# Combined Federal Campaign
CFC_Funds_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_CombFedCmpgn_Seek == 1)
  tab <- svyby(~FndRaise_CombFedCmpgn_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se, CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_CombFedCmpgn_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Combined Federal Campaign", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
CFC_Funds_Stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_CombFedCmpgn_Seek == 1)
  svychisq(~FndRaise_CombFedCmpgn_Rcv + SizeStrata, seek)
}

# Other Funding Programs
Other_Funds_Plot <- function(data) {
  design <- svydesign(ids = ~1, data = data, weights = ~year4wt)
  seek <- subset(design, FndRaise_OthrGvngPrgrm_Seek == 1)
  tab <- svyby(~FndRaise_OthrGvngPrgrm_Rcv, ~SizeStrata, seek, svymean, na.rm = TRUE)
  tab <- tab %>% mutate(CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se, CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se)
  ggplot(tab, aes(x = SizeStrata, y = FndRaise_OthrGvngPrgrm_Rcv)) +
    geom_col(fill = "#05bfff") + geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Other Giving Programs", subtitle = "By Nonprofit Size", x = "Size", y = "Proportion") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
Other_Funds_Stats <- function(data) {
  seek <- subset(svydesign(ids = ~1, data = data, weights = ~year4wt), FndRaise_OthrGvngPrgrm_Seek == 1)
  svychisq(~FndRaise_OthrGvngPrgrm_Rcv + SizeStrata, seek)
}
# Define UI for application
ui <- fluidPage(
  titlePanel("NGO Funding Statistics by Nonprofit Size and Region"),
  
  tabsetPanel(id = "tabs",
              tabPanel("Size",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("size_funding_type", "Select Funding Type:",
                                       choices = c(
                                         "Local Government Grants",
                                         "State Government Grants",
                                         "Federal Government Grants",
                                         "Local Government Contracts",
                                         "State Government Contracts",
                                         "Federal Government Contracts",
                                         "Private Grants",
                                         "Community Foundation Grants",
                                         "Donor Advised Funds",
                                         "Corporate Grants and Donations",
                                         "United Way Funding",
                                         "Combined Federal Funding",
                                         "Other Funding"
                                       )
                           )
                         ),
                         mainPanel(
                           plotOutput("size_plot"),
                           verbatimTextOutput("size_stats")
                         )
                       )
              ),
              
              tabPanel("Region Maps",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("region_funding_type", "Select Funding Type:",
                                       choices = c(
                                         "Local Government Grants",
                                         "State Government Grants",
                                         "Federal Government Grants",
                                         "Local Government Contracts",
                                         "State Government Contracts",
                                         "Federal Government Contracts",
                                         "Private Grants",
                                         "Community Foundation Grants",
                                         "Donor Advised Funds",
                                         "Corporate Grants and Donations",
                                         "United Way Funding",
                                         "Combined Federal Funding",
                                         "Other Funding"
                                       )
                           )
                         ),
                         mainPanel(
                           plotlyOutput("region_map_plot")
                         )
                       )
              )
  )
)
# Define server logic
server <- function(input, output) {
  
  # Render Size-based bar chart
  output$size_plot <- renderPlot({
    switch(input$size_funding_type,
           "Local Government Grants" = local_grant_plot(YEAR_04_DATA_PUF),
           "State Government Grants" = State_grant_plot(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Fed_grant_plot(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Local_contract_plot(YEAR_04_DATA_PUF),
           "State Government Contracts" = State_contract_plot(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Fed_contracts_plot(YEAR_04_DATA_PUF),
           "Private Grants" = Priv_grnt_plot(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Community_Grnt_Plot(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Donor_Funds_Plot(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Corp_Funds_Plot(YEAR_04_DATA_PUF),
           "United Way Funding" = UW_fund_Plot(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = CFC_Funds_Plot(YEAR_04_DATA_PUF),
           "Other Funding" = Other_Funds_Plot(YEAR_04_DATA_PUF)
    )
  })
  
  # Render Size-based chi-square test output
  output$size_stats <- renderPrint({
    switch(input$size_funding_type,
           "Local Government Grants" = local_grants_stats(YEAR_04_DATA_PUF),
           "State Government Grants" = State_grants_stats(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Fed_grant_stats(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Local_contract_stats(YEAR_04_DATA_PUF),
           "State Government Contracts" = State_contract_stats(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Fed_contracts_stats(YEAR_04_DATA_PUF),
           "Private Grants" = Priv_grnt_stats(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Community_Grnt_stats(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Donor_Funds_Stats(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Corp_Funds_Stats(YEAR_04_DATA_PUF),
           "United Way Funding" = UW_fund_Stats(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = CFC_Funds_Stats(YEAR_04_DATA_PUF),
           "Other Funding" = Other_Funds_Stats(YEAR_04_DATA_PUF)
    )
  })
  
  # Render region-based Plotly choropleth map
  output$region_map_plot <- renderPlotly({
    switch(input$region_funding_type,
           "Local Government Grants" = local_seek_plot_pro,
           "State Government Grants" = stateg_seek_plot_pro,
           "Federal Government Grants" = fedg_seek_plot_pro,
           "Local Government Contracts" = loc_seek_plot_pro,
           "State Government Contracts" = state_seek_plot_pro,
           "Federal Government Contracts" = fed_seek_plot_pro,
           "Private Grants" = pf_seek_plot_pro,
           "Community Foundation Grants" = cf_seek_plot_pro,
           "Donor Advised Funds" = daf_seek_plot_pro,
           "Corporate Grants and Donations" = corp_seek_plot_pro,
           "United Way Funding" = united_seek_plot_pro,
           "Combined Federal Funding" = combined_seek_plot_pro,
           "Other Funding" = other_seek_plot_pro
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



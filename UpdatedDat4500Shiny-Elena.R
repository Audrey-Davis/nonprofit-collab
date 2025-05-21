if(!require(tidyverse)) install.packages('tidyverse')
if(!require(dplyr)) install.packages('dplyr')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(survey)) install.packages('survey')
if(!require(shiny)) install.packages('shiny')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(plotly)
library(shiny)
library(purrr)

library(readr)
YEAR_04_DATA_PUF <- read_csv("YEAR-04-DATA-PUF.csv")


YEAR_04_DATA_PUF <- YEAR_04_DATA_PUF |>
  mutate(SizeStrata = recode(SizeStrata,
                             "1" = "<$100,000",
                             "2" = "$100,000-$499,999",
                             "3" = "$500,000-$999,999",
                             "4" = "$1 million-$9,999,999",
                             "5" = "$10 million and above"),
    SizeStrata = factor(SizeStrata,
                             levels = c(
                             "1" = "<$100,000",
                             "2" = "$100,000-$499,999",
                             "3" = "$500,000-$999,999",
                             "4" = "$1 million-$9,999,999",
                             "5" = "$10 million and above")))


# Create cleaned data with sector_group included
data04_cleaned <- YEAR_04_DATA_PUF |>
  mutate(
    sector_group = case_when(
      ntee1 == "A" ~ "I. Arts, Culture, Humanities",
      ntee1 == "B" ~ "II. Education",
      ntee1 %in% c("C", "D") ~ "III. Environment & Animals",
      ntee1 %in% c("E", "F", "G", "H") ~ "IV. Health",
      ntee1 %in% c("I", "J", "K", "L", "M", "N", "O", "P") ~ "V. Human Services",
      ntee1 == "Q" ~ "VI. International",
      ntee1 %in% c("R", "S", "T", "U", "V", "W") ~ "VII. Public/Societal Benefit",
      ntee1 == "X" ~ "VIII. Religion Related",
      ntee1 == "Y" ~ "IX. Mutual/Membership Benefit",
      ntee1 == "Z" ~ "X. Unknown/Unclassified",
      TRUE ~ "Other"
    )
  )



data04_cleaned <- data04_cleaned |>
  mutate(
    GeoArea_Type = case_when(
      GeoAreas_Local == 1 ~ "Local",
      GeoAreas_MultipleLocal == 1 ~ "Multiple Local",
      GeoAreas_RegionalWithin == 1 ~ "Regional Within",
      GeoAreas_State == 1 ~ "State",
      GeoAreas_RegionalAcross == 1 ~ "Regional Across",
      GeoAreas_MultipleState == 1 ~ "Multiple State",
      GeoAreas_National == 1 ~ "National",
      GeoAreas_International == 1 ~ "International",
      GeoAreas_Oth == 1 ~ "Other",
      TRUE ~ NA_character_
    )
  )

data04_cleaned <- data04_cleaned |>
  mutate(
    GeoArea_Group = case_when(
      GeoArea_Type %in% c("Local", "Multiple Local") ~ "Local Area",
      GeoArea_Type %in% c("Regional Within", "Regional Across") ~ "Regional",
      GeoArea_Type %in% c("State", "Multiple State") ~ "Statewide",
      GeoArea_Type == "National" ~ "National",
      GeoArea_Type == "International" ~ "International",
      GeoArea_Type == "Other" ~ "Other",
      TRUE ~ NA_character_
    )
  )





# Define grant variable pairs
grant_vars <- list(
  "Local" = c("FndRaise_LocGvtGrnt_Seek", "FndRaise_LocGvtGrnt_Rcv"),
  "State" = c("FndRaise_StateGvtGrnt_Seek", "FndRaise_StateGvtGrnt_Rcv"),
  "Federal" = c("FndRaise_FedGvtGrnt_Seek", "FndRaise_FedGvtGrnt_Rcv")
)

# Compute success rate data
success_by_sector_long <- purrr::map_dfr(
  names(grant_vars),
  function(grant_type) {
    seek_var <- grant_vars[[grant_type]][1]
    rcv_var <- grant_vars[[grant_type]][2]
    
    data04_cleaned |>
      filter(.data[[seek_var]] == 1) |>
      group_by(sector_group) |>
      summarise(
        grant_type = grant_type,
        total_sought = n(),
        total_received = sum(.data[[rcv_var]] == 1, na.rm = TRUE),
        success_rate = round(100 * total_received / total_sought, 1),
        .groups = "drop"
      )
  }
)








#Local Grant Stats
local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  contingency_table <- svytable(~FndRaise_LocGvtGrnt_Rcv + SizeStrata, Loc_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + SizeStrata, Loc_Grnt_design_seek)
#Local Grant Graph with error bar
  LocGvtGrntproptable <- svyby(
    ~FndRaise_LocGvtGrnt_Rcv,
    ~SizeStrata,
    Loc_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  LocGvtGrntproptable <- LocGvtGrntproptable |>
    mutate(
      CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(LocGvtGrntproptable, aes(x = SizeStrata, y = FndRaise_LocGvtGrnt_Rcv)) +
    geom_col(fill = "#05bfff") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Local Gov Grant",
         subtitle = "Based on Nonprofit Size (calculated by annual expense)",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  
return(plot)
  }
#This is how we print the stats on the shiny app
local_grants_stats <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + SizeStrata, Loc_Grnt_design_seek)
  return(chisq_result)
  }

#State Grant Stats
State_grant_plot <- function(data) {
State_Grnt_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
contingency_table <- svytable(~FndRaise_StateGvtGrnt_Rcv + SizeStrata, State_Grnt_design_seek)
chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + SizeStrata, State_Grnt_design_seek)
#Graph with error bar
StateGvtGrntproptable <- svyby(
  ~FndRaise_StateGvtGrnt_Rcv,
  ~SizeStrata,
  State_Grnt_design_seek,
  svymean,
  na.rm = TRUE
)

StateGvtGrntproptable <- StateGvtGrntproptable |>
  mutate(
    CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se)

plot <- ggplot(StateGvtGrntproptable, aes(x = SizeStrata, y = FndRaise_StateGvtGrnt_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Grant",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))

return(plot)
}
#State Grant Stats
State_grants_stats <- function(data) {
  State_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + SizeStrata, State_Grnt_design_seek)
  return(chisq_result)
}
#Fed Grant stats
Fed_grant_plot <- function(data) {
Fed_Grnt_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
contingency_table <- svytable(~FndRaise_FedGvtGrnt_Rcv + SizeStrata, Fed_Grnt_design_seek)
chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + SizeStrata, Fed_Grnt_design_seek)
#Graph with error bar
FedGvtGrntproptable <- svyby(
  ~FndRaise_FedGvtGrnt_Rcv,
  ~SizeStrata,
  Fed_Grnt_design_seek,
  svymean,
  na.rm = TRUE
)

FedGvtGrntproptable <- FedGvtGrntproptable |>
  mutate(
    CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se)

plot <- ggplot(FedGvtGrntproptable, aes(x = SizeStrata, y = FndRaise_FedGvtGrnt_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Grant",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#showing the fed grant stats
Fed_grant_stats <- function(data) {
  Fed_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + SizeStrata, Fed_Grnt_design_seek)
  return(chisq_result)
}

#local contract stats and graph
Local_contract_plot <- function(data) {
Loc_Cntrct_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Loc_Cntrct_design_seek <- subset(Loc_Cntrct_design, FndRaise_LocGvtCntrct_Seek == 1)
chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + SizeStrata, Loc_Cntrct_design_seek)
#Graph with error bar
LocGvtCntrctproptable <- svyby(
  ~FndRaise_LocGvtCntrct_Rcv,
  ~SizeStrata,
  Loc_Cntrct_design_seek,
  svymean,
  na.rm = TRUE
)

LocGvtCntrctproptable <- LocGvtCntrctproptable |>
  mutate(
    CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se)

plot <- ggplot(LocGvtCntrctproptable, aes(x = SizeStrata, y = FndRaise_LocGvtCntrct_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Local Gov Contract",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}

#local contract stats and graph
Local_contract_stats <- function(data) {
  Loc_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Loc_Cntrct_design_seek <- subset(Loc_Cntrct_design, FndRaise_LocGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + SizeStrata, Loc_Cntrct_design_seek)
 return(chisq_result) 
}

#State contract stats and graph
State_contract_plot <- function(data) {
State_Cntrct_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + SizeStrata, State_Cntrct_design_seek)
#Graphs with error bars
StateGvtCntrctproptable <- svyby(
  ~FndRaise_StateGvtCntrct_Rcv,
  ~SizeStrata,
  State_Cntrct_design_seek,
  svymean,
  na.rm = TRUE
)

StateGvtCntrctproptable <- StateGvtCntrctproptable |>
  mutate(
    CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se)

plot <- ggplot(StateGvtCntrctproptable, aes(x = SizeStrata, y = FndRaise_StateGvtCntrct_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Contract",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#showing state contract stats
State_contract_stats <- function(data) {
  State_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + SizeStrata, State_Cntrct_design_seek)
  return(chisq_result)
}

#Fed contract stats and graph 
Fed_contracts_plot <- function(data) {
Fed_Cntrct_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + SizeStrata, Fed_Cntrct_design_seek)
#Graphs with error bars
FedGvtCntrctproptable <- svyby(
  ~FndRaise_FedGvtCntrct_Rcv,
  ~SizeStrata,
  Fed_Cntrct_design_seek,
  svymean,
  na.rm = TRUE
)

FedGvtCntrctproptable <- FedGvtCntrctproptable |>
  mutate(
    CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se)

plot <- ggplot(FedGvtCntrctproptable, aes(x = SizeStrata, y = FndRaise_FedGvtCntrct_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Contract",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#Show the stats for the federal contracts
Fed_contracts_stats <- function(data) {
  Fed_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + SizeStrata, Fed_Cntrct_design_seek)
  return(chisq_result)
}

#stats and graph for private grants
Priv_grnt_plot <- function(data) {
Priv_Grnt_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + SizeStrata, Priv_Grnt_design_seek)
#Graphs with errors bars
PrivGrntproptable <- svyby(
  ~FndRaise_PFGrnt_Rcv,
  ~SizeStrata,
  Priv_Grnt_design_seek,
  svymean,
  na.rm = TRUE
)

PrivGrntproptable <- PrivGrntproptable |>
  mutate(
    CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se)

plot <- ggplot(PrivGrntproptable, aes(x = SizeStrata, y = FndRaise_PFGrnt_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Private Grants",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#Show private grant stats
Priv_grnt_stats <- function(data) {
  Priv_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + SizeStrata, Priv_Grnt_design_seek)
 return(chisq_result) 
}

#Community Foundation stats and graph
Community_Grnt_Plot <- function(data) {
Community_Grnt_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Community_Grnt_design_seek <- subset(Community_Grnt_design, FndRaise_CFGrnt_Seek == 1)
chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + SizeStrata, Community_Grnt_design_seek)
#Graphs with error bars
CommunityGrntproptable <- svyby(
  ~FndRaise_CFGrnt_Rcv,
  ~SizeStrata,
  Community_Grnt_design_seek,
  svymean,
  na.rm = TRUE
)

CommunityGrntproptable <- CommunityGrntproptable |>
  mutate(
    CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se)

plot <- ggplot(CommunityGrntproptable, aes(x = SizeStrata, y = FndRaise_CFGrnt_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Community Foundation Grants",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#Show community foundation grant stats
Community_Grnt_stats <- function(data) {
  Community_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Community_Grnt_design_seek <- subset(Community_Grnt_design, FndRaise_CFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + SizeStrata, Community_Grnt_design_seek)
 return(chisq_result) 
}

#stats and graph for donar advised funds
Donor_Funds_Plot <- function(data) {
Donor_Funds_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
chisq_result <- svychisq(~FndRaise_DAF_Rcv + SizeStrata, Donor_Funds_design_seek)
#graphs with error bars
Donor_Fund_proptable <- svyby(
  ~FndRaise_DAF_Rcv,
  ~SizeStrata,
  Donor_Funds_design_seek,
  svymean,
  na.rm = TRUE
)

Donor_Fund_proptable <- Donor_Fund_proptable |>
  mutate(
    CI_Low = FndRaise_DAF_Rcv - 1.96 * se,
    CI_High = FndRaise_DAF_Rcv + 1.96 * se)

plot <- ggplot(Donor_Fund_proptable, aes(x = SizeStrata, y = FndRaise_DAF_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Donor Advised Fudns",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}

Donor_Funds_Stats <- function(data){
  Donor_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + SizeStrata, Donor_Funds_design_seek)
  return(chisq_result)
}

#stats and graphs for corporate grants and donations
Corp_Funds_Plot <- function(data) {
Corp_Funds_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + SizeStrata, Corp_Funds_design_seek)
#Graphs with error bars
Corp_Fund_proptable <- svyby(
  ~FndRaise_Corp_Found_Grnt_Rcv,
  ~SizeStrata,
  Corp_Funds_design_seek,
  svymean,
  na.rm = TRUE
)

Corp_Fund_proptable <- Corp_Fund_proptable |>
  mutate(
    CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se,
    CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se)

plot <- ggplot(Corp_Fund_proptable, aes(x = SizeStrata, y = FndRaise_Corp_Found_Grnt_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Corporate Fudns",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}

Corp_Funds_Stats <- function(data) {
  Corp_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + SizeStrata, Corp_Funds_design_seek)
  return(chisq_result)
}

#Stats and graph for united way funding
UW_fund_Plot <- function(data) {
UW_Funds_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + SizeStrata, UW_Funds_design_seek)

#Graph with error bars
UW_Fund_proptable <- svyby(
  ~FndRaise_UntdWy_Rcv,
  ~SizeStrata,
  UW_Funds_design_seek,
  svymean,
  na.rm = TRUE
)

UW_Fund_proptable <- UW_Fund_proptable |>
  mutate(
    CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se,
    CI_High = FndRaise_UntdWy_Rcv + 1.96 * se)

plot <- ggplot(UW_Fund_proptable, aes(x = SizeStrata, y = FndRaise_UntdWy_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received United Way Funds",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}
#show stats for United way funding
UW_fund_Stats <- function(data) {
  UW_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + SizeStrata, UW_Funds_design_seek)
 return(chisq_result) 
}


#stats and grants for combined federal campaign funding
CFC_Funds_Plot <- function(data) {
CFC_Funds_design <- svydesign(
  ids = ~1,
  data = YEAR_04_DATA_PUF,
  weights = ~year4wt
)

CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + SizeStrata, CFC_Funds_design_seek)

#Graph error bars
CFC_Fund_proptable <- svyby(
  ~FndRaise_CombFedCmpgn_Rcv,
  ~SizeStrata,
  CFC_Funds_design_seek,
  svymean,
  na.rm = TRUE
)

CFC_Fund_proptable <- CFC_Fund_proptable |>
  mutate(
    CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se,
    CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se)

plot <- ggplot(CFC_Fund_proptable, aes(x = SizeStrata, y = FndRaise_CombFedCmpgn_Rcv)) +
  geom_col(fill = "#05bfff") +
  geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Combined Federal Campaign Funds",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
        axis.text.y = element_text(size = 15),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 17))
return(plot)
}

CFC_Funds_Stats <- function(data) {
    CFC_Funds_design <- svydesign(
      ids = ~1,
      data = YEAR_04_DATA_PUF,
      weights = ~year4wt
    )
    
    CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
    chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + SizeStrata, CFC_Funds_design_seek)
  return(chisq_result)
}

#Stats and graph for 
Other_Funds_Plot <- function(data) {
  Other_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + SizeStrata, Other_Funds_design_seek)
  #graphs with error bars
  Other_Fund_proptable <- svyby(
    ~FndRaise_OthrGvngPrgrm_Rcv,
    ~SizeStrata,
    Other_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  Other_Fund_proptable <- Other_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se,
      CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se)
  
 plot <- ggplot(Other_Fund_proptable, aes(x = SizeStrata, y = FndRaise_OthrGvngPrgrm_Rcv)) +
    geom_col(fill = "#05bfff") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Other Funds",
         subtitle = "Based on Nonprofit Size (calculated by annual expense)",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
         axis.text.y = element_text(size = 15),
         axis.title = element_text(size = 20),
         plot.title = element_text(size = 20, face = 'bold'),
         plot.subtitle = element_text(size = 17))
  return(plot)
}
#show stats of other funding
Other_Funds_Stats <- function(data) {
  Other_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + SizeStrata, Other_Funds_design_seek)
 return(chisq_result) 
}


#### NEW
# Plotly visualization function
sector_success_plotly <- function(data) {
  library(plotly)
  
  data_summary <- data |>
    filter(
      FndRaise_LocGvtGrnt_Seek == 1 |
        FndRaise_StateGvtGrnt_Seek == 1 |
        FndRaise_FedGvtGrnt_Seek == 1
    ) |>
    mutate(
      received_any_grant = if_else(
        FndRaise_LocGvtGrnt_Rcv == 1 |
          FndRaise_StateGvtGrnt_Rcv == 1 |
          FndRaise_FedGvtGrnt_Rcv == 1,
        "Yes", "No"
      )
    ) |>
    group_by(sector_group, received_any_grant) |>
    summarise(count = n(), .groups = "drop") |>
    pivot_wider(names_from = received_any_grant, values_from = count, values_fill = 0) |>
    mutate(
      total = Yes + No,
      success_rate = Yes / total
    ) |>
    arrange(desc(success_rate))
  
  plot_ly(
    data_summary,
    x = ~reorder(sector_group, -success_rate),
    y = ~success_rate,
    type = 'bar',
    text = ~paste0("Sector: ", sector_group, "<br>",
                   "Success Rate: ", scales::percent(success_rate, accuracy = 1)),
    hoverinfo = 'text',
    marker = list(color = '#F0746E')
  ) |>
    layout(
      title = list(text = "Success Rate by Sector (Gov Grants)", font = list(size = 20)),
      yaxis = list(title = "Success Rate", tickformat = ".0%", titlefont = list(size = 18), tickfont = list(size = 14)),
      xaxis = list(title = "Sector", titlefont = list(size = 18), tickfont = list(size = 13), tickangle = -45),
      margin = list(b = 120),
      font = list(size = 14)
    )
}



####### NEW!!!! Static sucess rate plot by funding type 

sector_success_static_plot <- function(data, seek_var, rcv_var, grant_label) {
  data_sought <- data |>
    filter(.data[[seek_var]] == 1) |>
    mutate(
      received = factor(.data[[rcv_var]], levels = c(0, 1)),
      sector_group = factor(sector_group)
    )
  
  success_by_sector <- data_sought |>
    group_by(sector_group) |>
    summarise(
      total_sought = n(),
      total_received = sum(received == 1, na.rm = TRUE),
      success_rate = round(100 * total_received / total_sought, 1),
      .groups = "drop"
    ) |>
    arrange(desc(success_rate))
  
  ggplot(success_by_sector, aes(x = reorder(sector_group, success_rate), y = success_rate, fill = sector_group)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = paste0(
        success_rate, "% (", total_received, "/", total_sought, ")"
      )),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    labs(
      title = paste("Success Rate of Receiving", grant_label, "(Among Seekers)"),
      x = "Nonprofit Sector",
      y = "Success Rate (%)"
    ) +
    theme_minimal(base_size = 13) +
    ylim(0, 110)
}


#### NEW!! Geo area function
geo_success_plot <- function(data, seek_var, rcv_var, grant_label) {
  data_filtered <- data |>
    filter(.data[[seek_var]] == 1, !is.na(GeoArea_Group)) |>
    mutate(
      received = factor(.data[[rcv_var]], levels = c(0, 1)),
      GeoArea_Group = factor(GeoArea_Group)
    )
  
  success_by_geo <- data_filtered |>
    group_by(GeoArea_Group) |>
    summarise(
      total_sought = n(),
      total_received = sum(received == 1, na.rm = TRUE),
      success_rate = round(100 * total_received / total_sought, 1),
      .groups = "drop"
    ) |>
    arrange(desc(success_rate))
  
  ggplot(success_by_geo, aes(x = reorder(GeoArea_Group, success_rate), y = success_rate, fill = GeoArea_Group)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = paste0(
        success_rate, "% (", total_received, "/", total_sought, ")"
      )),
      hjust = -0.1,
      size = 3.5
    ) +
    coord_flip() +
    labs(
      title = paste("Success Rate of Receiving", grant_label, "by Geographic Service Area"),
      x = "Geographic Service Area",
      y = "Success Rate (%)"
    ) +
    theme_minimal(base_size = 13) +
    ylim(0, 110)
}



#########Sector########
Sec_local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  contingency_table <- svytable(~FndRaise_LocGvtGrnt_Rcv + sector_group, Loc_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + sector_group, Loc_Grnt_design_seek)
  #Local Grant Graph with error bar
  LocGvtGrntproptable <- svyby(
    ~FndRaise_LocGvtGrnt_Rcv,
    ~sector_group,
    Loc_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  LocGvtGrntproptable <- LocGvtGrntproptable |>
    mutate(
      CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(LocGvtGrntproptable, aes(x = sector_group, y = FndRaise_LocGvtGrnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Local Gov Grant",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  
  return(plot)
}
#This is how we print the stats on the shiny app
Sec_local_grants_stats <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + sector_group, Loc_Grnt_design_seek)
  return(chisq_result)
}

#State Grant Stats
Sec_State_grant_plot <- function(data) {
  State_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  contingency_table <- svytable(~FndRaise_StateGvtGrnt_Rcv + sector_group, State_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + sector_group, State_Grnt_design_seek)
  #Graph with error bar
  StateGvtGrntproptable <- svyby(
    ~FndRaise_StateGvtGrnt_Rcv,
    ~sector_group,
    State_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  StateGvtGrntproptable <- StateGvtGrntproptable |>
    mutate(
      CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(StateGvtGrntproptable, aes(x = sector_group, y = FndRaise_StateGvtGrnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received State Gov Grant",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  
  return(plot)
}

#State Grant Stats
Sec_State_grants_stats <- function(data) {
  State_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + sector_group, State_Grnt_design_seek)
  return(chisq_result)
}

#Fed Grant stats
Sec_Fed_grant_plot <- function(data) {
  Fed_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
  contingency_table <- svytable(~FndRaise_FedGvtGrnt_Rcv + sector_group, Fed_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + sector_group, Fed_Grnt_design_seek)
  #Graph with error bar
  FedGvtGrntproptable <- svyby(
    ~FndRaise_FedGvtGrnt_Rcv,
    ~sector_group,
    Fed_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  FedGvtGrntproptable <- FedGvtGrntproptable |>
    mutate(
      CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(FedGvtGrntproptable, aes(x = sector_group, y = FndRaise_FedGvtGrnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Grant",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#showing the fed grant stats
Sec_Fed_grant_stats <- function(data) {
  Fed_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + sector_group, Fed_Grnt_design_seek)
  return(chisq_result)
}

#local contract stats and graph
Sec_Local_contract_plot <- function(data) {
  Loc_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Loc_Cntrct_design_seek <- subset(Loc_Cntrct_design, FndRaise_LocGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + sector_group, Loc_Cntrct_design_seek)
  #Graph with error bar
  LocGvtCntrctproptable <- svyby(
    ~FndRaise_LocGvtCntrct_Rcv,
    ~sector_group,
    Loc_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  LocGvtCntrctproptable <- LocGvtCntrctproptable |>
    mutate(
      CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se)
  
  plot <- ggplot(LocGvtCntrctproptable, aes(x = sector_group, y = FndRaise_LocGvtCntrct_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Local Gov Contract",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}

#local contract stats and graph
Sec_Local_contract_stats <- function(data) {
  Loc_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Loc_Cntrct_design_seek <- subset(Loc_Cntrct_design, FndRaise_LocGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + sector_group, Loc_Cntrct_design_seek)
  return(chisq_result) 
}

#State contract stats and graph
Sec_State_contract_plot <- function(data) {
  State_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + sector_group, State_Cntrct_design_seek)
  #Graphs with error bars
  StateGvtCntrctproptable <- svyby(
    ~FndRaise_StateGvtCntrct_Rcv,
    ~sector_group,
    State_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  StateGvtCntrctproptable <- StateGvtCntrctproptable |>
    mutate(
      CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se)
  
  plot <- ggplot(StateGvtCntrctproptable, aes(x = sector_group, y = FndRaise_StateGvtCntrct_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received State Gov Contract",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#showing state contract stats
Sec_State_contract_stats <- function(data) {
  State_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + sector_group, State_Cntrct_design_seek)
  return(chisq_result)
}

#Fed contract stats and graph 
Sec_Fed_contracts_plot <- function(data) {
  Fed_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + sector_group, Fed_Cntrct_design_seek)
  #Graphs with error bars
  FedGvtCntrctproptable <- svyby(
    ~FndRaise_FedGvtCntrct_Rcv,
    ~sector_group,
    Fed_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  FedGvtCntrctproptable <- FedGvtCntrctproptable |>
    mutate(
      CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se)
  
  plot <- ggplot(FedGvtCntrctproptable, aes(x = sector_group, y = FndRaise_FedGvtCntrct_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Contract",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#Show the stats for the federal contracts
Sec_Fed_contracts_stats <- function(data) {
  Fed_Cntrct_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + sector_group, Fed_Cntrct_design_seek)
  return(chisq_result)
}

#stats and graph for private grants
Sec_Priv_grnt_plot <- function(data) {
  Priv_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + sector_group, Priv_Grnt_design_seek)
  #Graphs with errors bars
  PrivGrntproptable <- svyby(
    ~FndRaise_PFGrnt_Rcv,
    ~sector_group,
    Priv_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  PrivGrntproptable <- PrivGrntproptable |>
    mutate(
      CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(PrivGrntproptable, aes(x = sector_group, y = FndRaise_PFGrnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Private Grants",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#Show private grant stats
Sec_Priv_grnt_stats <- function(data) {
  Priv_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + sector_group, Priv_Grnt_design_seek)
  return(chisq_result) 
}

#Community Foundation stats and graph
Sec_Community_Grnt_Plot <- function(data) {
  Community_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Community_Grnt_design_seek <- subset(Community_Grnt_design, FndRaise_CFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + sector_group, Community_Grnt_design_seek)
  #Graphs with error bars
  CommunityGrntproptable <- svyby(
    ~FndRaise_CFGrnt_Rcv,
    ~sector_group,
    Community_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CommunityGrntproptable <- CommunityGrntproptable |>
    mutate(
      CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se)
  
  plot <- ggplot(CommunityGrntproptable, aes(x = sector_group, y = FndRaise_CFGrnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Community Foundation Grants",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#Show community foundation grant stats
Sec_Community_Grnt_stats <- function(data) {
  Community_Grnt_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Community_Grnt_design_seek <- subset(Community_Grnt_design, FndRaise_CFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + sector_group, Community_Grnt_design_seek)
  return(chisq_result) 
}

#stats and graph for donar advised funds
Sec_Donor_Funds_Plot <- function(data) {
  Donor_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + sector_group, Donor_Funds_design_seek)
  #graphs with error bars
  Donor_Fund_proptable <- svyby(
    ~FndRaise_DAF_Rcv,
    ~sector_group,
    Donor_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  Donor_Fund_proptable <- Donor_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_DAF_Rcv - 1.96 * se,
      CI_High = FndRaise_DAF_Rcv + 1.96 * se)
  
  plot <- ggplot(Donor_Fund_proptable, aes(x = sector_group, y = FndRaise_DAF_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Donor Advised Fudns",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}

Sec_Donor_Funds_Stats <- function(data){
  Donor_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + sector_group, Donor_Funds_design_seek)
  return(chisq_result)
}

#stats and graphs for corporate grants and donations
Sec_Corp_Funds_Plot <- function(data) {
  Corp_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + sector_group, Corp_Funds_design_seek)
  #Graphs with error bars
  Corp_Fund_proptable <- svyby(
    ~FndRaise_Corp_Found_Grnt_Rcv,
    ~sector_group,
    Corp_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  Corp_Fund_proptable <- Corp_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se,
      CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se)
  
  plot <- ggplot(Corp_Fund_proptable, aes(x = sector_group, y = FndRaise_Corp_Found_Grnt_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Corporate Fudns",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}

Sec_Corp_Funds_Stats <- function(data) {
  Corp_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + sector_group, Corp_Funds_design_seek)
  return(chisq_result)
}

#Stats and graph for united way funding
Sec_UW_fund_Plot <- function(data) {
  UW_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + sector_group, UW_Funds_design_seek)
  
  #Graph with error bars
  UW_Fund_proptable <- svyby(
    ~FndRaise_UntdWy_Rcv,
    ~sector_group,
    UW_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  UW_Fund_proptable <- UW_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se,
      CI_High = FndRaise_UntdWy_Rcv + 1.96 * se)
  
  plot <- ggplot(UW_Fund_proptable, aes(x = sector_group, y = FndRaise_UntdWy_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received United Way Funds",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#show stats for United way funding
Sec_UW_fund_Stats <- function(data) {
  UW_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + sector_group, UW_Funds_design_seek)
  return(chisq_result) 
}


#stats and grants for combined federal campaign funding
Sec_CFC_Funds_Plot <- function(data) {
  CFC_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + sector_group, CFC_Funds_design_seek)
  
  #Graph error bars
  CFC_Fund_proptable <- svyby(
    ~FndRaise_CombFedCmpgn_Rcv,
    ~sector_group,
    CFC_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CFC_Fund_proptable <- CFC_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se,
      CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se)
  
  plot <- ggplot(CFC_Fund_proptable, aes(x = sector_group, y = FndRaise_CombFedCmpgn_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Combined Federal Campaign Funds",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}

Sec_CFC_Funds_Stats <- function(data) {
  CFC_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + sector_group, CFC_Funds_design_seek)
  return(chisq_result)
}

#Stats and graph for 
Sec_Other_Funds_Plot <- function(data) {
  Other_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + sector_group, Other_Funds_design_seek)
  #graphs with error bars
  Other_Fund_proptable <- svyby(
    ~FndRaise_OthrGvngPrgrm_Rcv,
    ~sector_group,
    Other_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  Other_Fund_proptable <- Other_Fund_proptable |>
    mutate(
      CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se,
      CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se)
  
  plot <- ggplot(Other_Fund_proptable, aes(x = sector_group, y = FndRaise_OthrGvngPrgrm_Rcv)) +
    geom_col(fill = "#fb602b") +
    geom_errorbar(aes(ymin = CI_Low, ymax = CI_High)) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Other Funds",
         subtitle = "Based on Sector",
         x = "Nonprofit Size", y = "Proportion") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          plot.title = element_text(size = 20, face = 'bold'),
          plot.subtitle = element_text(size = 17))
  return(plot)
}
#show stats of other funding
Sec_Other_Funds_Stats <- function(data) {
  Other_Funds_design <- svydesign(
    ids = ~1,
    data = YEAR_04_DATA_PUF,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + sector_group, Other_Funds_design_seek)
  return(chisq_result) 
}
# Define UI for application (i think this is good)
ui <- fluidPage(
  titlePanel("NGO Funding Statistics"),
  tabsetPanel(
    id = "tab",
    tabPanel(
      title = "Size",
      selectInput("size_funding_type", "Select Funding Type:",
                  choices = c("Local Government Grants",
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
                              )),
      mainPanel(
        plotOutput("size_plot", height = "800px", width = "800px"),
        div(
        verbatimTextOutput("size_stats"),
        style = "font-size: 60px; width: 800px"
      )
      )
    ),
    tabPanel(
      title = "Region",
      selectInput("Region_funding_type", "Select Funding Type:",
                  choices = c("Local Government Grants",
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
                  )),
      mainPanel(
        plotOutput("Region_plot"),
        verbatimTextOutput("Region_stats")
      )
    ),
    tabPanel(
      title = "Sector",
      selectInput("Sector_funding_type", "Select Funding Type:",
                  choices = c("Local Government Grants",
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
                  )),
      mainPanel(
        plotOutput("sector_plot", height = "800px", width = "800px"),
        verbatimTextOutput("sector_stats"),
        style = "font-size: 60px; width: 800px"
      )
    ),
    tabPanel(
      title = "Success Rate Plotly",
      mainPanel(
        plotlyOutput("success_rate_plotly", height = "700px")
      )
    ),
    tabPanel(
      title = "Sector Success Rates (Static)",
      selectInput("static_sector_funding_type", "Select Funding Type:",
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
                  )),
      mainPanel(
        plotOutput("sector_success_static_plot", height = "700px", width = "900px")
      )
    ),
    tabPanel(
      title = "Geographic Service Area",
      selectInput("geo_funding_type", "Select Funding Type:",
                  choices = c("Local Government Grants",
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
                              "Other Funding")),
      mainPanel(
        plotOutput("geo_plot", height = "700px")
      )
    )
  )
)
##Add region tab here 
# Define server logic
server <- function(input, output) {
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

  output$Region_plot <- renderPlot({
    switch(input$Region_funding_type,
           "Local Government Grants" = Reg_local_grant_plot(YEAR_04_DATA_PUF),
           "State Government Grants" = Reg_State_grant_plot(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Reg_Fed_grant_plot(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Reg_Local_contract_plot(YEAR_04_DATA_PUF),
           "State Government Contracts" = Reg_State_contract_plot(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Reg_Fed_contracts_plot(YEAR_04_DATA_PUF),
           "Private Grants" = Reg_Priv_grnt_plot(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Reg_Community_Grnt_Plot(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Reg_Donor_Funds_Plot(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Reg_Corp_Funds_Plot(YEAR_04_DATA_PUF),
           "United Way Funding" = Reg_UW_fund_Plot(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = Reg_CFC_Funds_Plot(YEAR_04_DATA_PUF),
           "Other Funding" = Reg_Other_Funds_Plot(YEAR_04_DATA_PUF)
    )
  })
  
  output$Region_stats <- renderPrint({
    switch(input$Region_funding_type,
           "Local Government Grants" = Reg_local_grants_stats(YEAR_04_DATA_PUF),
           "State Government Grants" = Reg_State_grants_stats(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Reg_Fed_grant_stats(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Reg_Local_contract_stats(YEAR_04_DATA_PUF),
           "State Government Contracts" = Reg_State_contract_stats(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Reg_Fed_contracts_stats(YEAR_04_DATA_PUF),
           "Private Grants" = Reg_Priv_grnt_stats(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Reg_Community_Grnt_stats(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Reg_Donor_Funds_Stats(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Reg_Corp_Funds_Stats(YEAR_04_DATA_PUF),
           "United Way Funding" = Reg_UW_fund_Stats(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = Reg_CFC_Funds_Stats(YEAR_04_DATA_PUF),
           "Other Funding" = Reg_Other_Funds_Stats(YEAR_04_DATA_PUF)
    )
  })
  
  output$sector_plot <- renderPlot({
    switch(input$Sector_funding_type,
           "Local Government Grants" = Sec_local_grant_plot(YEAR_04_DATA_PUF),
           "State Government Grants" = Sec_State_grant_plot(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Sec_Fed_grant_plot(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Sec_Local_contract_plot(YEAR_04_DATA_PUF),
           "State Government Contracts" = Sec_State_contract_plot(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Sec_Fed_contracts_plot(YEAR_04_DATA_PUF),
           "Private Grants" = Sec_Priv_grnt_plot(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Sec_Community_Grnt_Plot(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Sec_Donor_Funds_Plot(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Sec_Corp_Funds_Plot(YEAR_04_DATA_PUF),
           "United Way Funding" = Sec_UW_fund_Plot(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = Sec_CFC_Funds_Plot(YEAR_04_DATA_PUF),
           "Other Funding" = Sec_Other_Funds_Plot(YEAR_04_DATA_PUF)
    )
  })
  
  output$sector_stats <- renderPrint({
    switch(input$Sector_funding_type,
           "Local Government Grants" = Sec_local_grants_stats(YEAR_04_DATA_PUF),
           "State Government Grants" = Sec_State_grants_stats(YEAR_04_DATA_PUF),
           "Federal Government Grants" = Sec_Fed_grant_stats(YEAR_04_DATA_PUF),
           "Local Government Contracts" = Sec_Local_contract_stats(YEAR_04_DATA_PUF),
           "State Government Contracts" = Sec_State_contract_stats(YEAR_04_DATA_PUF),
           "Federal Government Contracts" = Sec_Fed_contracts_stats(YEAR_04_DATA_PUF),
           "Private Grants" = Sec_Priv_grnt_stats(YEAR_04_DATA_PUF),
           "Community Foundation Grants" = Sec_Community_Grnt_stats(YEAR_04_DATA_PUF),
           "Donor Advised Funds" = Sec_Donor_Funds_Stats(YEAR_04_DATA_PUF),
           "Corporate Grants and Donations" = Sec_Corp_Funds_Stats(YEAR_04_DATA_PUF),
           "United Way Funding" = Sec_UW_fund_Stats(YEAR_04_DATA_PUF),
           "Combined Federal Funding" = Sec_CFC_Funds_Stats(YEAR_04_DATA_PUF),
           "Other Funding" = Sec_Other_Funds_Stats(YEAR_04_DATA_PUF)
    )
  })
  output$success_rate_plotly <- renderPlotly({
    plot_ly(
      data = success_by_sector_long,
      x = ~success_rate,
      y = ~sector_group,
      color = ~grant_type,
      type = 'bar',
      orientation = 'h',
      text = ~paste0(
        "Grant Type: ", grant_type,
        "<br>Sector: ", sector_group,
        "<br>Success Rate: ", success_rate, "%",
        "<br>Total Sought: ", total_sought,
        "<br>Total Received: ", total_received
      ),
      hoverinfo = 'text'
    ) |>
      layout(
        title = "Success Rates of Receiving Government Grants by Sector and Type",
        xaxis = list(title = "Success Rate (%)", range = c(0, 100)),
        yaxis = list(title = ""),
        barmode = 'group',
        margin = list(l = 150)
      )
  })
  
  output$sector_success_static_plot <- renderPlot({
    switch(input$static_sector_funding_type,
           "Local Government Grants" = sector_success_static_plot(data04_cleaned, "FndRaise_LocGvtGrnt_Seek", "FndRaise_LocGvtGrnt_Rcv", "Local Government Grants"),
           "State Government Grants" = sector_success_static_plot(data04_cleaned, "FndRaise_StateGvtGrnt_Seek", "FndRaise_StateGvtGrnt_Rcv", "State Government Grants"),
           "Federal Government Grants" = sector_success_static_plot(data04_cleaned, "FndRaise_FedGvtGrnt_Seek", "FndRaise_FedGvtGrnt_Rcv", "Federal Government Grants"),
           "Local Government Contracts" = sector_success_static_plot(data04_cleaned, "FndRaise_LocGvtCntrct_Seek", "FndRaise_LocGvtCntrct_Rcv", "Local Government Contracts"),
           "State Government Contracts" = sector_success_static_plot(data04_cleaned, "FndRaise_StateGvtCntrct_Seek", "FndRaise_StateGvtCntrct_Rcv", "State Government Contracts"),
           "Federal Government Contracts" = sector_success_static_plot(data04_cleaned, "FndRaise_FedGvtCntrct_Seek", "FndRaise_FedGvtCntrct_Rcv", "Federal Government Contracts"),
           "Private Grants" = sector_success_static_plot(data04_cleaned, "FndRaise_PFGrnt_Seek", "FndRaise_PFGrnt_Rcv", "Private Grants"),
           "Community Foundation Grants" = sector_success_static_plot(data04_cleaned, "FndRaise_CFGrnt_Seek", "FndRaise_CFGrnt_Rcv", "Community Foundation Grants"),
           "Donor Advised Funds" = sector_success_static_plot(data04_cleaned, "FndRaise_DAF_Seek", "FndRaise_DAF_Rcv", "Donor Advised Funds"),
           "Corporate Grants and Donations" = sector_success_static_plot(data04_cleaned, "FndRaise_Corp_Found_Grnt_Seek", "FndRaise_Corp_Found_Grnt_Rcv", "Corporate Grants and Donations"),
           "United Way Funding" = sector_success_static_plot(data04_cleaned, "FndRaise_UntdWy_Seek", "FndRaise_UntdWy_Rcv", "United Way Funding"),
           "Combined Federal Funding" = sector_success_static_plot(data04_cleaned, "FndRaise_CombFedCmpgn_Seek", "FndRaise_CombFedCmpgn_Rcv", "Combined Federal Funding"),
           "Other Funding" = sector_success_static_plot(data04_cleaned, "FndRaise_OthrGvngPrgrm_Seek", "FndRaise_OthrGvngPrgrm_Rcv", "Other Funding")
    )
  })
  
  output$geo_plot <- renderPlot({
    switch(input$geo_funding_type,
           "Local Government Grants" = geo_success_plot(data04_cleaned, "FndRaise_LocGvtGrnt_Seek", "FndRaise_LocGvtGrnt_Rcv", "Local Government Grants"),
           "State Government Grants" = geo_success_plot(data04_cleaned, "FndRaise_StateGvtGrnt_Seek", "FndRaise_StateGvtGrnt_Rcv", "State Government Grants"),
           "Federal Government Grants" = geo_success_plot(data04_cleaned, "FndRaise_FedGvtGrnt_Seek", "FndRaise_FedGvtGrnt_Rcv", "Federal Government Grants"),
           "Local Government Contracts" = geo_success_plot(data04_cleaned, "FndRaise_LocGvtCntrct_Seek", "FndRaise_LocGvtCntrct_Rcv", "Local Government Contracts"),
           "State Government Contracts" = geo_success_plot(data04_cleaned, "FndRaise_StateGvtCntrct_Seek", "FndRaise_StateGvtCntrct_Rcv", "State Government Contracts"),
           "Federal Government Contracts" = geo_success_plot(data04_cleaned, "FndRaise_FedGvtCntrct_Seek", "FndRaise_FedGvtCntrct_Rcv", "Federal Government Contracts"),
           "Private Grants" = geo_success_plot(data04_cleaned, "FndRaise_PFGrnt_Seek", "FndRaise_PFGrnt_Rcv", "Private Grants"),
           "Community Foundation Grants" = geo_success_plot(data04_cleaned, "FndRaise_CFGrnt_Seek", "FndRaise_CFGrnt_Rcv", "Community Foundation Grants"),
           "Donor Advised Funds" = geo_success_plot(data04_cleaned, "FndRaise_DAF_Seek", "FndRaise_DAF_Rcv", "Donor Advised Funds"),
           "Corporate Grants and Donations" = geo_success_plot(data04_cleaned, "FndRaise_Corp_Found_Grnt_Seek", "FndRaise_Corp_Found_Grnt_Rcv", "Corporate Grants and Donations"),
           "United Way Funding" = geo_success_plot(data04_cleaned, "FndRaise_UntdWy_Seek", "FndRaise_UntdWy_Rcv", "United Way Funding"),
           "Combined Federal Funding" = geo_success_plot(data04_cleaned, "FndRaise_CombFedCmpgn_Seek", "FndRaise_CombFedCmpgn_Rcv", "Combined Federal Funding"),
           "Other Funding" = geo_success_plot(data04_cleaned, "FndRaise_OthrGvngPrgrm_Seek", "FndRaise_OthrGvngPrgrm_Rcv", "Other Funding")
    )
  })
  
  
  }
# Run the application 
shinyApp(ui = ui, server = server)
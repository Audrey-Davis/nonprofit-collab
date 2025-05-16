if(!require(tidyverse)) install.packages('tidyverse')
if(!require(dplyr)) install.packages('dplyr')
if(!require(ggplot2)) install.packages('ggplot2')
if(!require(survey)) install.packages('survey')
if(!require(shiny)) install.packages('shiny')
library(tidyverse)
library(dplyr)
library(ggplot2)
library(survey)
library(shiny)

YEAR_04_DATA_PUF <- YEAR_04_DATA_PUF |>
  mutate(SizeStrata = recode(SizeStrata,
                             "1" = "<$100,000",
                             "2" = "$100,000-$499,999",
                             "3" = "$500,000-$999,999",
                             "4" = "$1 million-$9,999,999",
                             "5" = "$10 million and above"))

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
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
        plotOutput("size_plot"),
        verbatimTextOutput("size_stats")
      )
    ),
    tabPanel(
      title = "Region",
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
        plotOutput("Region_plot"),
        verbatimTextOutput("Region_stats")
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
}

# Run the application 
shinyApp(ui = ui, server = server)
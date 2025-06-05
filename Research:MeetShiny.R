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
  mutate(ExtAffairs_MeetWork = recode(ExtAffairs_MeetWork,
                             "1" = "Never",
                             "2" = "Rarely",
                             "3" = "Occasionally",
                             "4" = "Frequently",
                             "5" = "Almost all the time"),
ExtAffairs_MeetWork = factor(ExtAffairs_MeetWork,
                    levels = c(
                      "Never",
                      "Rarely",
                      "Occasionally",
                      "Frequently",
                      "Almost all the time")))

#Local Grant Stats
local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1, 
    data = data, 
    weights = ~year4wt 
  )
  Loc_Grnt_design_seek <- subset( 
    Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  
  LocGvtGrntproptable <- svyby( 
    ~FndRaise_LocGvtGrnt_Rcv, 
    ~ExtAffairs_MeetWork, 
    Loc_Grnt_design_seek, 
    svymean, 
    na.rm = TRUE 
  )

  LocGvtGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Loc_Grnt_design_seek))

  LocGvtGrntproptable$ExtAffairs_MeetWork <- as.character(LocGvtGrntproptable$ExtAffairs_MeetWork)
  LocGvtGrntWidth$ExtAffairs_MeetWork <- as.character(LocGvtGrntWidth$ExtAffairs_MeetWork)

  LocGvtGrntproptable <- left_join(LocGvtGrntproptable, LocGvtGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 * se,
    )
  LocGvtGrntproptable <- LocGvtGrntproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  LocGvtGrntproptable$ExtAffairs_MeetWork <- factor(
    LocGvtGrntproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
   plot <- ggplot(LocGvtGrntproptable) +
     geom_col(aes(y = FndRaise_LocGvtGrnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
     geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Local Gov Grant",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + ExtAffairs_MeetWork, Loc_Grnt_design_seek)
  return(chisq_result)
}

#State Grant Stats
State_grant_plot <- function(data) {
  State_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  
  #Graph with error bar
  StateGvtGrntproptable <- svyby(
    ~FndRaise_StateGvtGrnt_Rcv,
    ~ExtAffairs_MeetWork,
    State_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  StateGvtGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, State_Grnt_design_seek))
  
  StateGvtGrntproptable$ExtAffairs_MeetWork <- as.character(StateGvtGrntproptable$ExtAffairs_MeetWork)
  StateGvtGrntWidth$ExtAffairs_MeetWork <- as.character(StateGvtGrntWidth$ExtAffairs_MeetWork)
  
  StateGvtGrntproptable <- left_join(StateGvtGrntproptable, StateGvtGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se,
    )
  StateGvtGrntproptable <- StateGvtGrntproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  StateGvtGrntproptable$ExtAffairs_MeetWork <- factor(
    StateGvtGrntproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(StateGvtGrntproptable) +
    geom_col(aes(y = FndRaise_StateGvtGrnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received State Gov Grant",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + ExtAffairs_MeetWork, State_Grnt_design_seek)
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
  #Graph with error bar
  FedGvtGrntproptable <- svyby(
    ~FndRaise_FedGvtGrnt_Rcv,
    ~ExtAffairs_MeetWork,
    Fed_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
FedGvtGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Fed_Grnt_design_seek))
  
  FedGvtGrntproptable$ExtAffairs_MeetWork <- as.character(FedGvtGrntproptable$ExtAffairs_MeetWork)
  FedGvtGrntWidth$ExtAffairs_MeetWork <- as.character(FedGvtGrntWidth$ExtAffairs_MeetWork)
  
  FedGvtGrntproptable <- left_join(FedGvtGrntproptable, FedGvtGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se,
    )
  FedGvtGrntproptable <- FedGvtGrntproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  FedGvtGrntproptable$ExtAffairs_MeetWork <- factor(
    FedGvtGrntproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(FedGvtGrntproptable) +
    geom_col(aes(y = FndRaise_FedGvtGrnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Grant",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + ExtAffairs_MeetWork, Fed_Grnt_design_seek)
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
  
  #Graph with error bar
  LocGvtCntrctproptable <- svyby(
    ~FndRaise_LocGvtCntrct_Rcv,
    ~ExtAffairs_MeetWork,
    Loc_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  LocCntrctGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Loc_Cntrct_design_seek))
  
  LocGvtCntrctproptable$ExtAffairs_MeetWork <- as.character(LocGvtCntrctproptable$ExtAffairs_MeetWork)
  LocCntrctGrntWidth$ExtAffairs_MeetWork <- as.character(LocCntrctGrntWidth$ExtAffairs_MeetWork)
  
  LocGvtCntrctproptable <- left_join(LocGvtCntrctproptable, LocCntrctGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se, 
      CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se,
    )
  LocGvtCntrctproptable <- LocGvtCntrctproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  LocGvtCntrctproptable$ExtAffairs_MeetWork <- factor(
    LocGvtCntrctproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(LocGvtCntrctproptable) +
    geom_col(aes(y = FndRaise_LocGvtCntrct_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Local Gov Contract",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Loc_Cntrct_design_seek <- subset(Loc_Cntrct_design, FndRaise_LocGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + ExtAffairs_MeetWork, Loc_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + ExtAffairs_MeetWork, State_Cntrct_design_seek)
  #Graphs with error bars
  StateGvtCntrctproptable <- svyby(
    ~FndRaise_StateGvtCntrct_Rcv,
    ~ExtAffairs_MeetWork,
    State_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  StateCntrctGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, State_Cntrct_design_seek))
  
  StateGvtCntrctproptable$ExtAffairs_MeetWork <- as.character(StateGvtCntrctproptable$ExtAffairs_MeetWork)
  StateCntrctGrntWidth$ExtAffairs_MeetWork <- as.character(StateCntrctGrntWidth$ExtAffairs_MeetWork)
  
  StateGvtCntrctproptable <- left_join(StateGvtCntrctproptable, StateCntrctGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se, 
      CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se,
    )
  StateGvtCntrctproptable <- StateGvtCntrctproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  StateGvtCntrctproptable$ExtAffairs_MeetWork <- factor(
    StateGvtCntrctproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(StateGvtCntrctproptable) +
    geom_col(aes(y = FndRaise_StateGvtCntrct_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received State Gov Contract",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + ExtAffairs_MeetWork, State_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + ExtAffairs_MeetWork, Fed_Cntrct_design_seek)
  #Graphs with error bars
  FedGvtCntrctproptable <- svyby(
    ~FndRaise_FedGvtCntrct_Rcv,
    ~ExtAffairs_MeetWork,
    Fed_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  FedCntrctGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Fed_Cntrct_design_seek))
  
  FedGvtCntrctproptable$ExtAffairs_MeetWork <- as.character(FedGvtCntrctproptable$ExtAffairs_MeetWork)
  FedCntrctGrntWidth$ExtAffairs_MeetWork <- as.character(FedCntrctGrntWidth$ExtAffairs_MeetWork)
  
  FedGvtCntrctproptable <- left_join(FedGvtCntrctproptable, FedCntrctGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se, 
      CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se,
    )
  FedGvtCntrctproptable <- FedGvtCntrctproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  FedGvtCntrctproptable$ExtAffairs_MeetWork <- factor(
    FedGvtCntrctproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(FedGvtCntrctproptable) +
    geom_col(aes(y = FndRaise_FedGvtCntrct_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Contract",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + ExtAffairs_MeetWork, Fed_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + ExtAffairs_MeetWork, Priv_Grnt_design_seek)
  #Graphs with errors bars
  PrivGrntproptable <- svyby(
    ~FndRaise_PFGrnt_Rcv,
    ~ExtAffairs_MeetWork,
    Priv_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  PrivGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Priv_Grnt_design_seek))
  
  PrivGrntproptable$ExtAffairs_MeetWork <- as.character(PrivGrntproptable$ExtAffairs_MeetWork)
  PrivGrntWidth$ExtAffairs_MeetWork <- as.character(PrivGrntWidth$ExtAffairs_MeetWork)
  
  PrivGrntproptable <- left_join(PrivGrntproptable, PrivGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se,
    )
  PrivGrntproptable <- PrivGrntproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  PrivGrntproptable$ExtAffairs_MeetWork <- factor(
    PrivGrntproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(PrivGrntproptable) +
    geom_col(aes(y = FndRaise_PFGrnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Private Grants",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + ExtAffairs_MeetWork, Priv_Grnt_design_seek)
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
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + ExtAffairs_MeetWork, Community_Grnt_design_seek)
  #Graphs with error bars
  CommunityGrntproptable <- svyby(
    ~FndRaise_CFGrnt_Rcv,
    ~ExtAffairs_MeetWork,
    Community_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CommunityGrntWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Community_Grnt_design_seek))
  
  CommunityGrntproptable$ExtAffairs_MeetWork <- as.character(CommunityGrntproptable$ExtAffairs_MeetWork)
  CommunityGrntWidth$ExtAffairs_MeetWork <- as.character(CommunityGrntWidth$ExtAffairs_MeetWork)
  
  CommunityGrntproptable <- left_join(CommunityGrntproptable, CommunityGrntWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se,
    )
  CommunityGrntproptable <- CommunityGrntproptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  CommunityGrntproptable$ExtAffairs_MeetWork <- factor(
    CommunityGrntproptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(CommunityGrntproptable) +
    geom_col(aes(y = FndRaise_CFGrnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Community Foundation Grants",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Community_Grnt_design_seek <- subset(Community_Grnt_design, FndRaise_CFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + ExtAffairs_MeetWork, Community_Grnt_design_seek)
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
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + ExtAffairs_MeetWork, Donor_Funds_design_seek)
  #graphs with error bars
  Donor_Fund_proptable <- svyby(
    ~FndRaise_DAF_Rcv,
    ~ExtAffairs_MeetWork,
    Donor_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  DonorFundWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Donor_Funds_design_seek))
  
  Donor_Fund_proptable$ExtAffairs_MeetWork <- as.character(Donor_Fund_proptable$ExtAffairs_MeetWork)
  DonorFundWidth$ExtAffairs_MeetWork <- as.character(DonorFundWidth$ExtAffairs_MeetWork)
  
  Donor_Fund_proptable <- left_join(Donor_Fund_proptable, DonorFundWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_DAF_Rcv - 1.96 * se, 
      CI_High = FndRaise_DAF_Rcv + 1.96 * se,
    )
  Donor_Fund_proptable <- Donor_Fund_proptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  Donor_Fund_proptable$ExtAffairs_MeetWork <- factor(
    Donor_Fund_proptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(Donor_Fund_proptable) +
    geom_col(aes(y = FndRaise_DAF_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Donor Advised Fudns",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + ExtAffairs_MeetWork, Donor_Funds_design_seek)
  return(chisq_result)
}

#stats and graphs for corporate grants and donations
Corp_Funds_Plot <- function(data) {
  Corp_Funds_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + ExtAffairs_MeetWork, Corp_Funds_design_seek)
  #Graphs with error bars
  Corp_Fund_proptable <- svyby(
    ~FndRaise_Corp_Found_Grnt_Rcv,
    ~ExtAffairs_MeetWork,
    Corp_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CorpFundWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Corp_Funds_design_seek))
  
  Corp_Fund_proptable$ExtAffairs_MeetWork <- as.character(Corp_Fund_proptable$ExtAffairs_MeetWork)
  CorpFundWidth$ExtAffairs_MeetWork <- as.character(CorpFundWidth$ExtAffairs_MeetWork)
  
  Corp_Fund_proptable <- left_join(Corp_Fund_proptable, CorpFundWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se, 
      CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se,
    )
  Corp_Fund_proptable <- Corp_Fund_proptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  Corp_Fund_proptable$ExtAffairs_MeetWork <- factor(
    Corp_Fund_proptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(Corp_Fund_proptable) +
    geom_col(aes(y = FndRaise_Corp_Found_Grnt_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Corporate Fudns",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + ExtAffairs_MeetWork, Corp_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + ExtAffairs_MeetWork, UW_Funds_design_seek)
  
  #Graph with error bars
  UW_Fund_proptable <- svyby(
    ~FndRaise_UntdWy_Rcv,
    ~ExtAffairs_MeetWork,
    UW_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  UWFundWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, UW_Funds_design_seek))
  
  UW_Fund_proptable$ExtAffairs_MeetWork <- as.character(UW_Fund_proptable$ExtAffairs_MeetWork)
  UWFundWidth$ExtAffairs_MeetWork <- as.character(UWFundWidth$ExtAffairs_MeetWork)
  
  UW_Fund_proptable <- left_join(UW_Fund_proptable, UWFundWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se, 
      CI_High = FndRaise_UntdWy_Rcv + 1.96 * se,
    )
  UW_Fund_proptable <- UW_Fund_proptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  UW_Fund_proptable$ExtAffairs_MeetWork <- factor(
    UW_Fund_proptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(UW_Fund_proptable) +
    geom_col(aes(y = FndRaise_UntdWy_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received United Way Funds",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + ExtAffairs_MeetWork, UW_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + ExtAffairs_MeetWork, CFC_Funds_design_seek)
  
  #Graph error bars
  CFC_Fund_proptable <- svyby(
    ~FndRaise_CombFedCmpgn_Rcv,
    ~ExtAffairs_MeetWork,
    CFC_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CFCFundWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, CFC_Funds_design_seek))
  
  CFC_Fund_proptable$ExtAffairs_MeetWork <- as.character(CFC_Fund_proptable$ExtAffairs_MeetWork)
  CFCFundWidth$ExtAffairs_MeetWork <- as.character(CFCFundWidth$ExtAffairs_MeetWork)
  
  CFC_Fund_proptable <- left_join(CFC_Fund_proptable, CFCFundWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se, 
      CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se,
    )
  CFC_Fund_proptable <- CFC_Fund_proptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  CFC_Fund_proptable$ExtAffairs_MeetWork <- factor(
    CFC_Fund_proptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(CFC_Fund_proptable) +
    geom_col(aes(y = FndRaise_CombFedCmpgn_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Combined Federal Campaign Funds",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + ExtAffairs_MeetWork, CFC_Funds_design_seek)
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
    ~ExtAffairs_MeetWork,
    Other_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  OtherFundWidth <- as.data.frame(svytable(~ExtAffairs_MeetWork, Other_Funds_design_seek))
  
  Other_Fund_proptable$ExtAffairs_MeetWork <- as.character(Other_Fund_proptable$ExtAffairs_MeetWork)
  OtherFundWidth$ExtAffairs_MeetWork <- as.character(OtherFundWidth$ExtAffairs_MeetWork)
  
  Other_Fund_proptable <- left_join(Other_Fund_proptable, OtherFundWidth, by = "ExtAffairs_MeetWork") |>
    mutate (
      CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se, 
      CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se,
    )
  Other_Fund_proptable <- Other_Fund_proptable |>
    mutate(
      barwidth = (Freq/max(Freq))
    )
  
  Other_Fund_proptable$ExtAffairs_MeetWork <- factor(
    Other_Fund_proptable$ExtAffairs_MeetWork,
    levels = c("Never", "Rarely", "Occasionally", "Frequently", "Almost all the time")
  )
  
  plot <- ggplot(Other_Fund_proptable) +
    geom_col(aes(y = FndRaise_OthrGvngPrgrm_Rcv, x = ExtAffairs_MeetWork, width = barwidth), fill = "#147878") +
    geom_errorbar(aes(x = ExtAffairs_MeetWork, ymin = CI_Low, ymax = CI_High), width = 0.2) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Proportion of NGO's Who Sought and Received Other Funds",
         subtitle = "Based on frequency of meeting Gov officals to discuss the NGO's current work",
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
    data = data,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + ExtAffairs_MeetWork, Other_Funds_design_seek)
  return(chisq_result) 
}


# Define UI for application (i think this is good)
ui <- fluidPage(
  titlePanel("NGO Funding Bar Charts & Statistics"),
    id = "tab",
    tabPanel(
      title = "Meet and Greet",
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
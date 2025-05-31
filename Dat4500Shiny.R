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
                             "5" = "$10 million and above"),
    SizeStrata = factor(SizeStrata,
                             levels = c(
                             "1" = "<$100,000",
                             "2" = "$100,000-$499,999",
                             "3" = "$500,000-$999,999",
                             "4" = "$1 million-$9,999,999",
                             "5" = "$10 million and above")))
 
#Local Grant Stats
local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1, #0 means clusters and 1 means no clusters
    data = data, #if this wasn't in a function, it would be data = year_04_data
    weights = ~year4wt #put in our weights variable
  )
  Loc_Grnt_design_seek <- subset( #filter for the NGOs who actually sought it
    Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  
  LocGvtGrntproptable <- svyby( #1st two arguments in svyby are like group_by and summarize
    ~FndRaise_LocGvtGrnt_Rcv, #this is what we are summarizing, did they rcv a grant?
    ~SizeStrata, #what we are grouping by 
    Loc_Grnt_design_seek, #the filtered object we just made
    svymean, #calculate the mean which is the proportion
    na.rm = TRUE #ignore NAs
  )

  #gets the total number of NGO's in each group. made into data frame so we can left_join 
  LocGvtGrntWidth <- as.data.frame(svytotal(~SizeStrata, Loc_Grnt_design_seek))
  #svytotal made weird row names. use rownames() to get them to the column
  #Use gsub to drop the extra stuff off the name in the new column
  LocGvtGrntWidth$SizeStrata <- gsub("SizeStrata", "", rownames(LocGvtGrntWidth))
  names(LocGvtGrntWidth)[1] <- "SampleSize" #change the frist column in the data to SampleSize
  
  LocGvtGrntproptable <- left_join(LocGvtGrntproptable, LocGvtGrntWidth, by = "SizeStrata") |>
   mutate (
     bar_width = (SampleSize / max(SampleSize)), #give a relative size for each bar
     CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se, #formula to calculate CI
     CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 *se,
     SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), #keep the bars in order
     x_center = as.numeric(SizeStrata), #get the CI's to be in the middle of the bars
     xmin = x_center - bar_width / 2, #make the bar symmetrical on both sides
     xmax = x_center + bar_width / 2
   )
  
  plot <- ggplot(LocGvtGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_LocGvtGrnt_Rcv), fill = "#05bfff") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = LocGvtGrntproptable$x_center, 
                       labels = LocGvtGrntproptable$SizeStrata) +
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
  data = data,
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

StateGvtGrntWidth <- as.data.frame(svytotal(~SizeStrata, State_Grnt_design_seek))
StateGvtGrntWidth$SizeStrata <- gsub("SizeStrata", "", rownames(StateGvtGrntWidth))
names(StateGvtGrntWidth)[1] <- "SampleSize" 

StateGvtGrntproptable <- left_join(StateGvtGrntproptable, StateGvtGrntWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
    )

plot <- ggplot(StateGvtGrntproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_StateGvtGrnt_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = StateGvtGrntproptable$x_center, 
                     labels = StateGvtGrntproptable$SizeStrata) +
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
    data = data,
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

FedGvtGrntWidth <- as.data.frame(svytotal(~SizeStrata, Fed_Grnt_design_seek))
FedGvtGrntWidth$SizeStrata <- gsub("SizeStrata", "", rownames(FedGvtGrntWidth))
names(FedGvtGrntWidth)[1] <- "SampleSize" 

FedGvtGrntproptable <- left_join(FedGvtGrntproptable, FedGvtGrntWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(FedGvtGrntproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_FedGvtGrnt_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = FedGvtGrntproptable$x_center, 
                     labels = FedGvtGrntproptable$SizeStrata) +
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
    data = data,
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

LocGvtCntrctWidth <- as.data.frame(svytotal(~SizeStrata, Loc_Cntrct_design_seek))
LocGvtCntrctWidth$SizeStrata <- gsub("SizeStrata", "", rownames(LocGvtCntrctWidth))
names(LocGvtCntrctWidth)[1] <- "SampleSize" 

LocGvtCntrctproptable <- left_join(LocGvtCntrctproptable, LocGvtCntrctWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(LocGvtCntrctproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_LocGvtCntrct_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = LocGvtCntrctproptable$x_center, 
                     labels = LocGvtCntrctproptable$SizeStrata) +
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
    data = data,
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

StateGvtCntrctWidth <- as.data.frame(svytotal(~SizeStrata, State_Cntrct_design_seek))
StateGvtCntrctWidth$SizeStrata <- gsub("SizeStrata", "", rownames(StateGvtCntrctWidth))
names(StateGvtCntrctWidth)[1] <- "SampleSize" 

StateGvtCntrctproptable <- left_join(StateGvtCntrctproptable, StateGvtCntrctWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(StateGvtCntrctproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_StateGvtCntrct_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = StateGvtCntrctproptable$x_center, 
                     labels = StateGvtCntrctproptable$SizeStrata) +
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
    data = data,
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

FedGvtCntrctWidth <- as.data.frame(svytotal(~SizeStrata, Fed_Cntrct_design_seek))
FedGvtCntrctWidth$SizeStrata <- gsub("SizeStrata", "", rownames(FedGvtCntrctWidth))
names(FedGvtCntrctWidth)[1] <- "SampleSize" 

FedGvtCntrctproptable <- left_join(FedGvtCntrctproptable, FedGvtCntrctWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se,
    CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(FedGvtCntrctproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_FedGvtCntrct_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = FedGvtCntrctproptable$x_center, 
                     labels = FedGvtCntrctproptable$SizeStrata) +
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
    data = data,
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

PrivGrntWidth <- as.data.frame(svytotal(~SizeStrata, Priv_Grnt_design_seek))
PrivGrntWidth$SizeStrata <- gsub("SizeStrata", "", rownames(PrivGrntWidth))
names(PrivGrntWidth)[1] <- "SampleSize" 

PrivGrntproptable <- left_join(PrivGrntproptable, PrivGrntWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(PrivGrntproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_PFGrnt_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = PrivGrntproptable$x_center, 
                     labels = PrivGrntproptable$SizeStrata) +
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
    data = data,
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

CommunityGrntWidth <- as.data.frame(svytotal(~SizeStrata, Community_Grnt_design_seek))
CommunityGrntWidth$SizeStrata <- gsub("SizeStrata", "", rownames(CommunityGrntWidth))
names(CommunityGrntWidth)[1] <- "SampleSize" 

CommunityGrntproptable <- left_join(CommunityGrntproptable, CommunityGrntWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se,
    CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(CommunityGrntproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_CFGrnt_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = CommunityGrntproptable$x_center, 
                     labels = CommunityGrntproptable$SizeStrata) +
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
    data = data,
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

DonorFundWidth <- as.data.frame(svytotal(~SizeStrata, Donor_Funds_design_seek))
DonorFundWidth$SizeStrata <- gsub("SizeStrata", "", rownames(DonorFundWidth))
names(DonorFundWidth)[1] <- "SampleSize" 

DonorFundproptable <- left_join(DonorFundproptable, DonorFundWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_DAF_Rcv - 1.96 * se,
    CI_High = FndRaise_DAF_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(DonorFundproptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_DAF_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = DonorFundproptable$x_center, 
                     labels = DonorFundproptable$SizeStrata) +
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
    data = data,
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

CorpFundWidth <- as.data.frame(svytotal(~SizeStrata, Corp_Funds_design_seek))
CorpFundWidth$SizeStrata <- gsub("SizeStrata", "", rownames(CorpFundWidth))
names(CorpFundWidth)[1] <- "SampleSize" 

Corp_Fund_proptable <- left_join(Corp_Fund_proptable, CorpFundWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se,
    CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(Corp_Fund_proptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_Corp_Found_Grnt_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = Corp_Fund_proptable$x_center, 
                     labels = Corp_Fund_proptable$SizeStrata) +
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
    data = data,
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

UWFundWidth <- as.data.frame(svytotal(~SizeStrata, UW_Funds_design_seek))
UWFundWidth$SizeStrata <- gsub("SizeStrata", "", rownames(UWFundWidth))
names(UWFundWidth)[1] <- "SampleSize" 

UW_Fund_proptable <- left_join(UW_Fund_proptable, UWFundWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se,
    CI_High = FndRaise_UntdWy_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(UW_Fund_proptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_UntdWy_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = UW_Fund_proptable$x_center, 
                     labels = UW_Fund_proptable$SizeStrata) +
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
    data = data,
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

CFCFundWidth <- as.data.frame(svytotal(~SizeStrata, CFC_Funds_design_seek))
CFCFundWidth$SizeStrata <- gsub("SizeStrata", "", rownames(CFCFundWidth))
names(CFCFundWidth)[1] <- "SampleSize" 

CFC_Fund_proptable <- left_join(CFC_Fund_proptable, CFCFundWidth, by = "SizeStrata") |>
  mutate(
    bar_width = (SampleSize / max(SampleSize)),
    CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se,
    CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se,
    SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
    x_center = as.numeric(SizeStrata), 
    xmin = x_center - bar_width / 2, 
    xmax = x_center + bar_width / 2
  )

plot <- ggplot(CFC_Fund_proptable) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_CombFedCmpgn_Rcv), fill = "#05bfff") +
  geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
  scale_x_continuous(breaks = CFC_Fund_proptable$x_center, 
                     labels = CFC_Fund_proptable$SizeStrata) +
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
      data = data,
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
  
OtherFundWidth <- as.data.frame(svytotal(~SizeStrata, Other_Funds_design_seek))
OtherFundWidth$SizeStrata <- gsub("SizeStrata", "", rownames(OtherFundWidth))
  names(OtherFundWidth)[1] <- "SampleSize" 
  
  Other_Fund_proptable <- left_join(Other_Fund_proptable, OtherFundWidth, by = "SizeStrata") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se,
      CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se,
      SizeStrata = factor(SizeStrata, levels = unique(SizeStrata)), 
      x_center = as.numeric(SizeStrata), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(Other_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_OthrGvngPrgrm_Rcv), fill = "#05bfff") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = Other_Fund_proptable$x_center, 
                       labels = Other_Fund_proptable$SizeStrata) +
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
    data = data,
    weights = ~year4wt
  )
  
  Other_Funds_design_seek <- subset(Other_Funds_design, FndRaise_OthrGvngPrgrm_Seek == 1)
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + SizeStrata, Other_Funds_design_seek)
 return(chisq_result) 
}

#########Sector########
Sec_local_grant_plot <- function(data) {
  Loc_Grnt_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  Loc_Grnt_design_seek <- subset(Loc_Grnt_design, FndRaise_LocGvtGrnt_Seek == 1)
  contingency_table <- svytable(~FndRaise_LocGvtGrnt_Rcv + ntmaj12, Loc_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + ntmaj12, Loc_Grnt_design_seek)
  #Local Grant Graph with error bar
  LocGvtGrntproptable <- svyby(
    ~FndRaise_LocGvtGrnt_Rcv,
    ~ntmaj12,
    Loc_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  LocGvtGrntWidth <- as.data.frame(svytotal(~ntmaj12, Loc_Grnt_design_seek))
  LocGvtGrntWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(LocGvtGrntWidth))
  names(LocGvtGrntWidth)[1] <- "SampleSize" 
  
  LocGvtGrntproptable <- left_join(LocGvtGrntproptable, LocGvtGrntWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_LocGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_LocGvtGrnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(LocGvtGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_LocGvtGrnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = LocGvtGrntproptable$x_center, 
                       labels = LocGvtGrntproptable$ntmaj12) +
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
  chisq_result <- svychisq(~FndRaise_LocGvtGrnt_Rcv + ntmaj12, Loc_Grnt_design_seek)
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
  contingency_table <- svytable(~FndRaise_StateGvtGrnt_Rcv + ntmaj12, State_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + ntmaj12, State_Grnt_design_seek)
  #Graph with error bar
  StateGvtGrntproptable <- svyby(
    ~FndRaise_StateGvtGrnt_Rcv,
    ~ntmaj12,
    State_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
StateGvtGrntWidth <- as.data.frame(svytotal(~ntmaj12, State_Grnt_design_seek))
StateGvtGrntWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(StateGvtGrntWidth))
  names(StateGvtGrntWidth)[1] <- "SampleSize" 
  
  StateGvtGrntproptable <- left_join(StateGvtGrntproptable, StateGvtGrntWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_StateGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_StateGvtGrnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(StateGvtGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_StateGvtGrnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = StateGvtGrntproptable$x_center, 
                       labels = StateGvtGrntproptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  State_Grnt_design_seek <- subset(State_Grnt_design, FndRaise_StateGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtGrnt_Rcv + ntmaj12, State_Grnt_design_seek)
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
  contingency_table <- svytable(~FndRaise_FedGvtGrnt_Rcv + ntmaj12, Fed_Grnt_design_seek)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + ntmaj12, Fed_Grnt_design_seek)
  #Graph with error bar
  FedGvtGrntproptable <- svyby(
    ~FndRaise_FedGvtGrnt_Rcv,
    ~ntmaj12,
    Fed_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
FedGvtGrntWidth <- as.data.frame(svytotal(~ntmaj12, Fed_Grnt_design_seek))
FedGvtGrntWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(FedGvtGrntWidth))
  names(FedGvtGrntWidth)[1] <- "SampleSize" 
  
  FedGvtGrntproptable <- left_join(FedGvtGrntproptable, FedGvtGrntWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_FedGvtGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_FedGvtGrnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(FedGvtGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_FedGvtGrnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = FedGvtGrntproptable$x_center, 
                       labels = FedGvtGrntproptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  Fed_Grnt_design_seek <- subset(Fed_Grnt_design, FndRaise_FedGvtGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtGrnt_Rcv + ntmaj12, Fed_Grnt_design_seek)
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
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + ntmaj12, Loc_Cntrct_design_seek)
  #Graph with error bar
  LocGvtCntrctproptable <- svyby(
    ~FndRaise_LocGvtCntrct_Rcv,
    ~ntmaj12,
    Loc_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
LocGvtCntrctWidth <- as.data.frame(svytotal(~ntmaj12, Loc_Cntrct_design_seek))
LocGvtCntrctWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(LocGvtCntrctWidth))
  names(LocGvtCntrctWidth)[1] <- "SampleSize" 
  
  LocGvtCntrctproptable <- left_join(LocGvtCntrctproptable, LocGvtCntrctWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_LocGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_LocGvtCntrct_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(LocGvtCntrctproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_LocGvtCntrct_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = LocGvtCntrctproptable$x_center, 
                       labels = LocGvtCntrctproptable$ntmaj12) +
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
  chisq_result <- svychisq(~FndRaise_LocGvtCntrct_Rcv + ntmaj12, Loc_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + ntmaj12, State_Cntrct_design_seek)
  #Graphs with error bars
  StateGvtCntrctproptable <- svyby(
    ~FndRaise_StateGvtCntrct_Rcv,
    ~ntmaj12,
    State_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
StateGvtCntrctWidth <- as.data.frame(svytotal(~ntmaj12, State_Cntrct_design_seek))
StateGvtCntrctWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(StateGvtCntrctWidth))
  names(StateGvtCntrctWidth)[1] <- "SampleSize" 
  
  StateGvtCntrctproptable <- left_join(StateGvtCntrctproptable, StateGvtCntrctWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_StateGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_StateGvtCntrct_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(StateGvtCntrctproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_StateGvtCntrct_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = StateGvtCntrctproptable$x_center, 
                       labels = StateGvtCntrctproptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  State_Cntrct_design_seek <- subset(State_Cntrct_design, FndRaise_StateGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_StateGvtCntrct_Rcv + ntmaj12, State_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + ntmaj12, Fed_Cntrct_design_seek)
  #Graphs with error bars
  FedGvtCntrctproptable <- svyby(
    ~FndRaise_FedGvtCntrct_Rcv,
    ~ntmaj12,
    Fed_Cntrct_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  FedGvtCntrctWidth <- as.data.frame(svytotal(~ntmaj12, Fed_Cntrct_design_seek))
  FedGvtCntrctWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(FedGvtCntrctWidth))
  names(FedGvtCntrctWidth)[1] <- "SampleSize" 
  
  FedGvtCntrctproptable <- left_join(FedGvtCntrctproptable, FedGvtCntrctWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_FedGvtCntrct_Rcv - 1.96 * se,
      CI_High = FndRaise_FedGvtCntrct_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(FedGvtCntrctproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_FedGvtCntrct_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = FedGvtCntrctproptable$x_center, 
                       labels = FedGvtCntrctproptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  Fed_Cntrct_design_seek <- subset(Fed_Cntrct_design, FndRaise_FedGvtCntrct_Seek == 1)
  chisq_result <- svychisq(~FndRaise_FedGvtCntrct_Rcv + ntmaj12, Fed_Cntrct_design_seek)
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
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + ntmaj12, Priv_Grnt_design_seek)
  #Graphs with errors bars
  PrivGrntproptable <- svyby(
    ~FndRaise_PFGrnt_Rcv,
    ~ntmaj12,
    Priv_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
 PrivGrntWidth <- as.data.frame(svytotal(~ntmaj12, Priv_Grnt_design_seek))
 PrivGrntWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(PrivGrntWidth))
  names(PrivGrntWidth)[1] <- "SampleSize" 
  
  PrivGrntproptable <- left_join(PrivGrntproptable, PrivGrntWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_PFGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_PFGrnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(PrivGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_PFGrnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = PrivGrntproptable$x_center, 
                       labels = PrivGrntproptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  Priv_Grnt_design_seek <- subset(Priv_Grnt_design, FndRaise_PFGrnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_PFGrnt_Rcv + ntmaj12, Priv_Grnt_design_seek)
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
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + ntmaj12, Community_Grnt_design_seek)
  #Graphs with error bars
  CommunityGrntproptable <- svyby(
    ~FndRaise_CFGrnt_Rcv,
    ~ntmaj12,
    Community_Grnt_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CommunityGrntWidth <- as.data.frame(svytotal(~ntmaj12, Community_Grnt_design_seek))
  CommunityGrntWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(CommunityGrntWidth))
  names(CommunityGrntWidth)[1] <- "SampleSize" 
  
  CommunityGrntproptable <- left_join(CommunityGrntproptable, CommunityGrntWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_CFGrnt_Rcv - 1.96 * se,
      CI_High = FndRaise_CFGrnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(CommunityGrntproptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_CFGrnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = CommunityGrntproptable$x_center, 
                       labels = CommunityGrntproptable$ntmaj12) +
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
  chisq_result <- svychisq(~FndRaise_CFGrnt_Rcv + ntmaj12, Community_Grnt_design_seek)
  return(chisq_result) 
}

#stats and graph for donor advised funds
Sec_Donor_Funds_Plot <- function(data) {
  Donor_Funds_design <- svydesign(
    ids = ~1,
    data = data,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + ntmaj12, Donor_Funds_design_seek)
  #graphs with error bars
  Donor_Fund_proptable <- svyby(
    ~FndRaise_DAF_Rcv,
    ~ntmaj12,
    Donor_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
 DonorFundWidth <- as.data.frame(svytotal(~ntmaj12, Donor_Funds_design_seek))
 DonorFundWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(DonorFundWidth))
  names(DonorFundWidth)[1] <- "SampleSize" 
  
  Donor_Fund_proptable <- left_join(Donor_Fund_proptable, DonorFundWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_DAF_Rcv - 1.96 * se,
      CI_High = FndRaise_DAF_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(Donor_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_DAF_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = Donor_Fund_proptable$x_center, 
                       labels = Donor_Fund_proptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  Donor_Funds_design_seek <- subset(Donor_Funds_design, FndRaise_DAF_Seek == 1)
  chisq_result <- svychisq(~FndRaise_DAF_Rcv + ntmaj12, Donor_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + ntmaj12, Corp_Funds_design_seek)
  #Graphs with error bars
  Corp_Fund_proptable <- svyby(
    ~FndRaise_Corp_Found_Grnt_Rcv,
    ~ntmaj12,
    Corp_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CorpFundWidth <- as.data.frame(svytotal(~ntmaj12, Corp_Funds_design_seek))
  CorpFundWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(CorpFundWidth))
  names(CorpFundWidth)[1] <- "SampleSize" 
  
  Corp_Fund_proptable <- left_join(Corp_Fund_proptable, CorpFundWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_Corp_Found_Grnt_Rcv - 1.96 * se,
      CI_High = FndRaise_Corp_Found_Grnt_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(Corp_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_Corp_Found_Grnt_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = Corp_Fund_proptable$x_center, 
                       labels = Corp_Fund_proptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  Corp_Funds_design_seek <- subset(Corp_Funds_design, FndRaise_Corp_Found_Grnt_Seek == 1)
  chisq_result <- svychisq(~FndRaise_Corp_Found_Grnt_Rcv + ntmaj12, Corp_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + ntmaj12, UW_Funds_design_seek)
  
  #Graph with error bars
  UW_Fund_proptable <- svyby(
    ~FndRaise_UntdWy_Rcv,
    ~ntmaj12,
    UW_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  UWFundWidth <- as.data.frame(svytotal(~ntmaj12, UW_Funds_design_seek))
  UWFundWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(UWFundWidth))
  names(UWFundWidth)[1] <- "SampleSize" 
  
  UW_Fund_proptable <- left_join(UW_Fund_proptable, UWFundWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_UntdWy_Rcv - 1.96 * se,
      CI_High = FndRaise_UntdWy_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(UW_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_UntdWy_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = UW_Fund_proptable$x_center, 
                       labels = UW_Fund_proptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  UW_Funds_design_seek <- subset(UW_Funds_design, FndRaise_UntdWy_Seek == 1)
  chisq_result <- svychisq(~FndRaise_UntdWy_Rcv + ntmaj12, UW_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + ntmaj12, CFC_Funds_design_seek)
  
  #Graph error bars
  CFC_Fund_proptable <- svyby(
    ~FndRaise_CombFedCmpgn_Rcv,
    ~ntmaj12,
    CFC_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  CFCFundWidth <- as.data.frame(svytotal(~ntmaj12, CFC_Funds_design_seek))
  CFCFundWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(CFCFundWidth))
  names(CFCFundWidth)[1] <- "SampleSize" 
  
  CFC_Fund_proptable <- left_join(CFC_Fund_proptable, CFCFundWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_CombFedCmpgn_Rcv - 1.96 * se,
      CI_High = FndRaise_CombFedCmpgn_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(CFC_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_CombFedCmpgn_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = CFC_Fund_proptable$x_center, 
                       labels = CFC_Fund_proptable$ntmaj12) +
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
    data = data,
    weights = ~year4wt
  )
  
  CFC_Funds_design_seek <- subset(CFC_Funds_design, FndRaise_CombFedCmpgn_Seek == 1)
  chisq_result <- svychisq(~FndRaise_CombFedCmpgn_Rcv + ntmaj12, CFC_Funds_design_seek)
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
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + ntmaj12, Other_Funds_design_seek)
  #graphs with error bars
  Other_Fund_proptable <- svyby(
    ~FndRaise_OthrGvngPrgrm_Rcv,
    ~ntmaj12,
    Other_Funds_design_seek,
    svymean,
    na.rm = TRUE
  )
  
  OtherFundWidth <- as.data.frame(svytotal(~ntmaj12, Other_Funds_design_seek))
  OtherFundWidth$ntmaj12 <- gsub("ntmaj12", "", rownames(OtherFundWidth))
  names(OtherFundWidth)[1] <- "SampleSize" 
  
  Other_Fund_proptable <- left_join(Other_Fund_proptable, OtherFundWidth, by = "ntmaj12") |>
    mutate(
      bar_width = (SampleSize / max(SampleSize)),
      CI_Low = FndRaise_OthrGvngPrgrm_Rcv - 1.96 * se,
      CI_High = FndRaise_OthrGvngPrgrm_Rcv + 1.96 * se,
      ntmaj12 = factor(ntmaj12, levels = unique(ntmaj12)), 
      x_center = as.numeric(ntmaj12), 
      xmin = x_center - bar_width / 2, 
      xmax = x_center + bar_width / 2
    )
  
  plot <- ggplot(Other_Fund_proptable) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = FndRaise_OthrGvngPrgrm_Rcv), fill = "#fb602b") +
    geom_errorbar(aes(x = x_center, ymin = CI_Low, ymax = CI_High), width = 0.1) +
    scale_x_continuous(breaks = Other_Fund_proptable$x_center, 
                       labels = Other_Fund_proptable$ntmaj12) +
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
  chisq_result <- svychisq(~FndRaise_OthrGvngPrgrm_Rcv + ntmaj12, Other_Funds_design_seek)
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
}
# Run the application 
shinyApp(ui = ui, server = server)
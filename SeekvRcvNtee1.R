library(tidyverse)
library(dplyr)
library(ggplot2)

YEAR_04_DATA_PUF<- YEAR_04_DATA_PUF |>
  mutate(ntee1 = recode(ntee1,
                               A = "I.", B = "II.", C = "III.", D = "III.",
                               E = "IV.", F = "IV.", G = "IV.", H = "IV.",
                               I = "V.", J = "V.", K = "V.", L = "V.",
                               M = "V.", N = "V.", O = "V.", P = "V.",
                               Q = "VI.", R = "VII.", S = "VII.", T = "VII.",
                               U = "VII.", V = "VII.", W = "VII.",
                               X = "VIII.", Y = "IX.", Z = "X."))

YEAR_04_DATA_PUF <- YEAR_04_DATA_PUF|>
  rename(
  I. = "Arts, Culture, and Humanities",
  II. = "Education",
  III. = "Environment and Animals",
  IV. = "Health",
  V. = "Human Services",
  VI. = "International, Foreign Affairs",
  VII. = "Public, Societal Benefit",
  VIII. = "Religion Related",
  IX. = "Mutual/Membership Benefit",
  X. = "Unknown, Unclassified"
)


Loc_Seek_Rcv_Grant<- YEAR_04_DATA_PUF |>
  filter(FndRaise_LocGvtGrnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_LocGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Loc_Seek_Rcv_Grant, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Local Gov Grant",
       subtitle = "Based on Region",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Seek and Receive State Grants
State_Seek_Rcv_Grant<- YEAR_04_DATA_PUF |>
  filter(FndRaise_StateGvtGrnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_StateGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(State_Seek_Rcv_Grant, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Grant",
       subtitle = "Based on Region",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#small NGOs are less successful for getting state grants 

#Seek and Receive Fed Gov Grants
Fed_Seek_Rcv_Grant<- YEAR_04_DATA_PUF |>
  filter(FndRaise_FedGvtGrnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_FedGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Fed_Seek_Rcv_Grant, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Grant",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#small NGOs get way less Fed Grants

#Seek and Receive Local Gov Contracts
Loc_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_LocGvtCntrct_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_LocGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Loc_Seek_Rcv_Cntrct, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Local Gov Contract",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#% Less small NGOs get local gov contracts

#Seek and Receive State Gov Contracts
State_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_StateGvtCntrct_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_StateGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(State_Seek_Rcv_Cntrct, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Contract",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# very few small NGOs get state contracts

#Seek and Receive Fed Gov Contracts
Fed_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_FedGvtCntrct_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_FedGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Fed_Seek_Rcv_Cntrct, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Federal Gov Contract",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#This is predicted. bigger NGOs get federal contracts easier than small ones. 

#Seek and Receive Private Grants
Private_Seek_Rcv_Grants<- YEAR_04_DATA_PUF |>
  filter(FndRaise_PFGrnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_PFGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Private_Seek_Rcv_Grants, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "red") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Private Grants",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#somewhat spread out. small ones get less private grants

#Seek and Receive Community Foundation Grants
Community_Seek_Rcv_Grants <- YEAR_04_DATA_PUF |>
  filter(FndRaise_CFGrnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_CFGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Community_Seek_Rcv_Grants, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "red") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Community Foundation Grants",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#somewhat spread out. small ones get less private grants

#Seek and Receive donor advised funds
Donor_Seek_Rcv_Funds <- YEAR_04_DATA_PUF |>
  filter(FndRaise_DAF_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_DAF_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Donor_Seek_Rcv_Funds, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "red") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Donor Advised Funds",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#somewhat spread out. small ones get less private grants

#Seek and Receive Corporate Grants and Donations
Corp_Seek_Rcv_Funds <- YEAR_04_DATA_PUF |>
  filter(FndRaise_Corp_Found_Grnt_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_Corp_Found_Grnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Corp_Seek_Rcv_Funds, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "orange") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Corporate Funding",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Pretty even 

#Seek and Receive United Way Funding
UW_Seek_Rcv_Funds <- YEAR_04_DATA_PUF |>
  filter(FndRaise_UntdWy_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_UntdWy_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(UW_Seek_Rcv_Funds, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "orange") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received United Way Funding",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Pretty even 

#Seek and Receive Combined Federal Campaign
Cfc_Seek_Rcv_Funds <- YEAR_04_DATA_PUF |>
  filter(FndRaise_CombFedCmpgn_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_CombFedCmpgn_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Cfc_Seek_Rcv_Funds, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "orange") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Combined Federal Campaign",
       subtitle = "Based on Region)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Smaller NGOs are not good at getting funds from the combined federal campaign

#Seek and Receive Combined Federal Campaign
Other_Seek_Rcv_Funds <- YEAR_04_DATA_PUF |>
  filter(FndRaise_OthrGvngPrgrm_Seek == 1) |>
  group_by(CensusRegion9) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_OthrGvngPrgrm_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Other_Seek_Rcv_Funds, aes(x = CensusRegion9, y = ProportionRcv)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Other Funding",
       subtitle = "Based on Nonprofit Size (calculated by annual expense)",
       x = "Nonprofit Size", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Big NGOs are the best at getting other funding 



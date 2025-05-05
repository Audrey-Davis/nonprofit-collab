library(tidyverse)
library(dplyr)
library(ggplot2)


### LOCAL FUNDING
YEAR_04_DATA_PUF<- YEAR_04_DATA_PUF |>
  mutate(ExtAffairs_GenEd = recode(ExtAffairs_GenEd, 
                "1" = "Never",
                "2" = "Rarely",
                "3" = "Occasionaly",
                "4" = "Frequently",
                "5" = "Almost all the time"))

local_nonseek_receive <- YEAR_04_DATA_PUF |>
  filter(FndRaise_LocGvtGrnt_Seek == 0, FndRaise_LocGvtGrnt_Rcv == 1)

local_GenEd <- local_nonseek_receive |>
  group_by(ExtAffairs_GenEd) |>
  summarize(weighted_count = sum(year4wt, na.rm = TRUE), .groups = "drop") |>
  mutate(proportion = weighted_count / sum(weighted_count))

ggplot(local_GenEd, aes( x = ExtAffairs_GenEd, y = proportion)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Weighted Share of NGOs That Received Local Gov Funding Without Seeking",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###State Funding 

state_nonseek_receive <- YEAR_04_DATA_PUF |>
  filter(FndRaise_StateGvtGrnt_Seek == 0, FndRaise_StateGvtGrnt_Rcv == 1)

state_GenEd <- state_nonseek_receive |>
  group_by(ExtAffairs_GenEd) |>
  summarize(weighted_count = sum(year4wt, na.rm = TRUE), .groups = "drop") |>
  mutate(proportion = weighted_count / sum(weighted_count))

ggplot(state_GenEd, aes( x = ExtAffairs_GenEd, y = proportion)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Weighted Share of NGOs That Received State Gov Funding Without Seeking",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Federal Funding
Fed_nonseek_receive <- YEAR_04_DATA_PUF |>
  filter(FndRaise_FedGvtGrnt_Seek == 0, FndRaise_FedGvtGrnt_Rcv == 1)

Fed_GenEd <- Fed_nonseek_receive |>
  group_by(ExtAffairs_GenEd) |>
  summarize(weighted_count = sum(year4wt, na.rm = TRUE), .groups = "drop") |>
  mutate(proportion = weighted_count / sum(weighted_count))

ggplot(Fed_GenEd, aes( x = ExtAffairs_GenEd, y = proportion)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Weighted Share of NGOs That Received Fed Gov Funding Without Seeking",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







#Seek and Receive stuff Local grants
local_Seek_Receive<- YEAR_04_DATA_PUF |>
  filter(FndRaise_LocGvtGrnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(year4wt,na.rm = TRUE), 
            RcvSum = sum(FndRaise_LocGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(local_Seek_Receive, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Local Gov Grant",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#nothing to write home about
  
#Seek and Receive stuff state grants
State_Seek_Receive<- YEAR_04_DATA_PUF |>
  filter(FndRaise_StateGvtGrnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_StateGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(State_Seek_Receive, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Grant",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#nothing to write home about. all the time is low but frequenlty is high. weird

#Seek and Receive stuff Fed grants
Fed_Seek_Receive<- YEAR_04_DATA_PUF |>
  filter(FndRaise_FedGvtGrnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_FedGvtGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Fed_Seek_Receive, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "tomato") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Grant",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#nothing to write home about. all the time is low but frequenlty is high. weird

#Seek and Receive stuff Fed Contracts
Fed_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_FedGvtCntrct_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_FedGvtCntrct_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_FedGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Fed_Seek_Rcv_Cntrct, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Fed Gov Contract",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#in general people are less successful. is it harder to get contract than grant?

#Seek and Receive stuff Local Contracts
Local_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_LocGvtCntrct_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_LocGvtCntrct_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_LocGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Local_Seek_Rcv_Cntrct, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Local Gov Contract",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Never is noticably low. very interesting 

#Seek and Receive stuff State Contracts
State_Seek_Rcv_Cntrct<- YEAR_04_DATA_PUF |>
  filter(FndRaise_StateGvtCntrct_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_StateGvtCntrct_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_StateGvtCntrct_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(State_Seek_Rcv_Cntrct, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "blue") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received State Gov Contract",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Never is noticably low. very interesting 

#Seek and Receive stuff private foundation grants
Priv_Seek_Rcv_Grant<- YEAR_04_DATA_PUF |>
  filter(FndRaise_PFGrnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_PFGrnt_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_PFGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Priv_Seek_Rcv_Grant, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "green") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Private Grants",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#all the same basically lmao

#Seek and Receive stuff community foundation grants
Community_Seek_Rcv_Grant<- YEAR_04_DATA_PUF |>
  filter(FndRaise_CFGrnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_CFGrnt_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_CFGrnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Community_Seek_Rcv_Grant, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "green") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Community Grants",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#all the same basically, nothing to see here. I thought there would be more of a difference

#Seek and Receive stuff Donor Advised Funds
Donor_Seek_Rcv_Funds<- YEAR_04_DATA_PUF |>
  filter(FndRaise_DAF_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_DAF_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_DAF_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Donor_Seek_Rcv_Funds, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "purple") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Donor Advised Funds",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#all the same basically, nothing to see here. I thought there would be more of a difference


#Seek and Receive stuff Corporate grants or donations
Corp_Seek_Rcv_GC<- YEAR_04_DATA_PUF |>
  filter(FndRaise_Corp_Found_Grnt_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_Corp_Found_Grnt_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_Corp_Found_Grnt_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Corp_Seek_Rcv_GC, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "orange") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Coporate Grants or Donations",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#all very similar. high success rate 

#Seek and Receive stuff United Way
UW_Seek_Rcv_Fund<- YEAR_04_DATA_PUF |>
  filter(FndRaise_UntdWy_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_UntdWy_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_UntdWy_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(UW_Seek_Rcv_Fund, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "yellow") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received United Way Funding",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#low success for people who always engage in gen ed public education

#Seek and Receive stuff Combined Federal Campaign
Cfc_Seek_Rcv_Fund<- YEAR_04_DATA_PUF |>
  filter(FndRaise_CombFedCmpgn_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_CombFedCmpgn_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_CombFedCmpgn_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Cfc_Seek_Rcv_Fund, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "black") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Combined Federal Campaign Funding",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#mixed bag. high success for frequently though 

#Seek and Receive stuff Other Federated Giving Programs
Other_Seek_Rcv_Fund<- YEAR_04_DATA_PUF |>
  filter(FndRaise_OthrGvngPrgrm_Seek == 1) |>
  group_by(ExtAffairs_GenEd) |>
  summarize(SeekSum = sum(FndRaise_OthrGvngPrgrm_Seek * year4wt,  na.rm = TRUE), 
            RcvSum = sum(FndRaise_OthrGvngPrgrm_Rcv * year4wt, na.rm = TRUE),
            .groups = "drop") |>
  mutate(ProportionRcv = ifelse(SeekSum == 0, NA, RcvSum / SeekSum))

ggplot(Other_Seek_Rcv_Fund, aes(x = ExtAffairs_GenEd, y = ProportionRcv)) +
  geom_col(fill = "grey") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of NGO's Who Sought and Received Other Federal Giving Funding",
       subtitle = "Public Education (Strategic Visibility)",
       x = "Public Education Frequency", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#mixed bag like the one above. most of them seek and rcv but never do gen ed. 


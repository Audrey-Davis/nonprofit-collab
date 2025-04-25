library(tidyverse)
library(dplyr)
library(ggplot2)
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


















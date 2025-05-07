library(tidyverse)
library(dplyr)
library(ggplot2)

data24 <- read_csv("YEAR-04-DATA-PUF.csv")

data24 <- data24 %>%
  mutate(sought_and_received = ifelse(FndRaise_LocGvtGrnt_Seek == 1 & FndRaise_LocGvtGrnt_Rcv == 1, 1, 0))

table(data24$sought_and_received)

table_region <- table(data24$CensusRegion9, data24$sought_and_received)
chisq.test(table_region)

# Perform chi-square test and extract null/expected if independent counts
chisq_result_region <- chisq.test(table_region)

# Convert observed table to data frame
observed_region <- as.data.frame(table_region)
colnames(observed_region) <- c("CensusRegion9", "Sought_Received", "Observed")

# Manually build null hypo data frame with proper column names
expected_vals_region <- chisq_result$expected
expected_region <- as.data.frame(as.table(expected_vals_region))
colnames(expected_region) <- c("CensusRegion9", "Sought_Received", "Null")

plot_region <- merge(observed_region, expected_region, by = c("CensusRegion9", "Sought_Received"))

# Filter only those who sought and received 
plot_df_filtered <- plot_region |>
  filter(Sought_Received == 1) |>
  complete(CensusRegion9 = factor(1:9), fill = list(Observed = 0, Null = 0))

ggplot(plot_df_filtered, aes(x = factor(CensusRegion9))) +
  geom_bar(aes(y = Observed), stat = "identity", fill = "#2c7fb8", alpha = 0.8) +
  geom_point(aes(y = Null), color = "red", size = 3) +
  labs(
    title = "Observed vs. Null Hypothesis NGOs That \nSought & Received Funding by Size",
    x = "NGO Census Region",
    y = "Count",
    caption = "Bars = Observed | Red Dots = Null Hypothesis"
  ) +
  theme_minimal()

### Weighted proportions maybe

data24_filtered <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 1)

region_weighted <- data24_filtered %>%
  group_by(CensusRegion9) %>%
  summarize(
    total_weight = sum(year4wt, na.rm = TRUE),
    received_weight = sum(year4wt[sought_and_received == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(proportion_received = received_weight / total_weight)

ggplot(region_weighted, aes(x = factor(CensusRegion9), y = proportion_received)) +
  geom_col(fill = "#2c7fb8", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Weighted Proportion of NGOs Who Sought\n and Received Local Gov Grant",
    x = "Census Region",
    y = "Proportion (Weighted)",
    caption = "Proportions calculated using year4wt"
  ) +
  theme_minimal()
#######
#######
#######

library(tidyverse)
library(survey)

data24 <- data24 %>%
  mutate(
    sought_and_received = ifelse(FndRaise_LocGvtGrnt_Seek == 1 & FndRaise_LocGvtGrnt_Rcv == 1, 1, 0)
  )

data24_seek <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 1)

# Define the survey design
design <- svydesign(
  ids = ~1, 
  data = data24_seek, 
  weights = ~year4wt
)

# Estimate weighted proportions by CensusRegion9
prop_region <- svyby(
  ~sought_and_received,
  ~CensusRegion9,
  design,
  svymean,
  na.rm = TRUE
)

# Rename columns for clarity
prop_region <- prop_region %>%
  rename(
    proportion = sought_and_received
  ) %>%
  mutate(
    ci_lower = proportion - 1.96 * se,
    ci_upper = proportion + 1.96 * se
  )

ggplot(prop_region, aes(x = factor(CensusRegion9), y = proportion)) +
  geom_col(fill = "#2c7fb8", alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  labs(
    title = "Survey-Weighted Proportion of NGOs Who\n Sought and Received Local Gov Grant",
    x = "Census Region",
    y = "Proportion (Weighted)",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






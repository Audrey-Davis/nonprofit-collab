library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
data24 <- read_csv("YEAR-04-DATA-PUF.csv")

# Create a new variable indicating sought AND received
data24 <- data24 %>%
  mutate(sought_and_received = ifelse(FndRaise_LocGvtGrnt_Seek == 1 & FndRaise_LocGvtGrnt_Rcv == 1, 1, 0))

table(data24$sought_and_received)

table_strata <- table(data24$SizeStrata, data24$sought_and_received)
chisq.test(table_strata)

# Perform chi-square test and extract null/expected if independent counts
chisq_result <- chisq.test(table_strata)

# Convert observed table to data frame
observed_df <- as.data.frame(table_strata)
colnames(observed_df) <- c("SizeStrata", "Sought_Received", "Observed")

# Manually build null hypo data frame with proper column names
expected_vals <- chisq_result$expected
expected_df <- as.data.frame(as.table(expected_vals))
colnames(expected_df) <- c("SizeStrata", "Sought_Received", "Null")

# Merge both data frames
plot_df <- merge(observed_df, expected_df, by = c("SizeStrata", "Sought_Received"))

# Filter only those who sought and received (value == 1)
plot_df_filtered <- subset(plot_df, Sought_Received == 1)

ggplot(plot_df_filtered, aes(x = factor(SizeStrata))) +
  geom_bar(aes(y = Observed), stat = "identity", fill = "#2c7fb8", alpha = 0.8) +
  geom_point(aes(y = Expected), color = "red", size = 3) +
  labs(
    title = "Observed vs. Expected NGOs That \nSought & Received Funding by Size",
    x = "NGO Size Stratum",
    y = "Count",
    caption = "Bars = Observed | Red Dots = Null Hypothesis"
  ) +
  theme_minimal()

##### PROPORTIONS
# Weighted observed proportions
obs_props <- data24 %>%
  filter(FndRaise_LocGvtGrnt_Seek == 1) %>%
  group_by(SizeStrata) %>%
  summarize(
    total_weight = sum(year4wt, na.rm = TRUE),
    received_weight = sum(year4wt[sought_and_received == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(observed_prop = received_weight / total_weight) |>
  mutate(SizeStrata = as.character(SizeStrata))

# Unweighted expected proportions from chi-squared
table_strata <- table(
  data24$SizeStrata[data24$FndRaise_LocGvtGrnt_Seek == 1],
  data24$sought_and_received[data24$FndRaise_LocGvtGrnt_Seek == 1]
)
chisq_result <- chisq.test(table_strata)

expected <- as.data.frame(as.table(chisq_result$expected))
colnames(expected) <- c("SizeStrata", "sought_and_received", "expected_count")

expected_props <- expected %>%
  filter(sought_and_received == 1) %>%
  group_by(SizeStrata) %>%
  mutate(
    total_expected = sum(expected_count),
    expected_prop = expected_count / total_expected
  ) %>%
  select(SizeStrata, expected_prop) |>
  mutate(SizeStrata = as.character(SizeStrata))

# Merge both into plot_df
plot_df <- left_join(obs_props, expected_props, by = "SizeStrata")

ggplot(plot_df, aes(x = factor(SizeStrata))) +
  geom_col(aes(y = observed_prop), fill = "#2c7fb8", alpha = 0.8) +
  geom_point(aes(y = expected_prop), color = "red", size = 3) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Observed vs. Expected Proportion of NGOs\nSought & Received Local Government Grant",
    x = "NGO Size Stratum",
    y = "Proportion",
    caption = "Bars = Observed (Weighted) \n Red Dots = Null Hypothesis (Expected Proportions)"
  ) +
  theme_minimal()

size_labels <- c("1" = "<$100,000",
                 "2" = "$100,000-$499,999",
                 "3" = "$500,000-$999,999",
                 "4" = "$1 million-$9,999,999",
                 "5" = "$10 million and above")


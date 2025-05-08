# Understanding Mental Health Trends Through Survey Data
# Author: Neeti Shah
# Date: May 9, 2025

# ----------------------------
# 1. Load Required Libraries
# ----------------------------
library(tidyverse)
library(readr)
library(ggcorrplot)
library(lubridate)

# ----------------------------
# 2. Load the Dataset
# ----------------------------
mental_health <- read_csv("mental_health.csv")
mental_health <- mental_health %>%
  rename_with(tolower) %>%
  drop_na()

# ----------------------------
# 3. Visualizations with Labels
# ----------------------------

# 1. Pie Chart – Treatment Proportion with Labels
mental_health %>%
  count(treatment) %>%
  mutate(prop = round(n / sum(n) * 100, 1),
         label = paste0(prop, "%")) %>%
  ggplot(aes(x = "", y = n, fill = treatment)) +
  geom_col(width = 1) +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Treatment Seeking Behavior", fill = "Treatment") +
  theme_void()

# 2. Bar Chart – Days Indoors with Count Labels
mental_health %>%
  count(days_indoors) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(days_indoors, n), y = n, fill = days_indoors)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.1) +
  coord_flip() +
  labs(title = "Reported Duration of Staying Indoors", x = "Days Indoors", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Segmented Bar Plot – Treatment by Gender with Percentage Labels
mental_health %>%
  count(gender, treatment) %>%
  group_by(gender) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = gender, y = n, fill = treatment)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(prop, "%")), position = position_fill(vjust = 0.5)) +
  labs(title = "Treatment Seeking Behavior by Gender", x = "Gender", y = "Proportion") +
  theme_minimal()

# 4. Segmented Bar Plot – Coping Struggles by Gender with Percentage Labels
mental_health %>%
  count(gender, coping_struggles) %>%
  group_by(gender) %>%
  mutate(prop = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = gender, y = n, fill = coping_struggles)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(prop, "%")), position = position_fill(vjust = 0.5)) +
  labs(title = "Coping Struggles by Gender", x = "Gender", y = "Proportion") +
  theme_minimal()

# 5. Bar Chart – Mood Swings with Count Labels
mental_health %>%
  filter(mood_swings %in% c("Low", "Medium", "High")) %>%
  count(mood_swings) %>%
  ggplot(aes(x = mood_swings, y = n, fill = mood_swings)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(title = "Overall Mood Swings Distribution", x = "Mood Swing Level", y = "Count") +
  theme_minimal()

# 6. Correlation Matrix
encoded <- mental_health %>%
  select(treatment, family_history, mental_health_history, coping_struggles, mood_swings, growing_stress) %>%
  mutate(across(everything(), ~ as.numeric(as.factor(.))))
cor_matrix <- cor(encoded, use = "complete.obs")
ggcorrplot(cor_matrix, lab = TRUE) +
  ggtitle("Correlation Matrix of Key Mental Health Variables")

# ----------------------------
# 4. Save Cleaned Dataset
# ----------------------------
write_csv(mental_health, "Shah_Cleaned_mental_health.csv")

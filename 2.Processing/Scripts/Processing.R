### Reshaping and merging the cleaned pilot data for DCE modeling ####
### Created by: Zoe Sidana Bunnath #############
### Updated on: 2026-07-23 ####################

# ------------------------------
# 0 — Libraries  #####
# ------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, readr etc.
library(here)        # find file paths relative to the .Rproj
library(magrittr)    # pipe (%<>%)
library(readxl)      # read_excel
library(corrplot)    # corrplot visualization
library(tibble)      # produce data frames
library(janitor)     # cleaning and tidying data
library(gt)          # produce table of results
#library(patchwork)  # combine multiple plots

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

# Read the already-cleaned data (produced + saved by 1.Cleaning/capstone_cleaning_v2.R)
# rather than re-running the whole cleaning script here.
Pilot <- read_csv(here("1.Cleaning", "Output", "Capstone_clean.csv"))

#------------------------------------------------------:
# (2): Pivot for DCE #####
#------------------------------------------------------:

# Pivot longer. Each respondent only answered ONE block of 8 tasks, so the
# other 8 Choice.Task_ columns are NA by design — values_drop_na = TRUE drops
# those automatically instead of turning them into fake "unanswered tasks".
Reshape.long.1 <- Pilot %>%
  pivot_longer(cols = starts_with("Choice.Task_"),
               names_to  = "Choice.Task",
               values_to = "Chosen.Alternative",
               values_drop_na = TRUE) %>%
  slice(rep(1:n(), each = 3)) %>%              # expand each task into 3 alternative rows
  mutate(Alternative = rep(1:3, length.out = n())) %>%
  ungroup()

# Choice.Task to numeric (strip "Choice.Task_" prefix)
Reshape.long.1 %<>%
  mutate(
    Choice.Task = gsub("Choice.Task_", "", Choice.Task),
    Choice.Task = as.numeric(Choice.Task),
    # This export already codes choices as 1/2/3 rather than text labels like
    # "Alternative one" — if your real Qualtrics export uses text labels
    # instead, recode them here the way the trail script did:
    # Chosen.Alternative = case_when(
    #   Chosen.Alternative == "Alternative one"     ~ 1,
    #   Chosen.Alternative == "Alternative two"      ~ 2,
    #   Chosen.Alternative == "None of the options"  ~ 3
    # )
    Chosen.Alternative = as.numeric(Chosen.Alternative)
  )

#------------------------------------------------------:
# Load experimental design from Ngene #####
#------------------------------------------------------:

# TODO: point this at your actual Ngene design export for the capstone survey.
design_ngene <- read_csv(here("0.RawData", "Ngene_Design(z4 - 15).csv"))

# TODO: confirm design_ngene has columns "Choice.Task" and "Alternative" that
# match the values produced above, plus one column per attribute. Based on
# your capstone attributes these are probably something like:
#   Chemical.Input.Restriction, Training.Timing,
#   Organic.Fertilizer.Support, Incentive.Payment
# — rename to match your file exactly if the names differ.

merge.data.1 <- merge(Reshape.long.1, design_ngene,
                      by = c("Choice.Task" = "Choice.Task", "Alternative" = "Alternative"),
                      all.y = TRUE)   # keep all rows from the design (right dataset)

# Binary dependent variable for the choice model
merge.data.1 %<>%
  mutate(Choice.Binary = coalesce(as.integer(Chosen.Alternative == Alternative), 0))

#------------------------------------------------------:
# (3): Reorder Columns #####
#------------------------------------------------------:

merge.data.1 %<>%
  select(
    Qualtrics.RID,
    Block,
    Choice.Task,
    Alternative,
    Chosen.Alternative,
    Choice.Binary,
    # TODO: these four are placeholders for your Ngene design's attribute
    # columns — rename to match exactly what's in design_ngene
    Chemical.Input.Restriction,
    Training.Timing,
    Organic.Fertilizer.Support,
    Incentive.Payment,
    starts_with("Demo_"),
    starts_with("Farm_"),
    Baseline_Fert.kg,
    Baseline_Herbicide,
    everything(),
  )

merge.data.1 %<>%
  arrange(Qualtrics.RID)

#------------------------------------------------------:
# (4): Reclass Columns #####
#------------------------------------------------------:

# TODO: if any attribute (e.g. Chemical.Input.Restriction) is ordinal like the
# trail survey's Habitat_Quality/Trail_Condition/Crowding, recode + order it
# the same way, e.g.:
# merge.data.1 %<>%
#   mutate(
#     Chemical.Input.Restriction = case_when(
#       Chemical.Input.Restriction == "0" ~ "None",
#       Chemical.Input.Restriction == "1" ~ "Low",
#       Chemical.Input.Restriction == "2" ~ "Medium",
#       Chemical.Input.Restriction == "3" ~ "High"
#     ),
#     Chemical.Input.Restriction = factor(Chemical.Input.Restriction,
#                                          levels = c("None","Low","Medium","High"),
#                                          ordered = TRUE)
#   )

merge.data.1 %<>%
  mutate(
    across(
      c(Block, Chosen.Alternative, Choice.Binary,
        Demo_Gender, Demo_Education, Farm_Tenure, Farm_Income.Source),
      ~ factor(.x)
    )
  )

merge.data.1 %<>%
  mutate(
    across(
      c(Demo_Age, Farm_Years, Farm_Land.Size, Farm_Income.Pct.Rice,
        Baseline_Fert.kg, Baseline_Herbicide, Incentive.Payment),
      ~ as.numeric(.x)
    )
  )

# Filter NA to remove unselected/blank merge rows
merge.data.1 %<>%
  filter(!is.na(Chosen.Alternative))

#------------------------------------------------------:
# (5): Sanity Check #####
#------------------------------------------------------:

# Block distribution — how many respondents got Block 1 vs Block 2
summary_block.distribution <- merge.data.1 %>%
  distinct(Qualtrics.RID, .keep_all = TRUE) %>%
  count(Block) %>%
  mutate(percentage = n / sum(n) * 100)

ggplot(summary_block.distribution, aes(x = Block, y = percentage)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Percentage of respondents per Block",
    x = "Block", y = "Percentage (%)"
  ) +
  theme_minimal()

rm(summary_block.distribution)

# Count how many times each alternative was chosen overall
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Alternative)

# Check balance within each block (make sure design isn't skewed)
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Block, Alternative)

# Check balance per choice task
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Choice.Task, Alternative) %>%
  mutate(pct = n / sum(n) * 100)

# Visualize overall balance across alternatives
merge.data.1 %>%
  filter(Choice.Binary == 1) %>%
  count(Alternative) %>%
  ggplot(aes(x = Alternative, y = n)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Balance check: Alternative choices",
    x = "Alternative", y = "Count of times chosen"
  )

# Visualize balance of attribute levels shown — repeat per attribute.
# TODO: swap in your real attribute column names (placeholders shown below)

merge.data.1 %>%
  count(Incentive.Payment) %>%
  ggplot(aes(x = Incentive.Payment, y = n)) +
  geom_col(fill = "darkorange") +
  labs(title = "Shown counts per Incentive Payment level")

merge.data.1 %>%
  count(Chemical.Input.Restriction) %>%
  ggplot(aes(x = Chemical.Input.Restriction, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Shown counts per Chemical Input Restriction level")

merge.data.1 %>%
  count(Training.Timing) %>%
  ggplot(aes(x = Training.Timing, y = n)) +
  geom_col(fill = "skyblue") +
  labs(title = "Shown counts per Training Timing level")

merge.data.1 %>%
  count(Organic.Fertilizer.Support) %>%
  ggplot(aes(x = Organic.Fertilizer.Support, y = n)) +
  geom_col(fill = "darkblue") +
  labs(title = "Shown counts per Organic Fertilizer Support level")
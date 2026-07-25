### Today I am working on the pilot dataset generated from Qualtrics ####
### Created by: Zoe Sidana Bunnath #############
### Updated on: 2026-07-15 ####################

# ------------------------------
# 0 — Libraries  #####
# ------------------------------
library(tidyverse)   # dplyr, tidyr, ggplot2, readr etc.
library(here)        # find file path
library(magrittr)    # pipe (%<>%) used in original script
library(readxl)      # read_excel
#library(corrplot)    # corrplot visualization
library(tibble)      # produce data frames
library(janitor)     # cleaning and tidying data
library(gt)          # produce table of results
#library(patchwork)  # combine multiple plots

# ------------------------------
# 1 — User settings / file paths
# ------------------------------

###Load Data
Pilot <- read.csv(here("0.RawData", "Zoe - Capstone Survey_July 15, 2026_13.51.csv"),
                  stringsAsFactors = FALSE)

# Qualtrics exports 3 header rows: variable names (already used as col names by
# read.csv), then a question-text row, then an import-ID row. Both of those
# land in the data as rows 1-2 unless dropped here.
Pilot <- Pilot[-c(1, 2), ]

#------------------------------------------------------:
# (2): Select and filter complete data #####
#------------------------------------------------------:

#Remove non use columns
Pilot %<>%
  select(-StartDate, -EndDate, -Status, -IPAddress, -Duration..in.seconds.,
         -Finished, -LocationLatitude, -LocationLongitude,
         -RecipientLastName, -RecipientFirstName, -RecipientEmail,
         -ExternalReference, -Progress, -DistributionChannel, -UserLanguage)

Pilot %<>%
  rename(
    #RID
    Qualtrics.RID = ResponseId,
    
    #Consent
    Consent = Introduction,     # numeric choice code — confirm which value = "I agree"
    
    #Farming baseline / current inputs
    Baseline_Fert.kg   = Q39,
    Baseline_Herbicide = Q40,
    
    #Choice task
    #Block 1
    Choice.Task_1 = Q1,
    Choice.Task_2 = Q2,
    Choice.Task_3 = Q3,
    Choice.Task_4 = Q4,
    Choice.Task_5 = Q5,
    Choice.Task_6 = Q6,
    Choice.Task_7 = Q7,
    Choice.Task_8 = Q8,
    #Block 2
    Choice.Task_9  = Q9,
    Choice.Task_10 = Q10,
    Choice.Task_11 = Q11,
    Choice.Task_12 = Q12,
    Choice.Task_13 = Q13,
    Choice.Task_14 = Q14,
    Choice.Task_15 = Q15,
    Choice.Task_16 = Q16,
    
    #Demographic
    Demo_Age             = Demographic,
    Demo_Gender          = Q26,
    Demo_Education       = Q27,
    Demo_Education.Text  = Q27_7_TEXT,
    
    #Farming background
    Farm_Years              = Q29,
    Farm_Land.Size          = Q30,
    Farm_Tenure             = Q31,
    Farm_Tenure.Text        = Q31_4_TEXT,
    Farm_Income.Source      = Q32,
    Farm_Income.Source.Text = Q32_7_TEXT,
    Farm_Income.Pct.Rice    = Q33,
  )

#Filter out testing RIDs — Qualtrics ResponseId format is "R_" + alphanumeric
Pilot %<>%
  filter(!is.na(Qualtrics.RID) & grepl("^R_[A-Za-z0-9]+$", Qualtrics.RID))

#Filter consented respondents — confirm the actual "I agree" code in Qualtrics
Pilot %<>%
  filter(Consent == "4")

write_csv(Pilot, here("1.Cleaning", "Output", "Capstone_clean.csv"))

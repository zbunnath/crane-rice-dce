### DCE modeling using Apollo ####
### Created by: Zoe Sidana Bunnath #############
### Updated on: 2026-07-31 ####################

# ================================================================= #
# DISCRETE CHOICE EXPERIMENT (DCE) - MULTINOMIAL LOGIT MODEL
# ================================================================= #

# ----------------------------------------------------------------- #
# STEP 1: LOAD LIBRARIES & MERGED DATA
# ----------------------------------------------------------------- #
library(apollo)
library(tidyverse)
library(here)

# Load the merged choice dataset
mlogit_clean <- read_csv(here("2.Processing", "Output", "Capstone_DCE_merged_final.csv"))

# Select relevant columns and rename Choice.Binary to choice
mlogit_clean <- mlogit_clean %>% 
  rename(choice = Choice.Binary) %>%
  select(
    Qualtrics.RID, Choice.Task, Alternative, choice, 
    chemical, training, support, cost, 
    Demo_Age, Demo_Gender, Farm_Land.Size
  )

# Standardize Alternative values to alt1, alt2, alt3
alt_nums <- parse_number(as.character(mlogit_clean$Alternative))
mlogit_clean$Alternative <- paste0("alt", alt_nums)

# Reshape from long format (3 rows per task) to wide format (1 row per task)
database <- reshape(
  data      = as.data.frame(mlogit_clean),
  timevar   = "Alternative",
  idvar     = c("Qualtrics.RID", "Choice.Task"), 
  direction = "wide",
  sep       = "."
)

# ----------------------------------------------------------------- #
# STEP 2: DUMMY CODING & DATA PREPARATION
# ----------------------------------------------------------------- #
# Helper function for safe dummy variable creation
make_dummy <- function(vec, target_levels) {
  as.numeric(vec %in% target_levels)
}

# --- 1. Chemical Input Restrictions (Baseline = Level 1/Low) ---
database$chemical.alt1.MED  <- make_dummy(database$chemical.alt1, c("2", 2, "Medium"))
database$chemical.alt1.HIGH <- make_dummy(database$chemical.alt1, c("3", 3, "High"))

database$chemical.alt2.MED  <- make_dummy(database$chemical.alt2, c("2", 2, "Medium"))
database$chemical.alt2.HIGH <- make_dummy(database$chemical.alt2, c("3", 3, "High"))

# --- 2. Training Support (Baseline = Level 1/Low) ---
database$training.alt1.MED  <- make_dummy(database$training.alt1, c("2", 2, "Before harvesting"))
database$training.alt1.HIGH <- make_dummy(database$training.alt1, c("3", 3, "Both"))

database$training.alt2.MED  <- make_dummy(database$training.alt2, c("2", 2, "Before harvesting"))
database$training.alt2.HIGH <- make_dummy(database$training.alt2, c("3", 3, "Both"))

# --- 3. Financial/Technical Support (Baseline = Level 1/Low) ---
database$support.alt1.MED  <- make_dummy(database$support.alt1, c("2", 2, "Option 2", "Medium"))
database$support.alt1.HIGH <- make_dummy(database$support.alt1, c("3", 3, "Option 3", "High"))

database$support.alt2.MED  <- make_dummy(database$support.alt2, c("2", 2, "Option 2", "Medium"))
database$support.alt2.HIGH <- make_dummy(database$support.alt2, c("3", 3, "Option 3", "High"))

# --- 4. Demographics & Gender Dummy ---
database$Gender_Male <- ifelse(tolower(as.character(database$Demo_Gender.alt1)) %in% c("male", "m", "1"), 1, 0)

# Create standard Respondent ID for Apollo
database$RID <- database$Qualtrics.RID

# Construct dependent choice variable (1, 2, or 3)
database$choice <- case_when(
  database$choice.alt1 %in% c(1, TRUE) ~ 1,
  database$choice.alt2 %in% c(1, TRUE) ~ 2,
  database$choice.alt3 %in% c(1, TRUE) ~ 3,
  TRUE ~ NA_real_
)

# Convert demographics to numeric
database$Demo_Age.alt1       <- as.numeric(as.character(database$Demo_Age.alt1))
database$Farm_Land.Size.alt1 <- as.numeric(as.character(database$Farm_Land.Size.alt1))

# Impute missing demographic values using means
database$Demo_Age.alt1[is.na(database$Demo_Age.alt1)]             <- mean(database$Demo_Age.alt1, na.rm = TRUE)
database$Farm_Land.Size.alt1[is.na(database$Farm_Land.Size.alt1)] <- mean(database$Farm_Land.Size.alt1, na.rm = TRUE)
database$Gender_Male[is.na(database$Gender_Male)]                 <- 0

# Drop rows missing critical choice or cost information
database <- database %>% filter(!is.na(choice), !is.na(cost.alt1))

# ----------------------------------------------------------------- #
# STEP 3: APOLLO MODEL INITIALIZATION
# ----------------------------------------------------------------- #
apollo_initialise()

# Set global Apollo parameters
apollo_control <- list(
  modelName  = "Capstone_DCE_Model",
  modelDescr = "MNL model with categorical attributes and demographic interactions",
  indivID    = "RID", 
  panelData  = TRUE
)

# Vector of starting Beta parameters to estimate
apollo_beta <- c(
  asc_optin        = 0, # ASC for generic opt-in alternatives (alt1 & alt2)
  asc_alt3         = 0, # Opt-out / Status Quo constant (fixed anchor)
  
  b_chemical_MED   = 0,
  b_chemical_HIGH  = 0,
  
  b_training_MED   = 0, 
  b_training_HIGH  = 0,
  
  b_support_MED    = 0,
  b_support_HIGH   = 0,
  
  b_cost           = 0,
  
  b_age            = 0,
  b_gender_male    = 0, 
  b_farm_size      = 0
)

# Fix asc_alt3 to 0 for model identification
apollo_fixed <- c("asc_alt3")

# Validate setup before running model
apollo_inputs <- apollo_validateInputs()

# ----------------------------------------------------------------- #
# STEP 4: DEFINE UTILITY FUNCTIONS & ESTIMATE MODEL
# ----------------------------------------------------------------- #
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach model inputs & detach upon function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  V <- list()
  
  # Base attribute utilities for program alternatives
  V_base_alt1 <- b_chemical_MED   * chemical.alt1.MED +
    b_chemical_HIGH  * chemical.alt1.HIGH +
    b_training_MED   * training.alt1.MED +
    b_training_HIGH  * training.alt1.HIGH +
    b_support_MED    * support.alt1.MED +
    b_support_HIGH   * support.alt1.HIGH +
    b_cost           * cost.alt1
  
  V_base_alt2 <- b_chemical_MED   * chemical.alt2.MED +
    b_chemical_HIGH  * chemical.alt2.HIGH +
    b_training_MED   * training.alt2.MED +
    b_training_HIGH  * training.alt2.HIGH +
    b_support_MED    * support.alt2.MED +
    b_support_HIGH   * support.alt2.HIGH +
    b_cost           * cost.alt2
  
  # Demographic interactions with program choices (opt-in)
  demographics <- b_age         * Demo_Age.alt1 + 
    b_gender_male * Gender_Male + 
    b_farm_size   * Farm_Land.Size.alt1
  
  # Total utility specification
  V[["alt1"]] <- asc_optin + V_base_alt1 + demographics
  V[["alt2"]] <- asc_optin + V_base_alt2 + demographics
  V[["alt3"]] <- asc_alt3 # Opt-out baseline fixed to 0
  
  # Define settings for Multinomial Logit (MNL) engine
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3), 
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1), 
    choiceVar    = choice,
    utilities    = V
  )
  
  # Calculate choice probabilities
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P            <- apollo_panelProd(P, apollo_inputs, functionality)
  P            <- apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}

# Run maximum likelihood estimation
model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Print full output summary to console
apollo_modelOutput(model)
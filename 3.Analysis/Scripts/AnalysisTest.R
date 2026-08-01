# ================================================================= #
# STEP 1: LOAD LIBRARIES & MERGED DATA
# ================================================================= #
library(apollo)
library(tidyverse)
library(here)

# Load the merged dataset
mlogit_clean <- read_csv(here("2.Processing", "Output", "Capstone_DCE_merged_final.csv"))

mlogit_clean=mlogit_clean[c(1:10)]
mlogit_clean$Alternative[which(mlogit_clean$Alternative=="1")]="alt1"
mlogit_clean$Alternative[which(mlogit_clean$Alternative=="2")]="alt2"
mlogit_clean$Alternative[which(mlogit_clean$Alternative=="3")]="alt3"
#mlogit_clean$Choice.Binary=NULL
#Note design_ngene still says 50
# mlogit_clean$Cost[which(mlogit_clean$Cost==50)]=40
mlogit_clean_wide=reshape(mlogit_clean,timevar = "Alternative",
                          idvar = c(1:3),
                          direction = "wide" ) # Each respondent-choice task becomes one row, with separate columns for each alternative’s attributes.

apollo_control <- list(
  modelName  = "CraneRice_Choice",
  modelDescr = "Simple MNL model with program alternatives",
  indivID    = "Qualtrics.RID" ,
  panelData = TRUE
)
database=mlogit_clean_wide

database$chemical.alt1.LOW=0
database$chemical.alt1.LOW[which(database$chemical.alt1=="1")]=1
database$chemical.alt1.MEDIUM=0
database$chemical.alt1.MEDIUM[which(database$chemical.alt1=="2")]=1
database$chemical.alt1.HIGH=0
database$chemical.alt1.HIGH[which(database$chemical.alt1=="3")]=1

database$chemical.alt2.LOW=0
database$chemical.alt2.LOW[which(database$chemical.alt2=="1")]=1
database$chemical.alt2.MEDIUM=0
database$chemical.alt2.MEDIUM[which(database$chemical.alt2=="2")]=1
database$chemical.alt2.HIGH=0
database$chemical.alt2.HIGH[which(database$chemical.alt2=="3")]=1


database$support.alt1.LOW=0
database$support.alt1.LOW[which(database$support.alt1=="Low")]=1
database$support.alt1.MEDIUM=0
database$support.alt1.MEDIUM[which(database$support.alt1=="Medium")]=1
database$support.alt1.HIGH=0
database$support.alt1.HIGH[which(database$support.alt1=="High")]=1

database$support.alt2.LOW=0
database$support.alt2.LOW[which(database$support.alt2=="Low")]=1
database$support.alt2.MEDIUM=0
database$support.alt2.MEDIUM[which(database$support.alt2=="Medium")]=1
database$support.alt2.HIGH=0
database$support.alt2.HIGH[which(database$support.alt2=="High")]=1
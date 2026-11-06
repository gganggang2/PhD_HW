# PRELIMINARIES: LOADING PACKAGES ----

# psych provides describe() for descriptive statistics
library(psych)

# summarytools provides dfSummary() for data summaries
library(summarytools)

# rstudioapi talks to RStudio, used here to find the folder that contains this script
library(rstudioapi)

# IMPORTING DATA ----

# Option 1: Read From the Course Website ----

# store the web address of the raw data file
filepath <- "https://raw.githubusercontent.com/craigenders/psych250a/main/data/BodySatWide.csv"

# read the file at that address into a data frame named BodySat
BodySat <- read.csv(filepath, stringsAsFactors = TRUE)

# Option 2: Read From the Folder That Contains the Script ----

# this block is switched off because the data file is not in the same folder as this script
# to use it, save the data file next to this script and remove the # from the three code lines below

# set the working directory to the folder that contains this script
# setwd(dirname(getActiveDocumentContext()$path))

# print the working directory to confirm the location
# getwd()

# read BodySatWide.csv from the working directory into a data frame named BodySat
# BodySat <- read.csv("BodySatWide.csv", stringsAsFactors = TRUE)

# CONVERTING CATEGORICAL VARIABLES TO FACTORS ----

# convert ParentEduc from numeric codes to a factor with descriptive labels
BodySat$ParentEduc <- factor(BodySat$ParentEduc,
                             levels = c(0, 1, 2),
                             labels = c("High school", "Some college", "Bachelor's degree"))

# convert ParentIncome from numeric codes to a factor with descriptive labels
BodySat$ParentIncome <- factor(BodySat$ParentIncome,
                               levels = c(0, 1, 2, 3),
                               labels = c("$5K or less", "$5K to $20K", "$20K to $40K", "$40K or more"))

# print the first few rows to confirm the labels replaced the codes
head(BodySat)

# SUMMARIZING DATA ----

# overview of every variable in the data frame
dfSummary(BodySat)

# overview of the two body satisfaction variables
dfSummary(BodySat[, c("BodySat18", "BodySat10")])

# COMPUTING A CHANGE SCORE VARIABLE ----

# create change scores
BodySat$BodySatCha <- BodySat$BodySat18 - BodySat$BodySat10

# DESCRIPTIVE STATISTICS ----

# descriptive statistics for entire data frame (psych package)
describe(BodySat)

# PAIRED-SAMPLES T-TEST ----

# paired t-test with default two-tailed alternate hypotheses (base R)
results <- t.test(BodySat$BodySat18, BodySat$BodySat10, paired = TRUE)
results

# Standard Error ----

# print standard error of the mean difference
results$stderr

# STANDARDIZED MEAN DIFFERENCE EFFECT SIZE ----

# standardized mean difference effect size
mean(BodySat$BodySatCha) / sd(BodySat$BodySatCha)

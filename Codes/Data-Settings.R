# Define the relative main path
main_path <- "../Data-Mat-files/"

# Load required libraries
libraries <- c(
  "rmatio", "BiocManager", "installr", "lme4", "sjPlot", "car", "tidyverse",
  "devtools", "hrbrthemes", "viridis", "R.matlab", "readr", "broom.mixed",
  "ggplot2", "haven", "sjstats", "sjmisc", "sjlabelled", "gridExtra", "data.table",
  "gapminder", "rstatix", "scales", "tidyr", "emmeans", "plotly", "ggpubr",
  "dplyr", "plyr", "zoo", "patchwork", "interactions", "lmerTest", "Matrix",
  "cowplot", "reshape2", "ggeffects", "robust", "effects", "tableHTML", "magrittr",
  "ggbreak", "ggiraphExtra", "ggpmisc", "robustbase", "ggplotify", "ggplotlyExtra",
  "ggiraph", "jtools", "caret", "BBmisc", "splines", "ggforce", "ggdist",
  "gghalves", "ggbeeswarm", "rhdf5"
)

# Load Libraries
lapply(libraries, library, character.only = TRUE)

# Function to load MAT files
load_motor_data <- function(file_path, is_hdf5 = FALSE) {
  if (is_hdf5) {
    data <- H5Fopen(file_path)
    df <- data.frame(data$alldata_Test)
  } else {
    data <- readMat(file_path)
    df <- data.frame(data[["alldata.Test"]])
  }
  
  return(df)
}

# Subject IDs (1-31)
subject_ids <- 1:31

# Create file paths and flags for HDF5 files (subjects >= 5 are HDF5)
file_paths <- sprintf("%sMotorEM(2023)_SubjID_%d_MotorEM(2023)TEST_.mat", main_path, subject_ids)
is_hdf5_flags <- subject_ids >= 5

# Load data for each subject
motor_data_list <- lapply(seq_along(subject_ids), function(i) {
  file_path <- file_paths[i]
  data <- load_motor_data(file_path, is_hdf5_flags[i])
  print(paste("Loaded data for Subject", subject_ids[i]))
  return(data)
})

# Combine all data into one data frame
data <- do.call(rbind, motor_data_list)

# Assign column names
colnames(data) <- c(
  "SubjID", "Age", "Sex", "Block", "Session", "Trial_Index", "Trial_Number", "ITI",
  "ReactPeriod", "Device", "R_Lhanded", "MotorSide", "MotorInvolvement",
  "TrialTotalTime", "ActionTiming", "ActionDataDelayToStart", "ActionDataDelayToEnd",
  "ActionDataConfidenceReaction", "ActionDataConfidenceXaxis", "ActionDataConfidenceYaxis",
  "HandDominance", "ArmDominance", "LegDominance", "Motor_coordination",
  "Gaming_frequency", "Gaming_type", "Music", "Inst_type", "CutOff", 
  "Std_Timing", "Std_Reacting", "Mean_Reacting", "Feedback"
)

# Check the structure of the combined data
str(data)


################################### Measures ###################################

# Fixing - Cleaning the data
data$Inst_type[data$SubjID == 29] <- 0  # Set Inst_type to 0 for Subject 29
data$Feedback[is.na(data$Feedback)] <- -1  # Use is.na() instead of == NA to fill NA values with -1

# Remove rows where ActionTiming is NaN
data <- subset(data, ActionTiming != 'NaN') 

# Convert Block variable to a factor and rename levels
data$Block <- factor(data$Block)
levels(data$Block) <- list("ReactingG" = 2, "TimingG" = 1)  # Change factor levels

# Convert Device variable to a factor and rename levels
data$Device <- factor(data$Device)
levels(data$Device) <- list("RightJoy" = 0, "LeftJoy" = 1)  # Change factor levels

# Convert R_Lhanded variable to a factor and rename levels
data$R_Lhanded <- factor(data$R_Lhanded)
levels(data$R_Lhanded) <- list("LHanded" = 2, "RHanded" = 1)  # Change factor levels

# Convert MotorSide variable to a factor and rename levels
data$MotorSide <- factor(data$MotorSide)
levels(data$MotorSide) <- list("DS" = 1, "NDS" = 2)  # Change factor levels

# Convert MotorInvolvement variable to a factor and rename levels
data$MotorInvolvement <- factor(data$MotorInvolvement)
levels(data$MotorInvolvement) <- list("Move" = 1, "Press" = 2)  # Change factor levels

# Convert SubjID variable to a factor
data$SubjID <- factor(data$SubjID)

# Subset data based on Block types
data_timing <- subset(data, Block == "TimingG") 
data_reacting <- subset(data, Block == "ReactingG") 

################################### Measures ###################################

# Clean data further: Remove timings below 200 ms
data_timing <- subset(data_timing, ActionTiming >= 0.5)

# Normalize ActionTiming for Timing data
data_timing <- ddply(data_timing, .(SubjID, Session), mutate, 
                     ActionTiming_z = (ActionTiming - mean(ActionTiming)) / sd(ActionTiming))
data_timing <- subset(data_timing, ActionTiming_z < 3 & ActionTiming_z > -3) 

# Normalize ActionTiming for Reacting data
data_reacting <- ddply(data_reacting, .(SubjID, Session), mutate, 
                       ActionTiming_z = (ActionTiming - mean(ActionTiming)) / sd(ActionTiming))
data_reacting <- subset(data_reacting, ActionTiming_z < 3 & ActionTiming_z > -3)

# Normalize ActionDataConfidenceReaction for Timing data
data_timing <- ddply(data_timing, .(SubjID), mutate, 
                     ActionDataConfidenceReaction_z = (ActionDataConfidenceReaction - mean(ActionDataConfidenceReaction)) / sd(ActionDataConfidenceReaction))
data_timing <- subset(data_timing, ActionDataConfidenceReaction_z < 3 & ActionDataConfidenceReaction_z > -3)

# Normalize ActionDataConfidenceReaction for Reacting data
data_reacting <- ddply(data_reacting, .(SubjID), mutate, 
                       ActionDataConfidenceReaction_z = (ActionDataConfidenceReaction - mean(ActionDataConfidenceReaction)) / sd(ActionDataConfidenceReaction))
data_reacting <- subset(data_reacting, ActionDataConfidenceReaction_z < 3 & ActionDataConfidenceReaction_z > -3)

# Calculate ActionDataConfidenceYaxis percentages for Timing data
data_timing$ActionDataConfidenceYaxis_percent = ((1275 - data_timing$ActionDataConfidenceYaxis) * 100) / 1200

# Calculate ActionDataConfidenceYaxis percentages for Reacting data
data_reacting$ActionDataConfidenceYaxis_percent = ((1275 - data_reacting$ActionDataConfidenceYaxis) * 100) / 1200

# Scale ActionDataConfidenceXaxis for Timing data
data_timing$ActionDataConfidenceXaxis = (data_timing$ActionDataConfidenceXaxis / 10) - 120

# Scale ActionDataConfidenceXaxis for Reacting data
data_reacting$ActionDataConfidenceXaxis = (data_reacting$ActionDataConfidenceXaxis / 10) - 120

# Invert ActionDataConfidenceYaxis for Timing data
data_timing$ActionDataConfidenceYaxis = -1 * ((data_timing$ActionDataConfidenceYaxis / 10) - 67.5)

# Invert ActionDataConfidenceYaxis for Reacting data
data_reacting$ActionDataConfidenceYaxis = -1 * ((data_reacting$ActionDataConfidenceYaxis / 10) - 67.5)

# Function to group data and create confidence levels
group_and_conf_level <- function(data) {   
  data %>%
    group_by(SubjID, Session) %>%
    mutate(conf_level = ifelse(ActionDataConfidenceYaxis <= median(ActionDataConfidenceYaxis), "Low", "High")) %>%
    mutate(conf_level = factor(conf_level, levels = c("Low", "High"))) %>%
    mutate(overall_motor_rate = Gaming_frequency + Motor_coordination) %>%
    mutate(motor_rate = ifelse(overall_motor_rate <= median(overall_motor_rate), "Low", "High")) %>%
    mutate(motor_rate = factor(motor_rate, levels = c("Low", "High"))) 
}

# Apply grouping and confidence level function
data_timing <- group_and_conf_level(data_timing) 
data_reacting <- group_and_conf_level(data_reacting) 


###################################################### SAVE THE ALL DATA SET  TRIM ####################################################################################################
fwrite(data_timing, "../Datasets/data_timing.csv")
fwrite(data_reacting, "../Datasets/data_reacting.csv")

################################### Measures ###################################
total_data <- rbind(data_timing, data_reacting) 

total_data=rbind(data_timing,data_reacting)
total_data <- total_data %>%                                        # Create SessionID by group
  group_by(SubjID,Session) %>%
  dplyr::mutate(SessionID = cur_group_id())
total_data 


data_count <- ddply(total_data, .(SubjID), summarize, total_session = length(unique(Session)))
total_session_count <- sum(data_count$total_session)
data_count_hand <- ddply(total_data, .(SubjID), summarize, R_L = (unique(R_Lhanded)))






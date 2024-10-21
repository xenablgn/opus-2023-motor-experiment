################################ General Stats - Error Monitoring Trial Level (Figure 2, Supplementary 1) ################################

### Author Information
cat("**Authors**: Sena Nur Bilgin, Tadeusz Kononowicz\n")
cat("**Affiliations**:\n")
cat("- Polish Academy of Sciences\n")
cat("- Paris Saclay University (Institute of Neuroscience, NEUROPSI)\n")
cat("**Corresponding Author**: [senanrbilgin@gmail.com](mailto:senanrbilgin@gmail.com)\n")

# Define color and line styles
cond_color_movement <- c('brown2', 'brown4')
cond_conf <- c('darkolivegreen3', 'darkgreen')
cond_line_dominance <- c('dotted', 'dashed')
cond_color_motor_rate <- c('blue', 'blue4')

# List of required libraries
libraries <- c(
  "rmatio", "installr", "lme4", "sjPlot", "car", "tidyverse", "devtools", "hrbrthemes",
  "viridis", "R.matlab", "readr", "broom.mixed", "ggplot2", "haven", "sjstats", "sjmisc",
  "sjlabelled", "gridExtra", "data.table", "gapminder", "rstatix", "scales", "tidyr",
  "emmeans", "plotly", "ggpubr", "dplyr", "plyr", "zoo", "patchwork", "interactions",
  "lmerTest", "Matrix", "cowplot", "reshape2", "ggeffects", "robust",
  "effects", "tableHTML", "report", "magrittr", "ggbreak", "ggiraphExtra", "ggpmisc", "robustbase",
  "ggplotify", "ggplotlyExtra", "ggiraph", "jtools", "ggpmisc", "caret", "BBmisc",
  "splines", "ggforce", "ggdist", "gghalves", "ggbeeswarm", "ggside"
)

# Function to load libraries
load_libraries <- function(libraries) {
  for (lib in libraries) {
    library(lib, character.only = TRUE)
  }
}

# Load libraries
load_libraries(libraries)

# Read data
read_data <- function(file_path) {
  fread(file_path)
}

timing_file <- "../Datasets/data_timing.csv"
reacting_file <- "../Datasets/data_reacting.csv"
data_timing <- read_data(timing_file)
data_reacting <- read_data(reacting_file)

# Convert relevant columns to factors with specified levels
convert_factors <- function(data, conf_levels_timing, conf_levels_reacting) {
  data$conf_level <- factor(data$conf_level, levels = conf_levels_timing)
  data$previous_feedback <- factor(data$previous_feedback, levels = c(0, 1))
  data$feedback_category <- factor(data$feedback_category, levels = c("No Feedback", "Bad", "Good"))
  data$motor_rate <- factor(data$motor_rate, levels = c("Low", "High"))
  return(data)
}

# Convert factors for both datasets
data_timing <- convert_factors(data_timing, c("Low", "High"), NULL)
data_reacting <- convert_factors(data_reacting, c("Low", "High"), NULL)


# Count subjects by sex
count_subjects <- function(data, sex) {
  data %>%
    filter(Sex == sex) %>%
    distinct(SubjID) %>%
    count()
}

# Print subject counts
subject_count_female <- count_subjects(data_reacting, 1)
print(subject_count_female)

subject_count_male <- count_subjects(data_timing, 2)
print(subject_count_male)


# Calculate and print average and standard deviation of age
average_age <- mean(data_timing$Age, na.rm = TRUE)
sd_age <- sd(data_timing$Age, na.rm = TRUE)

cat("Average Age:", average_age, "\n")
cat("Standard Deviation of Age:", sd_age, "\n")


# Subset data based on MotorInvolvement
move_data <- subset(data_timing, MotorInvolvement == "Move")
press_data <- subset(data_timing, MotorInvolvement == "Press")

# Perform t-test and output the result
t_test_result <- t.test(move_data$ActionTiming, press_data$ActionTiming)
print(t_test_result)

# Function to create and display density plots
create_density_plot <- function(data, variable, title) {
  density_plot <- density(data[[variable]], na.rm = TRUE)
  plot(density_plot, main = title)
}

# Plotting ActionDataConfidenceYaxis and ActionDataConfidenceYaxis_percent
create_density_plot(data_timing, "ActionDataConfidenceYaxis", "Density Plot of Action Data Confidence Y-axis")
create_density_plot(data_timing, "ActionDataConfidenceYaxis_percent", "Density Plot of Action Data Confidence Y-axis Percent")

# Create a new column for induced_motor based on conditions
df_max <- data_timing %>%
  mutate(induced_motor = case_when(
    MotorInvolvement == 'Press' & MotorSide == 'DS' ~ 'Low',
    MotorInvolvement == 'Move' & MotorSide == 'NDS' ~ 'High',
    TRUE ~ 'Other'
  )) %>%
  filter(induced_motor != "Other")  # Filter out 'Other' cases

# Screen Center and Dimensions
xCenter <- 1200
yCenter <- 675
screenXpixels <- 2400
screenYpixels <- 1350 


# Load required libraries
library(lme4)
library(ggeffects)
library(report)

# ------------------- Timing Analysis -------------------  

# Model with Confidence Level Interaction
stat_timing_conf_level_int <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * conf_level * MotorInvolvement + (1 | SubjID), data = data_timing) 
summary(stat_timing_conf_level_int)  

# Plotting Predictions for Timing with Confidence Level
data_reacting_conf_level_plot <- ggpredict(stat_timing_conf_level_int, terms = c("ActionDataConfidenceXaxis [all]", "conf_level", "MotorInvolvement")) 
plot(data_reacting_conf_level_plot, add.data = FALSE, dot.size = 2)  
report(stat_timing_conf_level_int)  

# -----------------------------------------------
# Timing Analysis for TP Task
# -----------------------------------------------

# Basic Timing Model
stat_timing <- lmer(ActionTiming ~ ActionDataConfidenceXaxis + (1 | SubjID), data = data_timing) 
summary(stat_timing)  

# Plotting Predictions for Basic Timing Model
data_timing_plot <- ggpredict(stat_timing, terms = c("ActionDataConfidenceXaxis [all]")) 
plot(data_timing_plot, add.data = FALSE, dot.size = 2)  
report(stat_timing)  

# Prepare Confidence Level Factor
data_timing$conf_level <- as.factor(data_timing$conf_level) 
data_timing$conf_level <- relevel(data_timing$conf_level, ref = "High")  

# Model with Confidence Level
stat_timing_conf_level <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * conf_level + (1 | SubjID), data = data_timing) 
summary(stat_timing_conf_level)  

# Plotting Predictions for Timing with Confidence Level
data_timing_conf_level_plot <- ggpredict(stat_timing_conf_level, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")) 
plot(data_timing_conf_level_plot, add.data = FALSE, dot.size = 2)  
report(stat_timing_conf_level)  

# ANOVA for Confidence Level Models
anova(stat_timing, stat_timing_conf_level)  

# -----------------------------------------------
# Motor Rate Interaction Model
# -----------------------------------------------

stat_timing_motor_rate <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * motor_rate + (1 | SubjID), data = data_timing) 
summary(stat_timing_motor_rate)  

# Plotting Predictions for Motor Rate Model
data_timing_motor_level_plot <- ggpredict(stat_timing_motor_rate, terms = c("ActionDataConfidenceXaxis [all]", "motor_rate")) 
plot(data_timing_motor_level_plot, add.data = FALSE, dot.size = 2)  

# ANOVA for Motor Rate Models
anova(stat_timing, stat_timing_motor_rate)  

# -----------------------------------------------
# Motor Side Interaction Model
# -----------------------------------------------

data_timing$MotorSide <- as.factor(data_timing$MotorSide) 
data_timing$MotorSide <- relevel(data_timing$MotorSide, ref = "DS")  

stat_timing_motor_side <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * MotorSide + (1 | SubjID), data = data_timing) 
summary(stat_timing_motor_side)  

# Plotting Predictions for Motor Side Model
data_timing_motor_side_plot <- ggpredict(stat_timing_motor_side, terms = c("ActionDataConfidenceXaxis [all]", "MotorSide")) 
plot(data_timing_motor_side_plot, add.data = FALSE, dot.size = 2)  
report(stat_timing_motor_side)  

# ANOVA for Motor Side Models
anova(stat_timing, stat_timing_motor_side)  

# -----------------------------------------------
# Motor Involvement Interaction Model
# -----------------------------------------------

data_timing$MotorInvolvement <- as.factor(data_timing$MotorInvolvement) 
data_timing$MotorInvolvement <- relevel(data_timing$MotorInvolvement, ref = "Move")  

stat_timing_motor_involvement <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * MotorInvolvement + (1 | SubjID), data = data_timing) 
summary(stat_timing_motor_involvement)  

# Plotting Predictions for Motor Involvement Model
data_timing_motor_involvement_plot <- ggpredict(stat_timing_motor_involvement, terms = c("ActionDataConfidenceXaxis [all]", "MotorInvolvement")) 
plot(data_timing_motor_involvement_plot, add.data = FALSE, dot.size = 2)  
report(stat_timing_motor_involvement)  

# ANOVA for Motor Involvement Models
anova(stat_timing, stat_timing_motor_involvement)  

# ------------------- Reacting Task Analysis -------------------  

# Basic Reacting Model
stat_reacting <- lmer(ActionTiming ~ ActionDataConfidenceXaxis + (1 | SubjID), data = data_reacting) 
summary(stat_reacting)  

# Plotting Predictions for Reacting Task
data_reacting_plot <- ggpredict(stat_reacting, terms = c("ActionDataConfidenceXaxis [all]")) 
plot(data_reacting_plot, add.data = FALSE, dot.size = 2)  
report(stat_reacting)  

# Prepare Confidence Level Factor for Reacting Task
data_reacting$conf_level <- as.factor(data_reacting$conf_level) 
data_reacting$conf_level <- relevel(data_reacting$conf_level, ref = "High")  

# Model with Confidence Level for Reacting Task
stat_reacting_conf_level <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * conf_level + (1 | SubjID), data = data_reacting) 
summary(stat_reacting_conf_level)  

# Plotting Predictions for Reacting Task with Confidence Level
data_reacting_conf_level_plot <- ggpredict(stat_reacting_conf_level, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")) 
plot(data_reacting_conf_level_plot, add.data = FALSE, dot.size = 2)  
report(stat_reacting_conf_level)  

# ANOVA for Confidence Level Models in Reacting Task
anova(stat_reacting, stat_reacting_conf_level)  

# -----------------------------------------------
# Motor Side Interaction Model for Reacting Task
# -----------------------------------------------

data_reacting$MotorSide <- as.factor(data_reacting$MotorSide) 
data_reacting$MotorSide <- relevel(data_reacting$MotorSide, ref = "NDS")  

stat_reacting_motor_side <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * MotorSide + (1 | SubjID), data = data_reacting) 
summary(stat_reacting_motor_side)  

# Plotting Predictions for Motor Side Model in Reacting Task
data_reacting_motor_side_plot <- ggpredict(stat_reacting_motor_side, terms = c("ActionDataConfidenceXaxis [all]", "MotorSide")) 
plot(data_reacting_motor_side_plot, add.data = FALSE, dot.size = 2)  
report(stat_reacting_motor_side)  

# ANOVA for Motor Side Models in Reacting Task
anova(stat_reacting, stat_reacting_motor_side)  

# -----------------------------------------------
# Motor Involvement Interaction Model for Reacting Task
# -----------------------------------------------

data_reacting$MotorInvolvement <- as.factor(data_reacting$MotorInvolvement) 
data_reacting$MotorInvolvement <- relevel(data_reacting$MotorInvolvement, ref = "Press")  

stat_reacting_motor_involvement <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * MotorInvolvement + (1 | SubjID), data = data_reacting) 
summary(stat_reacting_motor_involvement)  

# Report for Motor Involvement Model in Reacting Task
report(stat_reacting_motor_involvement)  

# Plotting Predictions for Motor Involvement Model in Reacting Task
data_reacting_motor_involvement_plot <- ggpredict(stat_reacting_motor_involvement, terms = c("ActionDataConfidenceXaxis [all]", "MotorInvolvement")) 
plot(data_reacting_motor_involvement_plot, add.data = FALSE, dot.size = 2)  

# ANOVA for Motor Involvement Models in Reacting Task
anova(stat_reacting, stat_reacting_motor_involvement)  


##############################  FIGURE 2 PLOTS ##############################

# Create a data frame for predicted Reaction Time (RTs) based on motor side
data_reacting_motor_side_plot <- data.frame(ggpredict(stat_reacting_motor_side, terms = c("ActionDataConfidenceXaxis [all]", "MotorSide")))
# Rename the 6th column to 'MotorSide' for clarity
colnames(data_reacting_motor_side_plot)[6] <- 'MotorSide'

# Generate a plot for Reaction Time (RTs) based on Action Data Confidence and Motor Side
ggplot(data_reacting_motor_side_plot, aes(x, predicted, linetype = MotorSide)) +
  geom_line(size = 4, color = "black") +  # Plot the predicted RTs as lines
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "grey", alpha = 0.2, color = 'black') +  # Add shaded confidence intervals
  geom_xsidedensity(aes(y = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = FALSE, fill = "grey", color = "black") +  # Add x-side density plot
  geom_ysidedensity(aes(x = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = FALSE, fill = "grey", color = "black") +  # Add y-side density plot
  labs(y = "Timing Response (s) (RTs)", x = "Monitoring Response") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black")) +  # Customize axis line appearance
  theme(axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text for better readability
        legend.title.align = 0.5,  # Center the legend title
        plot.subtitle = element_text(color = "black", size = 25, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 25, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 25, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 25, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 25),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA)) +  # Customize strip background
  scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +  # Customize y-axis breaks and rotation
  theme(axis.line = element_line(colour = "black"),  # Ensure the axis line is black
        ggside.panel.scale = .2,  # Adjust side panel scale
        ggside.axis.text = element_text(color = "black", size = 10, hjust = 0.5),  # Customize side axis text
        legend.position = "none")  # Hide legend

# Create a data frame for predicting confidence levels based on Reaction Time
data_reacting_conf_level_plot <- data.frame(ggpredict(stat_reacting_conf_level, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")))
# Rename the 6th column to 'Confidence'
colnames(data_reacting_conf_level_plot)[6] <- 'Confidence'

# Generate a plot for Reaction Time based on Confidence levels
ggplot(data_reacting_conf_level_plot, aes(x, predicted, fill = Confidence, color = Confidence)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Confidence), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  geom_xsidedensity(aes(y = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = FALSE) +  # Add x-side density plot
  geom_ysidedensity(aes(x = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = FALSE) +  # Add y-side density plot
  scale_color_manual(values=cond_conf, labels=c('1'='Low', '2'='High')) +  # Set color scale for confidence levels
  scale_fill_manual(values=cond_conf, labels=c('1'='Low', '2'='High')) +  # Set fill scale for confidence levels
  labs(y = "RT(s)", x = "EM Response") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black")) +  # Customize axis line appearance
  theme(axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text
        legend.title.align = 0.5,  # Center legend title
        plot.subtitle = element_text(color = "black", size = 25, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 25, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 25, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 25, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 25),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA)) +  # Customize strip background
  scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +  # Customize y-axis breaks and rotation
  theme(axis.line = element_line(colour = "black"),  # Ensure the axis line is black
        ggside.panel.scale = .2,  # Adjust side panel scale
        ggside.axis.text = element_text(color = "black", size = 10, hjust = 0.5),  # Customize side axis text
        legend.position = "none")  # Hide legend

# Create a data frame for predicting motor involvement's effect on Reaction Time
data_reacting_motor_involvement_plot <- data.frame(ggpredict(stat_reacting_motor_involvement, terms = c("ActionDataConfidenceXaxis [all]", "MotorInvolvement")))
# Rename the 6th column to 'MotorInvolvement'
colnames(data_reacting_motor_involvement_plot)[6] <- 'MotorInvolvement'

# Generate a plot for Reaction Time based on Motor Involvement
ggplot(data_reacting_motor_involvement_plot, aes(x, predicted, fill = MotorInvolvement, color = MotorInvolvement)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = MotorInvolvement), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  geom_xsidedensity(aes(y = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = TRUE) +  # Add x-side density plot
  geom_ysidedensity(aes(x = after_stat(scaled)), alpha = 0.2, size = 1, show.legend = TRUE) +  # Add y-side density plot
  scale_color_manual(values=cond_color_movement, labels=c('1'='Move', '2'='Press')) +  # Set color scale for motor involvement
  scale_fill_manual(values=cond_color_movement, labels=c('1'='Move', '2'='Press')) +  # Set fill scale for motor involvement
  labs(y = "Timing Response (RTs) (s)", x = "Monitoring Response") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black")) +  # Customize axis line appearance
  theme(axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text
        legend.title.align = 0.5,  # Center legend title
        plot.subtitle = element_text(color = "black", size = 25, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 25, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 25, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 25, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 25),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA)) +  # Customize strip background
  scale_ysidex_continuous(guide = guide_axis(angle = 90), minor_breaks = NULL) +  # Customize y-axis breaks and rotation
  theme(axis.line = element_line(colour = "black"),  # Ensure the axis line is black
        ggside.panel.scale = .2,  # Adjust side panel scale
        ggside.axis.text = element_text(color = "black", size = 10, hjust = 0.5),  # Customize side axis text
        legend.position = "none")  # Hide legend


############################## SUPPLEMENTARY FIGURE 1 ##############################

# Fit linear mixed-effects model for Reaction Time with 'Move' motor involvement
stat_move_conf_low <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * relevel(as.factor(conf_level), 2) + (1|SubjID), 
                           data = subset(data_timing, MotorInvolvement == "Move"))
summary(stat_move_conf_low)  # Display summary of the model
report(stat_move_conf_low)    # Generate a report of the model results

# Create a data frame for predicted Reaction Time based on Action Data Confidence and Confidence Level
data_move_conf_plot <- data.frame(ggpredict(stat_move_conf_low, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")))
colnames(data_move_conf_plot)[6] <- 'Confidence'  # Rename the 6th column to 'Confidence'

# Plotting Reaction Time for 'Move' motor involvement
ggplot(data_move_conf_plot, aes(x, predicted, fill = Confidence, color = Confidence)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Confidence), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  scale_color_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set color scale for confidence levels
  scale_fill_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set fill scale for confidence levels
  labs(y = "Timing R. (TPs)", x = "Monitoring R. (Move)") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black"),  # Customize axis line appearance
        axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text for readability
        legend.title.align = 0.5,  # Center the legend title
        plot.subtitle = element_text(color = "black", size = 30, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 30, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 30, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 30, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 30),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA),  # Customize strip background
        legend.position = "None")  # Hide legend

############################## PRESS MOTOR INVOLVEMENT ##############################

# Fit linear mixed-effects model for Reaction Time with 'Press' motor involvement
stat_press_conf_low <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * relevel(as.factor(conf_level), 2) + (1|SubjID), 
                            data = subset(data_timing, MotorInvolvement == "Press"))
summary(stat_press_conf_low)  # Display summary of the model
report(stat_press_conf_low)    # Generate a report of the model results

# Create a data frame for predicted Reaction Time based on Action Data Confidence and Confidence Level
data_press_conf_plot <- data.frame(ggpredict(stat_press_conf_low, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")))
colnames(data_press_conf_plot)[6] <- 'Confidence'  # Rename the 6th column to 'Confidence'

# Plotting Reaction Time for 'Press' motor involvement
ggplot(data_press_conf_plot, aes(x, predicted, fill = Confidence, color = Confidence)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Confidence), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  scale_color_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set color scale for confidence levels
  scale_fill_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set fill scale for confidence levels
  labs(y = "Timing R. (TPs)", x = "Monitoring R. (Press)") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black"),  # Customize axis line appearance
        axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text for readability
        legend.title.align = 0.5,  # Center the legend title
        plot.subtitle = element_text(color = "black", size = 30, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 30, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 30, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 30, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 30),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA),  # Customize strip background
        legend.position = "None")  # Hide legend

############################## NDS MOTOR SIDE ##############################

# Fit linear mixed-effects model for Reaction Time with 'NDS' motor side
stat_nds_conf_low <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * relevel(as.factor(conf_level), 2) + (1|SubjID), 
                          data = subset(data_timing, MotorSide == "NDS"))
summary(stat_nds_conf_low)  # Display summary of the model
report(stat_nds_conf_low)    # Generate a report of the model results

# Create a data frame for predicted Reaction Time based on Action Data Confidence and Confidence Level
data_nds_conf_plot <- data.frame(ggpredict(stat_nds_conf_low, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")))
colnames(data_nds_conf_plot)[6] <- 'Confidence'  # Rename the 6th column to 'Confidence'

# Plotting Reaction Time for 'NDS' motor side
ggplot(data_nds_conf_plot, aes(x, predicted, fill = Confidence, color = Confidence)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Confidence), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  scale_color_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set color scale for confidence levels
  scale_fill_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set fill scale for confidence levels
  labs(y = "Timing R. (TPs)", x = "Monitoring R. (NDS)") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black"),  # Customize axis line appearance
        axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text for readability
        legend.title.align = 0.5,  # Center the legend title
        plot.subtitle = element_text(color = "black", size = 30, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 30, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 30, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 30, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 30),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA),  # Customize strip background
        legend.position = "None")  # Hide legend

############################## DS MOTOR SIDE ##############################

# Fit linear mixed-effects model for Reaction Time with 'DS' motor side
stat_ds_conf_low <- lmer(ActionTiming ~ ActionDataConfidenceXaxis * relevel(as.factor(conf_level), 2) + (1|SubjID), 
                         data = subset(data_timing, MotorSide == "DS"))
summary(stat_ds_conf_low)  # Display summary of the model
report(stat_ds_conf_low)    # Generate a report of the model results

# Create a data frame for predicted Reaction Time based on Action Data Confidence and Confidence Level
data_ds_conf_plot <- data.frame(ggpredict(stat_ds_conf_low, terms = c("ActionDataConfidenceXaxis [all]", "conf_level")))
colnames(data_ds_conf_plot)[6] <- 'Confidence'  # Rename the 6th column to 'Confidence'

# Plotting Reaction Time for 'DS' motor side
ggplot(data_ds_conf_plot, aes(x, predicted, fill = Confidence, color = Confidence)) +
  geom_line(size = 4) +  # Add line for predicted RTs
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Confidence), color = 'white', alpha = 0.2) +  # Add shaded confidence intervals
  scale_color_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set color scale for confidence levels
  scale_fill_manual(values = cond_conf, labels = c('Low', 'High')) +  # Set fill scale for confidence levels
  labs(y = "Timing R. (TPs)", x = "Monitoring R. (DS)") +  # Set axis labels
  theme_classic() +  # Apply a classic theme
  theme(axis.line = element_line(colour = "black"),  # Customize axis line appearance
        axis.text.x = element_text(angle = 90, vjust = .5),  # Rotate x-axis text for readability
        legend.title.align = 0.5,  # Center the legend title
        plot.subtitle = element_text(color = "black", size = 30, face = "bold.italic"),  # Customize subtitle appearance
        plot.caption = element_text(color = "black", size = 30, face = "italic"),  # Customize caption appearance
        axis.title.x = element_text(color = "black", size = 30, hjust = 0.5),  # Customize x-axis title appearance
        axis.title.y = element_text(color = "black", size = 30, hjust = 0.5),  # Customize y-axis title appearance
        axis.text = element_text(color = "black", size = 30),  # Customize axis text appearance
        strip.text = element_text(color = "black", size = 18),  # Customize strip text appearance
        strip.background = element_rect(fill = NA, size = NA, color = NA),  # Customize strip background
        legend.position = "None")  # Hide legend

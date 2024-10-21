################################ General Stats - RT TP Performance Level (Figure 1) ################################

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
subject_count_female <- count_subjects(data_timing, 1)
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


###### Timing:
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



###### Reacting:
# Plotting ActionDataConfidenceYaxis and ActionDataConfidenceYaxis_percent
create_density_plot(data_reacting, "ActionDataConfidenceYaxis", "Density Plot of Action Data Confidence Y-axis")  # Changed from data_timing
create_density_plot(data_reacting, "ActionDataConfidenceYaxis_percent", "Density Plot of Action Data Confidence Y-axis Percent")  # Changed from data_timing

# Create a new column for induced_motor based on conditions
df_max <- data_reacting %>%  # Changed from data_timing
  mutate(induced_motor = case_when(
    MotorInvolvement == 'Press' & MotorSide == 'DS' ~ 'Low',
    MotorInvolvement == 'Move' & MotorSide == 'NDS' ~ 'High',
    TRUE ~ 'Other'
  )) %>%
  filter(induced_motor != "Other")  # Filter out 'Other' cases

################################################ RT Task ################################################ 

## Standard deviation - mean calculations:
# A): Summary statistics for reacting data
reacting_cv_normal <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)

# A): Summary statistics for reacting data - Conf
reacting_cv_normal_conf <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, conf_level),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)
reacting_cv_normal_conf <- na.omit(reacting_cv_normal_conf)  # Remove NA values

# A2): Summary statistics for reacting data - previous_feedback
reacting_cv_normal_feedback <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, previous_feedback),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)

# A2): Summary statistics for reacting data - Conf with previous_feedback
reacting_cv_normal_conf_feedback <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, conf_level, previous_feedback),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)
reacting_cv_normal_conf_feedback <- na.omit(reacting_cv_normal_conf_feedback)  # Remove NA values
reacting_cv_normal_feedback <- na.omit(reacting_cv_normal_feedback)  # Remove NA values

# A2): Summary statistics for reacting data - feedback_category
reacting_cv_normal_feedback_success <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, feedback_category),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)

# A2): Summary statistics for reacting data - Conf with feedback_category
reacting_cv_normal_conf_feedback_success <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, conf_level, feedback_category),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)
reacting_cv_normal_conf_feedback_success <- na.omit(reacting_cv_normal_conf_feedback)  # Remove NA values
reacting_cv_normal_conf_feedback_success <- na.omit(reacting_cv_normal_conf_feedback_success)  # Remove NA values

# B): Summary statistics for reacting data with MotorInvolvement
reacting_cv_MotorInvolvement <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, MotorInvolvement),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)

# B): Summary statistics for reacting data with MotorInvolvement - Conf
reacting_cv_MotorInvolvement_conf <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, MotorInvolvement, conf_level),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)
reacting_cv_MotorInvolvement_conf <- na.omit(reacting_cv_MotorInvolvement_conf)  # Remove NA values

# C): Summary statistics for reacting data with MotorSide
reacting_cv_MotorSide <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, MotorSide),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)

# C): Summary statistics for reacting data with MotorSide - Conf
reacting_cv_MotorSide_conf <- ddply(
  data_reacting,  # Changed from data_timing
  .(SubjID, Session, MotorSide, conf_level),
  summarise,
  mean_tp = mean(ActionTiming),
  sdev = sd(ActionTiming)
)
reacting_cv_MotorSide_conf <- na.omit(reacting_cv_MotorSide_conf)  # Remove NA values


# Model 1: Reacting at the Trial Level
model_reacting_trial <- lmer(ActionTiming ~ (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_trial)
model_reacting_trial_data <- ggpredict(model_reacting_trial, terms = "Session")
plot(model_reacting_trial_data)
report(model_reacting_trial)

# Model 2: Reacting with Confidence Level
model_reacting_conf <- lmer(ActionTiming ~ relevel(as.factor(conf_level), 1) + (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_conf)
model_reacting_conf_data <- ggpredict(model_reacting_conf, terms = "conf_level")
plot(model_reacting_conf_data)
report(model_reacting_conf)

# Model 3: Reacting with Motor Involvement
model_reacting_motor_involvement <- lmer(ActionTiming ~ relevel(as.factor(MotorInvolvement), 1) + (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_motor_involvement)
model_reacting_motor_involvement_data <- ggpredict(model_reacting_motor_involvement, terms = "MotorInvolvement")
plot(model_reacting_motor_involvement_data)
report(model_reacting_motor_involvement)

# Model 4: Reacting with Motor Involvement and Confidence Level
model_reacting_motor_involvement_conf <- lmer(ActionTiming ~ MotorInvolvement * conf_level + (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_motor_involvement_conf)
model_reacting_motor_involvement_conf_data <- ggpredict(model_reacting_motor_involvement_conf, terms = c("conf_level", "MotorInvolvement"))
plot(model_reacting_motor_involvement_conf_data)
report(model_reacting_motor_involvement_conf)

# Model 5: Reacting with Motor Side
model_reacting_motor_side <- lmer(ActionTiming ~ MotorSide + (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_motor_side)
model_reacting_motor_side_data <- ggpredict(model_reacting_motor_side, terms = "MotorSide")
plot(model_reacting_motor_side_data)
report(model_reacting_motor_side)

# Model 6: Reacting with Motor Side and Confidence Level
model_reacting_motor_side_conf <- lmer(ActionTiming ~ MotorSide * conf_level + (1 | SubjID/Session), data = data_reacting)
summary(model_reacting_motor_side_conf)
model_reacting_motor_side_conf_data <- ggpredict(model_reacting_motor_side_conf, terms = c("conf_level", "MotorSide"))
plot(model_reacting_motor_side_conf_data)
report(model_reacting_motor_side_conf)

# Linear mixed-effects models - Session Level:
# Model for Confidence Level Mean Reacting
mean_Session_normal_conf <- lmer(mean_tp ~ conf_level + (1 | SubjID), data = reacting_cv_normal_conf)
summary(mean_Session_normal_conf)
report(mean_Session_normal_conf)

# Model for Confidence Level Standard Deviation Reacting
mean_Session_normal_conf_sdev <- lmer(sdev ~ conf_level + (1 | SubjID), data = reacting_cv_normal_conf)
summary(mean_Session_normal_conf_sdev)
report(mean_Session_normal_conf_sdev)

################################################ Paper ################################################

# Convert MotorInvolvement to a factor and set reference level
reacting_cv_MotorInvolvement$MotorInvolvement <- as.factor(reacting_cv_MotorInvolvement$MotorInvolvement)
reacting_cv_MotorInvolvement$MotorInvolvement <- relevel(reacting_cv_MotorInvolvement$MotorInvolvement, ref = "Move")

# Model for Mean Reacting with Motor Involvement
mean_Session_MotorInvolvement <- lmer(mean_tp ~ MotorInvolvement + (1 | SubjID), data = reacting_cv_MotorInvolvement)
summary(mean_Session_MotorInvolvement)
report(mean_Session_MotorInvolvement)

# Model for Standard Deviation Reacting with Motor Involvement
mean_Session_MotorInvolvement_sdev <- lmer(sdev ~ MotorInvolvement + (1 | SubjID), data = reacting_cv_MotorInvolvement)
summary(mean_Session_MotorInvolvement_sdev)
report(mean_Session_MotorInvolvement_sdev)

# Convert MotorSide to a factor and set reference level
reacting_cv_MotorSide$MotorSide <- as.factor(reacting_cv_MotorSide$MotorSide)
reacting_cv_MotorSide$MotorSide <- relevel(reacting_cv_MotorSide$MotorSide, ref = "NDS")

# Model for Mean Reacting with Motor Side
mean_Session_MotorSide <- lmer(mean_tp ~ MotorSide + (1 | SubjID), data = reacting_cv_MotorSide)
summary(mean_Session_MotorSide)
report(mean_Session_MotorSide)

# Model for Standard Deviation Reacting with Motor Side
mean_Session_MotorSide_sdev <- lmer(sdev ~ MotorSide + (1 | SubjID), data = reacting_cv_MotorSide)
summary(mean_Session_MotorSide_sdev)
report(mean_Session_MotorSide_sdev)

################################################ TP Task ################################################ 


## Standard deviation - mean calculations:

# A): Summary statistics for timing data
timing_cv_normal <- ddply(
  data_timing, 
  .(SubjID, Session), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

# A): Summary statistics for timing data - Conf
timing_cv_normal_conf <- ddply(
  data_timing, 
  .(SubjID, Session, conf_level), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

timing_cv_normal_conf <- na.omit(timing_cv_normal_conf)  # Remove NA values

# A2): Summary statistics for timing data - previous_feedback
timing_cv_normal_feedback <- ddply(
  data_timing, 
  .(SubjID, Session, previous_feedback), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

# A2): Summary statistics for timing data - Conf with previous_feedback
timing_cv_normal_conf_feedback <- ddply(
  data_timing, 
  .(SubjID, Session, conf_level, previous_feedback), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

timing_cv_normal_conf_feedback <- na.omit(timing_cv_normal_conf_feedback)  # Remove NA values
timing_cv_normal_feedback <- na.omit(timing_cv_normal_feedback)  # Remove NA values

# A2): Summary statistics for timing data - feedback_category
timing_cv_normal_feedback_success <- ddply(
  data_timing, 
  .(SubjID, Session, feedback_category), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

# A2): Summary statistics for timing data - Conf with feedback_category
timing_cv_normal_conf_feedback_success <- ddply(
  data_timing, 
  .(SubjID, Session, conf_level, feedback_category), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

timing_cv_normal_conf_feedback_success <- na.omit(timing_cv_normal_conf_feedback)  # Remove NA values
timing_cv_normal_conf_feedback_success <- na.omit(timing_cv_normal_conf_feedback_success)  # Remove NA values

# B): Summary statistics for timing data with MotorInvolvement
timing_cv_MotorInvolvement <- ddply(
  data_timing, 
  .(SubjID, Session, MotorInvolvement), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

# B): Summary statistics for timing data with MotorInvolvement - Conf
timing_cv_MotorInvolvement_conf <- ddply(
  data_timing, 
  .(SubjID, Session, MotorInvolvement, conf_level), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

timing_cv_MotorInvolvement_conf <- na.omit(timing_cv_MotorInvolvement_conf)  # Remove NA values

# C): Summary statistics for timing data with MotorSide
timing_cv_MotorSide <- ddply(
  data_timing, 
  .(SubjID, Session, MotorSide), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

# C): Summary statistics for timing data with MotorSide - Conf
timing_cv_MotorSide_conf <- ddply(
  data_timing, 
  .(SubjID, Session, MotorSide, conf_level), 
  summarise, 
  mean_tp = mean(ActionTiming), 
  sdev = sd(ActionTiming)
)

timing_cv_MotorSide_conf <- na.omit(timing_cv_MotorSide_conf)  # Remove NA values


# Model 1: Timing at the Trial Level
model_timing_trial <- lmer(ActionTiming ~ (1 | SubjID/Session), data = data_timing)
summary(model_timing_trial)
model_timing_trial_data <- ggpredict(model_timing_trial, terms = "Session")
plot(model_timing_trial_data)
report(model_timing_trial)

# Model 2: Timing with Confidence Level
model_timing_conf <- lmer(ActionTiming ~ relevel(as.factor(conf_level), 1) + (1 | SubjID/Session), data = data_timing)
summary(model_timing_conf)
model_timing_conf_data <- ggpredict(model_timing_conf, terms = "conf_level")
plot(model_timing_conf_data)
report(model_timing_conf)

# Model 3: Timing with Motor Involvement
model_timing_motor_involvement <- lmer(ActionTiming ~ relevel(as.factor(MotorInvolvement), 1) + (1 | SubjID/Session), data = data_timing)
summary(model_timing_motor_involvement)
model_timing_motor_involvement_data <- ggpredict(model_timing_motor_involvement, terms = "MotorInvolvement")
plot(model_timing_motor_involvement_data)
report(model_timing_motor_involvement)

# Model 4: Timing with Motor Involvement and Confidence Level
model_timing_motor_involvement_conf <- lmer(ActionTiming ~ MotorInvolvement * conf_level + (1 | SubjID/Session), data = data_timing)
summary(model_timing_motor_involvement_conf)
model_timing_motor_involvement_conf_data <- ggpredict(model_timing_motor_involvement_conf, terms = c("conf_level", "MotorInvolvement"))
plot(model_timing_motor_involvement_conf_data)
report(model_timing_motor_involvement_conf)

# Model 5: Timing with Motor Side
model_timing_motor_side <- lmer(ActionTiming ~ MotorSide + (1 | SubjID/Session), data = data_timing)
summary(model_timing_motor_side)
model_timing_motor_side_data <- ggpredict(model_timing_motor_side, terms = "MotorSide")
plot(model_timing_motor_side_data)
report(model_timing_motor_side)

# Model 6: Timing with Motor Side and Confidence Level
model_timing_motor_side_conf <- lmer(ActionTiming ~ MotorSide * conf_level + (1 | SubjID/Session), data = data_timing)
summary(model_timing_motor_side_conf)
model_timing_motor_side_conf_data <- ggpredict(model_timing_motor_side_conf, terms = c("conf_level", "MotorSide"))
plot(model_timing_motor_side_conf_data)
report(model_timing_motor_side_conf)

# Linear mixed-effects models - Session Level:

# Model for Timing Session Level Mean Comparison
mean_Session_normal <- lmer(mean_tp ~ (1 | SubjID), data = timing_cv_normal)
summary(mean_Session_normal)

# Model for Confidence Level Mean Timing
mean_Session_normal_conf <- lmer(mean_tp ~ conf_level + (1 | SubjID), data = timing_cv_normal_conf)
summary(mean_Session_normal_conf)
report(mean_Session_normal_conf)

# Model for Confidence Level Standard Deviation Timing
mean_Session_normal_conf_sdev <- lmer(sdev ~ conf_level + (1 | SubjID), data = timing_cv_normal_conf)
summary(mean_Session_normal_conf_sdev)
report(mean_Session_normal_conf_sdev)

################################################ Paper ################################################ 

# Convert MotorInvolvement to a factor and set reference level
timing_cv_MotorInvolvement$MotorInvolvement <- as.factor(timing_cv_MotorInvolvement$MotorInvolvement)
timing_cv_MotorInvolvement$MotorInvolvement <- relevel(timing_cv_MotorInvolvement$MotorInvolvement, ref = "Move")

# Model for Mean Timing with Motor Involvement
mean_Session_MotorInvolvement <- lmer(mean_tp ~ MotorInvolvement + (1 | SubjID), data = timing_cv_MotorInvolvement)
summary(mean_Session_MotorInvolvement)
report(mean_Session_MotorInvolvement)

# Model for Standard Deviation Timing with Motor Involvement
mean_Session_MotorInvolvement_sdev <- lmer(sdev ~ MotorInvolvement + (1 | SubjID), data = timing_cv_MotorInvolvement)
summary(mean_Session_MotorInvolvement_sdev)
report(mean_Session_MotorInvolvement_sdev)

# Convert MotorSide to a factor and set reference level
timing_cv_MotorSide$MotorSide <- as.factor(timing_cv_MotorSide$MotorSide)
timing_cv_MotorSide$MotorSide <- relevel(timing_cv_MotorSide$MotorSide, ref = "NDS")

# Model for Mean Timing with Motor Side
mean_Session_MotorSide <- lmer(mean_tp ~ MotorSide + (1 | SubjID), data = timing_cv_MotorSide)
summary(mean_Session_MotorSide)
report(mean_Session_MotorSide)

# Model for Standard Deviation Timing with Motor Side
mean_Session_MotorSide_sdev <- lmer(sdev ~ MotorSide + (1 | SubjID), data = timing_cv_MotorSide)
summary(mean_Session_MotorSide_sdev)
report(mean_Session_MotorSide_sdev)



##------------------------------------------  TP&RT Performances Mean Std (Figure 1) ------------------------------------------  

# Define private labels and titles
y_label_tp <- "µ(TPs)"
y_label_sdev <- "σ(TPs)"
x_label_movement <- "Motor Involvement"
x_label_side <- "Motor Side"

# Define a common theme for consistency
common_theme <- theme_classic() +
  theme(
    axis.line = element_line(colour = "black"),
    plot.title = element_text(color = "black", size = 25, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 25, face = "bold.italic"),
    plot.caption = element_text(color = "black", size = 25, face = "italic"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = "black", size = 25, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 25),
    strip.text.x = element_text(size = 25, color = "black", face = "bold"),
    strip.text.y = element_text(size = 25, color = "black", face = "bold"),
    axis.text.x = element_text(color = "black", size = 25, hjust = 0.5),
    axis.text.y = element_text(size = 25, color = "black"),
    strip.background = element_rect(fill = "lightYellow", size = 1, color = "black")
  )

# 1. Timing Session Level Mean Comparison Plots
# Mean comparison for normal confidence levels
p_mean_conf <- ggplot(timing_cv_normal_conf, aes(x = conf_level, y = mean_tp, color = conf_level)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 10, pch = 20) + 
  geom_hline(aes(yintercept = 1.5), color = "black", linetype = "dashed") + 
  scale_color_manual(values = cond_conf) + 
  scale_fill_manual(values = cond_conf) + 
  labs(y = y_label_tp) + 
  common_theme + 
  coord_cartesian(ylim = c(1.00, 2.5))  # Set y-axis limits

# Mean comparison for Motor Involvement
p_mean_movement <- ggplot(timing_cv_MotorInvolvement, aes(x = MotorInvolvement, y = mean_tp, color = MotorInvolvement)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 8, pch = 20) + 
  geom_hline(aes(yintercept = 1.5), color = "black", linetype = "dashed") + 
  scale_color_manual(values = cond_color_movement) + 
  scale_fill_manual(values = cond_color_movement) + 
  labs(y = y_label_tp, x = x_label_movement) + 
  common_theme + 
  coord_cartesian(ylim = c(1.00, 2.5))  # Set y-axis limits

# Mean comparison for Motor Side
p_mean_side <- ggplot(timing_cv_MotorSide, aes(x = MotorSide, y = mean_tp, linetype = MotorSide)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA, color = "black") + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 8, pch = 20, fill = "white", color = "black") + 
  geom_hline(aes(yintercept = 1.5), color = "black", linetype = "dashed") + 
  labs(y = y_label_tp, x = x_label_side) + 
  common_theme

# 2. Timing Session Level Standard Deviation Comparison Plots
# Standard deviation comparison for normal confidence levels
p_sd_conf <- ggplot(timing_cv_normal_conf, aes(x = conf_level, y = sdev, color = conf_level)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 10, pch = 20) + 
  scale_color_manual(values = cond_conf) + 
  scale_fill_manual(values = cond_conf) + 
  labs(y = y_label_sdev) + 
  common_theme

# Standard deviation comparison for Motor Involvement
p_sd_movement <- ggplot(timing_cv_MotorInvolvement, aes(x = MotorInvolvement, y = sdev, color = MotorInvolvement)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA) + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 8, pch = 20) + 
  scale_color_manual(values = cond_color_movement) + 
  scale_fill_manual(values = cond_color_movement) + 
  labs(y = y_label_sdev, x = x_label_movement) + 
  common_theme

# Standard deviation comparison for Motor Side
p_sd_side <- ggplot(timing_cv_MotorSide, aes(x = MotorSide, y = sdev, linetype = MotorSide)) + 
  ggdist::stat_halfeye(adjust = .6, width = .6, .width = 0, justification = -.2, point_colour = NA) + 
  geom_boxplot(width = .2, outlier.shape = NA, color = "black") + 
  gghalves::geom_half_point(side = "l", range_scale = .6, alpha = .5, size = 8, pch = 20, fill = "white", color = "black") + 
  labs(y = y_label_sdev, x = x_label_side) + 
  common_theme

# Display plots (you can print each plot variable to visualize)
print(p_mean_conf)
print(p_mean_movement)
print(p_mean_side)
print(p_sd_conf)
print(p_sd_movement)
print(p_sd_side)

##------------------------------------------  TP Performances CV (Figure 1) ------------------------------------------  

# Model 1: Basic linear mixed-effects model
CV_basic <- lmer(mean_tp ~ sdev + (1|SubjID), data = timing_cv_normal)  # Fit model
summary(CV_basic)  # Display model summary
model_timing_session_data_basic <- ggpredict(CV_basic, terms = c("sdev"))  # Predict using model
plot(model_timing_session_data_basic)  # Plot predictions
report(CV_basic)  # Report model details

# Model 2: Linear mixed-effects model with confidence level
CV_conf <- lmer(mean_tp ~ sdev * conf_level + (1|SubjID), data = timing_cv_normal_conf)  # Fit model
summary(CV_conf)  # Display model summary
model_timing_session_data_conf <- ggpredict(CV_conf, terms = c("sdev", "conf_level"))  # Predict using model
plot(model_timing_session_data_conf)  # Plot predictions
report(CV_conf)  # Report model details

# Model 3: Linear mixed-effects model with motor involvement factor
CV_motor_involvement <- lmer(mean_tp ~ relevel(as.factor(MotorInvolvement),1) * sdev + (1|SubjID), data = timing_cv_MotorInvolvement)  # Fit model
summary(CV_motor_involvement)  # Display model summary
model_timing_session_data_motor_involvement <- ggpredict(CV_motor_involvement, terms = c("sdev", "MotorInvolvement"))  # Predict using model
plot(model_timing_session_data_motor_involvement)  # Plot predictions
report(CV_motor_involvement)  # Report model details

# Model 4: Linear mixed-effects model with motor side factor
CV_motor_side <- lmer(mean_tp ~ relevel(as.factor(MotorSide),1) * sdev + (1|SubjID), data = timing_cv_MotorSide)  # Fit model
summary(CV_motor_side)  # Display model summary
model_timing_session_data_motor_side <- ggpredict(CV_motor_side, terms = c("sdev", "MotorSide"))  # Predict using model
plot(model_timing_session_data_motor_side)  # Plot predictions
report(CV_motor_side)  # Report model details

# Create data frames for predictions
model_timing_session_data_motor_side <- data.frame(ggpredict(CV_motor_side, terms = c("sdev", "MotorSide")))
colnames(model_timing_session_data_motor_side)[6] <- c('MotorSide')  # Rename column for clarity
model_timing_session_data_motor_involvement <- data.frame(ggpredict(CV_motor_involvement, terms = c("sdev", "MotorInvolvement")))
colnames(model_timing_session_data_motor_involvement)[6] <- c('MotorInvolvement')  # Rename column for clarity
model_timing_session_data_conf <- data.frame(ggpredict(CV_conf, terms = c("sdev", "conf_level")))
colnames(model_timing_session_data_conf)[6] <- c('conf_level')  # Rename column for clarity

# Plot for Motor Involvement
ggplot(model_timing_session_data_motor_involvement,
       aes(x, predicted, color = MotorInvolvement, fill = MotorInvolvement)) +
  labs(title = 'CV') +
  labs(x = "σ(TPs)") +
  geom_line(size = 6) +  # Line plot
  labs(y = "µ(TPs)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = MotorInvolvement), alpha = 0.35, color = 'white') +  # Confidence ribbon
  scale_color_manual(values = cond_color_movement, labels = c('1' = 'Move', '2' = 'Press')) +  # Custom color scale
  scale_fill_manual(values = cond_color_movement, labels = c('1' = 'Move', '2' = 'Press')) +  # Custom fill scale
  theme(legend.title.align = 0.5) +
  theme_bw(base_size = 6) +  # Theme adjustments
  theme_classic() +
  theme(legend.justification = c(1, 0), legend.position = c(0.9, 0.5), legend.key.size = unit(0.25, "cm"),
        legend.background = element_rect(fill = NA, color = NA), legend.title = element_text(size = 40), legend.text = element_text(size = 40)) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(color = "black", size = 40, face = "bold.italic"),
        plot.caption = element_text(color = "black", size = 40, face = "italic"),
        axis.title.x = element_text(color = "black", size = 35),
        axis.title.y = element_text(color = "black", size = 35),
        axis.text.x = element_text(color = "black", size = 30),
        axis.text.y = element_text(color = "black", size = 35)) +
  theme(legend.position = "none")  # Hide legend

# Plot for Confidence Level
cond_conf <- c('darkgreen', 'darkolivegreen3')  # Color scheme for confidence levels

ggplot(model_timing_session_data_conf,
       aes(x, predicted, color = conf_level, fill = conf_level)) +
  labs(title = 'CV') +
  labs(x = "σ(TPs)") +
  geom_line(size = 2) +  # Line plot
  labs(y = "µ(TPs)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = conf_level), alpha = 0.35, color = 'white') +  # Confidence ribbon
  scale_color_manual(values = cond_conf, labels = c('1' = 'High', '2' = 'Low')) +  # Custom color scale
  scale_fill_manual(values = cond_conf, labels = c('1' = 'High', '2' = 'Low')) +  # Custom fill scale
  theme(legend.title.align = 0.5) +
  theme_bw(base_size = 6) +  # Theme adjustments
  theme_classic() +
  theme(legend.justification = c(1, 0), legend.position = c(0.9, 0.5), legend.key.size = unit(0.25, "cm"),
        legend.background = element_rect(fill = NA, color = NA), legend.title = element_text(size = 40), legend.text = element_text(size = 40)) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(color = "black", size = 40, face = "bold.italic"),
        plot.caption = element_text(color = "black", size = 40, face = "italic"),
        axis.title.x = element_text(color = "black", size = 40),
        axis.title.y = element_text(color = "black", size = 40),
        axis.text.x = element_text(color = "black", size = 40),
        axis.text.y = element_text(color = "black", size = 40)) +
  theme(legend.position = "none")  # Hide legend

# Plot for Motor Side
ggplot(model_timing_session_data_motor_side,
       aes(x, predicted, linetype = MotorSide), color = "black", fill = "grey") +
  labs(title = 'CV') +
  labs(x = "σ(TPs)") +
  geom_line(size = 3) +  # Line plot
  labs(y = "µ(TPs)") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "black", alpha = 0.35, color = 'white') +  # Confidence ribbon
  theme(legend.title.align = 0.5) +
  theme_bw(base_size = 6) +  # Theme adjustments
  theme_classic() +
  theme(legend.justification = c(1, 0), legend.position = c(0.9, 0.5), legend.key.size = unit(0.25, "cm"),
        legend.background = element_rect(fill = NA, color = NA), legend.title = element_text(size = 40), legend.text = element_text(size = 40)) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(color = "black", size = 40, face = "bold.italic"),
        plot.caption = element_text(color = "black", size = 40, face = "italic"),
        axis.title.x = element_text(color = "black", size = 35),
        axis.title.y = element_text(color = "black", size = 35),
        axis.text.x = element_text(color = "black", size = 30),
        axis.text.y = element_text(color = "black", size = 35)) +
  theme(legend.position = "none")  # Hide legend

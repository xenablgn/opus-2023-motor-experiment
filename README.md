# opus-2023-motor-experiment

## PhD Opus 2023: Time Perception and Motor Variance Experiment - Data & Codes

### Overview

This repository contains the code and data for analyzing the mechanisms of metacognitive processes involved in temporal error monitoring. Understanding how humans monitor and evaluate temporal errors is crucial for uncovering the mechanisms of metacognition, linking the fields of time perception and metacognition. This study investigates how different levels of motor variance influence temporal error monitoring outcomes, focusing on four key components:

- **Time Perception (TP) Performance**
- **Reaction Time (RT) Performance**
- **Error Monitoring**
- **Confidence and Decision-Making**

### Abstract

Understanding how humans monitor and evaluate temporal errors is essential for elucidating the mechanisms of metacognitive processes. In a typical paradigm, participants self-generate a time interval and subsequently evaluate its error. The implicit assumption has been that participants monitor temporal representations. Despite numerous replications of temporal error monitoring, it remains unclear what information participants monitor when assessing the just-generated interval.

This study assesses two scenarios where participants could monitor sources of variability in temporal error monitoring: the internal representation of duration (Clock Hypothesis) or motor variability (Motor Hypothesis). We manipulated motor variance by instructing participants to use either button presses or joystick movements to produce time intervals, enabling an evaluation of how different levels of motor execution variance affect temporal error monitoring.

- **Clock Hypothesis**: Additional motor variance impairs the accuracy of temporal error monitoring.
- **Motor Hypothesis**: Induced variance enhances the accuracy of temporal error monitoring.

In line with the Clock Hypothesis, error monitoring performance improved under conditions of lower motor variance. These findings suggest that humans evaluate their errors based on an informationally rich representation of internal duration, supporting metacognitive abilities in temporal error monitoring.

## Analysis Overview

The analysis consists of three main components, focusing on general performance metrics of response time (RT) and time perception (TP) across various manipulated conditions:

### 1. Response Time (RT) and Time Perception (TP) Performance
- **Objective**: Assess participants' RT and TP performance under varying conditions of motor demand and handedness.
- **Methods**:
  - Calculate summary statistics (mean, standard deviation) and Coefficient of Variation (CV) for RTs and TPs across subjects and sessions.
  - Employ linear mixed-effects models (LMMs) to analyze the impact of motor involvement and motor side on RT and TP performance.

### 2. Error Monitoring
- **Objective**: Evaluate the correlation between single-trial error monitoring performance, TP/RT performance, and confidence levels across different conditions.
- **Methods**:
  - Summarize error monitoring performance based on varying motor demands and dominant motor sides.
  - Assess error monitoring performance at different confidence levels, analyzing how these levels vary under distinct conditions to test the underlying hypotheses.

### 3. Confidence in Decision-Making
- **Objective**: Explore the relationships between confidence levels and decision time across different manipulated conditions.
- **Methods**:
  - Fit linear mixed-effects models to investigate how decision time and confidence are influenced by the degree of motor demand (involvement) and motor dominance.

### Visualization

All analyses include plots illustrating the relationships between RT, TP, confidence, and error monitoring. These visualizations, generated using the `ggplot2` library, effectively communicate the findings and enhance the interpretability of the results.

## Acknowledgments

- **Funding**: This work was supported by OPUS grant 2019/35/B/HS6/04389 to TWK from the National Center of Science in Poland.
- **Collaborators**: [Any Collaborators]

## Repository Structure

```plaintext
├── Data-txt-files
├── Data-mat-files
├── Datasets
│   ├── data_timing.csv         # Timing data for temporal performance analysis
│   └── data_reacting.csv       # Reacting data for error monitoring analysis
├── Codes
│   ├── Figure-1-temporal_performance_analysis.R   # Script for analyzing temporal performance and reaction time
│   ├── Figure-2-error_monitoring_analysis.R       # Script for analyzing error monitoring
│   └── Figure-3-confidence_decision_analysis.R     # Script for analyzing confidence and decision making
├── Results
│   ├── figures                   # Directory for storing generated figures
│   └── reports                   # Directory for storing generated reports and summaries
└── README.md                    # This README file



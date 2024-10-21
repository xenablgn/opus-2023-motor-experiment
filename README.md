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
- **Objective**: This analysis aims to assess participants' RT and TP performance under varying conditions of motor demand (i.e., button presses vs. joystick movements) and handedness (dominant vs. non-dominant hand). Understanding these performance metrics is crucial for unraveling the cognitive processes involved in temporal error monitoring.

- **Methods**:
  - **Statistical Calculations**: We calculate summary statistics, including mean and standard deviation (SD), for RTs and TPs across subjects and sessions. The Coefficient of Variation (CV) is also computed to evaluate the variability of responses relative to the mean, offering insights into the consistency of performance across different conditions.
  - **Modeling Approaches**: Linear mixed-effects models (LMMs) are employed to analyze the impact of motor involvement (Press vs. Move) and motor side (dominant vs. non-dominant) on RT and TP performance. This modeling accounts for both fixed effects (such as task conditions) and random effects (such as individual differences), thereby providing a comprehensive understanding of how these factors influence participants' timing and reaction capabilities.

### 2. Error Monitoring
- **Objective**: The focus of this analysis is to evaluate the correlation between single-trial error monitoring performance, TP/RT performance, and confidence levels across different experimental conditions. This relationship is essential for understanding how participants evaluate their own performance and the mechanisms underlying metacognitive processes.

- **Methods**:
  - **Performance Summarization**: We summarize error monitoring performance by analyzing responses across varying motor demands and dominant motor sides. This involves categorizing error monitoring data based on task conditions to identify patterns and differences in monitoring efficacy.
  - **Confidence Assessment**: Error monitoring performance is further assessed at different confidence levels. We analyze how confidence ratings vary under distinct conditions to test our underlying hypotheses regarding the Clock and Motor Hypotheses. Statistical tests will be applied to ascertain whether confidence levels significantly influence monitoring accuracy, with special attention to high-confidence versus low-confidence trials.

### 3. Confidence in Decision-Making
- **Objective**: This analysis explores the intricate relationships between confidence levels and decision time across manipulated conditions in both TP and RT tasks. Understanding these dynamics can shed light on how motor variance and task demands affect cognitive assessments during temporal tasks.

- **Methods**:
  - **Model Fitting**: We fit linear mixed-effects models to investigate how decision time and confidence ratings are influenced by the degree of motor demand (involvement) and motor dominance. The models will allow us to parse out the fixed effects of task conditions and the random effects associated with individual participants, thereby providing insights into how motor demands impact decision-making processes.
  - **Statistical Analysis**: We will evaluate the significance of the relationships between decision times and confidence levels, examining whether higher confidence correlates with shorter decision times across conditions. Additionally, we will investigate how induced variance in motor tasks may alter the decision-making landscape, potentially revealing the underlying cognitive mechanisms at play.

### Visualization

All analyses include plots illustrating the relationships between RT, TP, confidence, and error monitoring. These visualizations, generated using the `ggplot2` library, effectively communicate the findings and enhance the interpretability of the results.

## Acknowledgments

- **Funding**: This work was supported by OPUS grant 2019/35/B/HS6/04389 to TWK from the National Center of Science in Poland.

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

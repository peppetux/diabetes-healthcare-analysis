# Diabetes Healthcare Analysis

## Overview

This project explores a synthetic dataset containing 100,000 observations and 31 variables related to diabetes and health indicators.

The analysis was conducted using **R** and **Apache Spark**, with the aim of exploring relationships between health indicators and diabetes stages, and applying clustering techniques to identify patterns in the data.

## Project Workflow

### 1. Exploratory Data Analysis

- Examined demographic variables, including age and gender.
- Analysed the distributions of selected health indicators, such as BMI, HbA1c, fasting glucose, postprandial glucose, cholesterol and triglycerides.
- Used summary statistics, histograms and boxplots to explore the data.

### 2. Statistical Analysis

- Applied ANOVA tests to investigate differences in health indicators across diabetes groups.
- Used the Chi-square test to examine the association between age groups and diabetes diagnosis.
- Explored relationships between demographic characteristics and diabetes stages.

### 3. Clustering

- Applied hierarchical clustering using single, complete and average linkage methods.
- Standardised numerical variables before applying K-means.
- Used Apache Spark through the `sparklyr` package to perform K-means clustering.
- Compared the resulting clusters with the existing diabetes-stage labels.
- Explored an additional clustering model based on selected health indicators.

## Tools and Technologies

- R
- Apache Spark
- sparklyr
- ggplot2
- NbClust

## Key Findings

The statistical analysis highlighted differences in health indicators across diabetes groups.

The clustering analysis showed that K-means did not consistently reproduce the existing diabetes-stage categories. In particular, the overlap between groups made it difficult to distinguish reliably between diabetes, pre-diabetes and no diabetes.

These results highlight some of the limitations of unsupervised clustering when applied to overlapping health-related categories.

## Limitations

The dataset consists of synthetic observations and does not represent real patient records.

K-means is an unsupervised learning algorithm: the clusters it identifies do not represent clinical diagnoses. The results should therefore be interpreted as exploratory findings rather than as a diagnostic or predictive medical tool.

## How to Run

1. Download the dataset and place `diabetes_dataset.csv` in the project working directory.
2. Open `CODICE PROGETTO.R` in RStudio.
3. Install the required R packages and configure the local Apache Spark connection.
4. Run the script.

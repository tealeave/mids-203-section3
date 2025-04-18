# NBA Salary and Performance: Exploratory Data Analysis Plan

Pacific Boys: Marcelino Bautista, Konstantin Khorobrykh, Tim Platz, Da-Wei Lin  
Date: `r Sys.Date()`

---

## Overview

This plan outlines a comprehensive Exploratory Data Analysis (EDA) of NBA 2022-23 player salary and performance data. The goal is **purely descriptive**: to understand how player salaries relate to various performance metrics, age, experience, and star status. This EDA will inform subsequent regression modeling aligned with our research proposal.

We focus on 11 key predictors from our proposal, **plus Age, Experience, and Star Player status**. We will:

- Examine distributions of all variables
- Investigate bivariate relationships with Salary
- Explore multicollinearity
- Consider the impact of rookies and star players

---

## 1. YAML Header

```yaml
---
title: "NBA Salary and Performance: Exploratory Data Analysis"
author: "Pacific Boys: Marcelino Bautista, Konstantin Khorobrykh, Tim Platz, Da-Wei Lin"
date: "`r Sys.Date()`"
output:
  pdf_document:
    toc: true
    number_sections: true
fontsize: 11pt
geometry: margin=1in
---
```

---

## 2. Libraries

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(skimr)
library(GGally)
library(kableExtra)
library(patchwork)
```

---

## 3. Introduction

- Our goal is to **describe** how NBA player salaries relate to performance metrics, age, experience, and star status during the 2022-23 season.
- We focus on interpretable, descriptive insights rather than prediction or causality.
- This EDA will guide the specification of an OLS regression model.

---

## 4. Data Source

- **Primary dataset:** `nba_2022-23_all_stats_with_salary.csv` (52 columns, 467 players).
- **Working file:** `data_nba/eda.csv` (cleaned/filtered version).
- **Unit:** Individual NBA players, cross-sectional snapshot.
- **Notes:** PTS is already per-game average points. Additional variables include Age, Experience (`Exp`), and Star Player status.

---

## 5. Data Wrangling

```{r data-import}
nba_raw <- read_csv("data_nba/eda.csv")
```

- Filter out players with missing Salary or GP = 0.
- Use **PTS** directly (do NOT derive PPG).
- Create or verify variables:

| Variable       | Description                                         | Notes                                         |
|----------------|-----------------------------------------------------|-----------------------------------------------|
| Salary         | Player salary in USD                               | Log-transform due to skewness                 |
| PTS            | Points per game (already per-game)                 | Key scoring metric                            |
| MP             | Minutes played per game                            |                                               |
| FG             | Field goals made per game                          |                                               |
| TRB            | Total rebounds per game                            |                                               |
| 3P             | Three-pointers made per game                       |                                               |
| TOV            | Turnovers per game                                 |                                               |
| WS             | Win Shares                                         |                                               |
| GP             | Games played                                       |                                               |
| AST            | Assists per game                                   |                                               |
| Age            | Player age                                         | Additional variable of interest               |
| Star_Player    | Star status indicator (e.g., All-Star, top scorer) | Define criteria explicitly                    |
| Exp            | Years of NBA experience                            | Handle 'R' (rookie) as 0 or exclude rookies   |

- **Experience (Exp) Handling:**
  - Convert 'R' to 0 (rookie as 0 years).
  - Alternatively, exclude rookies for some analyses.
- Consider creating a binary **Star_Player** variable based on All-Star status or top percentile scorers.

---

## 6. Operationalization

- **Salary:** Player compensation in USD (log-transformed for skewness).
- **PTS:** Points per game (directly from dataset).
- **MP, FG, TRB, 3P, TOV, WS, GP, AST:** Per-game or season totals as defined.
- **Age:** Player age in years.
- **Star_Player:** Binary indicator (define criteria).
- **Experience (Exp):** Years in NBA, with rookie status handled as above.
- **Transformations:**
  - Log-transform Salary.
  - Possibly categorize Experience (rookie, early-career, veteran).

---

## 7. Exploratory Data Analysis

### A. Univariate Distributions

- Histograms/density plots for:
  - Salary (log scale)
  - PTS
  - MP
  - FG
  - TRB
  - 3P
  - TOV
  - WS
  - GP
  - AST
  - Age
  - Experience (numeric or categorical)
- Bar plots for:
  - Position
  - Star_Player status

### B. Bivariate Relationships with Salary

- Scatterplots or boxplots of **Salary vs. each predictor**:
  - PTS, MP, FG, TRB, 3P, TOV, WS, GP, AST, Age, Experience
  - Include smoothing lines (linear or LOESS)
- Separate analyses for:
  - Experience with 'R' as 0
  - Experience excluding rookies
- Boxplots of Salary by:
  - Position
  - Star_Player status
  - Experience categories (rookie vs. non-rookie)

### C. Multicollinearity

- Use `GGally::ggpairs()` and `GGally::ggcorr()` on all predictors plus Age, Experience, Star_Player.
- Identify highly correlated variables that may affect regression.

### D. Group Comparisons

- Salary differences by:
  - Position
  - Star_Player status
  - Experience group (rookies vs. non-rookies)

### E. Outliers

- Identify outliers in Salary and predictors.
- Consider their impact on analysis.

---

## 8. Initial Insights

- Summarize:
  - Distribution skewness (e.g., Salary)
  - Strongest associations with Salary
  - Multicollinearity concerns
  - Differences by position, experience, star status
  - Impact of rookies on salary and performance
  - Any surprising patterns or data issues

---

## 9. Next Steps

- Specify an OLS regression model with Salary as the dependent variable.
- Include the 11 predictors plus Age, Experience, and Star_Player.
- Consider models:
  - Including all players
  - Excluding rookies
  - With interaction terms (e.g., PTS × Star_Player)
- Address heteroscedasticity and multicollinearity as needed.
- Explore model improvements per proposal (robust SEs, interactions, advanced metrics).

---

## 10. Appendix

- **Variable Mapping Table** (from proposal, updated):

| Model Variable | Dataset Column | Notes                                         |
|----------------|----------------|-----------------------------------------------|
| Salary         | Salary         | Log-transform                                 |
| PTS            | PTS            | Points per game                               |
| MP             | MP             | Minutes per game                              |
| FG             | FG             | Field goals made per game                     |
| TRB            | TRB            | Total rebounds per game                       |
| 3P             | 3P             | Three-pointers made per game                  |
| TOV            | TOV            | Turnovers per game                            |
| WS             | WS             | Win Shares                                   |
| GP             | GP             | Games played                                 |
| AST            | AST            | Assists per game                              |
| Age            | Age            | Player age                                   |
| Star_Player    | Star_Player    | Binary indicator, define criteria             |
| Experience     | Exp            | 'R' as 0 or exclude rookies                   |

### How Toronto's TV Market Was Estimated

To estimate TV households for Toronto, I considered the population data and TV penetration rates. Given that Canadian TV penetration is high, often around 95-98%, I hypothesized that for a population of 7,800,000, the number of households might be around 3,120,000, assuming an average household size of 2.5. With 72% of households having TV subscriptions, this leads to approximately 2,246,400 TV households. However, this is a rough estimate and not supported by direct data for 2019.

- **Notes on missing data handling**
- **Additional plots or tables as needed**

---

## 11. Workflow Diagram

```mermaid
flowchart TD
    A[Load Data] --> B[Clean & Filter (GP > 0, Salary present)]
    B --> C[Verify Variables (PTS, MP, Age, Exp, Star_Player)]
    C --> D[Define Variables (per proposal + new vars)]
    D --> E[Univariate Distributions]
    E --> F[Bivariate Plots vs. Salary]
    F --> G[Multicollinearity (GGally)]
    G --> H[Group Comparisons]
    H --> I[Insights]
    I --> J[Specify Regression Models]
    J --> K[Modeling & Interpretation (future work)]
```

Hi this a test for git
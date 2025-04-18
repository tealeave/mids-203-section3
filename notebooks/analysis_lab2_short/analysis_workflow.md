# NBA Salary Analysis Workflow

This document outlines the workflow used in our NBA salary analysis project, visualizing the process from data collection to final model interpretation.

## 2. Exploratory Data Analysis

```mermaid
flowchart TD
    A[EDA Dataset] --> B[Distribution Analysis]
    A --> C[Correlation Analysis]
    A --> D[Relationship Exploration]
    
    B --> B1[Salary Distributions]
    B --> B2[Performance Metric Distributions]
    B1 --> B1a[Raw Salary - Right Skewed]
    B1 --> B1b[Log Salary - More Symmetric]
    B1 --> B1c[Box-Cox Salary - Normalized]
    
    C --> C1[Correlation Matrix]
    C --> C2[Pairwise Scatter Plots]
    C1 --> C1a[Identify Strong Correlations]
    C2 --> C2a[Detect Multicollinearity Concerns]
    
    D --> D1[Salary vs Points]
    D --> D2[Salary vs Minutes Played]
    D --> D3[Salary vs Win Shares]
    D --> D4[Salary vs Experience]
    D --> D5[Salary by Position]
    D --> D6[Salary by Team]
    D --> D7[TV Market Size by Team]
```

## 3. Model Testing

```mermaid
flowchart TD
    A[Model Formulation] --> B[Three Dependent Variables]
    B --> B1[Raw Salary - Models 1-3]
    B --> B2[Log Salary - Models 4-6]
    B --> B3[Box-Cox Salary - Models 7-9]
    
    A --> C[Three Model Specifications]
    C --> C1[Basic: PTS only]
    C --> C2[Performance: PTS + TRB + AST]
    C --> C3[Comprehensive: Performance + Exp_num + TVMS]
    
    D[Model Diagnostics] --> D1[Collinearity - VIF Tests]
    D --> D2[Heteroscedasticity Tests]
    D2 --> D2a[Visual Inspection]
    D2 --> D2b[Breusch-Pagan Tests]
    D --> D3[Robust Standard Errors]
    
    E[Model Comparison] --> E1[R-squared Values]
    E --> E2[Significance of Coefficients]
    E --> E3[Residual Analysis]
    
    F[Model Selection] --> F1[Select Model 3]
    F1 --> F1a[Raw Salary ~ PTS + TRB + AST + Exp_num + TVMS]
```

## 4. Final Model

```mermaid
flowchart TD
    A[Confirmatory Dataset] --> B[Apply Selected Model]
    B --> B1[Model 3: Salary ~ PTS + TRB + AST + Exp_num + TVMS]
    
    B1 --> C[Calculate Robust Standard Errors]
    B1 --> D[Evaluate Model Performance]
    
    D --> D1[Predicted vs Actual Values]
    D --> D2[Residual Analysis]
    
    E[Model Interpretation] --> E1[Coefficient Interpretation]
    E --> E2[Statistical Significance]
    E --> E3[Practical Significance]
    
    F[Conclusions] --> F1[Key Performance Metrics Impact]
    F --> F2[Experience Effect]
    F --> F3[Market Size Influence]
    F --> F4[Model Limitations]
```

## Key Findings

- Salary distribution is heavily right-skewed, with a few high earners inflating the average
- Strong correlations exist between performance metrics (PTS, MP, WS, FG)
- Experience (years in NBA) has a significant positive relationship with salary
- TV Market Size provides additional explanatory power beyond performance metrics
- The raw salary model with performance metrics, experience, and market size (Model 3) was selected as the final model
- Heteroscedasticity was detected and addressed using robust standard errors
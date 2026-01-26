# Linear-Regression-Project

## Overview
This project explores how different automobile features relate to vehicle price using linear regression. The goal was to identify which variables are the strongest predictors of price and evaluate how well simple and multiple regression models perform.

The analysis was completed using the Automobile dataset and focuses on understanding relationships between price and key features such as engine size, horsepower, fuel efficiency, and curb weight.

---

## Dataset
The dataset contains information about automobiles, including:
- Price  
- Engine size  
- Horsepower  
- Curb weight  
- City MPG  
- Highway MPG  

Initial attempts to visualize all variables at once were ineffective due to heavy overlap, so the analysis shifted toward smaller, more focused visualizations.

---

## Exploratory Data Analysis
When plotting the entire dataset at once, no clear patterns were visible. To address this, the data was broken into smaller plots focusing on individual predictors.

From these visualizations, several trends became clear:
- Price tends to increase with **engine size**, **horsepower**, and **curb weight**
- Price tends to decrease as **city MPG** and **highway MPG** increase

These observations guided the selection of predictors for the regression models.

---

## Regression Models

### Single-Predictor Models
Five simple linear regression models were tested using:
- Horsepower  
- City MPG  
- Highway MPG  
- Engine size  
- Curb weight  

Each model was evaluated using **R²** and **AIC**.

| Predictor       | R²     | AIC     |
|-----------------|--------|---------|
| Horsepower      | 0.6583 | 3817.784 |
| City MPG        | 0.4967 | 3892.529 |
| Highway MPG     | 0.5147 | 3885.500 |
| Engine Size     | 0.7888 | 3724.902 |
| Curb Weight     | 0.6963 | 3795.058 |

**Engine size** performed best and was selected as Model 1.

---

### Model 1: Price ~ Engine Size
Model 1 shows a strong positive relationship between engine size and price, with an R² of **0.7888**, meaning engine size alone explains nearly 79% of the variation in automobile prices.

A quadratic model was also tested, but it only improved the AIC by less than 1. Because the improvement was negligible and the linear model is simpler, the linear model was chosen.

---

### Model 2: Multiple Linear Regression
A second model was built using multiple predictors:

**Price ~ Engine Size + Curb Weight + Horsepower**

This model improved performance:
- **R²:** 0.8166  
- **AIC:** 3699.624  

While Model 1 was already strong, Model 2 explained more variability and provided better overall fit.

---

## Model Diagnostics
Diagnostic plots revealed:
- Increasing error variance for higher-priced vehicles (heteroscedasticity)
- Mild curvature in residuals, suggesting some nonlinearity
- Outliers associated with luxury or unusual vehicles
- A small number of influential observations affecting the model

Overall, the model performs well for average vehicles but is less accurate for extremely expensive cars.

---

## Conclusion
Engine size is the strongest single predictor of automobile price. However, adding curb weight and horsepower significantly improves model performance. Although the simpler model works well, the multiple regression model provides better predictive power and is better suited for estimating future automobile prices.

---

## Tools & Technologies
- R  
- Linear Regression  
- Data Visualization  
- Model Evaluation (R², AIC)  

---

## Future Improvements
- Explore interaction terms between predictors  
- Apply transformations to address heteroscedasticity  
- Test non-linear or regularized regression models  
- Evaluate performance using train/test splits or cross-validation

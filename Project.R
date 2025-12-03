auto <- read.csv("~/Desktop/UCI_Data_S_Sets/automobile/imports-85.csv")
View(auto)

# MODEL 1: Price ~ Engine Size
# Linear model
fit.lin <- lm(price ~ enginesize, data = auto)

# Quadratic model
fit.quad <- lm(price ~ enginesize + I(enginesize^2), data = auto)

# Scatterplot
plot(auto$enginesize, auto$price,
     main = "Price vs Engine Size with Linear & Quadratic Fits",
     xlab = "Engine Size",
     ylab = "Price ($)")

# Create x-grid for smooth curves
xmesh <- seq(min(auto$enginesize), max(auto$enginesize), length.out = 200)

# Predictions for each model
yhat.lin <- predict(fit.lin, newdata = data.frame(enginesize = xmesh))
yhat.quad <- predict(fit.quad, newdata = data.frame(enginesize = xmesh))

# Add lines to plot
lines(xmesh, yhat.lin, col = "blue", lwd = 2)
lines(xmesh, yhat.quad, col = "red", lwd = 2)

# Legend
legend("topleft",
       legend = c("Linear Fit", "Quadratic Fit"),
       col = c("blue", "red"),
       lwd = 2)




# Model 2: Multiple Regression

# Full model with all candidate predictors
m2.full <- lm(price ~ enginesize + curbweight + horsepower + citympg + highwaympg,
              data = auto)
summary(m2.full)

# Remove variables that are not significant (p > 0.05)
m2.final <- lm(price ~ enginesize + curbweight + horsepower,
               data = auto)
summary(m2.final)

# Compare Model 1 vs Model 2 using AIC
AIC(m1.es, m2.final)

# Diagnostic Plots for final model
plot(m2.final)   # Residuals vs fitted
plot(m2.final)   # Normal Q-Q plot



# ==============================
# Statistical Analysis of Ames Housing Dataset
# ==============================

library(tidyverse)
library(modeldata)
library(fitdistrplus)
library(moments)
library(caret)

# Load dataset
data(ames)

# ------------------------------
# 1. Exploratory Data Analysis
# ------------------------------
summary(ames$Sale_Price)

ggplot(ames, aes(x = Sale_Price)) +
  geom_histogram(bins = 40, fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Sale Prices")

# ------------------------------
# 2. Distribution Fitting
# ------------------------------
sale_price <- ames$Sale_Price

fit_norm <- fitdist(sale_price, "norm")
fit_lognorm <- fitdist(sale_price, "lnorm")

summary(fit_norm)
summary(fit_lognorm)

plot(fit_norm)
plot(fit_lognorm)

# ------------------------------
# 3. Hypothesis Testing
# ------------------------------
# Central Air vs Sale Price
t.test(Sale_Price ~ Central_Air, data = ames)

# Neighborhood effect
anova_model <- aov(Sale_Price ~ Neighborhood, data = ames)
summary(anova_model)

# ------------------------------
# 4. Regression Modeling
# ------------------------------
model <- lm(
  Sale_Price ~ Gr_Liv_Area + Overall_Qual + Year_Built,
  data = ames
)

summary(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)

# ------------------------------
# 5. Train-Test Evaluation
# ------------------------------
set.seed(42)
train_index <- createDataPartition(ames$Sale_Price, p = 0.8, list = FALSE)

train_data <- ames[train_index, ]
test_data <- ames[-train_index, ]

train_model <- lm(
  Sale_Price ~ Gr_Liv_Area + Overall_Qual + Year_Built,
  data = train_data
)

predictions <- predict(train_model, test_data)
rmse <- RMSE(predictions, test_data$Sale_Price)

rmse

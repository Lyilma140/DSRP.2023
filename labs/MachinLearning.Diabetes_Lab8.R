install.packages("tidymodels")
library(dplyr)
library(ggplot2)

diabetes <- diabetes_prediction_dataset_csv_1_
diabetes
## PCA plot
# remove non numerical variables 
diabetesAllNumeric <- select(diabetes, -c(gender,smoking_history))
diabetesAllNumeric

# do PCA
pcas <- prcomp(diabetes_num, scale. = T)
summary(pcas)
pcas$rotation
pcas$rotation^2

## get the x values of PCAs and make a data frame
pca_vals <- as.data.frame(pcas$x)
pca_vals$smoking_history <- diabetes$smoking_history

ggplot(pca_vals, aes(PC1, PC2, color = smoking_history))+
  geom_point()+
  theme_minimal()
ggsave()

## Step 1 : Collect Data ####
head(diabetes)


## Step 2: Clean and Process Data
diabetes_num <- mutate(diabetes,
                         smoking_history = as.integer(smoking_history))

noNas <- filter(diabetes, !is.na(smoking_history))


## Step 3: Visualize Data
library(reshape2)
library(ggplot2)
install.packages("reshape2")

cor(diabetesAllNumeric)

diabetesCors <- diabetesAllNumeric |>
  cor() |>
  melt() |>
  as.data.frame()

ggplot(diabetesCors, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
scale_fill_gradient2(low = "red", high = "blue", mid = "white",
                     midpoint = 0)



# Step 4:Perform Feature Selection
# Choose which variables you want to classify or predict
# Choose which variable you want to use as features in your model
# For iris data....
# Classify on smoking_history(Classification) and predict on blood_glucose_level(Regression)

#Step 5: Separate Data into Testing and Training Sets 
# Choose 70 -85% of data to train on
library(rsample)


## Set a seed for reproductability
set.seed(71723)

## Regression data set splits
# Create split
reg_split <- initial_split(diabetesAllNumeric, prop = .75)

#Use the split to form testing and training sets
reg_train <- training(reg_split)
reg_test <- testing(reg_split)


class_split <- initial_split(diabetes, prop = 0.75)

class_train <- training(class_split)
class_test <- testing(class_split)
class_test



library(parsnip)
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(blood_glucose_level ~ smoking_history + bmi + age + gender + diabetes + hypertension + heart_disease + HbA1c_level,
      data = reg_train)
lm_fit$fit
summary(lm_fit$fit)  

## Boosted Decision Trees 
install.packages("xgboost")
boost_reg_fit<- boost_tree()|>
  set_engine("xgboost") |>
  set_mode("regression") |>
  fit(blood_glucose_level ~ ., data = reg_train)
boost_reg_fit$fit$evaluation_log



## Step 8: Evaluate model performance on Test Set
# Calculate errors for regression
library(yardstick)
#lm_fit 
reg_results <- reg_test

reg_results$lm_pred <- predict(lm_fit, reg_test)$.pred
reg_results$boost_pred <- predict(boost_reg_fit, reg_test)$.pred

yardstick::mae(reg_results, blood_glucose_level, lm_pred)
yardstick::mae(reg_results, blood_glucose_level, boost_pred)

install.packages("Metrics")
library(Metrics)
reg_results

reg_results$lm_pred <- predict(lm_fit, class_test)$.pred_class
reg_results$boost_pred <- predict(boost_class_fit, reg_test)$.pred_class

f1_Score(class_results$Species, class_results$lm_pred)
f1_Score(class_results$Species, class_results$boost_pred)



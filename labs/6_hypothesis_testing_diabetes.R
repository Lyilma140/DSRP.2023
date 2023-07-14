
library(tidyr)

# Compare the average BMI between males and females
## null hypothesis: There is no significant difference between the average BMI of males and females 
## alternative hypothesis: There is a significant difference between average BMI between males and females 

diabetes_prediction_dataset_csv_1_
femalesbmi <- diabetes_prediction_dataset_csv_1_ |> filter(gender == "Female")
malesbmi <- diabetes_prediction_dataset_csv_1_ |> filter(gender == "Male")

t.test(femalesbmi$bmi, malesbmi$bmi, paired = F, alternative = "two.sided")

?t.test


## ANOVA ####
# null: There is no correlation between smoking history and blood glucose level
diabetes_prediction_dataset_csv_1_
anova_results <- aov(blood_glucose_level ~ smoking_history, data = diabetes_prediction_dataset_csv_1_)
summary(anova_results)
TukeyHSD(anova_results)



## Chi-squared ####
#null: There is no correlation between smoking history and diabetes 
diabetes_prediction_dataset_csv_1_.clean <- diabetes_prediction_dataset_csv_1_ |>
  filter(!is.na(smoking_history),
         !is.na(diabetes))
diabetes_prediction_dataset_csv_1_.clean

t <- table(diabetes_prediction_dataset_csv_1_$smoking_history, diabetes_prediction_dataset_csv_1_$diabetes)
chisq.test(t)

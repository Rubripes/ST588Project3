################################
#ST588-601 Project 3
#Matthew Bray
#API File
#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(tidyverse)
library(tidymodels)
library(ggplot2)

#read in the data and format in the same way as the was done in the EDA and Modeling files.
#read csv file
data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

#create list of variables to change to factor
factors<-c("diabetesBin", "highBP", "highChol", "cholCheck", "smoker", "stroke", "heartProb", "physAct", "fruits", "veg", "etOH", "hlthCare", "noDoc", "genHlth", "diffWalk", "sex", "age", "edu", "income")
#create list of variables to change to integers
integers<-c("bmi", "mentHlth", "physHlth")
#use rename() to rename all variables for consistency and mutate() to change each list to appropriate data type
data1 <- data |>
  rename("diabetesBin" = "Diabetes_binary",
         "highBP" = "HighBP",
         "highChol" = "HighChol",
         "cholCheck" = "CholCheck",
         "bmi" = "BMI",
         "smoker" = "Smoker",
         "stroke" = "Stroke",
         "heartProb" = "HeartDiseaseorAttack",
         "physAct" = "PhysActivity",
         "fruits" = "Fruits",
         "veg" = "Veggies",
         "etOH" = "HvyAlcoholConsump",
         "hlthCare" ="AnyHealthcare",
         "noDoc" = "NoDocbcCost",
         "genHlth" = "GenHlth",
         "mentHlth" = "MentHlth",
         "physHlth" = "PhysHlth",
         "diffWalk" = "DiffWalk",
         "sex" = "Sex",
         "age" = "Age",
         "edu" = "Education",
         "income" = "Income"
  ) |>
  mutate_at(factors, factor) |>
  mutate_at(integers, as.integer)

#create list of yes/no factors to be coded where 0=no, 1=yes
yesNo <- c("diabetesBin", "highBP", "highChol", "cholCheck", "smoker", "stroke", "heartProb", "physAct", "fruits", "veg", "etOH", "hlthCare", "noDoc", "diffWalk")
#mutate_at the list yesNo and equate 0 to no and 1 to yes.
data2 <- data1 |> 
  mutate_at(.vars = yesNo,
            .funs = fct_recode,
            "Yes" = "1",
            "No" = "0")

data3 <- data2 |>
  #define levels for genHlth
  mutate(genHlth = fct_recode(genHlth,
                              "Excellent" = "1",
                              "Very Good" = "2",
                              "Good" = "3",
                              "Fair" = "4",
                              "Poor" = "5"),
         #define levels for age
         age = fct_recode(age,
                          "18 to 24" = "1",
                          "25 to 30" = "2",
                          "30 to 34" = "3",
                          "35 to 39" = "4",
                          "40 to 44" = "5",
                          "45 to 49" = "6",
                          "50 to 54" = "7",
                          "55 to 59" = "8",
                          "60 to 64" = "9",
                          "65 to 69" = "10",
                          "70 to 74" = "11",
                          "75 to 79" = "12",
                          "80 and Older" = "13"),
         #define levels for edu
         edu = fct_recode(edu,
                          "No School/Only Kindergarten" = "1",
                          "Grades 1-8" = "2",
                          "Grades 9-11" = "3",
                          "Grade 12/GED" = "4",
                          "College Years 1-3" = "5",
                          "College Years >=4" = "6"),
         #define levels for income
         income =fct_recode(income,
                            "Less than $10K" = "1",
                            "$10K to less than $15K" = "2",
                            "$15K to less than $20K" = "3",
                            "$20K to less than $25K" = "4",
                            "$25K to less than $35K" = "5",
                            "$35K to less than $50K" = "6",
                            "$50K to less than $75K" = "7",
                            "Greater than $75K" = "8"),
         #define levels for sex
         sex = fct_recode(sex,
                          "Female" = "0",
                          "Male" = "1")
  )


#we'll create the recipe.  We'll define the variables to use in the model by defining the response (diabetesBin), and the feature variables (smoker, fruits, veg, etOH, sex, and age).
diabetes_rec <- 
  recipe(diabetesBin ~ smoker + fruits + veg + etOH + sex + age + physAct, data=data3)

#this is the model definition using the best mtry tuning hyperparameter, 3
rf_mod <- rand_forest(mtry = 3) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")


#create workflow for rf model engine
rf_wfl <- workflow() %>%
  add_recipe(diabetes_rec) %>%
  add_model(rf_mod)

#define workflow
#rf_final_wfl <- rf_wfl %>%
#  finalize_workflow(rf_best_params)


#define final model using the best rendom forest workflow from above and apply it to the full dataset.
final_model <- rf_wfl %>%
  fit(data3)

#fit final model to generate predictors for confusion matrix
predictors <- predict(final_model, new_data = data3)
data3$pred <- predictors$.pred_class
confMat <- conf_mat(data = data3, truth = diabetesBin, estimate = pred)

#* @apiTitle ST588-601 Fall 2024 Project 3
#* @apiDescription Make Predictions Using Best Model from Modeling QMD File

# Send message with my name and url for Github pages.
#* @get /info
function() {
    "My Name is Matthew Bray and the URL to my rendered Github pages site for this project is: https://rubripes.github.io/ST588Project3/"
}



#* Return the predicted class of "Yes" or "No" for having Diabetes
#* @get /pred
#* @param Smoker [No] Smoked 100 cigarettes in lifetime? Yes or No?
#* @param Fruits [Yes] Consume fruit daily? Yes or No?
#* @param Vegetables [Yes] Consume vegetables daily? Yes or No?
#* @param Drinker [No] Heavy Drinker? Yes or No?
#* @param Sex [Female] Male or Female?
#* @param AgeGroup [18 to 24] Age Group (18 to 24, 25 to 29, 30 to 34, 35 to 39, 40 to 44, 45 to 49, 50 to 54, 55 to 59, 60 to 64, 65 to 69, 70 to 74, 75 to 79, 80 and Older)
#* @param PhysicalActivity [Yes] Physical Activity in Last 30 Days? Yes or No?
value <- function(Smoker, Fruits, Vegetables, Drinker, Sex, AgeGroup, PhysicalActivity) {
    predict(final_model, new_data=data.frame(smoker=Smoker,
                                            fruits=Fruits,
                                            veg=Vegetables,
                                            etOH=Drinker,
                                            sex=Sex,
                                            age=AgeGroup,
                                            physAct=PhysicalActivity))
}

#Plot Confusion Matrix using autoplot from ggplot package
#* @serializer png
#* @get /confusion
function() {
 a<-autoplot(confMat, type="mosaic")
 b<-autoplot(confMat, type="heatmap")
 print(a)
 print(b)
}

###########################
#3 example function calls
#1)   http://127.0.0.1:8000/pred?Smoker=No&Fruits=Yes&Vegetables=Yes&Drinker=No&Sex=Female&AgeGroup=18%20to%2024&PhysicalActivity=Yes

#2)   http://127.0.0.1:8000/pred?Smoker=Yes&Fruits=No&Vegetables=No&Drinker=Yes&Sex=Male&AgeGroup=80%20and%20Older&PhysicalActivity=No

#3)   http://127.0.0.1:8000/pred?Smoker=Yes&Fruits=No&Vegetables=No&Drinker=No&Sex=Male&AgeGroup=70%20to%2074&PhysicalActivity=No





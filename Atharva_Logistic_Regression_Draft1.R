#Author : Atharva Joshi
# Fitting a Logistic Regression Model 
# Using Giulio's training and testing datasets 

################ Importing Libraries ############
library(readxl)
library(glmnet)
#The glmnet package provides tools for fitting regularized generalized linear models (GLMs).
library(tidymodels)
#The tidymodels ecosystem is designed for tidy and consistent modeling workflows.
library(tidyverse)
#he tidyverse is a collection of R packages that share a common philosophy:
#making data manipulation and visualization more intuitive and consistent.
library(readr)
#The readr package provides functions for efficiently reading and writing rectangular data files 
#(e.g., CSV, TSV, Excel).
library(xlsx)

######## Reading the data from excel ###########
train_data <- read_excel("train.xlsx")
train_data

######## Converting bankrupt to factor ########## 
train_data$`Bankrupt?` = as.factor(train_data$`Bankrupt?`)

####### Training the Model ######
# Train a logistic regression model
model <- logistic_reg(mixture = double(1), penalty = double(1)) %>%
  #This line initializes a logistic regression model using the tidymodels framework.
  #The logistic_reg() function creates a logistic regression model object.
  #The mixture and penalty arguments are set to double(1), which means that the model will use a mixture of L1 (Lasso) and L2 (Ridge) regularization.
  #These penalties help control the model’s complexity and prevent overfitting.
  
  set_engine("glmnet") %>%
  #The pipe operator (%>%) is used to chain operations together.
   #set_engine("glmnet") specifies that the model engine (backend) for this logistic regression model is the glmnet package.
  
  set_mode("classification") %>%
  #This line sets the mode of the model to “classification.”
  #Since were predicting bankruptcy (a binary outcome), classification mode is appropriate.
  #The model will learn to classify instances as either bankrupt or not bankrupt.
  
  fit(train_data$`Bankrupt?` ~ ., data = train_data)

####### Model summary ########

tidy(model)
summary(model)

####### Importing Testing set ########
test_data <- read_excel("test.xlsx")
test_data$Actual_Bankrupt = as.factor(test_data$Actual_Bankrupt)

##################Genrating Predictions ############
pred_class <- predict(model,
                      new_data = test_data,
                      type = "class")
          #new_data = test_data specifies the dataset for which predictions are needed.
          #type = "class" indicates that we want the predicted class labels (e.g., 0 or 1 for bankruptcy/non-bankruptcy).

#################Generating Probabilities###########
pred_proba <- predict(model,
                      new_data = test_data,
                      type = "prob")
          #type = "prob" ensures that we get the probability estimates for each class (e.g., probability of bankruptcy and non-bankruptcy).

############ Checking Accuracy #############

results <- test_data %>%
  select(Actual_Bankrupt) %>%
  bind_cols(pred_class, pred_proba)
          #This line combines the actual bankruptcy labels, predicted class labels, and predicted probabilities into a single data frame called results.
          #test_data %>% select(Actual_Bankrupt) selects only the column representing the actual bankruptcy labels (assuming it’s named “Actual_Bankrupt”).
          #bind_cols(pred_class, pred_proba) binds the predicted class labels and probabilities to the selected column

accuracy(results ,truth = Actual_Bankrupt, estimate = .pred_class)
          #truth = Actual_Bankrupt specifies the true class labels (actual bankruptcy status) from the test_data.
          #estimate = .pred_class refers to the predicted class labels obtained earlier.
          #The accuracy() function compares the predicted labels to the true labels and calculates the accuracy score.

#############Exporting Results to Excel###########
write.xlsx(results, "Model_Results.xlsx")

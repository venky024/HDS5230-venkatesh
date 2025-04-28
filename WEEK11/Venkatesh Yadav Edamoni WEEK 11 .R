library(mlbench)
library(purrr)

data("PimaIndiansDiabetes2")
ds <- as.data.frame(na.omit(PimaIndiansDiabetes2))
## fit a logistic regression model to obtain a parametric equation
logmodel <- glm(diabetes ~ .,
                data = ds,
                family = "binomial")
summary(logmodel)

cfs <- coefficients(logmodel) ## extract the coefficients
prednames <- variable.names(ds)[-9] ## fetch the names of predictors in a vector
prednames

sz <- 100000000 ## to be used in sampling
##sample(ds$pregnant, size = sz, replace = T)

dfdata <- map_dfc(prednames,
                  function(nm){ ## function to create a sample-with-replacement for each pred.
                    eval(parse(text = paste0("sample(ds$",nm,
                                             ", size = sz, replace = T)")))
                  }) ## map the sample-generator on to the vector of predictors
## and combine them into a dataframe

names(dfdata) <- prednames
dfdata

class(cfs[2:length(cfs)])

length(cfs)
length(prednames)
## Next, compute the logit values
pvec <- map((1:8),
            function(pnum){
              cfs[pnum+1] * eval(parse(text = paste0("dfdata$",
                                                     prednames[pnum])))
            }) %>% ## create beta[i] * x[i]
  reduce(`+`) + ## sum(beta[i] * x[i])
  cfs[1] ## add the intercept

## exponentiate the logit to obtain probability values of thee outcome variable
dfdata$outcome <- ifelse(1/(1 + exp(-(pvec))) > 0.5,
                         1, 0)

#XGBoost - Direct:
# Prepare the dataset
dfdata$outcome <- as.numeric(dfdata$outcome) # Make sure outcome is numeric 0/1

# Sizes you want to test
sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

# Create a result storage
results <- data.frame(Size = integer(),
                      Accuracy = numeric(),
                      Time_Taken = numeric())

set.seed(123) # For reproducibility

for (sz in sizes) {
  
  # Sample sz rows
  idx <- sample(1:nrow(dfdata), sz)
  data_sample <- dfdata[idx, ]
  
  # Split into train/test (80/20)
  train_idx <- sample(1:nrow(data_sample), 0.8 * nrow(data_sample))
  train_data <- data_sample[train_idx, ]
  test_data <- data_sample[-train_idx, ]
  
  # Convert to DMatrix (required by xgboost)
  dtrain <- xgb.DMatrix(data = as.matrix(train_data %>% select(-outcome)),
                        label = train_data$outcome)
  dtest <- xgb.DMatrix(data = as.matrix(test_data %>% select(-outcome)),
                       label = test_data$outcome)
  
  # Record start time
  start_time <- Sys.time()
  
  # Train the model
  model <- xgboost(data = dtrain,
                   objective = "binary:logistic",
                   nrounds = 50,
                   verbose = 0)
  
  # Record end time
  end_time <- Sys.time()
  
  # Make predictions
  preds <- predict(model, dtest)
  preds_class <- ifelse(preds > 0.5, 1, 0)
  
  # Calculate accuracy
  acc <- mean(preds_class == test_data$outcome)
  
  # Save results
  results <- rbind(results,
                   data.frame(Size = sz,
                              Accuracy = acc,
                              Time_Taken = as.numeric(difftime(end_time, start_time, units = "secs"))))
}

results


##XGBoost Caret:
library(caret)
library(xgboost)
library(dplyr)

# Prepare the dataset
dfdata$outcome <- as.factor(dfdata$outcome) # caret expects factor outcome for classification

# Define sizes
sizes <- c(100, 1000, 10000, 100000, 1000000, 10000000)

# Create result storage
results_caret <- data.frame(Size = integer(),
                            Accuracy = numeric(),
                            Time_Taken = numeric())

set.seed(123) # For reproducibility

for (sz in sizes) {
  
  # Sample sz rows
  idx <- sample(1:nrow(dfdata), sz)
  data_sample <- dfdata[idx, ]
  
  # Define 5-fold cross-validation
  ctrl <- trainControl(method = "cv", number = 5)
  
  # Record start time
  start_time <- Sys.time()
  
  # Train model using caret
  model <- train(outcome ~ ., 
                 data = data_sample, 
                 method = "xgbTree",
                 trControl = ctrl,
                 verbose = FALSE)
  
  # Record end time
  end_time <- Sys.time()
  
  # Best cross-validated accuracy
  acc <- max(model$results$Accuracy)
  
  # Save results
  results_caret <- rbind(results_caret,
                         data.frame(Size = sz,
                                    Accuracy = acc,
                                    Time_Taken = as.numeric(difftime(end_time, start_time, units = "secs"))))
}

results_caret

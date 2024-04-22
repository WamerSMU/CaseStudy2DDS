# CaseStudy2DDS
Youtube Link: https://www.youtube.com/watch?v=1hMAp3Fq93E
To run shiny app, runn this code:
library(tidyverse) 
library(dplyr) 
library(GGally) 
library(ggplot2) 
library(ggthemes) 
library(plotly) 
library(class) 
library(caret) 
library(e1071) 
library(readr)
library(reshape2)
library(rmarkdown)


CaseStudy2 <- read_csv("C:/Users/wamer/OneDrive/Desktop/SMU/DDS Final/CaseStudy2-data.csv")

###EDA  
#Exploring the factor of Job satisfaction, Age, and monthly income on Attrition  
#Monthly Income seems to be a big indicator of attrition  
#Age might play a role as well 

ggplot(data = CaseStudy2) +  
  geom_point(mapping = aes(x = Age, y = MonthlyIncome, color = Attrition)) + 
  geom_smooth(mapping = aes(x = Age, y = MonthlyIncome, linetype = Attrition, color = Attrition)) + facet_grid(~JobSatisfaction) 

CaseStudy2 %>% ggplot(mapping = aes(x = Department, y = DistanceFromHome, color= Attrition)) + geom_point(position = "jitter") 

p = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Department, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p1 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Age, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

 p2 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = MaritalStatus, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist()

p3 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = JobLevel, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist() 

p4 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = JobSatisfaction, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist()

p5 = CaseStudy2 %>%  
  ggplot(mapping = aes(x = Gender, y = MonthlyIncome, color = Attrition)) +  
  geom_point(position = "jitter") + ggtitle("Department vs MI") + theme_economist()

### Naive Bayes Age and Monthly Income
CaseStudy2$Age <- as.factor(CaseStudy2$Age)  # Ensuring Age is treated as a factor
model_Age_MonthlyIncome <- naiveBayes(as.factor(CaseStudy2$Attrition) ~ Age + MonthlyIncome, data = CaseStudy2, laplace = 1)
predictions_Age_MonthlyIncome <- predict(model_Age_MonthlyIncome, CaseStudy2[, c("Age", "MonthlyIncome")])
CM1_Age_MonthlyIncome <- confusionMatrix(as.factor(predictions_Age_MonthlyIncome), as.factor(CaseStudy2$Attrition))
print(CM1_Age_MonthlyIncome)

### Naive Bayes Job Satisfaction, Monthly Income, and Age
CaseStudy2$JobSatisfaction <- as.factor(CaseStudy2$JobSatisfaction)  # Ensure JobSatisfaction is treated as a factor
model_JobSatisfaction_Age_MonthlyIncome <- naiveBayes(as.factor(CaseStudy2$Attrition) ~ JobSatisfaction + Age + MonthlyIncome, data = CaseStudy2, laplace = 1)
predictions_JobSatisfaction_Age_MonthlyIncome <- predict(model_JobSatisfaction_Age_MonthlyIncome, CaseStudy2[, c("JobSatisfaction", "Age", "MonthlyIncome")])
CM1_JobSatisfaction_Age_MonthlyIncome <- confusionMatrix(as.factor(predictions_JobSatisfaction_Age_MonthlyIncome), as.factor(CaseStudy2$Attrition))
print(CM1_JobSatisfaction_Age_MonthlyIncome)

### Naive Bayes Job Level, Monthly Income, and Age
CaseStudy2$JobLevel <- as.factor(CaseStudy2$JobLevel)  # Ensure JobLevel is treated as a factor
model_JobLevel_Age_MonthlyIncome <- naiveBayes(as.factor(CaseStudy2$Attrition) ~ JobLevel + Age + MonthlyIncome, data = CaseStudy2, laplace = 1)
predictions_JobLevel_Age_MonthlyIncome <- predict(model_JobLevel_Age_MonthlyIncome, CaseStudy2[, c("JobLevel", "Age", "MonthlyIncome")])
CM1_JobLevel_Age_MonthlyIncome <- confusionMatrix(as.factor(predictions_JobLevel_Age_MonthlyIncome), as.factor(CaseStudy2$Attrition))
print(CM1_JobLevel_Age_MonthlyIncome)

###KNN Doesn't work
data <- read_csv("C:/Users/wamer/OneDrive/Desktop/SMU/DDS Final/CaseStudy2-data.csv")
data$Attrition <- factor(ifelse(data$Attrition == "Yes", 1, 0), levels = c(0, 1))
table(data$Attrition)
normalized_features <- scale(data[, c("Age", "MonthlyIncome")])
norm_data <- data
norm_data[, c("Age", "MonthlyIncome")] <- normalized_features
set.seed(123)
index <- createDataPartition(y = data$Attrition, p = 0.80, list = TRUE, times = 1)
train <- norm_data[index[[1]], ]
test <- norm_data[-index[[1]], ]
train_labels <- data$Attrition[index[[1]]]
test_labels <- data$Attrition[-index[[1]]]
if (any(table(train_labels) == 0) | any(table(test_labels) == 0)) {
  cat("Repartitioning due to unrepresented class\n")
  # Further steps to manually adjust the partition to ensure both classes are represented
}
ctrl <- trainControl(method = "cv", number = 10)

# Train KNN
knn_fit <- train(x = train[, c("Age", "MonthlyIncome")], y = train_labels, method = "knn", tuneLength = 10, trControl = ctrl)
# Predict using the best model
predictions <- predict(knn_fit, newdata = test)
# Evaluate the model
conf_mat <- confusionMatrix(predictions, test_labels)
print(conf_mat)

###Linear Regression
# Define the model fitting and diagnosing function
fit_and_diagnose <- function(formula, data) {
    fit <- lm(formula, data = data)
    summary(fit)
    par(mfrow = c(2, 2))
    plot(fit)
    return(fit)
}

# Variables 'MonthlyIncome' and 'JobSatisfaction'
simple_model <- lm(MonthlyIncome ~ JobSatisfaction, data = CaseStudy2)
summary(simple_model)

# 'Attrition' as a predictor along with 'JobSatisfaction'
multiple_model <- lm(MonthlyIncome ~ JobSatisfaction + Attrition, data = CaseStudy2)
summary(multiple_model)

# To test if the impact of 'JobSatisfaction' on 'MonthlyIncome' is different for those with 'Attrition' being 'Yes' or 'No'
interaction_model <- lm(MonthlyIncome ~ JobSatisfaction * Attrition, data = CaseStudy2)
summary(interaction_model)

### Monthly Income Attrition + Job Satisfaction Grid
groupedMI = CaseStudy2 %>% group_by(MonthlyIncome, Attrition) 
groupedMImean = groupedMI %>% summarise(mean(JobSatisfaction)) 
CaseStudy2 %>% ggplot(aes(JobSatisfaction, MonthlyIncome, color = Attrition)) + 
  geom_point(position = "jitter") + 
  facet_grid(Attrition~JobSatisfaction) + 
  ggtitle("Job Satisfaction vs MonthlyIncome by Attrition")

### Naive Bayes Threshold Adjustment

# Predicting class probabilities
predicted_probs <- predict(model_Age_MonthlyIncome, CaseStudy2, type = "raw")

# Defining a sequence of thresholds to test
thresholds <- seq(0.1, 0.9, by = 0.1)

# Empty vector to store balanced accuracy
balanced_accuracy <- numeric(length(thresholds))

# Ensure Attrition is a factor with the correct levels
CaseStudy2$Attrition <- factor(CaseStudy2$Attrition, levels = c("No", "Yes"))

# Looping through thresholds to calculate balanced accuracy
for(i in seq_along(thresholds)){
    threshold <- thresholds[i]
    predictions <- ifelse(predicted_probs[,2] > threshold, "Yes", "No")
    
    # Ensure that the predictions are factors with the correct levels
    predictions <- factor(predictions, levels = c("No", "Yes"))
    
    cm <- confusionMatrix(predictions, CaseStudy2$Attrition)
    sensitivity <- cm$byClass['Sensitivity']
    specificity <- cm$byClass['Specificity']
    balanced_accuracy[i] <- (sensitivity + specificity) / 2
}

# Find the threshold with the highest balanced accuracy
best_threshold <- thresholds[which.max(balanced_accuracy)]
best_threshold
# Applying the chosen threshold to generate new predictions
CaseStudy2$Predicted_Attrition <- ifelse(predicted_probs[, "Yes"] > 0.2, "Yes", "No")
CaseStudy2$Predicted_Attrition <- factor(CaseStudy2$Predicted_Attrition, levels = c("No", "Yes"))

# Calculating the confusion matrix with the new predictions
new_cm <- confusionMatrix(CaseStudy2$Predicted_Attrition, CaseStudy2$Attrition)

# Printing the confusion matrix and related metrics
print(new_cm)

# Apply the chosen threshold to the probabilities to make final predictions
CaseStudy2$Final_Predicted_Attrition <- ifelse(predicted_probs[, "Yes"] > best_threshold, "Yes", "No")
#####
# Load necessary libraries
library(corrplot)
library(MASS)
library(caret)

# Assuming 'data' is your dataset
# First, refine the dataset to exclude non-numeric columns as well as columns with zero variance
numeric_data <- data[, sapply(data, is.numeric)]
numeric_data_refined <- numeric_data[, apply(numeric_data, 2, var) != 0]

# Exclude specific columns known to be irrelevant or constant
excluded_columns <- c("StandardHours", "EmployeeCount", "EmployeeNumber")
numeric_data_refined <- numeric_data_refined[, !(names(numeric_data_refined) %in% excluded_columns)]

# Calculate the correlation matrix, handling NA values by specifying use = "complete.obs"
cor_matrix <- cor(numeric_data_refined, use = "complete.obs")

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

data$Department <- as.factor(data$Department)

# Load the dataset
data_path <- "C:/Users/wamer/OneDrive/Desktop/SMU/DDS Final/CaseStudy2-data.csv"
data <- read.csv(data_path)

# Starting model: Intercept only
start.model <- lm(MonthlyIncome ~ 1, data = data)

# Maximum model: Includes all potential predictors, now adding JobLevel and TotalWorkingYears
max.model <- lm(MonthlyIncome ~ Age + Department + YearsAtCompany + JobLevel + TotalWorkingYears, data = data)

# Perform forward selection
forward.model <- stepAIC(start.model, direction = "forward",
                         scope = list(lower = start.model, upper = max.model),
                         trace = FALSE)

# Display the summary of the selected model
summary(forward.model)

# Define control method: 10-fold CV with RMSE
trainControl <- trainControl(method = "cv", number = 10, summaryFunction = defaultSummary)

# Model training with cross-validation focusing on RMSE, now including JobLevel and TotalWorkingYears
model_cv <- train(MonthlyIncome ~ Age + Department + YearsAtCompany + JobLevel + TotalWorkingYears, data = data,
                  method = "lm",
                  trControl = trainControl,
                  metric = "RMSE")

# Print the results, including RMSE
print(model_cv)

# Evaluate if the RMSE goal of < $3000 is met
if(model_cv$results$RMSE < 3000) {
  cat("Success: The model's RMSE of", model_cv$results$RMSE, "meets the goal of being less than $3000.\n")
} else {
  cat("Attention: The model's RMSE of", model_cv$results$RMSE, "does not meet the goal of being less than $3000.\n")
}
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  titlePanel("Employee Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("incomeRange", "Monthly Income Range:",
                  min = min(CaseStudy2$MonthlyIncome, na.rm = TRUE),
                  max = max(CaseStudy2$MonthlyIncome, na.rm = TRUE),
                  value = range(CaseStudy2$MonthlyIncome, na.rm = TRUE)),
      selectInput("jobSatisfaction", "Job Satisfaction Level:",
                  choices = unique(CaseStudy2$JobSatisfaction),
                  selected = unique(CaseStudy2$JobSatisfaction)[1])
    ),
    mainPanel(
      plotOutput("incomePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$incomePlot <- renderPlot({
    filtered <- CaseStudy2 %>%
      filter(MonthlyIncome >= input$incomeRange[1], MonthlyIncome <= input$incomeRange[2],
             JobSatisfaction == input$jobSatisfaction)
    
    ggplot(filtered, aes(x = Age, y = MonthlyIncome, color = Attrition)) +
      geom_point() +
      labs(title = "Monthly Income vs. Age", x = "Age", y = "Monthly Income")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

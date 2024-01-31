# lets load our dataset in R studio
data<-read.csv('smoking_dataset.csv')
#let’s take overview of the dataset
str(data)
# correcting the data types.
data$hearing.left.<-as.factor(data$hearing.left.)
data$hearing.right.<-as.factor(data$hearing.right.)
data$dental.caries<-as.factor(data$dental.caries)
data$smoking<-as.factor(data$smoking)
data$Urine.protein<-as.factor(data$Urine.protein)
# Removing ID column as it not variable.
data <- subset(data, select = -ID)
str(data)

#Task 1
#checking the missing values and handle them
sum(is.na(data))
#not a single observation is missing data was complete.
#identify and treat any outlier
#for that extract numeric variables beacuse outliers are associated with numeric data
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]
#identify outliers by boxplot
boxplot(numeric_data)
#outliers are present in all numeric variables now let’s remove the outliers form each variable one by one
library(ggplot2)
#AST
Q1 <- quantile(data$AST, 0.25)
Q3 <- quantile(data$AST, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$AST >= lower_bound & data$AST <= upper_bound, ]
ggplot(data, aes(y=AST)) +geom_boxplot()
#ALT
Q1 <- quantile(data$ALT, 0.25)
Q3 <- quantile(data$ALT, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$ALT >= lower_bound & data$ALT <= upper_bound, ]
ggplot(data, aes(y=ALT)) +geom_boxplot()
#serum.creatinine
Q1 <- quantile(data$serum.creatinine, 0.25)
Q3 <- quantile(data$serum.creatinine, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$serum.creatinine >= lower_bound & data$serum.creatinine <= upper_bound, ]
ggplot(data, aes(y=serum.creatinine)) +geom_boxplot()
#fasting.blood.sugar
Q1 <- quantile(data$fasting.blood.sugar, 0.25)
Q3 <- quantile(data$fasting.blood.sugar, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$fasting.blood.sugar >= lower_bound & data$fasting.blood.sugar <= upper_bound, ]
ggplot(data, aes(y= fasting.blood.sugar)) +geom_boxplot()
find_outliers <- function(data)
  #triglyceride
  Q1 <- quantile(data$triglyceride, 0.25)
Q3 <- quantile(data$triglyceride, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$triglyceride >= lower_bound & data$triglyceride <= upper_bound, ]
ggplot(data, aes(y=triglyceride)) +geom_boxplot()
#eyesight.left.
Q1 <- quantile(data$eyesight.left., 0.25)
Q3 <- quantile(data$eyesight.left., 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$eyesight.left. >= lower_bound & data$eyesight.left. <= upper_bound, ]
ggplot(data, aes(y=eyesight.left.)) +geom_boxplot()
#eyesight.right.
Q1 <- quantile(data$eyesight.right., 0.25)
Q3 <- quantile(data$eyesight.right., 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$eyesight.right. >= lower_bound & data$eyesight.right. <= upper_bound, ]
ggplot(data, aes(y=eyesight.right.)) +geom_boxplot()
#HDL
Q1 <- quantile(data$HDL, 0.25)
Q3 <- quantile(data$HDL, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$HDL >= lower_bound & data$HDL <= upper_bound, ]
ggplot(data, aes(y=HDL)) +geom_boxplot()
#systolic
Q1 <- quantile(data$systolic, 0.25)
Q3 <- quantile(data$systolic, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$systolic >= lower_bound & data$systolic <= upper_bound, ]
ggplot(data, aes(y=systolic)) +geom_boxplot()
#relaxation
Q1 <- quantile(data$relaxation, 0.25)
Q3 <- quantile(data$relaxation, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$relaxation >= lower_bound & data$relaxation <= upper_bound, ]
ggplot(data, aes(y=relaxation)) +geom_boxplot()
#Cholesterol
Q1 <- quantile(data$Cholesterol, 0.25)
Q3 <- quantile(data$Cholesterol, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$Cholesterol >= lower_bound & data$Cholesterol <= upper_bound, ]
ggplot(data, aes(y=Cholesterol)) +geom_boxplot()
#LDL
Q1 <- quantile(data$LDL, 0.25)
Q3 <- quantile(data$LDL, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$LDL >= lower_bound & data$LDL <= upper_bound, ]
ggplot(data, aes(y=LDL)) +geom_boxplot()
#hemoglobin
Q1 <- quantile(data$hemoglobin, 0.25)
Q3 <- quantile(data$hemoglobin, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$hemoglobin >= lower_bound & data$hemoglobin <= upper_bound, ]
ggplot(data, aes(y=hemoglobin)) +geom_boxplot()
#age
Q1 <- quantile(data$age, 0.25)
Q3 <- quantile(data$age, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$age >= lower_bound & data$age <= upper_bound, ]
ggplot(data, aes(y=age)) +geom_boxplot()
#height.cm.
Q1 <- quantile(data$height.cm., 0.25)
Q3 <- quantile(data$height.cm., 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$height.cm. >= lower_bound & data$height.cm. <= upper_bound, ]
ggplot(data, aes(y=height.cm.)) +geom_boxplot()
#weight.kg.
Q1 <- quantile(data$weight.kg., 0.25)
Q3 <- quantile(data$weight.kg., 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$weight.kg. >= lower_bound & data$weight.kg. <= upper_bound, ]
ggplot(data, aes(y=weight.kg.)) +geom_boxplot()
#waist.cm.
Q1 <- quantile(data$waist.cm., 0.25)
Q3 <- quantile(data$waist.cm., 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$waist.cm. >= lower_bound & data$waist.cm. <= upper_bound, ]
ggplot(data, aes(y=waist.cm.)) +geom_boxplot()
#Gtp
Q1 <- quantile(data$Gtp, 0.25)
Q3 <- quantile(data$Gtp, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
data<- data[data$Gtp >= lower_bound & data$Gtp <= upper_bound, ]
ggplot(data, aes(y=Gtp)) +geom_boxplot()
#boxplot again to see new graph
numeric_vars <- sapply(data, is.numeric)
numeric_data <- data[, numeric_vars]
boxplot(numeric_data)

#task 2
#Summarize the data
summary(data)
#Create visualizations of the variables
ggplot(data, aes(x=gender)) +geom_bar()
ggplot(data, aes(x=age)) +geom_bar()
ggplot(data, aes(x=height.cm.)) +geom_bar()
ggplot(data, aes(x=weight.kg.)) +geom_bar()
ggplot(data, aes(x=waist.cm.)) +geom_bar()
ggplot(data, aes(x=eyesight.left.)) +geom_bar()
ggplot(data, aes(x=eyesight.right.)) +geom_bar()
ggplot(data, aes(x=hearing.left.)) +geom_bar()
ggplot(data, aes(x=hearing.right.)) +geom_bar()
ggplot(data, aes(x=systolic)) +geom_bar()
ggplot(data, aes(x=relaxation)) +geom_bar()
ggplot(data, aes(x= fasting.blood.sugar)) +geom_bar()
ggplot(data, aes(x=Cholesterol)) +geom_bar()
ggplot(data, aes(x=triglyceride)) +geom_bar()
ggplot(data, aes(x=HDL)) +geom_bar()
ggplot(data, aes(x=LDL)) +geom_bar()
ggplot(data, aes(x=hemoglobin)) +geom_bar()
ggplot(data, aes(x=Urine.protein)) +geom_bar()
ggplot(data, aes(x= serum.creatinine)) +geom_bar()
ggplot(data, aes(x=AST)) +geom_bar()
ggplot(data, aes(x=ALT)) +geom_bar()
ggplot(data, aes(x=Gtp)) +geom_bar()
ggplot(data, aes(x= oral)) +geom_bar()
ggplot(data, aes(x= dental.caries)) +geom_bar()
ggplot(data, aes(x= tartar)) +geom_bar()
ggplot(data, aes(x= smoking)) +geom_bar()

#Task 3
#finding relationship between smoking and other variables by visualization
#plotting categorical variable relation
#Smoking-Gender Relation
ggplot(data, aes(x = gender, fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Gender Relation",
       x = "Gender",
       y = "Frequency")
#Smoking-Hearing.Left. Relation
ggplot(data, aes(x = hearing.left., fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Hearing.Left. Relation",
       x = "Hearing.Left.",
       y = "Frequency")
#it is hard to interpret data because value=2  have low frequency and lets amplify it so bar is interpretable 
# Hearing.Left. Value=2
ggplot(data[data$hearing.left. == 2, ], aes(x = factor(hearing.left.), fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Hearing.left. Value=2",
       x = "Hearing.left.",
       y = "Frequency")
#Smoking-Hearing.Right. Relation
ggplot(data, aes(x = hearing.right., fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Hearing.Right. Relation",
       x = "Hearing.Right.",
       y = "Frequency")
#it is hard to interpret data because value=2  have low frequency and lets amplify it so bar is interpretable 
# Hearing.Right. Value=2
ggplot(data[data$hearing.right. == 2, ], aes(x = factor(hearing.right.), fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = " Hearing.Right. Value=2)",
       x = "Hearing.Right.",
       y = "Frequency")
#Smoking-Urine.Protein Relation
ggplot(data, aes(x = Urine.protein, fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Urine.Protein Relation",
       x = "Urine.Protein",
       y = "Frequency")
#it is hard to interpret data because certain values have low frequency and let’s use table for its interpretation
table(data$Urine.protein,data$smoking)
#Smoking-Dental.Caries Relation
ggplot(data, aes(x = dental.caries, fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Dental.Caries Relation",
       x = "Dental.Caries",
       y = "Frequency")
#Smoking-Tartar Relation
ggplot(data, aes(x = tartar, fill = smoking)) +
  geom_bar(position = "stack") +
  labs(title = "Smoking-Tartar Relation",
       x = "Tartar",
       y = "Frequency")
#plotting numeric variable relation
#Smoking-Age Relation
ggplot(data, aes(x= smoking, y=age)) +geom_violin()+
  labs(title = "Smoking-Age Relation",
       x = "smoking",
       y = "age")
#Smoking-Height.cm. Relation
ggplot(data, aes(x= smoking, y=height.cm.)) +geom_violin()+
  labs(title = "Smoking-Height.cm. Relation",
       x = "smoking",
       y = "height.cm.")
#Smoking-Weight.kg. Relation
ggplot(data, aes(x= smoking, y=weight.kg.)) +geom_violin()+
  labs(title = "Smoking-Weight.kg. Relation",
       x = "smoking",
       y = "weight.kg.")
#Smoking-Waist.cm. Relation
ggplot(data, aes(x= smoking, y=waist.cm.)) +geom_violin()+
  labs(title = "Smoking-Waist.cm. Relation",
       x = "smoking",
       y = "waist.cm.")
#Smoking-Eyesight.Left. Relation
ggplot(data, aes(x= smoking, y=eyesight.left.)) +geom_violin()+
  labs(title = "Smoking-Eyesight.Left. Relation",
       x = "smoking",
       y = "eyesight.left.")
#Smoking-Eyesight.Right. Relation
ggplot(data, aes(x= smoking, y=eyesight.right.)) +geom_violin()+
  labs(title = "Smoking-Eyesight.Right. Relation",
       x = "smoking",
       y = "eyesight.right.")
#Smoking-Systolic Relation
ggplot(data, aes(x= smoking, y=systolic)) +geom_violin()+
  labs(title = "Smoking-Systolic Relation",
       x = "smoking",
       y = "systolic")
#Smoking-Relaxation Relation
ggplot(data, aes(x= smoking, y=relaxation)) +geom_violin()+
  labs(title = "Smoking-Relaxation Relation",
       x = "smoking",
       y = "relaxation")
#Smoking-Fasting.Blood.Sugar Relation
ggplot(data, aes(x= smoking, y=fasting.blood.sugar)) +geom_violin()+
  labs(title = "Smoking-Fasting.Blood.Sugar Relation",
       x = "smoking",
       y = "fasting.blood.sugar")
#Smoking-Cholesterol  Relation
ggplot(data, aes(x= smoking, y=Cholesterol)) +geom_violin()+
  labs(title = "Smoking-Cholesterol Relation",
       x = "smoking",
       y = "Cholesterol")
#Smoking-Triglyceride Relation
ggplot(data, aes(x= smoking, y=triglyceride)) +geom_violin()+
  labs(title = "Smoking-Triglyceride Relation",
       x = "smoking",
       y = "triglyceride")
#Smoking-HDL Relation
ggplot(data, aes(x= smoking, y=HDL)) +geom_violin()+
  labs(title = "Smoking-HDL Relation",
       x = "smoking",
       y = "HDL")
#Smoking-LDL Relation
ggplot(data, aes(x= smoking, y=LDL)) +geom_violin()+
  labs(title = "Smoking-LDL Relation",
       x = "smoking",
       y = "LDL")
#Smoking-Hemoglobin Relation
ggplot(data, aes(x= smoking, y=hemoglobin)) +geom_violin()+
  labs(title = "Smoking-Hemoglobin Relation",
       x = "smoking",
       y = "hemoglobin")
#Smoking-Serum.creatinine Relation
ggplot(data, aes(x= smoking, y=serum.creatinine)) +geom_violin()+
  labs(title = "Smoking-Serum.creatinine Relation",
       x = "smoking",
       y = "serum.creatinine")
#Smoking-AST Relation
ggplot(data, aes(x= smoking, y=AST)) +geom_violin()+
  labs(title = "Smoking-AST Relation",
       x = "smoking",
       y = "AST")
#Smoking-ALT Relation
ggplot(data, aes(x= smoking, y=ALT)) +geom_violin()+
  labs(title = "Smoking-ALT Relation",
       x = "smoking",
       y = "ALT")
#Smoking-Gtp Relation
ggplot(data, aes(x= smoking, y=Gtp)) +geom_violin()+
  labs(title = "Smoking-Gtp Relation",
       x = "smoking",
       y = "Gtp")
#Task 4
#making logistic regression to find relationship
#Smoking-Age Regression Model
model <- glm(smoking ~ age, data = data, family = "binomial")
summary(model)
#Smoking-Height.cm. Regression Model
model <- glm(smoking ~ height.cm., data = data, family = "binomial")
summary(model)
#Smoking-Weight.kg. Regression Model
model <- glm(smoking ~ weight.kg., data = data, family = "binomial")
summary(model)
#Smoking-Waist.cm. Regression Model
model <- glm(smoking ~ waist.cm., data = data, family = "binomial")
summary(model)
#Smoking-Eyesight.Left. Regression Model
model <- glm(smoking ~ eyesight.left., data = data, family = "binomial")
summary(model)
#Smoking-Eyesight.Right. Regression Model
model <- glm(smoking ~ eyesight.right., data = data, family = "binomial")
summary(model)
#Smoking-Systolic Regression Model
model <- glm(smoking ~ systolic, data = data, family = "binomial")
summary(model)
#Smoking-Relaxation Regression Model
model <- glm(smoking ~ relaxation, data = data, family = "binomial")
summary(model)
#Smoking-Fasting.Blood.Sugar Regression Model
model <- glm(smoking ~ fasting.blood.sugar, data = data, family = "binomial")
summary(model)
#Smoking-Cholesterol Regression Model
model <- glm(smoking ~ Cholesterol, data = data, family = "binomial")
summary(model)
#Smoking-Triglyceride Regression Model
model <- glm(smoking ~ triglyceride, data = data, family = "binomial")
summary(model)
#Smoking-HDL Regression Model
model <- glm(smoking ~ HDL, data = data, family = "binomial")
summary(model)
#Smoking-LDL Regression Model
model <- glm(smoking ~ LDL, data = data, family = "binomial")
summary(model)
#Smoking-Hemoglobin Regression Model
model <- glm(smoking ~ hemoglobin, data = data, family = "binomial")
summary(model)
#Smoking-Serum.Creatinine Regression Model
model <- glm(smoking ~ serum.creatinine, data = data, family = "binomial")
summary(model)
#Smoking-AST Regression Model
model <- glm(smoking ~ AST, data = data, family = "binomial")
summary(model)
#Smoking-ALT Regression Model
model <- glm(smoking ~ ALT, data = data, family = "binomial")
summary(model)
#Smoking-Gtp Regression Model
model <- glm(smoking ~ Gtp, data = data, family = "binomial")
summary(model)
#Smoking-Gender Regression Model
model <- glm(smoking ~ gender, data = data, family = "binomial")
summary(model)
#Smoking-Hearing.Left. Regression Model
model <- glm(smoking ~ hearing.left., data = data, family = "binomial")
summary(model)
#Smoking-Hearing.Right. Regression Model
model <- glm(smoking ~ hearing.right., data = data, family = "binomial")
summary(model)
#Smoking-Urine.Protein Regression Model
model <- glm(smoking ~ Urine.protein, data = data, family = "binomial")
summary(model)
#Smoking-Dental.Caries Regression Model
model <- glm(smoking ~ dental.caries, data = data, family = "binomial")
summary(model)
#Smoking-Tartar Regression Model
model <- glm(smoking ~ tartar, data = data, family = "binomial")
summary(model)

#Task 5
#Smoking regression model with key variables.
model <- glm(smoking ~gender+height.cm.+weight.kg.+waist.cm.+triglyceride+HDL+hemoglobin+serum.creatinine+Gtp, data = data, family = "binomial")
summary(model)
# accuracy of the model
predictions <- predict(model, newdata = data, type = "response")
binary_predictions <- ifelse(predictions > 0.5, 1, 0)
conf_matrix <- table(binary_predictions, data$smoking)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%")

#task 6
# make random tree forest 
library(randomForest)
# Split the data into training and testing sets (adjust the ratio as needed)
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]
# Build the random forest model
rf_model <- randomForest(train_data[, c('gender', 'height.cm.', 'weight.kg.', 'waist.cm.',
                                        'triglyceride', 'HDL', 'hemoglobin', 
                                        'serum.creatinine', 'Gtp')],
                         train_data$smoking,
                         ntree = 500,  # Number of trees in the forest
                         mtry = 2)     # Number of features to consider at each split
# Evaluate model performance
predictions <- predict(rf_model, newdata = test_data[, c('gender', 'height.cm.', 'weight.kg.', 'waist.cm.',
                                                         'triglyceride', 'HDL', 'hemoglobin', 
                                                         'serum.creatinine', 'Gtp')])
conf_matrix <- table(predictions, test_data$smoking)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%")
# evaluate performance using complete dataset
predictions <- predict(rf_model, newdata = data, type = "response")
conf_matrix <- table(predictions, data$smoking)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy * 100, "%")
# lets split the data into 10 subsets (use 2 as test while 8 to train) to see what are the results
set.seed(123)
fold_indices <- cut(seq(1, nrow(data)), breaks = 10, labels = FALSE)
for (i in 1:10) {
  test_indices <- which(fold_indices == i)
  train_indices <- setdiff(seq(1, nrow(data)), test_indices)
  if (i <= 8) {
    train_data <- data[train_indices, ]
    test_data <- data[test_indices, ]    
    # build a random forest model
    rf_model <- randomForest(train_data[, c('gender', 'height.cm.', 'weight.kg.', 'waist.cm.',
                                            'triglyceride', 'HDL', 'hemoglobin', 
                                            'serum.creatinine', 'Gtp')],
                             train_data$smoking,
                             ntree = 500,
                             mtry = 2)
    # Make predictions on the test set
    predictions <- predict(rf_model, newdata = test_data[, c('gender', 'height.cm.', 'weight.kg.', 'waist.cm.',
                                                             'triglyceride', 'HDL', 'hemoglobin', 
                                                             'serum.creatinine', 'Gtp')], type = "response")
    # Evaluate model performance on the test set
    conf_matrix <- table(predictions, test_data$smoking)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    cat("Accuracy for Fold", i, ":", accuracy * 100, "%\n")
  }
}
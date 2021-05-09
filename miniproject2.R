# Loading the required library
install.packages('corrplot')
install.packages('tidyverse')
installed.packages('ggcorrplot')
install.packages('ggplot2')
install.packages('plyr')
install.packages('caret')
install.packages('caTools')

library('corrplot')
library('tidyverse')
library('ggcorrplot')
library('ggplot2')
library('plyr') # to use count function
library('caret')
library('caTools')



# Importing the data file
data <- read_csv('heart.csv')



# DISPLAY THE FIRST FEW ROWS OF DATA
head(data)



# DISPLAY THE NUMBER OF ROWS AND COLUMNS
nrow(data)
ncol(data)


# Understanding the datatype of dataset
str(data)

# DISPLAY THE SUMMARY
summary(data)





# Displaying the coralation matrix
corr <- cor(data)

# Visualize the correlation matrix
corrplot(corr)




## Data wrangling and counting missing values


# Deleting not related variables
data = subset(data, select = c(-fbs,-chol,-restecg))

# Coverting the categorical data to factor
data$s <- as.factor(data$sex)
data$target <- as.factor(data$target)
data$cp <- as.factor(data$cp)
data$ca <- as.factor(data$ca)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)



# Summary after pre-processing the data
summary(data)



# DISPLAY THE NUMBER OF NAs IN EACH COLUMN
colSums(is.na(data))




## Target variable Analysis

# Bar plot for target (Heart disease) 
data$target <- as.factor(data$target)
ggplot(data, aes(x=data$target, fill=data$target)) + 
  geom_bar() +
  xlab("Heart Disease") +
  ylab("Count") +
  ggtitle("Analysis of Presence and Absence of Heart Disease") +
  scale_fill_discrete(name = "Heart Disease", labels = c("Absence", "Presence"))



## Age variable Analysis


# Counting the frequency of the values of the age
ageCount <- count(data, 'age')
ageCount <- subset(ageCount[which(ageCount$freq > 10), ])

#ploting the age with frquency greater than 10
ggplot(ageCount, aes(x=ageCount$age, y=ageCount$freq)) + 
  ggtitle("Age Analysis") +
  xlab("Age")  +
  ylab("Age Count") +
  geom_bar(stat="identity")





# Group the different ages in three groups (young, middle, old)
young <- data[which((data$age<45)), ]
middle <- data[which((data$age>=45)&(data$age<55)), ]
elderly <- data[which(data$age>55), ]
groups <- data.frame(age_group = c("young","middle","elderly"), group_count = c(NROW(young$age), NROW(middle$age), NROW(elderly$age)))

#ploting different age groups
ggplot(groups, aes(x=groups$age_group, y=groups$group_count, fill=groups$age_group)) + 
  ggtitle("Age Analysis") +
  xlab("Age Group")  +
  ylab("group Count") +
  geom_bar(stat="identity") +
  scale_fill_discrete(name = "Age Group", labels = c("Elderly", "Middle", "Young"))


# Adding the age groups to the dataset
data <- cbind(data, groups = ifelse((data$age<45), 0, ifelse((data$age>=45)&(data$age<55), 1, 2)))
data$groups <- as.factor(data$groups)

data = subset(data, select = c(-age))



# age_group ~ target ~ sex
ggplot(data, aes(x= factor(data$groups), y=data$sex, colour=target)) + 
  geom_boxplot(stat = "boxplot",
               position = "dodge2") +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2) +
  xlab("Age Groups") +
  ylab("Gender") +
  ggtitle("Analysis of gender with different age group with presence or absense of heart disease")


 

## Sex variable analysis


# Bar plot for sex
ggplot(data, aes(x= data$sex, fill=data$target)) + 
  geom_bar() +
  xlab("Gender") +
  ylab("Gender Count") +
  ggtitle("Analysis of Gender") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))





data = subset(data, select = c(-sex))


## cp (Chest Pain) variable Analysis


# Bar plot for The chest pain experienced 
ggplot(data, aes(x= cp, fill=cp)) + 
  geom_bar() +
  xlab("Chest Pain Type") +
  ylab("Count") +
  ggtitle("Analysis of Chest Pain Experienced") +
  scale_fill_discrete(name = "Chest Pain Type", labels = c("Typical angina pain", "Atypical angina pain", "Non-Anginal pain", "Asymptomatic pain"))







# Bar for ca (number of major vessels (0-3))
ggplot(data, aes(x= ca, fill=ca)) + 
geom_bar() +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  theme(legend.position="none")


ggplot(data, aes(x= ca, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("number of major vessels") +
  ylab("Count") +
  ggtitle("Analysis of number of major vessels") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))






## trestbps (resting blood pressure) variable analysis

# Histogram for trestbps (resting blood pressure)
ggplot(data, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")
```



# removing the outliers
data$trestbps = ifelse(data$trestbps > 180, NA, data$trestbps)
data$trestbps = ifelse(is.na(data$trestbps), median(data$trestbps[which(!is.na(data$trestbps))]), data$trestbps)

# After the removal of outliers
ggplot(data, aes(x=trestbps)) + 
  geom_histogram() +
  xlab("Resting blood pressure") +
  ylab("Count") +
  ggtitle("Analysis of blood pressure")




# Density graph for trestbps (resting blood pressure)
ggplot(data, aes(x = trestbps, fill = target)) +
  geom_density(alpha=0.5) +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))




data = subset(data, select = c(-trestbps))


## oldpeak variable analysis


# Histogram for oldpeak (ST depression induced by exercise relative to rest)
ggplot(data, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")




# From the above histogram 
data$oldpeak <- log1p(data$oldpeak)

ggplot(data, aes(x=oldpeak)) + 
  geom_histogram() +
  xlab("ST depression induced by exercise relative to rest") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced by exercise relative to rest")




# Density plot for oldpeak ~ target
ggplot(data, aes(x = oldpeak, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("ST depression induced") +
  ylab("Count") +
  ggtitle("Analysis of ST depression induced and presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))



## Slope variable analysis

# Bar plot for slope (slope of the peak exercise ST segment) 
data$slope <- ifelse(data$slope == 0, 1, print(data$slope))
data$slope <- as.factor(data$slope)
ggplot(data, aes(x=data$slope, fill=data$slope)) + 
  geom_bar() +
  xlab("Slope of ST segment") +
  ylab("Count") +
  ggtitle("Analysis of slope of the peak exercise ST segment") +
  scale_fill_discrete(name = "Slope of ST segment", labels = c("Upsloping", "Flat", "Downsloping"))




# Plot for slope ~ target
ggplot(data, aes(x= slope, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("slope of the peak exercise ST segment") +
  ylab("count") +
  ggtitle("Analysis of slope of the peak exercise ST segment with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))



## thalach (maximum heart rate achieved) variable analysis


# Histogram for thalach (maximum heart rate achieved)
ggplot(data, aes(x=thalach)) + 
  geom_histogram() +
  xlab("Maximum heart rate achieved") +
  ylab("Count") +
  ggtitle("Analysis of maximum heart rate achieved")




# Replacing the outlier value with median value of thalach
data$thalach = ifelse(data$thalach < 75, NA, data$thalach)
data$thalach = ifelse(is.na(data$thalach), median(data$thalach[which(!is.na(data$thalach))]), data$thalach)

ggplot(data, aes(x=thalach)) + 
  geom_histogram() +
  xlab("Maximum heart rate achieved") +
  ylab("Count") +
  ggtitle("Analysis of maximum heart rate achieved")




# Density plot for thalach ~ target
ggplot(data, aes(x = thalach, fill = target)) +
  geom_density(alpha=0.5) +
  xlab("Maximum Heart Rate Achieved") +
  ylab("Count") +
  ggtitle("Analysis of relation of heart rate with presence of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))



## thal (blood disorder called thalassemia) variable analysis


# Bar graph for thal (blood disorder called thalassemia)
ggplot(data, aes(x=thal)) + 
  geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)")




# Replacing the invalid value with mode value of thal
data$thal = ifelse(data$thal == 0, 2, data$thal)
data$thal <- as.factor(data$thal)

ggplot(data, aes(x=thal, fill=thal)) + 
  geom_bar() +
  xlab("Blood disorder type") +
  ylab("Count") +
  ggtitle("Analysis of blood disorder (thalassemia)") +
  scale_fill_discrete(name = "Blood disorder", labels = c("Normal", "Fixed defect", "reversable defect"))



```{r}
# Bar plot for thal ~ target
ggplot(data, aes(x= thal, fill=target)) + 
  geom_bar(position = 'dodge') +
  xlab("blood disorder") +
  ylab("count") +
  ggtitle("Analysis of blood disorder with presence or absense of heart disease") +
  scale_fill_discrete(name = "Heart disease", labels = c("No", "Yes"))






## Model implementation



# To get the same set every time we run the code
set.seed(123)

# Rearranging the columns to make the target as the last column
data <- data[, c(1, 2, 3, 4, 5, 6, 7, 9, 8)]

# Dividing the data set in train and test datasets
dataSample <- sample.split(data[,ncol(data)-1], SplitRatio=0.80)
trainSet = subset(data,dataSample == TRUE)
testSet = subset(data,dataSample == FALSE)

# Creating a logistic model
logisticmodel <- glm(target~.,data = trainSet, family = "binomial")

# Summary of the created model
summary(logisticmodel)



# Making prediction with the above model
predictdata <- predict(logisticmodel, newdata = testSet[, -9], type="response")
pred <- ifelse(predictdata>=0.5,1,0)
pred <- as.factor(pred)
observed <- testSet[,9]



# Checking the accuracy of the model
confusionMatrix(pred, observed)



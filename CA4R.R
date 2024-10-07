wine = read.csv(file.choose())
df = wine
df
head(df)

print("Structure of the data")
nrow(df)
ncol(df)
str(df)

is.data.frame(wine)
is.na(df)


print("Minimum quality: ")
min(df$quality)
print("Maximum quality: ")
max(df$quality)
summary(df)
summary(df$pH)

install.packages("plotly")
library(plotly)
boxplot(df$pH,col = "red",xlab="pH",ylab="alcohol",main="Ph distribution",
        horizontal = TRUE)

#linear regression
install.packages('factoextra')
library('factoextra')

alcopH = lm(alcohol ~ pH, data = wine) #Create the linear regression
summary(alcopH) #Review the results
df=data.frame(wine$alcohol,wine$pH)
plot(df, pch = 16, col = "blue")#Plot the results
abline(df) #Add a regression line
summary(alcopH) #Review the results

#multi-linear regression
#Create a linear regression with two variables
alcopHden = lm(alcohol~pH + density, data = wine) 
summary(alcopHden) #Review the results
plot(alcopHden, pch = 16, col = "red") #plot the results



#logistic regression
install.packages("e1071")
install.packages("caTools")
install.packages("class")
library(e1071)
library(caTools)
library(class)

sample_data = sample.split(wine, SplitRatio = 0.8)
train_data <- subset(wine, sample_data == TRUE)
test_data <- subset(wine, sample_data == FALSE)

fit <-glm(chlorides~., data = train_data, family = 'binomial')
predict_unseen <-predict(fit, test_data,family = 'binomial')
table_mat <- table(test_data$quality, predict_unseen)
table_mat
head(table_mat)
ac_Test <- sum(diag(table_mat)) / sum(table_mat)
ac_Test
print(paste('Accuracy for test is found to be', ac_Test))


#knn
# Loading data
head(wine)

# Splitting data into train
# and test data
split <- sample.split(wine, SplitRatio = 0.7)
train_cl <- subset(wine, split == "TRUE")
test_cl <- subset(wine, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[,1:12])
test_scale <- scale(test_cl[, 1:12])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$quality,
                      k = 1)
classifier_knn

# Confusion Matrix
cm <- table(test_cl$quality, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$quality)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$quality,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$quality)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$quality,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$quality)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$quality,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$quality)
print(paste('Accuracy =', 1-misClassError))









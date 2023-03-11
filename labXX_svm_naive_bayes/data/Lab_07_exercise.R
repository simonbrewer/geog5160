#Setting up packages and working directory
##The goal is to discriminate between
##the type of sonar singals bounced back
##From a rock, or a metal object(M)
# setwd("~/Documents/Spring 2020/Geog 6160- Spatial Modeling/Lab 7")
library(dplyr)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(precrec)
library(e1071)

##Connecting CSV
sonar <- read.csv("Sonar.csv")
head(sonar)
summary(sonar)
sonar = sonar %>%
  mutate(class_new = ifelse(Class == "R", "1", "0"))

sonar = sonar %>% select(-Class)

#data.frame(sonar)
boxplot(V1 ~ class_new, sonar)
boxplot(V30 ~ class_new, sonar)
sonar$class_new <- as.numeric(sonar$class_new)
class(sonar$class_new)


##Logistic regression
sonar.glm <- glm(class_new ~  ., sonar, family = "binomial", maxit = 100)
summary(sonar.glm)
exp(coef(sonar.glm))

##Logistic Regression in MLR3
class(sonar$class_new)
sonar = sonar %>%
  mutate(class_new = as.factor(class_new))
class(sonar$class_new)

task_class = TaskClassif$new(id = "Class", backend = sonar, target = "class_new")
print(task_class)

##Training Set
set.seed(1234)
train_set = sample(task_class$nrow, 0.8 * task_class$ncol)
test_set = setdiff(seq_len(task_class$nrow), train_set)

learner = lrn("classif.log_reg", maxit = 100)
learner

learner$train(task_class, row_ids = train_set)
learner$model

pred_train = learner$predict(task_class, row_ids = train_set)
pred_train

pred_train$confusion

learner$predict_type <- "prob"
pred_train = learner$predict(task_class, row_ids = train_set)
pred_train

measure = msr("classif.auc")
pred_train$score(measure)

autoplot(pred_train, type = 'roc')

##Resampling
resampling = rsmp("cv", folds = 5)
resampling$instantiate(task_class)
rr = resample(task_class, learner, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

##Naive Bayes
learner = lrn("classif.naive_bayes", predict_type = "prob")
learner
learner$train(task_class, row_ids = train_set)
learner$model

rr = resample(task_class, learner, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

##SVM
sonar = sonar %>%
  mutate(class_new = ifelse(Class == "M", "1", "0"))
class(sonar$class_new)
sonar = sonar %>%
  mutate(class_new = as.factor(class_new))

class(sonar$class_new)
set.seed(1234)



train_set = sample(task_class$nrow, 0.8 * task_class$ncol)

task_class = TaskClassif$new(id = "class", backend = sonar, target = "class_new")

learner = lrn("classif.svm", kernel = "linear", scale = TRUE, predict_type = "prob")
learner

learner$train(task_class, row_ids = train_set)
learner$model

##Linear Kernel
rr = resample(task_class, learner, resampling, store_models = FALSE)

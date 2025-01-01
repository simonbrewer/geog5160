## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(png)
library(grid)


## ----echo=FALSE----------------------------------------------------------------------------------
set.seed(1234)


## ----message=FALSE-------------------------------------------------------------------------------
library(dplyr)
library(neuralnet)
library(NeuralNetTools)


## ------------------------------------------------------------------------------------------------
# Read the Data
data = read.csv("../datafiles/cereals.csv")
head(data)


## ------------------------------------------------------------------------------------------------
mydat = data %>% 
  select(rating, calories, protein, fat, sodium, fiber)


## ------------------------------------------------------------------------------------------------
library(GGally)
ggcorr(mydat, 
               label = TRUE, nbreaks = 6,
               palette = "PuOr")


## ------------------------------------------------------------------------------------------------
train_set = sample(nrow(mydat), 0.8 * nrow(mydat))
test_set = setdiff(seq_len(nrow(mydat)), train_set)
train = mydat[train_set, ]
test = mydat[test_set, ]


## ------------------------------------------------------------------------------------------------
cereal.lm <- lm(rating ~ ., train)
summary(cereal.lm)


## ------------------------------------------------------------------------------------------------
pred.test = predict(cereal.lm, test)
sqrt(mean((pred.test - test$rating)^2))


## ------------------------------------------------------------------------------------------------
maxs = apply(mydat, 2, max)
mins = apply(mydat, 2, min)
scaled = as.data.frame(scale(mydat, center = mins, scale = maxs - mins))


## ------------------------------------------------------------------------------------------------
train_ <- scaled[train_set,]
test_ <- scaled[test_set,]


## ------------------------------------------------------------------------------------------------
# fit neural network
cereal.nn = neuralnet(rating ~ calories + protein + fat + sodium + fiber, 
                      data = train_, hidden = 3 , linear.output = TRUE)


## ----results='hide'------------------------------------------------------------------------------
summary(cereal.nn)


## ------------------------------------------------------------------------------------------------
cereal.nn$result.matrix


## ------------------------------------------------------------------------------------------------
# plot neural network
plot(cereal.nn)


## ------------------------------------------------------------------------------------------------
plotnet(cereal.nn)


## ------------------------------------------------------------------------------------------------
pred.test = neuralnet::compute(cereal.nn, test_)
pred.test = (pred.test$net.result * (maxs[1] - mins[1]) + mins[1])
sqrt(mean((pred.test - test$rating)^2))


## ----warning=FALSE, results='hide', message=FALSE, eval=FALSE------------------------------------
## #import 'gar.fun' from beckmw's Github - this is Garson's algorithm
## devtools::source_gist('6206737')
## #use the function on the model created above
## gar.fun('rating',cereal.nn)


## ------------------------------------------------------------------------------------------------
k = 5
outs <- NULL

foldID = sample(seq(1,k), nrow(mydat), replace = TRUE)
for(i in 1:k)
{
  
  traincv <- mydat[(foldID != i), ]
  testcv <- mydat[(foldID == i), ]
  train_cv <- scaled[(foldID != i), ]
  test_cv <- scaled[(foldID == i), ]
  
  cereal.nn.cv = neuralnet(rating ~ calories + protein + fat + sodium + fiber,
                           data = train_cv, hidden = 3,
                           linear.output = TRUE)
  
  pred.test = neuralnet::compute(cereal.nn, test_cv)
  pred.test = (pred.test$net.result * (maxs[1] - mins[1]) + mins[1])
  
  outs[i] = sqrt(mean((pred.test - testcv$rating)^2))
}

print(mean(outs))


## ----eval=TRUE-----------------------------------------------------------------------------------
# fit neural network
cereal.nn = neuralnet(rating ~ calories + protein + fat + sodium + fiber, 
                      data = train_, hidden = c(6, 3), 
                      linear.output = TRUE)
plotnet(cereal.nn)


## ----echo=FALSE, results='hide'------------------------------------------------------------------
k = 5
outs <- NULL

foldID = sample(seq(1,k), nrow(mydat), replace = TRUE)
for(i in 1:k)
{
  
  traincv <- mydat[(foldID != i), ]
  testcv <- mydat[(foldID == i), ]
  train_cv <- scaled[(foldID != i), ]
  test_cv <- scaled[(foldID == i), ]
  
  cereal.nn.cv = neuralnet(rating ~ calories + protein + fat + sodium + fiber,
                           data = train_cv, hidden = 3,
                           linear.output = TRUE)
  
  pred.test = neuralnet::compute(cereal.nn, test_cv)
  pred.test = (pred.test$net.result * (maxs[1] - mins[1]) + mins[1])
  
  outs[i] = sqrt(mean((pred.test - testcv$rating)^2))
}

print(mean(outs))


## ------------------------------------------------------------------------------------------------
credit_data <- read.csv("../datafiles/credit_data.csv")
str(credit_data)


## ------------------------------------------------------------------------------------------------
credit_data <- credit_data %>% mutate_if(is.character,as.factor)


## ------------------------------------------------------------------------------------------------
summary(credit_data)


## ------------------------------------------------------------------------------------------------
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
credit_task <- TaskClassif$new("credit", backend = credit_data,
                               target = "Status")


## ----results='hide'------------------------------------------------------------------------------
library(mlr3pipelines)
mlr_pipeops


## ------------------------------------------------------------------------------------------------
impute_cat <- po("imputemode",
             affect_columns = selector_type("factor"))
encode <- po("encode", method = "treatment",
             affect_columns = selector_type("factor"))


## ------------------------------------------------------------------------------------------------
cat <- impute_cat %>>%
  encode 


## ------------------------------------------------------------------------------------------------
cat$train(credit_task)[[1]]$data()


## ----eval=FALSE----------------------------------------------------------------------------------
## summary(cat$train(credit_task)[[1]]$data())


## ------------------------------------------------------------------------------------------------
impute_num <- po("imputemedian", 
            affect_columns = selector_type("integer"))
scale <- po("scalerange", param_vals = list(lower = 0, upper = 1), 
            affect_columns = selector_type("integer"))
num <- impute_num %>>%
  scale 


## ------------------------------------------------------------------------------------------------
num$train(credit_task)[[1]]$data()


## ------------------------------------------------------------------------------------------------
graph <- cat %>>%
  num


## ------------------------------------------------------------------------------------------------
graph$train(credit_task)[[1]]$data()
summary(graph$train(credit_task)[[1]]$data())


## ----eval=FALSE----------------------------------------------------------------------------------
## lrn_nn <- lrn("classif.nnet", size = 10, maxit = 500)


## ----echo=FALSE----------------------------------------------------------------------------------
lrn_nn <- lrn("classif.nnet", size = 10, maxit = 500, trace = FALSE)


## ------------------------------------------------------------------------------------------------
graph <- cat %>>%
  num %>>%
  lrn_nn


## ------------------------------------------------------------------------------------------------
plot(graph)


## ----message=FALSE-------------------------------------------------------------------------------
graph$train(credit_task)


## ------------------------------------------------------------------------------------------------
glrn = GraphLearner$new(graph)


## ----echo=FALSE----------------------------------------------------------------------------------
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")


## ------------------------------------------------------------------------------------------------
resampling = rsmp("cv", folds = 5)
resampling$instantiate(credit_task)
measure = msr("classif.auc")
rr = resample(credit_task, glrn, resampling, store_models = TRUE)


## ------------------------------------------------------------------------------------------------
rr$score(measure)
rr$aggregate(measure)


## ------------------------------------------------------------------------------------------------
library(paradox)
tune_ps = ParamSet$new(list(
  ParamInt$new("classif.nnet.size", lower = 2, upper = 20)
))
tune_ps


## ------------------------------------------------------------------------------------------------
evals = trm("evals", n_evals = 20)
tuner = tnr("random_search")


## ------------------------------------------------------------------------------------------------
at_nn = AutoTuner$new(learner = glrn, 
                      resampling = rsmp("holdout"),
                      measure = measure, 
                      search_space = tune_ps,
                      terminator = evals,
                      tuner = tuner)


## ----message=FALSE-------------------------------------------------------------------------------
at_nn$train(credit_task)


## ------------------------------------------------------------------------------------------------
at_nn$learner
at_nn$tuning_result


## ------------------------------------------------------------------------------------------------
new_credit <- data.frame(Seniority = 8, 
                         Home = "rent",
                         Time = 36,
                         Age = 26,
                         Marital = "single",
                         Records = "no",
                         Job = "fixed",
                         Expenses = 50,
                         Income = 100,
                         Assets = 0,
                         Debt = 10,
                         Amount = 100,
                         Price = 125)

at_nn$learner$predict_newdata(newdata = new_credit)


## ------------------------------------------------------------------------------------------------
library(mlr3filters)
mlr_filters


## ------------------------------------------------------------------------------------------------
filter_mim <-   po("filter", flt("mim"), filter.nfeat = 3)


## ------------------------------------------------------------------------------------------------
graph <- cat %>>%
  num %>>%
  filter_mim 


## ------------------------------------------------------------------------------------------------
graph$train(credit_task)[[1]]$data()


## ------------------------------------------------------------------------------------------------
graph <- cat %>>%
  num %>>%
  filter_mim %>>%
  lrn_nn

glrn = GraphLearner$new(graph)


## ------------------------------------------------------------------------------------------------
tune_ps = ParamSet$new(list(
  ParamInt$new("mim.filter.nfeat", lower = 2, upper = 20),
  ParamInt$new("classif.nnet.size", lower = 2, upper = 20)
))
tune_ps


## ------------------------------------------------------------------------------------------------
evals = trm("evals", n_evals = 50)
tuner = tnr("random_search")
at_nn = AutoTuner$new(learner = glrn, 
                      resampling = rsmp("holdout"),
                      measure = measure, 
                      search_space = tune_ps,
                      terminator = evals,
                      tuner = tuner)


## ------------------------------------------------------------------------------------------------
at_nn$train(credit_task)


## ------------------------------------------------------------------------------------------------
at_nn$learner
at_nn$tuning_result


## ------------------------------------------------------------------------------------------------
pca <- po("pca")
filter <- po("filter", filter = mlr3filters::flt("variance"), filter.frac = 0.5)
graph <- cat %>>%
  num %>>%
  pca %>>% 
  filter


## ------------------------------------------------------------------------------------------------
graph$train(credit_task)[[1]]$data()


## ------------------------------------------------------------------------------------------------
graph <- cat %>>%
  num %>>%
  pca %>>% 
  filter %>>%
  lrn_nn

plot(graph)


## ------------------------------------------------------------------------------------------------
glrn = GraphLearner$new(graph)


## ------------------------------------------------------------------------------------------------
tune_ps = ParamSet$new(list(
  ParamDbl$new("variance.filter.frac", lower = 0.05, upper = 0.95),
  
  ParamInt$new("classif.nnet.size", lower = 2, upper = 20)
))
tune_ps


## ------------------------------------------------------------------------------------------------
evals = trm("evals", n_evals = 50)
tuner = tnr("random_search")


## ------------------------------------------------------------------------------------------------
at_nn = AutoTuner$new(learner = glrn, 
                      resampling = rsmp("holdout"),
                      measure = measure, 
                      search_space = tune_ps,
                      terminator = evals,
                      tuner = tuner)


## ------------------------------------------------------------------------------------------------
at_nn$train(credit_task)


## ------------------------------------------------------------------------------------------------
at_nn$learner
at_nn$tuning_result


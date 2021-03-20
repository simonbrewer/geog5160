#remotes::install_github("mlr-org/mlr3extralearners")
set.seed(1234)
library(dplyr)
library(mlr3)
library(mlr3learners)
#library(mlr3extralearners)
library(mlr3pipelines)
library(paradox)

credit_data <- read.csv("credit_data.csv")

credit_data <- credit_data %>% mutate_if(is.character,as.factor)

credit_task <- TaskClassif$new("credit", backend = credit_data,
                               target = "Status")

impute_cat <- po("imputemode")
encode <- po("encode", method = "treatment",
             affect_columns = selector_type("factor"))

cat <- impute_cat %>>%
  encode 

cat$train(credit_task)[[1]]$data()

impute_num <- po("imputemedian")
scale <- po("scalerange", param_vals = list(lower = 0, upper = 1), 
             affect_columns = selector_type("integer"))

num <- impute_num %>>%
  scale 

num$train(credit_task)[[1]]$data()

graph <- cat %>>%
  num

graph$train(credit_task)[[1]]$data()

graph$is_trained

lrn_nn <- lrn("classif.nnet", size = 10, maxit = 500)

graph <- cat %>>%
  num %>>%
  lrn_nn

graph$train(credit_task)

graph$predict(credit_task)

graph$plot()

resamp_hout = rsmp("holdout", ratio = 0.8)
resamp_hout$instantiate(credit_task)

rr = resample(credit_task, graph, resamp_hout)
rr$score(measure)
rr$aggregate(measure)

## For resampling we need to convert to a GraphLearner
glrn = GraphLearner$new(graph)

resampling = rsmp("cv", folds = 5)
resampling$instantiate(credit_task)

measure = msr("classif.auc")
rr = resample(credit_task, glrn, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

## Now its a graph learner we can predict for new data
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
glrn$predict_newdata(newdata = new_credit)

## Tuning with pipeline

##nnet parameter set
glrn$param_set
tune_ps = ParamSet$new(list(
  ParamInt$new("classif.nnet.size", lower = 2, upper = 20)
))
tune_ps

evals = trm("evals", n_evals = 20)

tuner = tnr("random_search")

at_nn = AutoTuner$new(learner = glrn, 
                      resampling = rsmp("holdout"),
                      measure = measure, 
                      search_space = tune_ps,
                      terminator = evals,
                      tuner = tuner)


at_nn$train(credit_task)

at_nn$learner

## PCA transform

pca <- po("pca")
filter <- po("filter", filter = mlr3filters::flt("variance"), filter.frac = 0.5)

graph <- cat %>>%
  num %>>%
  pca %>>% 
  filter %>>%
  lrn_nn

plot(graph)

## For resampling we need to convert to a GraphLearner
glrn = GraphLearner$new(graph)

resampling = rsmp("cv", folds = 5)
resampling$instantiate(credit_task)

measure = msr("classif.auc")
rr = resample(credit_task, glrn, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

## Tuning processing as well as algorithm!
## nnet parameter set
glrn$param_set
tune_ps = ParamSet$new(list(
  ParamDbl$new("variance.filter.frac", lower = 0.05, upper = 0.95),
  
  ParamInt$new("classif.nnet.size", lower = 2, upper = 20)
))
tune_ps

evals = trm("evals", n_evals = 50)

tuner = tnr("random_search")

at_nn = AutoTuner$new(learner = glrn, 
                      resampling = rsmp("holdout"),
                      measure = measure, 
                      search_space = tune_ps,
                      terminator = evals,
                      tuner = tuner)


at_nn$train(credit_task)

at_nn$learner
at_nn$tuning_result

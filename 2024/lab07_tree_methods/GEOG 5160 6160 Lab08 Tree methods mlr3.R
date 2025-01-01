## ----setup, include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(png)
library(grid)


## ----results='hide'-----------------------------------------------------------------
list.files()


## ----message=FALSE, warning=FALSE---------------------------------------------------
library(rgdal)
library(dismo)
library(raster)
library(RColorBrewer)
library(mlr3)
library(mlr3learners)
library(mlr3viz)

set.seed(1234)

## -----------------------------------------------------------------------------------
pe_pres = read.csv("./Pinus_edulis.csv")
head(pe_pres)
str(pe_pres)
class(pe_pres)


## -----------------------------------------------------------------------------------
coordinates(pe_pres) <- ~longitude+latitude
class(pe_pres)

## ----fig.keep='none'----------------------------------------------------------------
plot(pe_pres)


## -----------------------------------------------------------------------------------
borders = readOGR("./ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

## -----------------------------------------------------------------------------------
plot(pe_pres, xlim=c(-120,-80), ylim=c(25,55), pch=21, bg="darkorange", axes=TRUE)
plot(borders, add=TRUE)


## ----eval=FALSE---------------------------------------------------------------------
## rattler=gbif('crotalus','horridus')


## ----echo=FALSE---------------------------------------------------------------------
rattler = read.csv("rattler.csv")


## -----------------------------------------------------------------------------------
rattler = subset(rattler, !is.na(rattler$lon))
coordinates(rattler) <- ~lon+lat


## -----------------------------------------------------------------------------------
plot(rattler, pch=21, bg="green", axes=TRUE)
plot(borders, add=TRUE)


## ----eval=FALSE---------------------------------------------------------------------
## current.env=getData("worldclim", var="bio", res=2.5)

## ----echo=FALSE---------------------------------------------------------------------
load("current.env.RData")


## -----------------------------------------------------------------------------------
plot(raster(current.env,1))


## -----------------------------------------------------------------------------------
my.pal = brewer.pal(9,"YlOrRd")
plot(raster(current.env,1), col=my.pal)


## -----------------------------------------------------------------------------------
myext = extent(c(-130,-100,30,50))
current.env = crop(current.env, myext)


## -----------------------------------------------------------------------------------
plot(raster(current.env,1), col=my.pal)
plot(pe_pres, add=TRUE, pch=21, bg="olivedrab")


## -----------------------------------------------------------------------------------
summary(current.env)

## -----------------------------------------------------------------------------------
current.env = stack(subset(current.env, seq(1,11)) / 10, subset(current.env, seq(12,19)))
# summary(current.env)

## -----------------------------------------------------------------------------------
elev = raster("worldclim_elev_2.5.nc")
plot(elev)

## -----------------------------------------------------------------------------------
current.env = stack(current.env, elev)

## -----------------------------------------------------------------------------------
# Randomly sample points (same number as our observed points)
pe_abs <- randomPoints(mask = raster(current.env, 1),  
                       n = nrow(pe_pres), p=pe_pres)
pe_abs = as.data.frame(pe_abs)
coordinates(pe_abs) <- ~x+y

## -----------------------------------------------------------------------------------
plot(elev)
points(pe_abs, pch=16, cex=0.75)
plot(pe_pres, add=TRUE, pch=21, bg="darkorange")

## Get data
pe_pres_crds = coordinates(pe_pres)
pe_abs_crds = coordinates(pe_abs)

pe_env = extract(current.env, rbind(pe_pres_crds, pe_abs_crds))

pe_pa = as.factor(c(rep(1, nrow(pe_pres_crds)), 
                    rep(0, nrow(pe_abs_crds))))

pe_df = data.frame(rbind(pe_pres_crds, pe_abs_crds),
                   pa = pe_pa, pe_env)
rownames(pe_df) <- 1:598
## -----------------------------------------------------------------------------------
## RPART tree

task_pe = TaskClassif$new(id = "pe", backend = pe_df, 
                          target = "pa")

## Check the task details
task_pe$col_roles
task_pe$col_roles$feature = setdiff(task_pe$col_roles$feature,
                                    c("longitude", "latitude", "alt"))
task_pe$feature_names


train_set = sample(task_pe$nrow, 0.8 * task_pe$nrow)
test_set = setdiff(seq_len(task_pe$nrow), train_set)

lrn_ct = lrn("classif.rpart", predict_type = "prob")
lrn_ct$param_set

lrn_ct$train(task_pe, row_ids = train_set)

measure = msr("classif.auc")
pred_train = lrn_ct$predict(task_pe, row_ids = train_set)
pred_train
pred_train$score(measure)
autoplot(pred_train, type = 'roc')

pred_test = lrn_ct$predict(task_pe, row_ids = test_set)
pred_test$score(measure)
autoplot(pred_test, type = 'roc')

resampling = rsmp("cv", folds = 5)
resampling$instantiate(task_pe)
rr = resample(task_pe, lrn_ct, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

autoplot(rr, type = 'roc')

library(rpart.plot)
rr$data$learner
rr$data$learner[[1]]$model
prp(rr$data$learner[[1]]$model, roundint = FALSE)

## Tuning
library(mlr3tuning)
library(paradox)

## Define resampling strategy
resampling = rsmp("cv", folds = 5)

## Build parameter set
tune_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1, upper = 12)
))
tune_ps

## Define terminator
evals = term("evals", n_evals = 50)

## Build tuner
tuner = tnr("grid_search", resolution = 10)

## Set up instance
instance = TuningInstance$new(
  task = task_pe,
  learner = lrn_ct,
  resampling = resampling,
  measures = measure,
  param_set = tune_ps,
  terminator = evals
)
print(instance)

result = tuner$tune(instance)

instance$archive(unnest = "params")[, c("cp", "minsplit", "classif.auc")]

instance$result$params

## Set learner parameters to best set
lrn_ct$param_set$values = instance$result$params
## Learn task
lrn_ct$train(task_pe, row_ids = train_set)

pred_test = lrn_ct$predict(task_pe, row_ids = test_set)
pred_test$score(measure)
autoplot(pred_test, type = 'roc')

## -----------------------------------------------------------------------------------
## Random forest

lrn_rf = lrn("classif.ranger", predict_type = "prob")
lrn_rf$param_set

resampling$instantiate(task_pe)
rr = resample(task_pe, lrn_rf, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

autoplot(rr, type = 'roc')

## Build parameter set
tune_ps = ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = 8),
  ParamInt$new("num.trees", lower = 100, upper = 1000)
))
tune_ps

## Define terminator
evals = term("evals", n_evals = 50)

## Build tuner
tuner = tnr("grid_search", resolution = 10)

## Set up instance
instance = TuningInstance$new(
  task = task_pe,
  learner = lrn_rf,
  resampling = resampling,
  measures = measure,
  param_set = tune_ps,
  terminator = evals
)
print(instance)

result = tuner$tune(instance)

instance$archive(unnest = "params")[, c("mtry", "num.trees", "classif.auc")]

instance$result$params

## Set learner parameters to best set
lrn_rf$param_set$values = instance$result$params
lrn_rf$param_set$values = list(importance = "impurity")
## Learn task
lrn_rf$train(task_pe, row_ids = train_set)

pred_test = lrn_rf$predict(task_pe, row_ids = test_set)
pred_test$score(measure)
autoplot(pred_test, type = 'roc')

ranger::importance(lrn_rf$model)
rf_vi = lrn_rf$model$variable.importance

## ADD PLOT OF VI HERE

## -----------------------------------------------------------------------------------
## Boosted regression tree

lrn_brt = lrn("classif.xgboost", predict_type = "prob", subsample = 0.5)

resampling$instantiate(task_pe)
rr = resample(task_pe, lrn_brt, resampling, store_models = TRUE)
rr$score(measure)
rr$aggregate(measure)

autoplot(rr, type = 'roc')

## Build parameter set
lrn_brt$param_set
tune_ps = ParamSet$new(list(
  ParamDbl$new("eta", lower = 0.001, upper = 0.1),
  ParamInt$new("max_depth", lower = 1, upper = 6), ## Raise upper limit
  ParamInt$new("nrounds", lower = 100, upper = 1000)
))
tune_ps

## Define terminator
evals = term("evals", n_evals = 50)

## Build tuner
tuner = tnr("grid_search", resolution = 10)

## Set up instance
instance = TuningInstance$new(
  task = task_pe,
  learner = lrn_brt,
  resampling = resampling,
  measures = measure,
  param_set = tune_ps,
  terminator = evals
)
print(instance)

result = tuner$tune(instance)

instance$archive(unnest = "params")[, c("eta", "max_depth", "nrounds", "classif.auc")]

instance$result$params

## Set learner parameters to best set
lrn_brt$param_set$values = instance$result$params

## Learn task
lrn_brt$train(task_pe, row_ids = train_set)

pred_test = lrn_brt$predict(task_pe, row_ids = test_set)
pred_test$score(measure)
autoplot(pred_test, type = 'roc')

## -----------------------------------------------------------------------------------
## Modern predictions
current.env.df = as.data.frame(getValues(current.env))
current.env.df = current.env.df[,-20]
pe.curr.pred = lrn_brt$predict_newdata(current.env.df)
# xx$data$prob[which(is.na(x$bio1)),] <- NA

## Raster...
pe.curr.pred.prob.r = setValues(raster(current.env, 1), pe.curr.pred$prob[,2])
plot(pe.curr.pred.prob.r)
points(pe_pres, pch=16, cex=0.5)

pe.curr.pred.pa.r = setValues(raster(current.env, 1), pe.curr.pred$response)
plot(pe.curr.pred.pa.r)
points(pe_pres, pch=16, cex=0.5)

## -----------------------------------------------------------------------------------
## Future predictions
# future.env=getData('CMIP5', var='bio', res=2.5, rcp=85, model='HE', year=70)
load("future.env.RData")

future.env = crop(future.env, myext)

future.env = stack(subset(future.env, seq(1,11)) / 10, 
                   subset(future.env, seq(12,19)))

rcp85.env.df = as.data.frame(getValues(future.env))
rcp85.env.df = rcp85.env.df[,-20]
pe.rcp85.pred = lrn_brt$predict_newdata(rcp85.env.df) ## Doesn't work
names(rcp85.env.df) <- names(current.env.df)
pe.rcp85.pred = lrn_brt$predict_newdata(rcp85.env.df)

# xx$data$prob[which(is.na(x$bio1)),] <- NA

## Raster...
pe.rcp85.pred.prob.r = setValues(raster(future.env, 1), 
                                 pe.rcp85.pred$prob[,2])
plot(pe.rcp85.pred.prob.r)
points(pe_pres, pch=16, cex=0.5)
plot(borders, add=TRUE)

pe.rcp85.pred.pa.r = setValues(raster(future.env, 1), 
                               pe.rcp85.pred$response)
plot(pe.rcp85.pred.pa.r)
points(pe_pres, pch=16, cex=0.5)
plot(borders, add=TRUE)


my.pal = brewer.pal(3,'PRGn')
plot(pe.rcp85.pred.pa.r - pe.curr.pred.pa.r, col=my.pal)
plot(borders, add=TRUE)

stop()

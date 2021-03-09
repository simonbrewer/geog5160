library(dplyr)
library(RColorBrewer)
library(rgdal)
library(raster)
library(dismo)
library(tree)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlr3tuning)
library(vip)
library(pdp)


pe = read.csv("./Pinus_edulis.csv")
coordinates(pe) <- ~longitude+latitude
borders = readOGR("./borders/ne_50m_admin_0_countries.shp")
plot(pe, xlim=c(-120,-80), ylim=c(25,55), pch=21, bg="darkorange", axes=TRUE)
plot(borders, add=TRUE)
load("current.env.RData")
load("future.env.RData")
my.pal = brewer.pal(9,"YlOrRd")
plot(raster(current.env,1))
plot(pe, add=TRUE, pch=21, bg="olivedrab")

x = circles(pe, d=100000, lonlat=T)
absence = spsample(x@polygons, type='random', n=nrow(pe))


pe.crds = rbind(coordinates(pe), coordinates(absence))
pe.env = raster::extract(current.env, pe.crds)
pe.pa = c(rep(1, nrow(pe)),
          rep(0, nrow(pe)))

pe.df = data.frame(pe.crds, pa = pe.pa, pe.env)
pe.df$pa <- as.factor(pe.df$pa)
row.names(pe.df) <- 1:598
pe.df$pa2 = as.factor(sample(c(0,1), nrow(pe.df), replace=TRUE))
task_pe = TaskClassif$new(id = "pe", backend = pe.df, 
                          target = "pa2")
task_pe$col_roles

task_pe$col_roles$feature = setdiff(task_pe$col_roles$feature,
                                    c("longitude", "latitude"))
task_pe$feature_names


measure = msr("classif.auc")
lrn_ct = lrn("classif.rpart", predict_type = "prob")

resamp_hout = rsmp("holdout", ratio = 0.8)
resamp_hout$instantiate(task_pe)
rr = resample(task_pe, lrn_ct, resamp_hout, store_models = TRUE)
rr$aggregate(measure)


# autotuner has an error
library(paradox)
lrn_ct$param_set

tune_ps = ParamSet$new(list(
  ParamDbl$new("cp", lower = 0.0001, upper = 0.1),
  ParamInt$new("minsplit", lower = 2, upper = 20)
))
tune_ps

evals = term("evals", n_evals = 50)
tuner = tnr("grid_search", resolution = 10)

at_ct = AutoTuner$new(learner = lrn_ct, 
                      resampling = rsmp("holdout"),
                      measures = measure, 
                      tune_ps = tune_ps,
                      terminator = evals,
                      tuner = tuner)

at_ct$train(task_pe)
at_ct$learner

# got error





resampling_inner = rsmp("holdout", ratio = 0.8)
resampling_outer = rsmp("cv", folds = 3)

at_ct = AutoTuner$new(learner = lrn_ct, 
                      resampling = resampling_inner,
                      measures = measure, 
                      tune_ps = tune_ps,
                      terminator = evals,
                      tuner = tuner)
rr_ct = resample(task = task_pe, learner = at_ct, 
                 resampling = resampling_outer, store_models = TRUE)

#got error





library(ranger)
lrn_rf = lrn("classif.ranger", 
             predict_type = "prob", 
             importance = "permutation")
rr = resample(task_pe, lrn_rf, resamp_hout, store_models = TRUE)
rr$aggregate(measure)
# no error, problem is not ranger,maybe because of autotune


tune_ps = ParamSet$new(list(
  ParamInt$new("mtry", lower = 1, upper = 8),
  ParamInt$new("num.trees", lower = 100, upper = 1000)
))


at_rf = AutoTuner$new(learner = lrn_rf, 
                      resampling = resampling_inner,
                      measures = measure, 
                      tune_ps = tune_ps,
                      terminator = evals,
                      tuner = tuner)

rr_rf = resample(task = task_pe, learner = at_rf, 
                 resampling = resampling_outer, store_models = TRUE)

# got error


lrn_brt = lrn("classif.xgboost", 
              predict_type = "prob", 
              subsample = 0.5)
rr = resample(task_pe, lrn_brt, resamp_hout, store_models = TRUE)

# no error





tune_ps = ParamSet$new(list(
  ParamDbl$new("eta", lower = 0.001, upper = 0.1),
  ParamInt$new("max_depth", lower = 1, upper = 6), ## Raise upper limit
  ParamInt$new("nrounds", lower = 100, upper = 1000)
))

at_brt = AutoTuner$new(learner = lrn_brt, 
                       resampling = resampling_inner,
                       measures = measure, 
                       tune_ps = tune_ps,
                       terminator = evals,
                       tuner = tuner)

rr_brt = resample(task = task_pe, learner = at_brt, 
                  resampling = resampling_outer, store_models = TRUE)

rr_brt$data$learner[[1]]$param_set$values
# no error until here

rr_brt$score(measure)
rr_brt$aggregate(measure)
vip(rr_brt$learners[[1]]$model$learner$model)











current.env.df = as.data.frame(getValues(current.env))

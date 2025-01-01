## ----echo=FALSE---------------------------------------------------------------------
set.seed(1234)


## ----message=FALSE------------------------------------------------------------------
library(dplyr)
library(neuralnet)
library(NeuralNetTools)


## -----------------------------------------------------------------------------------
# Read the Data
mydat = read.csv("banknote_data.csv")
head(mydat)


## -----------------------------------------------------------------------------------
train_set = sample(nrow(mydat), 0.8 * nrow(mydat))
test_set = setdiff(seq_len(nrow(mydat)), train_set)
train = mydat[train_set, ]
test = mydat[test_set, ]


## -----------------------------------------------------------------------------------
maxs = apply(mydat, 2, max)
mins = apply(mydat, 2, min)
scaled = as.data.frame(scale(mydat, center = mins, scale = maxs - mins))


## -----------------------------------------------------------------------------------
train_ <- scaled[train_set,]
test_ <- scaled[test_set,]


## -----------------------------------------------------------------------------------
# fit neural network
banknote.nn = neuralnet(forgery ~ ., 
                      data = train_, hidden = 3, linear.output = FALSE,
                      act.fct = "logistic", err.fct = "ce")


## -----------------------------------------------------------------------------------
plotnet(banknote.nn)


## -----------------------------------------------------------------------------------
pr.nn <- compute(banknote.nn,test_)
xtab = table(pr.nn$net.result>0.5, test_$forgery)
rownames(xtab) <- c("0","1")
print(xtab)


## -----------------------------------------------------------------------------------
caret::confusionMatrix(xtab)


## -----------------------------------------------------------------------------------
k = 5
outs <- NULL

# Crossvalidate, go!
foldID = sample(seq(1,k), nrow(iris), replace = TRUE)
for(i in 1:k)
{
  
  traincv <- mydat[(foldID != i), ]
  testcv <- mydat[(foldID == i), ]
  train_cv <- scaled[(foldID != i), ]
  test_cv <- scaled[(foldID == i), ]
  
  banknote.nn.cv = neuralnet(forgery ~ ., err.fct = "ce",
                         data = train_cv, hidden = c(6,6), 
                         linear.output = FALSE, act.fct = "logistic")
  
  pr.nn <- compute(banknote.nn,test_cv)
  xtab = table(pr.nn$net.result>0.5, test_cv$forgery)
  rownames(xtab) <- c("0","1")
  print(xtab)
  pfm <- caret::confusionMatrix(xtab)
  outs[i] <- pfm$overall[1]
}

mean(outs)


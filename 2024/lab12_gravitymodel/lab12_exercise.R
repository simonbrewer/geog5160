library(tidyverse)
library(sf)
library(tmap)
library(reshape2)
library(stplanr)
library(leaflet)
library(broom)
library(ggplot2)

load("aussie_travel.RData")

## Subset out by removing the intrao zone flows
mdatasub <- mdata[mdata$Orig_code!=mdata$Dest_code,]

#use the od2line function from RObin Lovelace's excellent stplanr package - remove all but the origin, destination and flow columns
mdatasub_skinny <- mdatasub[,c(2,4,5)]
travel_network <- od2line(flow = mdatasub_skinny, zones = AusSF)

#and set the line widths to some sensible value according to the flow
w <- mdatasub_skinny$Flow / max(mdatasub_skinny$Flow) * 10

#now plot it...
plot(st_geometry(AusSF))
plot(travel_network['Flow'], add = TRUE, lwd = w)

#now we can create pivot table to turn paired list into matrix (and compute the margins as well)
# OD Matrix
mdatasubmat <- dcast(mdatasub, Orig_code ~ Dest_code, sum, value.var = "Flow", margins=c("Orig_code", "Dest_code"))
mdatasubmat

## ----------------------------------------------------------------------------
## Dennett's model 1
#set up some variables to hold our parameter values in:
mu <- 1
alpha <- 1
beta <- -2
k <- 1
T2 <- sum(mdatasub$Flow)


vi1_mu <- mdatasub$vi1_origpop^mu
wj3_alpha <- mdatasub$wj3_destmedinc^alpha
dist_beta <- mdatasub$dist^beta
T1 <- vi1_mu*wj3_alpha*dist_beta
k <- T2/sum(T1)

#run the model and store all of the new flow estimates in a new column in the dataframe
mdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj3_alpha*dist_beta,0)
#check that the sum of these estimates makes sense
sum(mdatasub$unconstrainedEst1)
sum(mdatasub$Flow)

## ----------------------------------------------------------------------------
## Model 1 my way
#3 First the term $V_i^\mu$
mu <- 1
vi1_mu <- mdatasub$vi1_origpop^mu

## Next the term $W_j^\alpha$
alpha <- 1
wj2_alpha <- mdatasub$wj3_destmedinc^alpha

## Then the term $d_{ij}^\mu$
beta <- -2
dist_beta <- mdatasub$dist^beta

mdatasub$unconstrainedEst1 <- round(k*vi1_mu*wj2_alpha*dist_beta,0)

sum(mdatasub$unconstrainedEst1)
sum(mdatasub$Flow)

plot(mdatasub$Flow, mdatasub$unconstrainedEst1)
abline(0,1)

ggplot(mdatasub, aes(x = Flow, y = unconstrainedEst1)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

## RMSE
sqrt(mean((mdatasub$Flow - mdatasub$unconstrainedEst1)^2))

#run the unconstrained model
fit <- glm(Flow ~ log(vi1_origpop) + log(wj3_destmedinc) + log(dist), 
           # na.action = na.exclude, 
           family = poisson(link = "log"), data = mdatasub)


mdatasub <- mdatasub %>%
  mutate(lorigpop = log(vi1_origpop),
         ldestmedinc = log(wj3_destmedinc),
         ldist = log(dist))

fit <- glm(Flow ~ lorigpop + ldestmedinc + ldist, 
           # na.action = na.exclude, 
           family = poisson(link = "log"), data = mdatasub)

coef(fit)
mdatasub$unconstrainedEst2 <- predict(fit, type = "response")
## RMSE
sqrt(mean((mdatasub$Flow - mdatasub$unconstrainedEst2)^2))

ggplot(mdatasub, aes(x = Flow, y = unconstrainedEst2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

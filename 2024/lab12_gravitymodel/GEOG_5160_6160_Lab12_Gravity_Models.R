## ----setup, include=FALSE------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(1234)


## ----message=FALSE, warning=FALSE----------------------------------------------------------------
library(sf)
library(MASS)
library(reshape2)
library(maptools)
library(dplyr)
library(stplanr)
library(ggplot2)


## ------------------------------------------------------------------------------------------------
cdata = read.csv("../datafiles/LondonCommuting2001.csv")
head(cdata)


## ------------------------------------------------------------------------------------------------
nrow(cdata)


## ------------------------------------------------------------------------------------------------
LondonBNG = st_read("../datafiles/LondonBNG/London.shp")
plot(st_geometry(LondonBNG))


## ------------------------------------------------------------------------------------------------
dist <- st_distance(st_centroid(LondonBNG))
dist <- units::drop_units(dist)

dist[1:10,1:10]


## ------------------------------------------------------------------------------------------------
distPair <- melt(dist, varnames = c("Dest","Orig"), value.name = "dist")
head(distPair)


## ----results='hide'------------------------------------------------------------------------------
cdata$dist <- distPair$dist
head(cdata)


## ------------------------------------------------------------------------------------------------
cdata2 <- dplyr::select(cdata, OrigCodeNew, DestCodeNew, Total, everything())
cdata2$Orig <- as.factor(cdata2$Orig)
cdata2$Dest <- as.factor(cdata2$Dest)


## ------------------------------------------------------------------------------------------------
cdata2 <- cdata2[cdata2$OrigCode!=cdata2$DestCode,]


## ------------------------------------------------------------------------------------------------
travel_network <- od2line(flow = cdata2, zones = LondonBNG)
ldn_travel_network <- travel_network

## ------------------------------------------------------------------------------------------------
w <- cdata2$Total / max(cdata2$Total) * 10
plot(st_geometry(LondonBNG))
plot(st_geometry(travel_network), lwd = w, add=TRUE)


## ------------------------------------------------------------------------------------------------
cdata2mat <- dcast(cdata2, Orig ~ Dest, sum, 
                   value.var = "Total", margins=c("Orig", "Dest"))
cdata2mat[1:10,1:10]


## ------------------------------------------------------------------------------------------------
mu <- 1
vi1_mu <- cdata2$OrigPop^mu


## ------------------------------------------------------------------------------------------------
alpha <- 1
wj2_alpha <- cdata2$DestSal^alpha


## ------------------------------------------------------------------------------------------------
beta <- -2
dist_beta <- cdata2$dist^beta


## ------------------------------------------------------------------------------------------------
k = sum(cdata2$Total) / sum(vi1_mu * wj2_alpha * dist_beta)


## ------------------------------------------------------------------------------------------------
cdata2$unconstrainedEst1 <- round(k * vi1_mu * wj2_alpha * dist_beta, 0)


## ------------------------------------------------------------------------------------------------
sum(cdata2$Total)


## ------------------------------------------------------------------------------------------------
sum(cdata2$unconstrainedEst1)


## ------------------------------------------------------------------------------------------------
#turn it into a little matrix and have a look at your handy work
cdata2mat1 <- dcast(cdata2, Orig ~ Dest, sum, 
                    value.var = "unconstrainedEst1", margins=c("Orig", "Dest"))


## ------------------------------------------------------------------------------------------------
cdata2mat1[1:10,1:10]


## ------------------------------------------------------------------------------------------------
sqrt(mean((cdata2$Total-cdata2$unconstrainedEst1)^2))


## ------------------------------------------------------------------------------------------------
plot(cdata$dist, cdata$Total,
     main="London commuting flow data", xlab="Dist (dij)", ylab="Flow (Tij)")


## ------------------------------------------------------------------------------------------------
plot(log(cdata$dist), log(cdata$Total),
     main="London commuting flow data", 
     xlab="Dist (dij)", ylab="Flow (Tij)")


## ------------------------------------------------------------------------------------------------
uncosim <- glm(Total ~ log(OrigPop)+log(DestSal)+log(dist), 
               family = poisson(link = "log"), data = cdata2)


## ------------------------------------------------------------------------------------------------
summary(uncosim)


## ------------------------------------------------------------------------------------------------
cdata2$unconstrainedEst2 = round(fitted(uncosim), 0)
head(cdata2$unconstrainedEst2)


## ------------------------------------------------------------------------------------------------
## Model 1
sqrt(mean((cdata2$Total-cdata2$unconstrainedEst1)^2))
## Model 2
sqrt(mean((cdata2$Total-cdata2$unconstrainedEst2)^2))


## ------------------------------------------------------------------------------------------------
sum(cdata2$unconstrainedEst2)


## ------------------------------------------------------------------------------------------------
sum(cdata2$Total)


## ----results='hide'------------------------------------------------------------------------------
cdata2mat2 <- dcast(cdata2, Orig ~ Dest, sum, value.var = "unconstrainedEst2", margins=c("Orig", "Dest"))
cdata2mat2[1:10,1:10]


## ------------------------------------------------------------------------------------------------
totoutflowObs = cdata2mat[,"(all)"]
totoutflowPred = cdata2mat2[,"(all)"]
plot(totoutflowObs, totoutflowPred, log="xy",
     xlab="Observed outflow", ylab="Predicted outflow")
abline(0,1)


## ------------------------------------------------------------------------------------------------
totinflowObs = as.numeric(cdata2mat[cdata2mat$Orig=="(all)", -1])
totinflowPred = as.numeric(cdata2mat2[cdata2mat2$Orig=="(all)", -1])
plot(totinflowObs, totinflowPred, log="xy",
     xlab="Observed inflow", ylab="Predicted inflow")
abline(0,1)


## ------------------------------------------------------------------------------------------------
w <- cdata2$unconstrainedEst2 / max(cdata2$unconstrainedEst2) * 10
plot(st_geometry(LondonBNG))
plot(st_geometry(travel_network), lwd = w, add=TRUE)


## ------------------------------------------------------------------------------------------------
r <- cdata2$unconstrainedEst2 - cdata2$Total
w <- abs(r) / sum(abs(r)) * 250
err <- ifelse(r > 0, 2, 4)
plot(st_geometry(LondonBNG))
plot(st_geometry(travel_network), lwd = w, add=TRUE, col=err)


## ----message=FALSE, warning=FALSE----------------------------------------------------------------
ausdata <- read.csv("../datafiles/aussie_flow.csv")
ausdata <- ausdata[ausdata$Orig_code != ausdata$Dest_code,]


## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'------------------------------------
AusSF <- st_read("../datafiles/aus_gccsa/aus_gccsa.shp")
ausdatasub <- ausdata[,c(2,4,5)]
travel_network <- od2line(flow = ausdatasub, zones = AusSF)
#convert the flows to WGS84

w <- ausdatasub$Flow / max(ausdatasub$Flow) * 10
plot(st_geometry(AusSF))
plot(st_geometry(travel_network), lwd = w, add = TRUE)


## ----eval = FALSE--------------------------------------------------------------------------------
## mu <- 1
## vi1_mu <- ausdata$OrigPop^mu
## alpha <- 1
## wj2_alpha <- ausdata$DestSal^alpha
## beta <- -2
## dist_beta <- ausdata$dist^beta
## k <- sum(ausdata$Total) / sum(vi1_mu * wj2_alpha * dist_beta)


## ----results='hide'------------------------------------------------------------------------------
cdata2$Orig2 = relevel(cdata2$Orig, ref = "City of London")
prodSim <- glm(Total ~ Orig2 + log(DestSal) + log(dist) - 1, 
               family = poisson(link = "log"), data = cdata2)
summary(prodSim)


## ------------------------------------------------------------------------------------------------
modcoef = coef(prodSim)[-c(34, 35)]
LondonBNG$prodcoef = 1 / modcoef


## ------------------------------------------------------------------------------------------------
plot(LondonBNG["prodcoef"])


## ----results='hide'------------------------------------------------------------------------------
cdata2$prodsimFitted <- round(fitted(prodSim), 0)


## ----results='hide'------------------------------------------------------------------------------
cdata2mat3 <- dcast(cdata2, Orig ~ Dest, sum, value.var = "prodsimFitted", 
                    margins=c("Orig", "Dest"))
cdata2mat3


## ------------------------------------------------------------------------------------------------
totoutflowObs = cdata2mat[, "(all)"]
totoutflowPred = cdata2mat3[, "(all)"]
plot(totoutflowObs, totoutflowPred, log="xy",
     xlab = "Observed outflow", ylab = "Predicted outflow")
abline(0, 1)


## ------------------------------------------------------------------------------------------------
sqrt(mean((cdata2$Total - cdata2$prodsimFitted)^2))

## ------------------------------------------------------------------------------------------------
## Attempt at prediction

cdata_pred <- cdata2
cdata_pred$lOrigPop <- log(cdata2$OrigPop)
cdata_pred$lDestSal <- log(cdata2$DestSal)
cdata_pred$ldist <- log(cdata2$dist)

prod_lm <- glm(Total ~ Orig2 + lDestSal + ldist - 1, 
               family = poisson(link = "log"), data = cdata_pred)
summary(prod_lm)

cdata_pred2 <- cdata_pred
refsal <- cdata_pred2$DestSal[cdata_pred2$Dest == "Bexley"][1] * 2
cdata_pred2$lDestSal[cdata_pred2$Dest == "Bexley"] <- log(refsal)

prod_lm2 <- glm(Total ~ Orig2 + lDestSal + ldist - 1, 
               family = poisson(link = "log"), data = cdata_pred2)
summary(prod_lm2)

plot(fitted(prod_lm), fitted(prod_lm2))

r <- fitted(prod_lm2) - fitted(prod_lm)
w <- abs(r) / sum(abs(r)) * 250
err <- ifelse(r > 0, 4, 2)
plot(st_geometry(LondonBNG))
plot(st_geometry(ldn_travel_network), col = err, add=TRUE)

ldn_travel_network$r <- r
ldn_travel_network$w <- w
ldn_travel_network$err <- err

bex_travel_network <- ldn_travel_network %>%
  filter(Dest == "Bexley")
plot(st_geometry(LondonBNG))
plot(st_geometry(bex_travel_network), lwd = sqrt(bex_travel_network$w)*2, col = bex_travel_network$err, add=TRUE)

bex_travel_network <- ldn_travel_network %>%
  filter(Orig == "Greenwich")
plot(st_geometry(LondonBNG))
plot(st_geometry(bex_travel_network), lwd = sqrt(bex_travel_network$w)*2, col = bex_travel_network$err, add=TRUE)

modcoef = coef(prod_lm)[-c(34, 35)]
modcoef2 = coef(prod_lm2)[-c(34, 35)]
LondonBNG$pred = 1 / modcoef
LondonBNG$pred2 = 1 / modcoef2

plot(LondonBNG["pred"])
plot(LondonBNG["pred2"])

library(tmap)
tm_shape(LondonBNG) +
  tm_fill("pred", n = 6)

LondonBNG2 <- rbind(LondonBNG, LondonBNG)
LondonBNG2$pred <- c(LondonBNG$pred, LondonBNG$pred2)
LondonBNG2$type <- rep(c("Original", "Doubled"), each = nrow(LondonBNG))

tm_shape(LondonBNG2) +
  tm_fill("pred") +
  tm_facets(by = "type")

stop()
## ----results='hide'------------------------------------------------------------------------------
cdata2$Dest2 = relevel(cdata2$Dest, ref = "City of London")
attrSim <- glm(Total ~ Dest2 + log(OrigPop) + log(dist) - 1, 
               family = poisson(link = "log"), data = cdata2)
summary(attrSim)


## ------------------------------------------------------------------------------------------------
modcoef = coef(attrSim)[-c(34, 35)]
LondonBNG$attrcoef = 1 / modcoef


## ------------------------------------------------------------------------------------------------
plot(LondonBNG["attrcoef"])


## ------------------------------------------------------------------------------------------------
cdata2$attrsimFitted <- round(fitted(attrSim),0)


## ----results='hide'------------------------------------------------------------------------------
cdata2mat4 <- dcast(cdata2, Orig ~ Dest, sum, value.var = "attrsimFitted", 
                    margins=c("Orig", "Dest"))
cdata2mat4


## ------------------------------------------------------------------------------------------------
totinflowObs = as.numeric(cdata2mat[cdata2mat$Orig=="(all)", -1])
totinflowPred = as.numeric(cdata2mat4[cdata2mat4$Orig=="(all)", -1])
plot(totinflowObs, totinflowPred, log = "xy",
     xlab = "Observed inflow", ylab = "Predicted inflow")
abline(0,1)


## ------------------------------------------------------------------------------------------------
sqrt(mean((cdata2$Total-cdata2$attrsimFitted)^2))


## ----results='hide'------------------------------------------------------------------------------
doubSim <- glm(Total ~ Orig + Dest + log(dist), 
               family = poisson(link = "log"), data = cdata2)
#let's have a look at it's summary...
summary(doubSim)


## ------------------------------------------------------------------------------------------------
cdata2$doubsimFitted <- round(fitted(doubSim),0)


## ----results='hide'------------------------------------------------------------------------------
cdata2mat5 <- dcast(cdata2, Orig ~ Dest, sum, value.var = "doubsimFitted", 
                    margins=c("Orig", "Dest"))
cdata2mat5[1:10,1:10]


## ------------------------------------------------------------------------------------------------
sqrt(mean((cdata2$Total-cdata2$doubsimFitted)^2))


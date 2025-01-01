## ----setup, include=FALSE------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(png)
library(grid)


## ----echo=FALSE----------------------------------------------------------------------------
set.seed(1234)


## ------------------------------------------------------------------------------------------
gap_df = read.csv("../datafiles/gapminder_1800_2018.csv")
head(gap_df)


## ------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)


## ----warning=FALSE-------------------------------------------------------------------------
names(gap_df)
h1 <- gap_df %>%
  gghistogram(x = "child_mortality")
h2 <- gap_df %>%
  gghistogram(x = "fertility")
h3 <- gap_df %>%
  gghistogram(x = "per_cap_co2")
h4 <- gap_df %>%
  gghistogram(x = "income")
h5 <- gap_df %>%
  gghistogram(x = "life_expectancy")
h6 <- gap_df %>%
  gghistogram(x = "population")
ggarrange(h1, h2, h3, h4, h5, h6)


## ------------------------------------------------------------------------------------------
gap_df_scale <- gap_df %>% 
  drop_na() %>%
  filter(per_cap_co2 > 0) %>%
  mutate(
    child_mortality = scale(log(child_mortality)),
    fertility = scale(log(fertility)),
    per_cap_co2 = scale(log(per_cap_co2)),
    income = scale(log(income)),
    life_expectancy = scale(log(life_expectancy)),
    population = scale(log(population))
  )


## ------------------------------------------------------------------------------------------
gap_df_scale2 <- gap_df_scale %>%
  filter(year == 2018)


## ------------------------------------------------------------------------------------------
names(gap_df_scale2)


## ------------------------------------------------------------------------------------------
gap_kmeans = kmeans(gap_df_scale2[,3:8], centers = 6)


## ------------------------------------------------------------------------------------------
gap_kmeans$size


## ------------------------------------------------------------------------------------------
gap_kmeans$cluster


## ------------------------------------------------------------------------------------------
gap_kmeans$centers


## ------------------------------------------------------------------------------------------
library(sf)
borders = st_read("../datafiles/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")


## ------------------------------------------------------------------------------------------
gap_df_scale2$kmeans = as.factor(gap_kmeans$cluster)


## ------------------------------------------------------------------------------------------
library(countrycode)
gap_df_scale2 <- gap_df_scale2 %>% 
  mutate(ISO3 = countrycode(country, "country.name", "iso3c"))


## ------------------------------------------------------------------------------------------
cluster_sf = merge(borders, gap_df_scale2, by.x = "ADM0_A3", by.y = "ISO3")


## ------------------------------------------------------------------------------------------
library(tmap)
tm_shape(cluster_sf) + 
  tm_fill("kmeans", palette = "Set2") +
  tm_legend(legend.position = c("left", "bottom"))


## ------------------------------------------------------------------------------------------
gap_d <- dist(gap_df_scale2[,3:8])


## ------------------------------------------------------------------------------------------
gap_hclust <- hclust(gap_d)


## ------------------------------------------------------------------------------------------
plot(gap_hclust)


## ------------------------------------------------------------------------------------------
gap_cluster <- cutree(gap_hclust, 6)
head(gap_cluster)


## ------------------------------------------------------------------------------------------
plot(gap_hclust)
rect.hclust(gap_hclust, k = 6)


## ------------------------------------------------------------------------------------------
gap_df_scale2$hclust <- as.factor(gap_cluster)
cluster_sf = merge(borders, gap_df_scale2, by.x = "ADM0_A3", by.y = "ISO3")

tm_shape(cluster_sf) + 
  tm_fill("hclust", palette = "Set2") +
  tm_legend(legend.position = c("left", "bottom"))


## ------------------------------------------------------------------------------------------
library(kohonen)


## ------------------------------------------------------------------------------------------
som_grid = somgrid(xdim = 30, ydim = 20, topo = "hexagonal")


## ------------------------------------------------------------------------------------------
gap_mat = as.matrix(gap_df_scale[,3:8])
gap_som = som(gap_mat, som_grid)


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "changes")


## ------------------------------------------------------------------------------------------
gap_som = som(gap_mat, som_grid, rlen = 1000)
plot(gap_som, type = "changes")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "dist")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "counts")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "code")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "property", property = gap_som$codes[[1]][,"population"],
     main = "Pop")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "property", property = gap_som$codes[[1]][,"life_expectancy"],
     main = "Life Exp.")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "property", property = gap_som$codes[[1]][,"income"],
     main = "Income")


## ------------------------------------------------------------------------------------------
plot(gap_som, type = "property", property = gap_som$codes[[1]][,"per_cap_co2"],
     main = "Per capita CO2")


## ------------------------------------------------------------------------------------------
dist_codes <- dist(gap_som$codes[[1]])
som_cluster <- cutree(hclust(dist_codes), 6)


## ------------------------------------------------------------------------------------------
# Define a color palette
mypal = brewer.pal(6, "Set2")
plot(gap_som, type="mapping", bgcol = mypal[som_cluster], 
     main = "Clusters", pch = '.') 
add.cluster.boundaries(gap_som, som_cluster)


## ------------------------------------------------------------------------------------------
dist_grid <- unit.distances(som_grid)


## ------------------------------------------------------------------------------------------
dist_adj <- as.matrix(dist_codes) * dist_grid


## ------------------------------------------------------------------------------------------
clust_adj = hclust(as.dist(dist_adj), 'ward.D2')
som_cluster_adj = cutree(clust_adj, 6)

plot(gap_som, type="mapping", bgcol = mypal[som_cluster_adj], 
     main = "Clusters", pch = '.') 
add.cluster.boundaries(gap_som, som_cluster_adj)


## ------------------------------------------------------------------------------------------
nodeByCountry <- gap_som$unit.classif
gap_df_scale$somclust <- as.factor(som_cluster_adj[nodeByCountry])


## ------------------------------------------------------------------------------------------
gap_df_clus <- merge(gap_df, gap_df_scale, by = c("country", "year"))


## ------------------------------------------------------------------------------------------
cluster.vals <- aggregate(gap_df_clus[,3:8], 
                         by = list(gap_df_scale$somclust), mean)


## ----echo=FALSE----------------------------------------------------------------------------
knitr::kable(cluster.vals)


## ------------------------------------------------------------------------------------------
country_id <- which(gap_df_scale$country == "United States")
country_nodes <- gap_som$unit.classif[country_id]
country_crds <- som_grid$pts[country_nodes, ]


## ------------------------------------------------------------------------------------------
plot(gap_som, type="mapping", bgcol = mypal[som_cluster_adj], 
     main = "Clusters", pch = '', keepMargins = TRUE) 
#add.cluster.boundaries(gap_som, som_cluster_adj)
lines(country_crds, lwd=2)

years <- ifelse(gap_df_scale$year[country_id] %% 20 == 0, as.character(gap_df_scale$year[country_id]), "")
text(jitter(country_crds), labels = years, cex = 0.75)
stop()

## ------------------------------------------------------------------------------------------
gap_df_scale <- gap_df_scale %>% 
  mutate(ISO3 = countrycode(country, "country.name", "iso3c"))


## ----eval = TRUE---------------------------------------------------------------------------
myyear <- 2000
tmp.df <- gap_df_scale %>%
  filter(year == myyear)
cluster_sf = merge(borders, tmp.df, by.x = "ADM0_A3", by.y = "ISO3",
                   all.x = TRUE)


## ----echo=FALSE, eval=FALSE----------------------------------------------------------------
## #load("cluster_sf.RData")


## ------------------------------------------------------------------------------------------
cluster_sf %>%
  filter(country == "United States")


## ------------------------------------------------------------------------------------------
tm_shape(cluster_sf) + 
  tm_fill("somclust", palette = "Set2") +
  tm_layout(main.title = paste("GAPMinder data",  myyear))


## ----eval=FALSE----------------------------------------------------------------------------
## housing.crds = housing %>%
##   select(longitude, latitude)


## ----eval=FALSE----------------------------------------------------------------------------
## housing$cluster = as.factor(som.cluster[housing.som$unit.classif])
## housing = cbind(housing, housing.crds)
## coordinates(housing) = ~longitude+latitude


som_df <- as.data.frame(gap_som$codes[[1]])

som_df$x <- gap_som$grid$pts[,1]
som_df$y <- gap_som$grid$pts[,2]
xbins <- gap_som$grid$xdim
ybins <- gap_som$grid$ydim

som_df %>%
  ggplot(aes(x = x, y = y, col = population)) +
  geom_hex(bins = c(xbins, ybins))

som_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_hex(binwidth = c(1,1))

som_df %>%
  ggplot(aes(x = x, y = y)) +
  stat_bin_hex(binwidth = c(1,1))

som_df %>%
  ggplot(aes(x = x, y = y, z = income)) + 
  stat_summary_hex(fun = function(x) mean(x, na.rm = TRUE), binwidth = c(1,1)) +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed()


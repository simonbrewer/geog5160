## All GapMinder data
library(tidyverse)
library(reshape2)
library(ggpubr)
library(RColorBrewer)

cmort <- read.csv("child_mortality_0_5_year_olds_dying_per_1000_born.csv")
fert <- read.csv("children_per_woman_total_fertility.csv")
co2 <- read.csv("co2_emissions_tonnes_per_person.csv")
inc <- read.csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")
lexp <- read.csv("life_expectancy_years.csv")
popn <- read.csv("population_total.csv")

cmort <- melt(cmort, variable.name = "year", value.name = "child_mortality")
fert <- melt(fert, variable.name = "year", value.name = "fertility")
co2 <- melt(co2, variable.name = "year", value.name = "per_cap_co2")
inc <- melt(inc, variable.name = "year", value.name = "income")
lexp <- melt(lexp, variable.name = "year", value.name = "life_expectancy")
popn <- melt(popn, variable.name = "year", value.name = "population")

all_gap <- merge(cmort, fert, by = c("country", "year"), all = TRUE)
all_gap <- merge(all_gap, co2, by = c("country", "year"), all = TRUE)
all_gap <- merge(all_gap, inc, by = c("country", "year"), all = TRUE)
all_gap <- merge(all_gap, lexp, by = c("country", "year"), all = TRUE)
all_gap <- merge(all_gap, popn, by = c("country", "year"), all = TRUE)

all_gap$year <- as.numeric(substr(all_gap$year, 2, 6))

all_gap <- all_gap %>% 
  drop_na() %>%
  filter(per_cap_co2 > 0) %>%
  mutate(
    child_mortality = scale(log(child_mortality)),
    fertility = log(fertility),
    per_cap_co2 = log(per_cap_co2),
    income = log(income),
    life_expectancy = log(life_expectancy),
    population = log(population)
  )

all_gap %>%
  filter(country == "Sudan") %>%
  ggline(x = "year", y = "child_mortality", numeric.x.axis = TRUE) 

all_gap %>%
  filter(country == "Sudan") %>%
  gghistogram(x = "child_mortality", numeric.x.axis = TRUE) 

library(kohonen)
som_grid = somgrid(xdim = 30, ydim = 20, topo = "hexagonal")
all_gap_mat = as.matrix(all_gap[,3:8])
gap_som = som(all_gap_mat, som_grid)

plot(gap_som, type = 'changes')
plot(gap_som)

plot(gap_som, type = "dist")
plot(gap_som, type = "counts")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"child_mortality"],
     main = "Child mortality")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"fertility"],
     main = "Fertility")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"per_cap_co2"],
     main = "per_cap_co2")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"income"],
     main = "income")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"life_expectancy"],
     main = "life_expectancy")

plot(gap_som, type = "property", property = gap_som$codes[[1]][,"population"],
     main = "population")

nclus = 6

som_cluster <- cutree(hclust(dist(gap_som$codes[[1]])), 6)

mypal = brewer.pal(nclus, "Set2")
# plot these results:
plot(gap_som, type="mapping", bgcol = mypal[som_cluster], 
     main = "Clusters", pch = '.') 
add.cluster.boundaries(gap_som, som_cluster)


dist_codes <- dist(gap_som$codes[[1]]) %>%
  as.matrix()
dist_grid <- unit.distances(som_grid)

dist_adj <- dist_codes * dist_grid


clust_adj = hclust(as.dist(dist_adj), 'ward.D2')
som_cluster_adj = cutree(clust_adj, 6)

plot(gap_som, type="mapping", bgcol = mypal[som_cluster_adj], 
     main = "Clusters", pch = '.') 
add.cluster.boundaries(gap_som, som_cluster_adj)

country_id <- which(all_gap$country == "United States")
country_nodes <- gap_som$unit.classif[country_id]
country_crds <- som_grid$pts[country_nodes, ]

plot(gap_som, type="mapping", bgcol = mypal[som_cluster_adj], 
     main = "Clusters", pch = '', keepMargins = TRUE) 
add.cluster.boundaries(gap_som, som_cluster_adj)
lines(country_crds, lwd=2)

years <- ifelse(all_gap$year[country_id] %% 20 == 0, as.character(all_gap$year[country_id]), "")
text(jitter(country_crds), labels = years, cex = 0.75)

library(sf)
library(tmap)
borders <- st_read("../../datafiles/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")

library(countrycode)
all_gap <- all_gap %>% 
  mutate(ISO3 = countrycode(country, "country.name", "iso3c"))

nodeByCountry = gap_som$unit.classif
all_gap$somclust  = as.factor(som_cluster_adj[nodeByCountry])


myyear <- 1900
tmp.df <- all_gap %>%
  filter(year == myyear)
cluster_sf = merge(borders, tmp.df, by.x = "ADM0_A3", by.y = "ISO3",
                   all = TRUE)

tm_shape(cluster_sf) + 
  tm_fill("somclust", palette = "Set2") +
  tm_layout(main.title = paste("GAPMinder data",  myyear))

library(animation)

saveGIF({
  all_years <- sort(unique(all_gap$year))
  for (i in 1:length(all_years)) {
    myyear <- all_years[i]
    tmp.df <- all_gap %>%
      filter(year == myyear)
    cluster_sf = merge(borders, tmp.df, by.x = "ADM0_A3", by.y = "ISO3",
                       all = TRUE)
    
    m1 <- tm_shape(cluster_sf) + 
      tm_fill("somclust", palette = "Set2") +
      tm_layout(main.title = paste("GAPMinder data",  myyear))
    print(m1)
    
  }
}, movie.name = "gapminder_1800_2018.gif", interval = 0.1, ani.width = 600
)


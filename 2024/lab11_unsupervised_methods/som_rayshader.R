## ----echo=FALSE----------------------------------------------------------------------------
set.seed(1234)


## ------------------------------------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(kohonen)
# library(rayshader)

## ------------------------------------------------------------------------------------------
gap_df = read.csv("../datafiles/gapminder_1800_2018.csv")
head(gap_df)


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
som_grid = somgrid(xdim = 30, ydim = 20, topo = "hexagonal")
gap_mat = as.matrix(gap_df_scale[,3:8])

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
som_df <- as.data.frame(gap_som$codes[[1]])

som_df$x <- gap_som$grid$pts[,1]
som_df$y <- gap_som$grid$pts[,2]
xbins <- gap_som$grid$xdim
ybins <- gap_som$grid$ydim

p <- som_df %>%
  ggplot(aes(x = x, y = y, z = income)) + 
  stat_summary_hex(fun = function(x) mean(x, na.rm = TRUE), binwidth = c(1,1)) +
  scale_fill_viridis_c(option = "magma") +
  coord_fixed() + 
  theme_minimal()

## ------------------------------------------------------------------------------------------
plot_gg(p)
render_camera(fov = 70, zoom = 0.4, theta = 35, phi = 55)

render_snapshot(clear = FALSE, filename = "som_rayshader1.png")
# render_snapshot(clear = FALSE, filename = "som_rayshader1.png",
#                 software_render = TRUE, width = 1000, height = 800)

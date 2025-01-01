## plot_slope

plot_slope <- function(a, b, c, 
                       min_elev = 0, max_elev = 500) {
  pelev <- seq(min_elev, max_elev)
  pveg = (1 - a) + (a / (1 + exp( (b - pelev ) / c) ) )
  
  plot(pelev, pveg, type = 'l',
       xlab = "Elevation", ylab = "Probability",
       ylim = c(0, 1))
  return(data.frame(elev = pelev, pveg = pveg))
  
}

## Lab values
a <- 0.99
b <- 240
c <- -5

## Defaults
pveg <- plot_slope(a, b, c)

## Shallow slope
a <- 0.99
b <- 240
c <- -50
pveg <- plot_slope(a, b, c)

## Move midpoint
a <- 0.99
b <- 400
c <- -5
pveg <- plot_slope(a, b, c)

## Move baseline
a <- 0.5
b <- 240
c <- -5
pveg <- plot_slope(a, b, c)

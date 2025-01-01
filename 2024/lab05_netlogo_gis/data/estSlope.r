require(raster)

elev = raster("local-elevation.asc")
proj4string(elev) <- CRS("+init=epsg:4326")
slope = terrain(elev, opt='slope')
plot(slope)
plot(slope*180/pi)
writeRaster(slope*180/pi, "local-slope.asc", overwrite=TRUE)

a=0.4
b=20
c=5
x = seq(0,45)
y = 0.6 + (a / (1+exp((b-x)/c)))
plot(x,y)

slope2 = slope*180/pi
plot(0.1 + (a/(1+exp(b-slope2)/c)))

## Possible vegetation function
a=0.8
b=230
c=-5.
x = seq(210,270)
y = (1-a) + (a / (1+exp((b-x)/c)))
plot(x,y)

# plot(0.1 + (a/(1+exp(b-elev)/c)))

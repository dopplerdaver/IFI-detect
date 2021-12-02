#----------------------------
#
# Name:    OPEN_RADIA_NC_F24.R
# 
# Created: 3.12.2019 dserke
#
#----------------------------

library(RColorBrewer)
library(useful)
library(NISTunits)
library(tidyverse)

YOR.cols             <- brewer.pal(9, "YlOrRd")

## read in nc file
radar.tilt         <- nc_open("/d1/serke/projects/case_studies/ICICLE_2018/data/nc_radia/KDVN/polar/20190227/015347.nc", write = FALSE, verbose = TRUE)
print(paste("The file has", radar.tilt$nvars, "variables"))
radar.tilt.var.num <- c(1:5)
for (i in 1:length(radar.tilt.var.num)) {
  radar.tilt.nam <- paste("v", radar.tilt.var.num[i], sep = "")
  assign(radar.tilt.nam, radar.tilt$var[[radar.tilt.var.num[i]]])
  print(paste("V", i, " has the name", v1$name))
}
FZDZ.1                      <- ncvar_get(radar.tilt, v1 )
#FZDZ.2                      <- ncvar_get(radar.tilt, v2 )
#FZDZ.3                      <- ncvar_get(radar.tilt, v3 )
#FZDZ.4                      <- ncvar_get(radar.tilt, v4 )
#FZDZ.5                      <- ncvar_get(radar.tilt, v5 )
#nc_close(radar.tilt)

nc.radia.filename    <- "/d1/serke/projects/case_studies/ICICLE_2018/data/nc_radia/KDVN/polar/20190227/015347.nc"
nc.radia.l1          <- raster(nc.radia.filename, varname = "FRZDRZ", level = 1)
nc.radia.l2          <- raster(nc.radia.filename, varname = "SLW",    level = 2)
nc.radia.l3          <- raster(nc.radia.filename, varname = "MIXPHA", level = 3)
nc.radia.l4          <- raster(nc.radia.filename, varname = "PLATES", level = 4)
nc.radia.l5          <- raster(nc.radia.filename, varname = "RADIA2", level = 5)

nc.radar.filename    <- "/d1/serke/projects/case_studies/ICICLE_2018/data/nc_nexrad/KDVN/polar/20190227/015347.nc"
nc.radar.l1          <- raster(nc.radar.filename, varname = "DBZ",    level = 1)
nc.radar.l2          <- raster(nc.radar.filename, varname = "VEL",    level = 2)
nc.radar.l3          <- raster(nc.radar.filename, varname = "WIDTH",  level = 3)
nc.radar.l4          <- raster(nc.radar.filename, varname = "ZDR",    level = 4)
nc.radar.l5          <- raster(nc.radar.filename, varname = "PHIDP",  level = 5)
nc.radar.l6          <- raster(nc.radar.filename, varname = "RHOHV",  level = 6)

# check and edit raster CRS
nc.radia.l1@crs
crs(nc.radia.l1)     <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
nc.radia.l1@crs

# convert polar to cartesian
# define nexrad azimuth values array, in degrees
nexrad.azim.deg.arry <- seq(179.75, 359.25, by = 0.50)
#nexrad.azim.deg.arry <- seq(-0.25, 359.25, by = 0.50)
# convert nexrad azim degrees to radians
nexrad.azim.rad.arry <- NISTdegTOradian(nexrad.azim.deg.arry)
nexrad.azim.rad.matr <- rep.row(nexrad.azim.rad.arry, 1832)
nexrad.azim.rad.matr <- nexrad.azim.rad.matr[1:458,]
# define nexrad range values array, in km
nexrad.range.km.arry <- seq(2, 460, by = 0.25)
nexrad.range.km.matr <- rep.col(nexrad.range.km.arry, dim(nexrad.azim.rad.matr)[2])
nexrad.range.km.matr <- nexrad.range.km.matr[1:458,]
# convert polar to cart coords
nexrad.coords.cart   <- pol2cart(as.vector(nexrad.range.km.matr), as.vector(nexrad.azim.rad.matr), degrees = FALSE)

#define constants
ninetydeg.rad <- 90 * pi / 180
R             <- 6378.1         # radius of earth, in km
# load KDVN lon/lat
radia.KDVN.x  <- NEXRAD.site.df$lon[38]
radia.KDVN.y  <- NEXRAD.site.df$lat[38]
lat.0.rad     <- NISTdegTOradian(radia.KDVN.y)
lon.0.rad     <- NISTdegTOradian(radia.KDVN.x)
lat.new.deg   <- rep(0, length(nexrad.coords.cart$y))
lon.new.deg   <- rep(0, length(nexrad.coords.cart$x))
y             <- rep(0, length(nexrad.coords.cart$y))
x             <- rep(0, length(nexrad.coords.cart$x))

# compute lat/lon for every x/y pair in radia.coords.xy and add array to data frame
for (kk in 1:length(nexrad.coords.cart$x)) {
  anglesxy        <- atan(nexrad.coords.cart$y[kk] / nexrad.coords.cart$x[kk])
  tot.dist.km     <- nexrad.coords.cart$x[kk]      / cos(anglesxy)         # in km from radar
  bearing.rad     <- anglesxy + ninetydeg.rad
  lat.new.rad     <- asin(sin(lat.0.rad) * cos(tot.dist.km/R) + cos(lat.0.rad) * sin(tot.dist.km/R) * cos(bearing.rad))
  lon.new.rad     <- lon.0.rad + atan2(sin(bearing.rad) * sin(tot.dist.km/R) * cos(lat.0.rad), cos(tot.dist.km/R) * sin(lat.new.rad) )
  y[kk]           <- NISTradianTOdeg(lat.new.rad)
  x[kk]           <- NISTradianTOdeg(lon.new.rad)
  #print(lat.deg[kk])
  #print(lon.deg[kk])
}
# save computed lat/lon into radia.coords df
lat                 <- y
lon                 <- x
rm(x)
rm(y)
x                   <- nexrad.coords.cart$x
y                   <- nexrad.coords.cart$y
radia.int.vals      <- as.data.frame(as.vector(nc.radia.l1[360:720,1:457]))
FZDZ                <- radia.int.vals$FRZDRZ
FZDZ[is.na(FZDZ)]   <- -0.1
radia.coords.df     <- data.frame(x, y, lon, lat, FZDZ)
#radia.coords.df     <- data.frame(x, y, FZDZ)
head(radia.coords.df)

#-------------------------------------------------------
# plotting begins .....
#   plot newly computed lon/lat converted from r, theta
plot.new()
plot(lon, lat)

# polar coordinate radarscope plot for KDVN on 2/26/2019 at 1:53 GMT
KDVN <- plot_ly(type = 'scatterpolar', mode = "lines+markers") %>%
        #add_trace(r = c(1,2,3,4,5),                   theta = c(0,90,180,360,0),                                                    line = list(color = "#ff66ab"), marker = list(color = "#8090c7", symbol = 'square', size = 8), text = "sector: 135->225<br>rotation: 90<br>direction: counterclockwise") %>%
        add_trace(r = c( 20, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 20, 20, 20, 20, 20, 20, 20), theta = c(235, 235,   225, 202.5,   180,  157.5,   135,   112.5,  90,   67.5, 45,   22.5, 0,  353, 353,     0,  45,    90, 135, 180, 215, 235),        fill = 'toself', fillcolor = 'green', line = list(color = 'black'), opacity = 0.5, name = "no hazard detected") %>%
        add_trace(r = c(130, 130, 130, 130, 130, 130, 130, 130,  75,  95, 110, 105, 105, 120),                        theta = c(235, 235, 248.5,   270, 292.5,    315, 337.5,     353, 353,    315, 292.5, 270, 248.5, 235),        fill = 'toself', fillcolor = 'orange', line = list(color = 'black'), opacity = 0.5, name = "detectable, cond-inner") %>%
        add_trace(r = c(  0,  80,  80,  80,  65,  65,  65, 65,    0,   0,   0,   0,   0,   0, 0),                     theta = c(235, 235, 248.5,   270, 292.5,    315, 337.5,     353, 353,  337.5, 315, 292.5, 270, 248.5, 235), fill = 'toself', fillcolor = 'orange', line = list(color = 'black'), opacity = 0.5, name = "detectable, cond-neighbor") %>%
        add_trace(r = c( 66, 130, 120, 105, 110, 110,  95, 75,   66,  66,  66, 66),                                   theta = c(235, 235, 248.5,   270, 292.5,    315, 337.5,     353, 353,    315, 270, 235),                      fill = 'toself', fillcolor = 'red',    line = list(color = 'black'), opacity = 1.0, name = "detected, inRadIA") %>%
        add_trace(r = c( 20,  20,  20,  20,  20,  20,  20, 20,   20),                                                 theta = c(  0,  45,    90,   135,   180,    225,   270,     315,   0),                                     fill = 'toself', fillcolor = 'red',    line = list(color = 'black'), opacity = 1.0, name = "detected, notinRadIA") %>%
        layout(polar = list(domain = list(x = c(0.0, 1.0), y = c(0.5, 1.0)), radialaxis = list(tickfont = list(size = 8)), angularaxis = list(tickfont = list(size = 8), rotation = 90, direction = 'clockwise')), showlegend = TRUE)
KDVN

# polar coordinate radarscope plot for KDMX on 2/26/2019 at 1:50 GMT
plot.new()
KDMX <- plot_ly(type = 'scatterpolar', mode = "lines+markers") %>%
        add_trace(r = c(135, 135, 135, 135, 135, 135, 135, 130, 111,  66,  52,  65,  81, 135, 152, 135, 135, 135, 135, 135, 135, 135, 135, 135), theta = c(0, 22.5, 45, 67.5,  90, 112.5, 135,  90,  45,   0, 315, 270, 225, 180, 135, 180, 202.5, 225, 247.5, 270, 292.5, 315, 337.5, 0), fill = 'toself', fillcolor = 'orange', line = list(color = 'black'), opacity = 0.5, name = "detectable, cond-neighbor") %>%
        add_trace(r = c( 66, 111, 130, 152, 135,  81,  65,  52,  66),                                                                            theta = c(0,   45, 90,  135, 180,   225, 270, 315,   0),                                       fill = 'toself', fillcolor = 'red',    line = list(color = 'black'), opacity = 0.75, name = "detected, inRadIA") %>%
        layout(polar = list(domain = list(x = c(0.0, 1.0), y = c(0.5, 1.0)), radialaxis = list(tickfont = list(size = 8)), angularaxis = list(tickfont = list(size = 8), rotation = 90, direction = 'clockwise')), showlegend = TRUE)
KDMX

#   plot 
plot.new()
par(mfrow=c(1, 2))
image(nc.radia.l1, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]", main = "RadIA FZDZ INT [0-1]")
grid()
image(nc.radar.l1, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]", main = "NEXRAD REFL [dBZ]")
grid()

plot.new()
image(nc.radia.l2, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]")

plot.new()
image(nc.radia.l3, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]")

plot.new()
image(nc.radia.l4, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]")

plot.new()
image(nc.radia.l5, col = YOR.cols, ylab = "azim [deg]", xlab = "range [km]")

# hists of INT values
hist(nc.radia.l1, main="Distribution of FZDZ INT values", col= "purple", maxpixels=22000000)

#--------------------------------------------
#
# Name:    ICICLE.18.ALLFLIGHTS.RadIA.v2.5.R
#
# Purpose: 1) 
#          2) 
#          3) make plots
#
# How to run:
#
#
# Created: 6.15.2021 dserke
#
# Things to do:
# 1. 
#----------------------------------------------

# add libraries
#  data processing
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(gridExtra)
library(fields)
library(stringr)
library(pracma)
library(RANN)

# data plotting
library(ggplot2)

#  spatial
library(raster)
library(rasterVis)
#library(rgdal)
library(dismo)

#  mapping
library(lattice)
library(latticeExtra)
library(data.table)
#library(tmap)
library(maps)
library(mapdata)
library(maptools)
library(ggmap)

# data input format
library(ncdf4)

# unit conversion
library(NISTunits)

# date manipulation
library(lubridate)

#----------------------------------------------
# constants and parameters
#----------------------------------------------
options(max.print=20000) 

# field campaign information
#   field.campaign                   <- "SNOWIE_2017"
field.campaign                      <- "ICICLE_2019"

#   ICICLE domain lat/lon grid bounds, pre-defined
ICICLE.max.lon                      <- 95              # deg W
ICICLE.min.lon                      <- 83              # deg W
ICICLE.max.lat                      <- 48              # deg N
ICICLE.min.lat                      <- 36              # deg N

#   HRRR model vertical coordinates
HRRR.pres.mb                        <- seq(from=-1000, to=-25, by=25)

# mosaic version input data information
#   latest mosaic data version number
#latest.mosaic.ver.num               <- 1             # as of 06.17.2020, produced by Dave A. late in 2019
#latest.mosaic.ver.num               <- 2.1           # as of 08.27.2020, produced by Dave A.
#latest.mosaic.ver.num               <- 2.2            # as of 09.21.2020, produced by Dave A., but science team should be able to process as of this date
#latest.mosaic.ver.num               <- 2.4            # as of 03.12.2021, produced by Dave S. with removed std
latest.mosaic.ver.num               <- 2.5            # as of 03.12.2021, produced by Dave S. with removed std
#latest.mosaic.ver.num               <- 3             # as of ??.??.????, produced by Dave S. and Scott E., first outputted version produced by science team

#
latest.CNV.30smean.ver.num          <- 4

#----------------------------------------------
# define data paths, list data files in path
#----------------------------------------------
# define base path dir
base.path.lcl.dir                        <- file.path("/d1/serke/projects/case_studies/")
#base.path.ext.dir                        <- file.path("/media/ice_d3/projects/case_studies/")

# define NEXRAD site location csv file path
nexrad.site.dataframetxt.dir             <- file.path(paste(base.path.lcl.dir, "SNOWIE_2017/data/RadIA_data/nexrad_site_data/", sep=""))

# define output matchup data file path
output.matchup.ALLFLIGHTS.v2.2.dir       <- file.path(paste(base.path.lcl.dir, field.campaign, "/data/ICICLE.7_output/ver2.2/full_flight_clo_pix/CNV_v", latest.CNV.30smean.ver.num, "/", sep=""))
output.matchup.ALLFLIGHTS.v2.5.dir       <- file.path(paste(base.path.lcl.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/full_flight_clo_pix/CNV_v", latest.CNV.30smean.ver.num, "/", sep=""))

# define ALLFLIGHTS csv data filename
output.matchup.ALLFLIGHTS.v2.2.listfiles <- list.files(path = output.matchup.ALLFLIGHTS.v2.2.dir , include.dirs = FALSE)
output.matchup.ALLFLIGHTS.v2.2.filename  <- output.matchup.ALLFLIGHTS.v2.2.listfiles[23]
output.matchup.ALLFLIGHTS.v2.5.listfiles <- list.files(path = output.matchup.ALLFLIGHTS.v2.5.dir , include.dirs = FALSE)
output.matchup.ALLFLIGHTS.v2.5.filename  <- output.matchup.ALLFLIGHTS.v2.5.listfiles[1]

#---------------------------------------------
# Start writing debugging information to an output file
#---------------------------------------------
# Print some pre-processing info
print("---------------------------------------")
print(paste("Available ", field.campaign, " matchup files..."))
print("---------------------------------------")
print(paste("  Field Campaign: ",           field.campaign,                     sep=""))
print(paste("  ALLFLIGHTS v2.2 data filename: ", output.matchup.ALLFLIGHTS.v2.2.filename, sep=""))
print(paste("  ALLFLIGHTS v2.5 data filename: ", output.matchup.ALLFLIGHTS.v2.5.filename, sep=""))
print("---------------------------------------")

#-----------------------------------------------
# load ALLFLIGHTS Rm version number csv data files 
#-----------------------------------------------
# .. for Rmv2.2
print(paste("    Loading ALLFLIGHTS Rmv2.2 csv data file...", sep=""))
ALLFLIGHTS.v2.2.df  <- read.csv(paste(output.matchup.ALLFLIGHTS.v2.2.dir, output.matchup.ALLFLIGHTS.v2.2.filename, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
head(ALLFLIGHTS.v2.2.df)
#str(ALLFLIGHTS.v2.2.df)

# .. for RMv2.5
print(paste("    Loading ALLFLIGHTS RMv2.5 csv data file...", sep=""))
ALLFLIGHTS.v2.5.df  <- read.csv(paste(output.matchup.ALLFLIGHTS.v2.5.dir, output.matchup.ALLFLIGHTS.v2.5.filename, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
head(ALLFLIGHTS.v2.5.df)
#str(ALLFLIGHTS.v2.5.df)

#----------------------------------------------
# load ICICLE Campaign-related overview datasets
#----------------------------------------------
print(paste("Manage ICICLE Campaign-related overview datasets...", sep=""))

# load the NEXRAD site location text file
print(paste("  Loading NEXRAD site file from: ", nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep=""))
NEXRAD.site.df                       <- read.csv(paste(nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep = ""), header = FALSE, sep = ",", dec = ".", stringsAsFactors=FALSE)
colnames(NEXRAD.site.df)             <- c("NCDCID", "ICAO", "WBAN", "radname", "COUNTRY", "STATE", "COUNTY", "lat", "lon", "elev", "GMTdiff", "STN_TYPE")
#head(NEXRAD.site.df)

#------------------------------------------------        
#
# Manipulate data
#
#------------------------------------------------
# NEXRAD site information
#print(paste("Loading location info for NEXRAD = ", NEXRAD.site.name, sep=""))
#ind.NEXRAD.site.name          <- which(NEXRAD.site.df$ICAO == NEXRAD.site.name)
#NEXRAD.site.lat.deg           <- NEXRAD.site.df$lat[ind.NEXRAD.site.name]
#NEXRAD.site.lon.deg           <- NEXRAD.site.df$lon[ind.NEXRAD.site.name]
#print(paste("  NEXRAD = ", NEXRAD.site.name, " at lat = ", NEXRAD.site.lat.deg, ", lon = ", NEXRAD.site.lon.deg,  sep=""))

# Find closest lat and lon in array
#   make vector from matrix
#lon0.INT.array                <- as.vector(lon0.INT)
#lat0.INT.array                <- as.vector(lat0.INT)
#   make lon/lat df
#lon0.lat0.INT.df              <- as.data.frame(cbind(lon0.INT.array, lat0.INT.array))
#   process through nearest neighbor function
#nearest                       <- nn2(lon0.lat0.INT.df, t(replicate(dim(lon0.lat0.INT.df)[1], c(NEXRAD.site.lon.deg , NEXRAD.site.lat.deg ))), searchtype="radius", radius=0.0245)
#max(nearest$nn.idx)
#   find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
#ind.closest.mosaic.lat.pixel  <- max(nearest$nn.idx)%/%dim(lon0.INT)[1] + 1 # the next column (+1) after the whole number divisor
#ind.closest.mosaic.lon.pixel  <- max(nearest$nn.idx)%%dim(lon0.INT)[1]
#ind.closest.mosaic.pres.pixel <- which.min(abs(CNV.30s.mean.df$pres.hpa[j] - abs(HRRR.pres.mb)))

# index v2.2 on seg type == 0
ind.v2.2.segt.eq0.truSLW                 <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 1 | ALLFLIGHTS.v2.2.df$truth.cat == 2))
ALLFLIGHTS.v2.2.segt.eq0.truSLW.df       <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truSLW, ]
ind.v2.2.segt.eq0.truFZDZ                <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 3 | ALLFLIGHTS.v2.2.df$truth.cat == 4))
ALLFLIGHTS.v2.2.segt.eq0.truFZDZ.df      <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truFZDZ, ]
ind.v2.2.segt.eq0.truFZRA                <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 5))
ALLFLIGHTS.v2.2.segt.eq0.truFZRA.df      <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truFZRA, ]
ind.v2.2.segt.eq0.truMPICEDOM            <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 6))
ALLFLIGHTS.v2.2.segt.eq0.truMPICEDOM.df  <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truMPICEDOM, ]
ind.v2.2.segt.eq0.truMPsLIQDOM           <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 7))
ALLFLIGHTS.v2.2.segt.eq0.truMPsLIQDOM.df <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truMPsLIQDOM, ]
ind.v2.2.segt.eq0.truMPlLIQDOM           <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 8))
ALLFLIGHTS.v2.2.segt.eq0.truMPlLIQDOM.df <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truMPlLIQDOM, ]
ind.v2.2.segt.eq0.truICE                 <- which(ALLFLIGHTS.v2.2.df$seg.t == 0 & (ALLFLIGHTS.v2.2.df$truth.cat == 9))
ALLFLIGHTS.v2.2.segt.eq0.truICE.df       <- ALLFLIGHTS.v2.2.df[ind.v2.2.segt.eq0.truICE, ]
# index v2.2 on seg type == any
ind.v2.2.truSLW                 <- which((ALLFLIGHTS.v2.2.df$truth.cat == 1 | ALLFLIGHTS.v2.2.df$truth.cat == 2))
ALLFLIGHTS.v2.2.truSLW.df       <- ALLFLIGHTS.v2.2.df[ind.v2.2.truSLW, ]
ind.v2.2.truFZDZ                <- which((ALLFLIGHTS.v2.2.df$truth.cat == 3 | ALLFLIGHTS.v2.2.df$truth.cat == 4))
ALLFLIGHTS.v2.2.truFZDZ.df      <- ALLFLIGHTS.v2.2.df[ind.v2.2.truFZDZ, ]
ind.v2.2.truFZRA                <- which((ALLFLIGHTS.v2.2.df$truth.cat == 5))
ALLFLIGHTS.v2.2.truFZRA.df      <- ALLFLIGHTS.v2.2.df[ind.v2.2.truFZRA, ]
ind.v2.2.truMPICEDOM            <- which((ALLFLIGHTS.v2.2.df$truth.cat == 6))
ALLFLIGHTS.v2.2.truMPICEDOM.df  <- ALLFLIGHTS.v2.2.df[ind.v2.2.truMPICEDOM, ]
ind.v2.2.truMPsLIQDOM           <- which((ALLFLIGHTS.v2.2.df$truth.cat == 7))
ALLFLIGHTS.v2.2.truMPsLIQDOM.df <- ALLFLIGHTS.v2.2.df[ind.v2.2.truMPsLIQDOM, ]
ind.v2.2.truMPlLIQDOM           <- which((ALLFLIGHTS.v2.2.df$truth.cat == 8))
ALLFLIGHTS.v2.2.truMPlLIQDOM.df <- ALLFLIGHTS.v2.2.df[ind.v2.2.truMPlLIQDOM, ]
ind.v2.2.truICE                 <- which((ALLFLIGHTS.v2.2.df$truth.cat == 9))
ALLFLIGHTS.v2.2.truICE.df       <- ALLFLIGHTS.v2.2.df[ind.v2.2.truICE, ]

# index v2.5 on seg type == 0
ind.v2.5.segt.eq0.truSLW                 <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 1 | ALLFLIGHTS.v2.5.df$truth.cat3 == 2))
ALLFLIGHTS.v2.5.segt.eq0.truSLW.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truSLW, ]
ind.v2.5.segt.eq0.truFZDZ                <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 3 | ALLFLIGHTS.v2.5.df$truth.cat3 == 4))
ALLFLIGHTS.v2.5.segt.eq0.truFZDZ.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truFZDZ, ]
ind.v2.5.segt.eq0.truFZRA                <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 5))
ALLFLIGHTS.v2.5.segt.eq0.truFZRA.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truFZRA, ]
ind.v2.5.segt.eq0.truMPICEDOM            <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 6))
ALLFLIGHTS.v2.5.segt.eq0.truMPICEDOM.df  <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truMPICEDOM, ]
ind.v2.5.segt.eq0.truMPsLIQDOM           <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 7))
ALLFLIGHTS.v2.5.segt.eq0.truMPsLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truMPsLIQDOM, ]
ind.v2.5.segt.eq0.truMPlLIQDOM           <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 8))
ALLFLIGHTS.v2.5.segt.eq0.truMPlLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truMPlLIQDOM, ]
ind.v2.5.segt.eq0.truICE                 <- which(ALLFLIGHTS.v2.5.df$seg.t == 0 & (ALLFLIGHTS.v2.5.df$truth.cat3 == 9))
ALLFLIGHTS.v2.5.segt.eq0.truICE.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.segt.eq0.truICE, ]
# index v2.5 on seg type == any
ind.v2.5.truSLW                 <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 1 | ALLFLIGHTS.v2.5.df$truth.cat3 == 2))
ALLFLIGHTS.v2.5.truSLW.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.truSLW, ]
ind.v2.5.truFZDZ                <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 3 | ALLFLIGHTS.v2.5.df$truth.cat3 == 4))
ALLFLIGHTS.v2.5.truFZDZ.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.truFZDZ, ]
ind.v2.5.truFZRA                <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 5))
ALLFLIGHTS.v2.5.truFZRA.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.truFZRA, ]
ind.v2.5.truMPICEDOM            <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 6))
ALLFLIGHTS.v2.5.truMPICEDOM.df  <- ALLFLIGHTS.v2.5.df[ind.v2.5.truMPICEDOM, ]
ind.v2.5.truMPsLIQDOM           <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 7))
ALLFLIGHTS.v2.5.truMPsLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.truMPsLIQDOM, ]
ind.v2.5.truMPlLIQDOM           <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 8))
ALLFLIGHTS.v2.5.truMPlLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.truMPlLIQDOM, ]
ind.v2.5.truICE                 <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 9))
ALLFLIGHTS.v2.5.truICE.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.truICE, ]

# index v2.5 on dist < 100 km
ind.v2.5.d100.truSLW                 <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 1 | ALLFLIGHTS.v2.5.df$truth.cat3 == 2) & (ALLFLIGHTS.v2.5.df$conc.liq.cm3 >= 0.01 & ALLFLIGHTS.v2.5.df$conc.ice.cm3 < 0.01) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truSLW.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truSLW, ]
ind.v2.5.d100.truFZDZ                <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 3 | ALLFLIGHTS.v2.5.df$truth.cat3 == 4) & (ALLFLIGHTS.v2.5.df$conc.liq.cm3 >= 0.01 & ALLFLIGHTS.v2.5.df$conc.ice.cm3 < 0.01) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truFZDZ.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truFZDZ, ]
ind.v2.5.d100.truFZRA                <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 5) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truFZRA.df      <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truFZRA, ]
ind.v2.5.d100.truMPICEDOM            <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 6) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truMPICEDOM.df  <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truMPICEDOM, ]
ind.v2.5.d100.truMPsLIQDOM           <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 7) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truMPsLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truMPsLIQDOM, ]
ind.v2.5.d100.truMPlLIQDOM           <- which((ALLFLIGHTS.v2.5.df$truth.cat3 == 8) & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 )
ALLFLIGHTS.v2.5.d100.truMPlLIQDOM.df <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truMPlLIQDOM, ]
ind.v2.5.d100.truICE                 <- which((ALLFLIGHTS.v2.5.df$conc.liq.cm3 < 0.01 & ALLFLIGHTS.v2.5.df$conc.ice.cm3 > 0.01 & ALLFLIGHTS.v2.5.df$dist.radartoac.km < 100 ))
ALLFLIGHTS.v2.5.d100.truICE.df       <- ALLFLIGHTS.v2.5.df[ind.v2.5.d100.truICE, ]

#head(ALLFLIGHTS.v2.5.truFZDZ.df)

ind.v2.2.truFZDZ.match.v2.5     <- which(ALLFLIGHTS.v2.2.df$f. == ALLFLIGHTS.v2.5.truFZDZ.df$f. & ALLFLIGHTS.v2.2.df$hh == ALLFLIGHTS.v2.5.truFZDZ.df$hh & ALLFLIGHTS.v2.2.df$mm == ALLFLIGHTS.v2.5.truFZDZ.df$mm.1 & ALLFLIGHTS.v2.2.df$ss == ALLFLIGHTS.v2.5.truFZDZ.df$ss)
length(ind.v2.2.truFZDZ.match.v2.5)

#----------------------------------------------
#
# plotting
#
#----------------------------------------------
par(mfrow = c(1, 1))
par(mar   = c(6, 6, 3, 3))
plot(ALLFLIGHTS.v2.5.d100.truICE.df$dmax.85.per.L.um, ALLFLIGHTS.v2.5.d100.truICE.df$mean.DBZ.FZDZ.pix, xlab="Dmax-85/L", ylab="mean(DBZ)", cex.axis=3, cex.lab=3, col="grey", pch=4, type="p", xlim=c(0,500), ylim=c(-30,30))
lines(ALLFLIGHTS.v2.5.d100.truFZDZ.df$dmax.85.per.L.um, ALLFLIGHTS.v2.5.d100.truFZDZ.df$mean.DBZ.FZDZ.pix, col="blue", pch=1, type="p")
abline(h=2, col="blue", type="l")
abline(h=3, col="blue", type="l")
text(50, 1.7, "R-m-v2 val=1", col="blue")
text(50, 3.4, "R-m-v2 val=0", col="blue")
grid()

par(mfrow = c(1, 1))
par(mar   = c(6, 6, 3, 3))
plot(ALLFLIGHTS.v2.5.d100.truICE.df$mean.DBZ.FZDZ.pix, ALLFLIGHTS.v2.5.d100.truICE.df$mean.ZDR.corr.SLW.pix, xlab="mean(DBZ)", ylab="mean(DBZ)", cex.axis=3, cex.lab=3, col="grey", pch=4, type="p", xlim=c(-30,30), ylim=c(-2,2))
lines(ALLFLIGHTS.v2.5.d100.truFZDZ.df$mean.DBZ.FZDZ.pix, ALLFLIGHTS.v2.5.d100.truFZDZ.df$mean.ZDR.corr.SLW.pix, col="blue", pch=1, type="p")
grid()

# DENSITY HISTS AND FRACZTION OF ALGO TRUTH POINTS THAT MATCH GIVEN ALGO INT VALUE PLOTS
# .. for Rmv2=FZDZ, tru = ICEONLY & FZDZ
h.Rmv2.FZDZ.truICE      <- hist(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix,  breaks=49,              col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.FZDZ.truFZDZ     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, breaks=49,              col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
h.Rmv2.FZDZ.truICE.den  <- plot( h.Rmv2.FZDZ.truICE$mids[2:50],  2*h.Rmv2.FZDZ.truICE$density[2:50],  col=rgb(0.7,0.7,0.7,0.5), ylim=c(0,100), type="b", lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.FZDZ.truFZDZ.den <- lines(h.Rmv2.FZDZ.truFZDZ$mids[2:50], 2*h.Rmv2.FZDZ.truFZDZ$density[2:50], col=rgb(0.0,0.9,0.9,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=ICEONLY, Rmv2=FZDZ", "tru=FZDZ,       Rmv2=FZDZ"),                   col=c(rgb(0.7,0.7,0.7), rgb(0.0,0.9,0.9)), lty=1:1, lwd=2:2, cex=0.8)
grid()
ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix[ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix==0] <- NaN
h.Rmv2.FZDZ.truICE.4br      <- hist(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix,  breaks=4,              col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.FZDZ.truFZDZ.4br     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, breaks=4,              col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
truepos.FZDZ.v.truFZDZ      <- h.Rmv2.FZDZ.truFZDZ.4br$density / (h.Rmv2.FZDZ.truICE.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density) 
falsepos.FZDZ.v.truICE      <- h.Rmv2.FZDZ.truICE.4br$density /  (h.Rmv2.FZDZ.truICE.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density)
plot(h.Rmv2.FZDZ.truFZDZ.4br$mids, truepos.FZDZ.v.truFZDZ, type="b", lwd=2, col="blue", xlab="FZDZ INT", ylab="Frac values truth = FZDZ", xlim=c(0,1), ylim=c(0,1))
grid()

# .. for Rmv2=FZDZ when Dist <100km, tru = ICEONLY & FZDZ
h.Rmv2.d100.FZDZ.truICE      <- hist(ALLFLIGHTS.v2.5.d100.truICE.df$int.full.FZDZ.pix,  breaks=49,                   col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.d100.FZDZ.truFZDZ     <- hist(ALLFLIGHTS.v2.5.d100.truFZDZ.df$int.full.FZDZ.pix, breaks=49,                   col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
par(mfrow = c(1, 1))
par(mar   = c(6, 6, 3, 3))
h.Rmv2.d100.FZDZ.truICE.den  <- plot( h.Rmv2.d100.FZDZ.truICE$mids[2:50],  2*h.Rmv2.d100.FZDZ.truICE$density[2:50],  col=rgb(0.7,0.7,0.7,0.5), ylim=c(0,100), type="b", cex.axis=3, cex.lab=3, lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.d100.FZDZ.truFZDZ.den <- lines(h.Rmv2.d100.FZDZ.truFZDZ$mids[2:50], 2*h.Rmv2.d100.FZDZ.truFZDZ$density[2:50], col=rgb(0.0,0.9,0.9,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=ICEONLY, Rmv2=FZDZ", "tru=FZDZ,       Rmv2=FZDZ"),                   col=c(rgb(0.7,0.7,0.7), rgb(0.0,0.9,0.9)), lty=1:1, lwd=2:2, cex=0.8)
grid()
ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix[ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix==0] <- NaN
h.Rmv2.FZDZ.truICE.4br      <- hist(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix,  breaks=4,              col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.FZDZ.truFZDZ.4br     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, breaks=4,              col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
truepos.FZDZ.v.truFZDZ      <- h.Rmv2.FZDZ.truFZDZ.4br$density / (h.Rmv2.FZDZ.truICE.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density) 
falsepos.FZDZ.v.truICE      <- h.Rmv2.FZDZ.truICE.4br$density /  (h.Rmv2.FZDZ.truICE.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density)
plot(h.Rmv2.FZDZ.truFZDZ.4br$mids, truepos.FZDZ.v.truFZDZ, type="b", lwd=2, col="blue", xlab="FZDZ INT", ylab="Frac values truth = FZDZ", xlim=c(0,1), ylim=c(0,1))
grid()

# .. for Rmv2=FZDZ, tru = SLW & FZDZ
h.Rmv2.FZDZ.truSLW      <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.FZDZ.pix,  breaks=49,              col=rgb(0.0,1.0,0.0,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.FZDZ.truFZDZ     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, breaks=49,              col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
h.Rmv2.FZDZ.truSLW.den  <- plot( h.Rmv2.FZDZ.truSLW$mids[2:50],  2*h.Rmv2.FZDZ.truICE$density[2:50],  col=rgb(0.0,1.0,0.0,0.5), ylim=c(0,100), type="b", lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.FZDZ.truFZDZ.den <- lines(h.Rmv2.FZDZ.truFZDZ$mids[2:50], 2*h.Rmv2.FZDZ.truFZDZ$density[2:50], col=rgb(0.0,0.9,0.9,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=SLW, Rmv2=FZDZ", "tru=FZDZ,       Rmv2=FZDZ"),                   col=c(rgb(0.0,1.0,0.0), rgb(0.0,0.9,0.9)), lty=1:1, lwd=2:2, cex=0.8)
grid()
ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix[ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix==0] <- NaN
ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix[ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix==0]     <- NaN
h.Rmv2.FZDZ.truSLW.4br      <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.FZDZ.pix,  breaks=4,              col=rgb(0.0,1.0,0.0,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.FZDZ.truFZDZ.4br     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, breaks=4,              col=rgb(0.0,0.9,0.9,0.3), add=TRUE)
truepos.FZDZ.v.truFZDZ      <- h.Rmv2.FZDZ.truFZDZ.4br$density / (h.Rmv2.FZDZ.truSLW.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density) 
falsepos.FZDZ.v.truSLW      <- h.Rmv2.FZDZ.truSLW.4br$density /  (h.Rmv2.FZDZ.truSLW.4br$density + h.Rmv2.FZDZ.truFZDZ.4br$density)
plot(h.Rmv2.FZDZ.truFZDZ.4br$mids, truepos.FZDZ.v.truFZDZ, type="b", lwd=2, col="blue", xlab="FZDZ INT", ylab="Frac values truth = FZDZ", xlim=c(0,1), ylim=c(0,1))
grid()

# .. for Rmv2=SLW, tru = ICEONLY & SLW
h.Rmv2.SLW.truICE      <- hist(ALLFLIGHTS.v2.5.truICE.df$int.full.SLW.pix,  breaks=49,            col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.SLW.truSLW      <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix, breaks=49,             col=rgb(0.0,1.0,0.0,0.3), add=TRUE)
h.Rmv2.SLW.truICE.den  <- plot( h.Rmv2.SLW.truICE$mids[2:50], 2*h.Rmv2.SLW.truICE$density[2:50],  col=rgb(0.7,0.7,0.7,0.5), ylim=c(0,100), type="b", lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.SLW.truSLW.den  <- lines(h.Rmv2.SLW.truSLW$mids[2:50], 2*h.Rmv2.SLW.truSLW$density[2:50],  col=rgb(0.0,1.0,0.0,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=ICEONLY, Rmv2=SLW", "tru=SLW,        Rmv2=SLW"),                 col=c(rgb(0.7,0.7,0.7), rgb(0.0,1.0,0.5)), lty=1:1, lwd=2:2, cex=0.8)
grid()
#ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix[ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix==0] <- NaN
h.Rmv2.SLW.truICE.4br  <- hist(ALLFLIGHTS.v2.5.truICE.df$int.full.SLW.pix,  breaks=4,             col=rgb(0.7,0.7,0.7,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.SLW.truSLW.4br  <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix,  breaks=4,             col=rgb(0.0,1.0,0.0,0.5), add=TRUE)
truepos.SLW.v.truSLW   <- h.Rmv2.SLW.truSLW.4br$density / (h.Rmv2.SLW.truICE.4br$density + h.Rmv2.SLW.truSLW.4br$density) 
falsepos.SLW.v.truICE  <- h.Rmv2.SLW.truICE.4br$density /  (h.Rmv2.SLW.truICE.4br$density + h.Rmv2.SLW.truSLW.4br$density)
plot(h.Rmv2.SLW.truSLW.4br$mids, truepos.SLW.v.truSLW, type="b", lwd=2, col="green", xlab="SLW INT", ylab="Frac values truth = SLW", xlim=c(0,1), ylim=c(0,1))
grid()

# .. for Rmv2=SLW, tru = FZDZ & SLW
h.Rmv2.SLW.truFZDZ     <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.SLW.pix,  breaks=49,             col=rgb(0.0,0.9,0.9,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.SLW.truSLW      <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix, breaks=49,               col=rgb(0.0,1.0,0.0,0.3), add=TRUE)
h.Rmv2.SLW.truFZDZ.den <- plot( h.Rmv2.SLW.truFZDZ$mids[1:50],  2*h.Rmv2.SLW.truFZDZ$density[1:50], col=rgb(0.0,0.9,0.9,0.3), ylim=c(0,100), type="b", lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.SLW.truSLW.den  <- lines(h.Rmv2.SLW.truSLW$mids[1:50], 2*h.Rmv2.SLW.truSLW$density[1:50],    col=rgb(0.0,1.0,0.0,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=FZDZ, Rmv2=SLW", "tru=SLW,        Rmv2=SLW"),                      col=c(rgb(0.0,0.9,0.9), rgb(0.0,1.0,0.5)), lty=1:1, lwd=2:2, cex=0.8)
grid()
#ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix[ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix==0] <- NaN
h.Rmv2.SLW.truFZDZ.4br <- hist(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.SLW.pix,  breaks=4,              col=rgb(0.0,0.9,0.9,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.SLW.truSLW.4br  <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix,  breaks=4,               col=rgb(0.0,1.0,0.0,0.5), add=TRUE)
truepos.SLW.v.truSLW   <- h.Rmv2.SLW.truSLW.4br$density / (h.Rmv2.SLW.truFZDZ.4br$density + h.Rmv2.SLW.truSLW.4br$density) 
falsepos.SLW.v.truFZDZ <- h.Rmv2.SLW.truFZDZ.4br$density /  (h.Rmv2.SLW.truFZDZ.4br$density + h.Rmv2.SLW.truSLW.4br$density)
plot(h.Rmv2.SLW.truSLW.4br$mids, truepos.SLW.v.truSLW, type="b", lwd=2, col="green", xlab="SLW INT", ylab="Frac values truth = SLW, not FZDZ", xlim=c(0,1), ylim=c(0,1))
grid()

# .. for Rmv2=MPHALIQDOM, tru = MPHAICEDOM & MPHAlLIQDOM
h.Rmv2.MPHAlLIQDOM.truMPHAICEDOM      <- hist(ALLFLIGHTS.v2.5.truMPICEDOM.df$int.full.MIXPHA.pix,  breaks=49, col=rgb(1.0,0.9,0.9,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM     <- hist(ALLFLIGHTS.v2.5.truMPlLIQDOM.df$int.full.MIXPHA.pix, breaks=49, col=rgb(1.0,0.5,0.0,0.3), add=TRUE)
h.Rmv2.MPHAlLIQDOM.truICE.den         <- plot( h.Rmv2.MPHAlLIQDOM.truICE$mids[1:50],         2*h.Rmv2.MPHAlLIQDOM.truICE$density[1:50],         col=rgb(0.7,0.7,0.7,0.5), ylim=c(0,100), type="b", lwd=3, xlab="INT", ylab="PDF = Prob / INT", main="")
h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM.den <- lines(h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM$mids[1:50], 2*h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM$density[1:50], col=rgb(1.0,0.5,0.0,0.5), type="b", lwd=3, add=TRUE)
legend("topright", legend=c("tru=ICEONLY, Rmv2=MPHA-largeLIQDOM", "tru=MPHA-largeLIQDOM,        Rmv2=MPHA-largeLIQDOM"),                        col=c(rgb(0.7,0.7,0.7), rgb(1.0,0.5,0.0)), lty=1:1, lwd=2:2, cex=0.8)
grid()
h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM.4br <- hist(ALLFLIGHTS.v2.5.truMPlLIQDOM.df$int.full.MIXPHA.pix, breaks=4,                                    col=rgb(1.0,0.5,0.0,0.3), xlab="INT", ylab="Freq [#]", xlim=c(0,1), ylim=c(0,700), main="")
#h.Rmv2.SLW.truSLW.4br  <- hist(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix,  breaks=4,             col=rgb(0.0,1.0,0.0,0.5), add=TRUE)
#truepos.SLW.v.truSLW   <- h.Rmv2.SLW.truSLW.4br$density / (h.Rmv2.SLW.truICE.4br$density + h.Rmv2.SLW.truSLW.4br$density) 
truepos.MPHAlLIQDOM.v.truMPHAlLIQDOM  <- h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM.4br$counts / sum(h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM.4br$counts) 
#falsepos.SLW.v.truICE  <- h.Rmv2.SLW.truICE.4br$density /  (h.Rmv2.SLW.truICE.4br$density + h.Rmv2.SLW.truSLW.4br$density)
plot(h.Rmv2.MPHAlLIQDOM.truMPHAlLIQDOM.4br$mids, truepos.MPHAlLIQDOM.v.truMPHAlLIQDOM, type="b", lwd=2, col="orange", xlab="MPHA INT", ylab="Frac values truth = MPHA-largedrop-LIQ-DOM", xlim=c(0,1), ylim=c(0,1))
grid()

#--------------
# Rm-v1.0 plots
# .. SLW INT v NEV when truth = ICEONLY
p <- ggplot(ALLFLIGHTS.v2.2.truICE.df, aes(x=int.full.SLW.clo.pix, y=NEV.1.IWC.gm3)) +
            xlim(0.00, 1.20) +
            ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.2.truICE.df$int.full.SLW.clo.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.2.truICE), sep=""), color="black")
# .. FZDZ INT v NEV when truth = ICEONLY
p <- ggplot(ALLFLIGHTS.v2.2.truICE.df, aes(x=int.full.FZDZ.clo.pix, y=NEV.1.IWC.gm3)) +
            xlim(0.00, 1.20) +
            ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.2.truICE.df$int.full.FZDZ.clo.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.2.truICE), sep=""), color="black")
# .. SLW INT v NEV when truth = SLW
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truSLW.df, aes(x=int.full.SLW.clo.pix, y=NEV.1.LWC.gm3)) +
            geom_point(size=2, shape=23) +
            xlim(0.00, 1.20) +
            ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'SLW', seg.type = 'S/L'") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truSLW), sep=""), color="black")
# .. FZDZ INT v NEV when truth = FZDZ
# .... for S/L  types
#p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truFZDZ.df, aes(x=int.full.FZDZ.clo.pix, y=NEV.1.LWC.gm3)) +
#            xlim(0.00, 1.20) +
#            ylim(0.00, 0.75)
#p + ggtitle("Rm-v1.0, truth = 'FZDZ', seg.type = 'S/L'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.2.segt.eq0.truFZDZ.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")
# .... for any/all types
p <- ggplot(ALLFLIGHTS.v2.2.truFZDZ.df, aes(x=int.full.FZDZ.clo.pix, y=NEV.1.LWC.gm3)) +
            xlim(0.00, 1.20) +
            ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.2.truFZDZ.df$int.full.FZDZ.clo.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.2.truFZDZ), sep=""), color="black")
## .. FZDZ INT v RID when truth = FZDZ
#p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truFZDZ.df, aes(x=int.full.FZDZ.clo.pix, y=RID.LWC.gm3)) +
#            geom_point(size=2, shape=23, alpha =0.5) +
#            xlim(0.00, 1.00) +
#            ylim(0.00, 0.75)
#p + ggtitle("Rm-v2.2, truth = 'FZDZ', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")
# .. FZDZ INT v RID when truth = MIXPHA large drop
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPlLIQDOM.df, aes(x=int.full.FZDZ.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23, alpha=0.5) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/FZDZ', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")
# .. 
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPlLIQDOM.df, aes(x=int.full.MIXPHA.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/FZDZ', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")
# .. FZDZ INT v RID when truth = MIXPHA small drop
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPsLIQDOM.df, aes(x=int.full.SLW.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/SLW', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truSLW), sep=""), color="black")
# .. 
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPsLIQDOM.df, aes(x=int.full.MIXPHA.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/SLW', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truSLW), sep=""), color="black")
# .. FZDZ INT v RID when truth = MIXPHA ICE DOM
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPICEDOM.df, aes(x=int.full.FZDZ.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/ICE DOM', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")
# .. 
p <- ggplot(ALLFLIGHTS.v2.2.segt.eq0.truMPICEDOM.df, aes(x=int.full.MIXPHA.clo.pix, y=NEV.1.LWC.gm3)) +
  geom_point(size=2, shape=23) +
  xlim(0.00, 1.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v1.0, truth = 'MIXPHA w/ICE DOM', seg.type = 'S/L'") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.2.segt.eq0.truFZDZ), sep=""), color="black")

#--------------
# Rm-v2.5 plots
# .. SLW INT v NEV when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=int.full.SLW.pix, y=NEV.TWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$int.full.SLW.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ INT v NEV when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=int.full.FZDZ.pix, y=NEV.TWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ INT v RHO when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=int.full.FZDZ.pix, y=RHO.MOMS.FZDZ.pix)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 1.20)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ INT v SLW when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=int.full.FZDZ.pix, y=int.full.SLW.pix)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 1.20)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 600)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ INT v DBZ when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=int.full.FZDZ.pix, y=ZDR.MOMS.FZDZ.pix)) +
  xlim(0.00, 1.20) +
  ylim(-2.00, 2.00)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 50)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ component RHOHV v NEV when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=RHO.MOMS.FZDZ.pix, y=NEV.TWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$RHO.MOMS.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. FZDZ component meanDBZ v NEV when truth = ICEONLY
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=mean.DBZ.FZDZ.pix, y=NEV.TWC.gm3)) +
  xlim(-20.00, 30.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$mean.DBZ.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")
# .. DBZ from NAW v NEV when truth = ICE
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truICE.df, aes(x=NAW.zennad.REFL.30s.mean.updown, y=NEV.TWC.gm3)) +
  xlim(-30.00, 30.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'ICEONLY', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truICE.df$NAW.zennad.REFL.30s.mean.updown, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truICE), sep=""), color="black")


# .. SLW INT v NEV when truth = SLW
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truSLW.df, aes(x=int.full.SLW.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'SLW', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truSLW.df$int.full.SLW.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truSLW), sep=""), color="black")
# .. FZDZ INT v NEV when truth = FZDZ
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truFZDZ.df, aes(x=int.full.FZDZ.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truFZDZ), sep=""), color="black")
# .. FZDZ INT v RHO when truth = FZDZ
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truFZDZ.df, aes(x=int.full.FZDZ.pix, y=RHO.MOMS.FZDZ.pix)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 1.20)
p + ggtitle("Rm-v2.5, truth3 = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truFZDZ.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truFZDZ), sep=""), color="black")
# .. FZDZ component RHOHV v NEV when truth = FZDZ
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truFZDZ.df, aes(x=RHO.MOMS.FZDZ.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truFZDZ.df$RHO.MOMS.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truFZDZ), sep=""), color="black")
# .. FZDZ component meanDBZ v NEV when truth = FZDZ
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truFZDZ.df, aes(x=mean.DBZ.FZDZ.pix, y=NEV.LWC.gm3)) +
  xlim(-20.00, 30.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 10)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truFZDZ.df$mean.DBZ.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truFZDZ), sep=""), color="black")
# .. DBZ from NAW v NEV when truth = FZDZ
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truFZDZ.df, aes(x=NAW.zennad.REFL.30s.mean.updown, y=NEV.LWC.gm3)) +
  xlim(-30.00, 30.00) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth3 = 'FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.10, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truFZDZ.df$NAW.zennad.REFL.30s.mean.updown, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.10, y=0.75, label=paste("N=", length(ind.v2.5.truFZDZ), sep=""), color="black")


# .. FZDZ INT v NEV LWC when truth = MIXPHA large drop dom
# ....for S/L seg types
#p <- ggplot(ALLFLIGHTS.v2.5.segt.eq0.truMPlLIQDOM.df, aes(x=int.full.FZDZ.pix, y=NEV.LWC.gm3)) +
#  xlim(0.00, 1.20) +
#  ylim(0.00, 0.75)
#p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/FZDZ', seg.type = 'S/L'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.segt.eq0.truMPlLIQDOM.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPlLIQDOM), sep=""), color="black")
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truMPlLIQDOM.df, aes(x=int.full.FZDZ.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPlLIQDOM.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black")+ annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.truMPlLIQDOM), sep=""), color="black")
# .. MIXPHA INT v LWC when truth = MIXPHA large drop dom
# ....for S/L seg types
#p <- ggplot(ALLFLIGHTS.v2.5.segt.eq0.truMPlLIQDOM.df, aes(x=int.full.MIXPHA.pix, y=NEV.LWC.gm3)) +
#  xlim(0.00, 1.20) +
#  ylim(0.00, 0.75)
#p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/FZDZ', seg.type = 'S/L'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.segt.eq0.truMPlLIQDOM.df$int.full.MIXPHA.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPlLIQDOM), sep=""), color="black")
# .... for any/all  types
p <- ggplot(ALLFLIGHTS.v2.5.truMPlLIQDOM.df, aes(x=int.full.MIXPHA.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/FZDZ', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPlLIQDOM.df$int.full.MIXPHA.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPlLIQDOM), sep=""), color="black")
# .. SLW INT v NEV LWC when truth = MIXPHA small drop dom
# ....for S/L seg types
#p <- ggplot(ALLFLIGHTS.v2.5.segt.eq0.truMPsLIQDOM.df, aes(x=int.full.SLW.pix, y=NEV.LWC.gm3)) +
#  xlim(0.00, 1.20) +
#  ylim(0.00, 0.75)
#p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/SLW', seg.type = 'S/L'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.segt.eq0.truMPsLIQDOM.df$int.full.SLW.pix, na.rm=TRUE), sep=""), color="black")+ annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPsLIQDOM), sep=""), color="black")
# ....for any/all seg types
p <- ggplot(ALLFLIGHTS.v2.5.truMPsLIQDOM.df, aes(x=int.full.SLW.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/SLW', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPsLIQDOM.df$int.full.SLW.pix, na.rm=TRUE), sep=""), color="black")+ annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPsLIQDOM), sep=""), color="black")
# .. MIXPHA INT v NEV LWC when truth = MIXPHA small drop dom
# ....for any/all seg types
p <- ggplot(ALLFLIGHTS.v2.5.truMPsLIQDOM.df, aes(x=int.full.MIXPHA.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/SLW', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPsLIQDOM.df$int.full.MIXPHA.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.segt.eq0.truMPsLIQDOM), sep=""), color="black")
# .. FZDZ INT v NEV LWC when truth = MIXPHA ICE DOM
# ....for any/all seg types
p <- ggplot(ALLFLIGHTS.v2.5.truMPICEDOM.df, aes(x=int.full.FZDZ.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/ICE DOM', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPICEDOM.df$int.full.FZDZ.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.truMPICEDOM), sep=""), color="black")
# .. SLW INT v NEV LWC when truth = MIXPHA ICE DOM
# ....for any/all seg types
p <- ggplot(ALLFLIGHTS.v2.5.truMPICEDOM.df, aes(x=int.full.SLW.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/ICE DOM', seg.type = 'any'") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPICEDOM.df$int.full.SLW.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.truMPICEDOM), sep=""), color="black")
# .. MIXPHA INT v NEV LWC when truth = MIXPHA ICE DOM
# ....for any/all seg types
p <- ggplot(ALLFLIGHTS.v2.5.truMPICEDOM.df, aes(x=int.full.MIXPHA.pix, y=NEV.LWC.gm3)) +
  xlim(0.00, 1.20) +
  ylim(0.00, 0.75)
p + ggtitle("Rm-v2.5, truth2 = 'MIXPHA w/ICE DOM', seg.type = 'any") + stat_bin2d(bins=50) + scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 20)) + annotate(geom="text", x=0.00, y=0.70, label=paste("median=", median(ALLFLIGHTS.v2.5.truMPICEDOM.df$int.full.MIXPHA.pix, na.rm=TRUE), sep=""), color="black") + annotate(geom="text", x=0.00, y=0.75, label=paste("N=", length(ind.v2.5.truMPICEDOM), sep=""), color="black")
# above plots updated 6.22.21
#############################
#############################

# compare v2.2 to v2.5 in known 
plot(ALLFLIGHTS.v2.2.segt.eq0.truFZDZ.df$int.full.FZDZ.clo.pix, ALLFLIGHTS.v2.5.segt.eq0.truFZDZ.df$int.full.FZDZ.pix, xlab="R-mv1, FZDZ INT", ylab="R-mv2, FZDZ INT")
abline(0, 1)
grid()

plot(ALLFLIGHTS.v2.2.segt.eq0.truSLW.df$int.full.SLW.clo.pix, ALLFLIGHTS.v2.5.segt.eq0.truSLW.df$int.full.SLW.pix)
abline(0, 1)
grid()






# pre Rmv2.5 plots start here 
plot( RAP.temp.KDVN.02Z.df$temp.c,                                                            RAP.temp.KDVN.02Z.df$alt.msl.m, type="b", col="black",      pch=19, xlim=c(-25, 5), ylim=c(0, 7000), xlab="Temp [deg C]", ylab="Alt MSL [m]")
lines(RAP.temp.KDMX.02Z.df$temp.c,                                                            RAP.temp.KDMX.02Z.df$alt.msl.m, type="b", col="grey",       pch=19)
lines(TEMP.CELSIUS[        ind.closest.mosaic.lon.pixel, ind.closest.mosaic.lat.pixel, 1:24], (mosaic.alt.array*1000)-400,    type="b", col="purple",     pch=19)
abline(v=-12, col="blue",      pch=19)
abline(v=-2,  col="lightblue", pch=19)
abline(v=2,   col="red",       pch=19)
legend("bottomleft", c("KDVN", "KDMX", "RadIA-m.v2.T"),                                                                                 col=c("black", "grey", "purple"), pch=c(19, 19, 19))
grid()

plot(MIXPHA.INT[           ind.closest.mosaic.lon.pixel, ind.closest.mosaic.lat.pixel, 1:24], (mosaic.alt.array*1000)-400,    type="b", col="purple",     pch=19)
abline(v=-12, col="blue",      pch=19)
abline(v=-2,  col="lightblue", pch=19)
abline(v=2,   col="red",       pch=19)
legend("bottomleft", c("KDVN", "KDMX", "RadIA-m.v2.T"),                                                                                 col=c("black", "grey", "purple"), pch=c(19, 19, 19))
grid()

#######################################
#pres.mb <- [1000, 975,  950, 925, 900, 875, 850, 825, 800, 775,  750,  725,  700,  675,  650,  625,  600,  575,  550,  525,  500,  475,  450,  425,  400,  375,  350,  325,  300,  275,  250,  225,  200, 175, 150,  125,  100,   75, 50, 25]

#------------------------------
# plot for v2.4 and 2.5 for FZDZ
par(mfrow=c(1,2))
par(mar=c(4, 4, 2, 3))
par(cex.axis=2, cex.lab=2, cex.main=1, cex.sub=1)
main <- ""
# F28 @ 13:30Z
image.plot(lon0.INT, lat0.INT, FZDZ.v2.4.INT[ , , 5], xlim=c(-87.8, -85.5), ylim=c(42.4, 43.7), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
# F24 @ 02:10Z
#image.plot(lon0.INT, lat0.INT, FZDZ.v2.4.INT[ , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()
par(cex.axis=2, cex.lab=2, cex.main=1, cex.sub=1)
main <- ""
# F28 @ 13:30Z
image.plot(lon0.INT, lat0.INT, FZDZ.v2.5.INT[ , , 5], xlim=c(-87.8, -85.5), ylim=c(42.4, 43.7), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
# F24 @ 02:10Z
#image.plot(lon0.INT, lat0.INT, FZDZ.v2.5.INT[ , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()

#------------------------------
# plot for v2.4 and 2.5 for SLW
par(mfrow=c(1,2))
par(mar=c(4, 4, 2, 3))
par(cex.axis=2, cex.lab=2, cex.main=1, cex.sub=1)
main <- ""
# F28 @ 13:30Z
#image.plot(lon0.INT, lat0.INT, SLW.v2.4.INT[  , , 5], xlim=c(-87.8, -85.5), ylim=c(42.4, 43.7), zlim=c(0,1), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
# F24 @ 02:10Z
image.plot(lon0.INT, lat0.INT, SLW.v2.4.INT[  , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5), zlim=c(0,1), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()
main <- ""
# F28 @ 13:30Z
#image.plot(lon0.INT, lat0.INT, SLW.v2.5.INT[  , , 5], xlim=c(-87.8, -85.5), ylim=c(42.4, 43.7), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
# F24 @ 02:10Z
image.plot(lon0.INT, lat0.INT, SLW.v2.5.INT[  , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5), zlim=c(0,1), xlab="Lon [deg]", ylab="Lat [deg]", main=main)
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()


########################################
########################################


image.plot(lon0.INT, lat0.INT, Int_meanDBZ[ , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5))
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()

image.plot(lon0.INT, lat0.INT, MeanDBZ[ , , 5], xlim=c(-92, -90.5), ylim=c(41, 42.5), zlim=c(-15, 15))
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg,      "*",              cex=2)
text(NEXRAD.site.lon.deg-0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()





##########  OLD SHIT

image.plot(lon0.INT, lat0.INT, MIXPHA.INT[            , , 10], xlim=c(-92, -90), ylim=c(40, 44), xlab="Lon [deg]", ylab="Lat [deg]")
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg, "*",                   cex=2)
text(NEXRAD.site.lon.deg+0.1, NEXRAD.site.lat.deg+0.1,  NEXRAD.site.name, cex=2)
grid()

image.plot(lon0.INT, lat0.INT, (MIXPHA.INT[            , , 10]-MIXPHA.INT.Tneg02C[    , , 10]), xlab="Lon [deg]", ylab="Lat [deg]", xlim=c(-95, -83), ylim=c(48, 36), zlim=c(-1, 1))
map("usa",   fill=FALSE, col="grey", bg="white", xlim=c(-95, -83), ylim=c(48, 36), add=TRUE)
map('state', fill=FALSE, col = "grey", add=TRUE)
title("MIXPHA(3DHRRR) - MIXPHA(T=-02C) @ 750 mb, 2/27/2019, 02 UTC")
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg, "*",                   cex=2)
text(NEXRAD.site.lon.deg+0.5, NEXRAD.site.lat.deg+0.5,  NEXRAD.site.name, cex=2)
grid()

image.plot(lon0.INT, lat0.INT, (MIXPHA.INT[            , , 10]-MIXPHA.INT.Tneg12C[    , , 10]), xlab="Lon [deg]", ylab="Lat [deg]", xlim=c(-95, -83), ylim=c(48, 36), zlim=c(-1, 1))
map("usa",   fill=FALSE, col="grey", bg="white", xlim=c(-95, -83), ylim=c(48, 36), add=TRUE)
map('state', fill=FALSE, col = "grey", add=TRUE)
title("MIXPHA(3DHRRR) - MIXPHA(T=-12C) @ 750 mb, 2/27/2019, 02 UTC")
text(NEXRAD.site.lon.deg,     NEXRAD.site.lat.deg, "*",                   cex=2)
text(NEXRAD.site.lon.deg+0.5, NEXRAD.site.lat.deg+0.5,  NEXRAD.site.name, cex=2)
grid()

#########################################
broswer()

  #   HRRR-T profile at flight location
  plot(TEMP_CELSIUS[510, 340, ], HRRR.pres.mb, pch=16, type="b", xlab="T [C]", ylab="P [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, HRRR-T prof @ CNV", sep=""))
  lines(c(-10.55, -10.3, -9.5, -9.2, -9.6, -9.3), c(-668, -674, -683, -695, -705, -720), pch = 18, col = "blue", type = "b", cex=1.2)
  legend("bottomleft", legend=c("HRRR-T", "CNV-T"), col=c("black", "blue"), lty = 1:1, cex=0.8)
  grid()
  
  # plot timeseries of flight segment fields
  dev.new()
  par(mfrow=c(2,2))
  par(mar=c(4, 4, 2, 3))
  plot(CNV.30s.mean.df$lon.deg[ind.hhmm.total], CNV.30s.mean.df$lat.deg[ind.hhmm.total], type="b", lty=1, pch=19, xlab="lon [deg]", ylab="lat [deg]", main=paste(yyyymmdd, " F", flight.num, ", ", hh.min.for.plotting, ":", mm.min.for.plotting, "-", hh.max.for.plotting, ":", mm.max.for.plotting, " Z, type= ", mean(as.numeric(CNV.30s.mean.df$seg.type[ind.hhmm.total])), sep=""))
  text(NEXRAD.site.df$lon[38],       NEXRAD.site.df$lat[38],      "*",    col="blue")
  text(NEXRAD.site.df$lon[38]+0.01,  NEXRAD.site.df$lat[38]+0.01, "KDVN", col="blue")
  grid()

  plot( CNV.30s.mean.df$NEV.LWC.gm3[ind.hhmm.total], ind.hhmm.total, type="b", lty=1, pch=19, col="blue", ylab="time [index #]", xlab="LWC [gm-3]", ylim=c(max(ind.hhmm.total), min(ind.hhmm.total)))
  lines(CNV.30s.mean.df$RID.LWC.gm3[ind.hhmm.total], ind.hhmm.total, type="b", lty=1, pch=19, col="red")
  lines(CNV.30s.mean.df$CDP.LWC.gm3[ind.hhmm.total], ind.hhmm.total, type="b", lty=1, pch=19, col="green")
  abline(h=110, col="orange")
  abline(h=114, col="orange")
  abline(h=118, col="orange")
  abline(h=122, col="orange")
  text(0.04, 108, "Clear air", col="orange")
  text(0.04, 112, "App C & O", col="orange")
  text(0.04, 116, "MIXPHA",    col="orange")
  text(0.02, 120, "App O",     col="orange")
  legend(0.3, 125, c("Nev", "RID", "CDP"), col=c("blue", "red", "green"), pch=c(19, 19, 19))
  grid()

  #plot( CNV.30s.mean.df$alt.m[ind], ind, type="b", lty=1, pch=19, col="blue", ylab="time [hhmmss]", xlab="Alt [m]", ylim=c(max(ind), min(ind)))
  #abline(h=114, col="orange")
  #grid()

  plot( as.numeric(CNV.30s.mean.df$NEV.LWC.gm3[ind.hhmm.total]) - as.numeric(CNV.30s.mean.df$NEV.TWC.gm3[ind.hhmm.total]), ind.hhmm.total, type="b", lty=1, pch=19, col="blue", ylab="time [index #]", xlab="LWC [gm-3]", xlim=c(-0.1, 0.1), ylim=c(max(ind.hhmm.total), min(ind.hhmm.total)))
  lines(as.numeric(CNV.30s.mean.df$RID.LWC.gm3[ind.hhmm.total]) - as.numeric(CNV.30s.mean.df$CDP.LWC.gm3[ind.hhmm.total]), ind.hhmm.total, type="b", lty=1, pch=19, col="purple")
  abline(h=110, col="orange")
  abline(h=114, col="orange")
  abline(h=118, col="orange")
  abline(h=122, col="orange")
  text(-0.07, 108, "Clear air", col="orange")
  text(-0.07, 112, "App C & O", col="orange")
  text(-0.07, 116, "MIXPHA",    col="orange")
  text(-0.07, 120, "App O",     col="orange")
  legend(0.015, 130, c("Nev.LWC-Nev.TWC", "RID.LWC-CDP.LWC"), col=c("blue", "purple"), pch=c(19, 19))
  grid()

  plot( CNV.30s.mean.df$dmax.85.per.L.um[ind.hhmm.total], ind.hhmm.total, type="b", lty=1, pch=19, col="blue", ylab="time [index #]", xlab="Dmax [um]", ylim=c(max(ind.hhmm.total), min(ind.hhmm.total)), xlim=c(0, max(as.numeric(CNV.30s.mean.df$dmax.85.per.L.um[ind.hhmm.total]))))
  lines(CNV.30s.mean.df$MVD.um[ind.hhmm.total],           ind.hhmm.total, type="b", lty=1, pch=19, col="black", xlim=c(0, max(as.numeric(CNV.30s.mean.df$dmax.85.per.L.um[ind.hhmm.total]))))
  abline(v=100, col="red")
  abline(h=110, col="orange")
  abline(h=114, col="orange")
  abline(h=118, col="orange")
  abline(h=122, col="orange")
  text(20, 108, "Clear air", col="orange")
  text(20, 112, "App C & O", col="orange")
  text(20, 116, "MIXPHA",    col="orange")
  text(20, 120, "App O",     col="orange")
  text(70,  140, "AppC",     col="red")
  text(130, 140, "AppO",     col="red")
  legend(180, 135, c("MVD.um", "Dmax.85perm3.um"), col=c("black", "blue"), pch=c(19, 19))
  grid()
  
  # quick sample plots
  
  # prep data for plotting
  FZDZ.INT[is.na(FZDZ.INT)] <- NaN
  ind.HRRR.pres.mb          <- 9
  
  #   ICICLE full domain
  #image.plot(lon0, lat0, SLW.INT[,,ind.HRRR.pres.mb], zlim=c(0,1), main=paste("yyyymmdd, F##, hh:mm UTC, RadIA-mos:ver2:FZDZ, P-lev=5", sep=""))
  image.plot(lon0.INT[655:675, 335:355], lat0.INT[655:675, 335:355], SLW.INT[655:675, 335:355, ind.HRRR.pres.mb], zlim=c(0,1), main=paste("yyyymmdd, F##, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, P-lev=5", sep=""))
  lines(CNV.30s.mean.df$lon.deg, CNV.30s.mean.df$lat.deg, type="l")
  #grid()
  
  #   ICICLE full domain, horiz slice
  #     for INTs
  ################### NEED TO AUTOMATE yyyymmdd & flight in title ##################################
  #
  image.plot(lon0.INT, lat0.INT, FZDZ.INT[ , , ind.HRRR.pres.mb], xlim=c(-95, -83), ylim=c(36, 47), xlab="Lon [deg]", ylab="Lat [deg]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, P=", abs(HRRR.pres.mb[ind.HRRR.pres.mb]), "mb",  sep=""))
  lines(CNV.30s.mean.df$lon.deg, CNV.30s.mean.df$lat.deg, type="l")
  text(NEXRAD.site.df$lon, NEXRAD.site.df$lat, "*", col="grey", cex=2)
  text(NEXRAD.site.df$lon+0.4, NEXRAD.site.df$lat+0.4, NEXRAD.site.df$ICAO, col="grey", cex=1)
  text(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg, "*", col="black", cex=2)
  text(NEXRAD.pri.lon.deg+0.4, NEXRAD.pri.lat.deg+0.4, NEXRAD.pri.name, col="black", cex=1)
  grid()
  #
  image.plot(lon0.INT, lat0.INT, SLW.INT[ , , ind.HRRR.pres.mb], xlim=c(-95, -83), ylim=c(36, 47), xlab="Lon [deg]", ylab="Lat [deg]", main=paste("20190224, F21, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:SLW, P=", abs(HRRR.pres.mb[ind.HRRR.pres.mb]), "mb",  sep=""))
  lines(CNV.30s.mean.df$lon.deg, CNV.30s.mean.df$lat.deg, type="l")
  text(NEXRAD.site.df$lon, NEXRAD.site.df$lat, "*", col="grey", cex=2)
  text(NEXRAD.site.df$lon+0.4, NEXRAD.site.df$lat+0.4, NEXRAD.site.df$ICAO, col="grey", cex=1)
  text(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg, "*", col="black", cex=2)
  text(NEXRAD.pri.lon.deg+0.4, NEXRAD.pri.lat.deg+0.4, NEXRAD.pri.name, col="black", cex=1)
  grid()
  # SLW zoom
  image.plot(lon0.INT, lat0.INT, SLW.INT[ , , ind.HRRR.pres.mb], xlim=c(-86, -84), ylim=c(41, 43), xlab="Lon [deg]", ylab="Lat [deg]", main=paste("20190224, F21, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:SLW, P=", abs(HRRR.pres.mb[ind.HRRR.pres.mb]), "mb",  sep=""))
  lines(CNV.30s.mean.df$lon.deg, CNV.30s.mean.df$lat.deg, type="l")
  text(NEXRAD.site.df$lon, NEXRAD.site.df$lat, "*", col="grey", cex=2)
  text(NEXRAD.site.df$lon+0.1, NEXRAD.site.df$lat+0.1, NEXRAD.site.df$ICAO, col="grey", cex=1)
  text(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg, "*", col="black", cex=2)
  text(NEXRAD.pri.lon.deg+0.1, NEXRAD.pri.lat.deg+0.1, NEXRAD.pri.name, col="black", cex=1)
  grid()
  #
  image.plot(lon0.INT, lat0.INT, TEMP_CELSIUS[ , , ind.HRRR.pres.mb], xlim=c(-95, -83), ylim=c(36, 47), xlab="Lon [deg]", ylab="Lat [deg]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:TEMP, P=", abs(HRRR.pres.mb[ind.HRRR.pres.mb]), "mb",  sep=""))
  lines(CNV.30s.mean.df$lon.deg, CNV.30s.mean.df$lat.deg, type="l")
  text(NEXRAD.site.df$lon, NEXRAD.site.df$lat, "*", col="grey", cex=2)
  text(NEXRAD.site.df$lon+0.4, NEXRAD.site.df$lat+0.4, NEXRAD.site.df$ICAO, col="grey", cex=1)
  text(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg, "*", col="black", cex=2)
  text(NEXRAD.pri.lon.deg+0.4, NEXRAD.pri.lat.deg+0.4, NEXRAD.pri.name, col="black", cex=1)
  grid()
  
  CNV.pres         <- c(  -725,   -670,   -904,   -963,   -918,   -866)
  CNV.lon          <- c(-89.80, -90.27, -90.53, -90.57, -90.10, -89.70)
  
  ## attempt to extract reverse diagonal of SLW.INT
  #r                <- 660
  #c                <- 338
  #SLW.INT.revdiag  <- SLW.INT[row(SLW.INT[,,ind.HRRR.pres.mb]) + col(SLW.INT[,,ind.HRRR.pres.mb]) == r + c ]
  
  #   ICICLE F17 KDVN domain, vert slice
  #     DBZ.MOMS
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), DBZ.MOMS[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-25, 25), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     ZDR.MOMS
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), ZDR.MOMS[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-3, 3), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     RHO.MOMS
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), RHO.MOMS[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0.80, 1.00), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     KDP.MOMS
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), KDP.MOMS[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 2), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     INTs FZDZ
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), FZDZ.INT[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:SLW, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(c(-90.4),              c(-720),               "*",             col="black", cex=2)
  text(c(-90.4+0.4),          c(-720+40),            "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     INTs SLW
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), SLW.INT[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  #text(-91, -300, "12:28 koro: MIXPHA, dom by ice , w/ FZDZ")
  #text(-91, -350, "12:30 greg: good mix of drop sizes, FZDZ")
  #text(-91, -400, "12:30 koro: FZDZ, DZ generating layer")
  #text(-90.5, -300, "12:38 koro: FZDZ, DZ generating layer")
  #text(-90.5, -350, "12:42 koro: SLD at cld top, quasi-adiab LWC prof, no ice")
  #text(-90.5, -300, "12:48 koro: Dmax ~600 um, all liq")
  #text(-90.5, -350, "12:50 koro: below cld base Dmax ~1mm, gnd vis below")
  #text(-90.5, -400, "12:51 wolde: no ice during dcnt, all SLD/FZDZ")
  #text(-90.5, -450, "12:54 koro: FZDZ only, Dmax ~500um")
  #text(-90.5, -500, "12:56 greg: impressive donuts on CPI")
  #text(-90.5, -300, "13:04 koro: less FZDZ on this run")
  #text(-90.5, -300, "13:10 greg: neither FZDZ or ice atm")
  #text(-90.5, -350, "13:12 koro: sparse precip ice")
  #text(-90.5, -400, "13:13 wolde: all ice @ FL")
  #text(-90.5, -300, "13:19 wolde: sust liq layer @ 800m above AC, start ascent into it")
  #text(-90.5, -350, "13:23 wolde: back in FZDZ")
  #text(-90.5, -400, "13:24 koro: juicy FZDZ")
  grid()
  #     INTs MIXPHA
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), MIXPHA.INT[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:MPHA, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[1],            CNV.pres[1],               "*",             col="black", cex=2)
  text(CNV.lon[1],            CNV.pres[1]+20,            "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  #text(-91, -300, "12:28 koro: MIXPHA, dom by ice , w/ FZDZ")
  #text(-91, -350, "12:30 greg: good mix of drop sizes, FZDZ")
  #text(-91, -400, "12:30 koro: FZDZ, DZ generating layer")
  grid()
  #     INTs PLATES
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), PLATES.INT[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:PLAT, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(c(-90.4),              c(-720),               "*",             col="black", cex=2)
  text(c(-90.4+0.4),          c(-720+40),            "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     HRRR TEMP
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), TEMP_CELSIUS[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-20,0), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:TEMP, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(c(-90.4),              c(-720),               "*",             col="black", cex=2)
  text(c(-90.4+0.4),          c(-720+40),            "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     FZDZ INT components
  par(mfrow=c(2,3))
  par(mar=c(5, 5, 5, 5))
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.T.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:int.T.DBZ.FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.mean.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.mean.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.sdev.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.sdev.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.block.median.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.block.median.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.block.sdev.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.block.sdev.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     FZDZ stat components
  par(mfrow=c(2,3))
  par(mar=c(5, 5, 5, 5))
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), T.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0,30), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:T.DBZ.FZDZ, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), mean.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-25, 25), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("mean.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), sdev.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 15), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("sdev.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), block.median.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-25, 25), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("block.median.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), block.sdev.DBZ.FZDZ[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 15), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("block.sdev.DBZ.FZDZ", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     SLW INT components
  par(mfrow=c(2, 2))
  par(mar=c(5, 5, 5, 5))
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.mean.ZDR.corr.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:int.mean.ZDR.corr.SLW, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.sdev.ZDR.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.sdev.ZDR.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.mean.KDP.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.mean.KDP.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), int.sdev.KDP.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("int.sdev.KDP.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  #     SLW stat components
  par(mfrow=c(2, 2))
  par(mar=c(5, 5, 5, 5))
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), mean.ZDR.corr.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-3, 3), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, RadIA-mos:ver2:mean.ZDR.corr.SLW, Lat[deg]=", round(lat0[478,248], digits=2), sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), sdev.ZDR.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(-3, 3), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("sdev.ZDR.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), mean.KDP.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 0.5), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("mean.KDP.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  image.plot(replicate(40, lon0.MOMS[, 340]), t(replicate(1001, HRRR.pres.mb)), sdev.KDP.SLW[, 340, ], xlim=c(-92, -89), ylim=c(-1000, -300), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]", main=paste("sdev.KDP.SLW", sep=""))
  text(CNV.lon[6],            CNV.pres[6],           "*",             col="black", cex=2)
  text(CNV.lon[6],            CNV.pres[6]+20,        "CNV",           col="black", cex=1)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]),    "*",             col="black", cex=2)
  text(c(NEXRAD.pri.lon.deg), c(HRRR.pres.mb[1]-40), NEXRAD.pri.name, col="black", cex=1)
  grid()
  
  #   HRRR-T profile at flight location
  plot(TEMP_CELSIUS[510, 340, ], HRRR.pres.mb, pch=16, type="b", xlab="T [C]", ylab="P [mb]", main=paste("20190217, F17, ", substr(radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], 1, 4), " UTC, HRRR-T prof @ CNV", sep=""))
  lines(c(-10.55, -10.3, -9.5, -9.2, -9.6, -9.3), c(-668, -674, -683, -695, -705, -720), pch = 18, col = "blue", type = "b", cex=1.2)
  legend("bottomleft", legend=c("HRRR-T", "CNV-T"), col=c("black", "blue"), lty = 1:1, cex=0.8)
  grid()
  
  #sink()
  closeAllConnections()
  


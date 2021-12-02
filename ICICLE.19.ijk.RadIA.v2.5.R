#--------------------------------------------
#
# Name:    ICICLE.19.ijk.RadIA.v2.5.R
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
base.path.lcl.dir                   <- file.path("/d1/serke/projects/case_studies/")
#base.path.ext.dir                   <- file.path("/media/ice_d3/projects/case_studies/")

# define NEXRAD site location csv file path
nexrad.site.dataframetxt.dir        <- file.path(paste(base.path.lcl.dir, "SNOWIE_2017/data/RadIA_data/nexrad_site_data/", sep=""))

# define output matchup data file path
output.matchup.ijk.v2.2.dir         <- file.path(paste(base.path.lcl.dir, field.campaign, "/data/ICICLE.7_output/ver2.2/", sep=""))
#output.matchup.ALLFLIGHTS.v2.5.dir       <- file.path(paste(base.path.lcl.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/full_flight_clo_pix/CNV_v", latest.CNV.30smean.ver.num, "/", sep=""))

# define ALLFLIGHTS csv data filename
output.matchup.ijk.v2.2.listfiles   <- list.files(path = output.matchup.ijk.v2.2.dir , include.dirs = FALSE)
output.matchup.ijk.v2.2.filename    <- output.matchup.ijk.v2.2.listfiles[1]
output.matchup.ijk.allyson.filename <- output.matchup.ijk.v2.2.listfiles[8]
#output.matchup.ALLFLIGHTS.v2.5.listfiles <- list.files(path = output.matchup.ALLFLIGHTS.v2.5.dir , include.dirs = FALSE)
#output.matchup.ALLFLIGHTS.v2.5.filename  <- output.matchup.ALLFLIGHTS.v2.5.listfiles[1]

#---------------------------------------------
# Start writing debugging information to an output file
#---------------------------------------------
# Print some pre-processing info
print("---------------------------------------")
print(paste("Available ", field.campaign, " matchup files..."))
print("---------------------------------------")
print(paste("  Field Campaign: ",           field.campaign,                     sep=""))
print(paste("  ALLFLIGHTS v2.2 data filename: ", output.matchup.ijk.v2.2.filename, sep=""))
print(paste("  ALLFLIGHTS v2.2 Allyson data filename: ", output.matchup.ijk.allyson.filename, sep=""))
print("---------------------------------------")

#-----------------------------------------------
# load ALLFLIGHTS Rm version number csv data files 
#-----------------------------------------------
# .. for Rmv2.2
print(paste("    Loading ijk Rmv2.2 csv data file...", sep=""))
ijk.v2.2.df  <- read.csv(paste(output.matchup.ijk.v2.2.dir, output.matchup.ijk.v2.2.filename, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
head(ijk.v2.2.df)
#str(ALLFLIGHTS.v2.2.df)

print(paste("    Loading ijk allyson csv data file...", sep=""))
ijk.allyson.df  <- read.csv(paste(output.matchup.ijk.v2.2.dir, output.matchup.ijk.allyson.filename, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
head(ijk.allyson.df)

# .. for RMv2.5
#print(paste("    Loading ALLFLIGHTS RMv2.5 csv data file...", sep=""))
#ALLFLIGHTS.v2.5.df  <- read.csv(paste(output.matchup.ALLFLIGHTS.v2.5.dir, output.matchup.ALLFLIGHTS.v2.5.filename, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
#head(ALLFLIGHTS.v2.5.df)
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

#----------------------------------------------
#
# plotting
#
#----------------------------------------------
par(mfrow = c(3, 1))
par(mar   = c(6, 6, 3, 3))
plot(ijk.v2.2.df$grid.i, xlab="mm", ylab="grid.i",              cex.axis=3, cex.lab=3, col="black", pch=20, cex=2, type="p", main="ICICLE F17 2/17/2019 : Dave (black) and Allyson (red)")
lines(ijk.allyson.df$i.index[25:451], xlab="mm", ylab="grid.i", cex.axis=3, cex.lab=3, col="red",   pch=20, cex=2, type="p")
grid()
plot(ijk.v2.2.df$grid.j,              xlab="mm", ylab="grid.j", cex.axis=3, cex.lab=3, col="black", pch=20, cex=2, type="p")
lines(ijk.allyson.df$j.index[25:451], xlab="mm", ylab="grid.j", cex.axis=3, cex.lab=3, col="red",   pch=20, cex=2, type="p")
legend("topright", legend=c("Dave's", "Allyson's"), col=c("black", "red"), lwd=3, lty = 1:1, cex=4)
grid()
plot(ijk.v2.2.df$grid.k,              xlab="mm", ylab="grid.k", cex.axis=3, cex.lab=3, col="black", pch=20, cex=2, type="p")
lines(ijk.allyson.df$k.index[25:451], xlab="mm", ylab="grid.k", cex.axis=3, cex.lab=3, col="red",   pch=20, cex=2, type="p")
grid()

par(mfrow = c(1, 3))
par(mar   = c(6, 6, 3, 3))
hist(ijk.v2.2.df$grid.i - ijk.allyson.df$i.index[25:451], cex.axis=3, cex.lab=3, xlim=c(-5,5), ylim=c(0,500), xlab="diff")
grid()
hist(ijk.v2.2.df$grid.j - ijk.allyson.df$j.index[25:451], cex.axis=3, cex.lab=3, xlim=c(-5,5), ylim=c(0,500), xlab="diff")
grid()
hist(ijk.v2.2.df$grid.k - ijk.allyson.df$k.index[25:451], cex.axis=3, cex.lab=3, xlim=c(-5,5), ylim=c(0,500), xlab="diff")
grid()

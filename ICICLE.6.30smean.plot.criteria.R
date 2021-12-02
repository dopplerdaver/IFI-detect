#--------------------------------------------
#
# Name:    ICICLE.6.30smean.plot.criteria.R
#
# Purpose: 1) load AC 30-sec averaged (ver3) txt files
#          2) 
#          3) make plots
#
# How to run:
#
#
# Created: 10.22.2020 dserke
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

#   mosaic grid parameters
mosaic.alt.array                    <- c(0.5, 0.75, 1.00, 1.25, 1.50, 1.75, 2.00, 2.25, 2.50, 2.75, 3.00, 3.50, 4.00, 4.50, 5.00, 5.50, 6.00, 6.50, 7.00, 7.50, 8.00, 8.50, 9.00, 10.00)
mosaic.lat.array                    <- seq(from =  ICICLE.min.lat, to =  ICICLE.max.lat-0.01, by = 0.01)
mosaic.lon.array                    <- seq(from = -ICICLE.max.lon, to = -ICICLE.min.lon-0.01, by = 0.01)

# mosaic version 1 input data information
#   latest mosaic data version number
#latest.mosaic.ver.num               <- 1             # as of 06.17.2020, produced by Dave A. late in 2019
#latest.mosaic.ver.num               <- 2.1           # as of 08.27.2020, produced by Dave A.
latest.mosaic.ver.num               <- 2.2           # as of 09.21.2020, produced by Dave A., but science team should be able to process as of this date
#latest.mosaic.ver.num               <- 3             # as of ??.??.????, produced by Dave S. and Scott E., first outputted version produced by science team

# aircraft CNV 30-s mean input data information
#   latest data version number
#latest.30s.mean.ver.num            <- 1               # as of 04.01.2020, original output from Allyson
#latest.30s.mean.ver.num            <- 2               # as of 06.10.2020, fixed column headers, Dmax pass through corrected
#latest.30s.mean.ver.num            <- 3               # as of 10.22.2020, when ECCC sends SNDI-like 2DS field data
latest.30s.mean.ver.num             <- 4              # as of 12.16.2020, upgraded QC, aircraft state parameters use best available source

# aircraft CNV NAW nc data information
#   latest zen/nad data version number
latest.NAW.zennad.ver.num           <- 1              # as of 08.13.2020
#   latest horiz data version number
latest.NAW.horiz.ver.num            <- 1              # as of 08.19.2020

# aircraft CNV homog IFI csv data information
#   latest data version number
latest.homog.IFI.ver.num            <- 1              # as of 08.19.2020

#   mosaic matchup volume parameters
if (latest.mosaic.ver.num == 1) {
  ind.pixels.around.closest.lat       <- -5:5
  ind.pixels.around.closest.lon       <- -5:5
  ind.pixels.around.closest.alt       <- -1:1
} else if (latest.mosaic.ver.num >= 2) {
  ind.pixels.around.closest.lat       <- -3:3
  ind.pixels.around.closest.lon       <- -3:3
  ind.pixels.around.closest.pres      <- -1:1
} # end of if (latest.mosaic.ver.num == 1)

# user-defined case flight information
#   params for ALL available ICICLE cases
#fl.num                              <- "ALL"

#   params for 20190217 F07 case
#yyyymmdd                            <- "2019-02-05"
#flight.num                          <- 7
#hh.for.plotting                     <- 20
#mm.min.for.plotting                 <- 40
#mm.max.for.plotting                 <- 59

# params for single case processing
##   F06
#flight.num                          <- 6 
#yyyy.mm.dd                          <- "20190204"
##   F07
#flight.num                          <- 7 
#yyyy.mm.dd                          <- "20190205"
##   F08
#flight.num                          <- 8 
#yyyy.mm.dd                          <- "20190206"
##   F09
#flight.num                          <- 9 
#yyyy.mm.dd                          <- "20190207"
##   F10 was a clear air ferry flight to Ottawa for repairs
##   F11
#flight.num                          <- 11 
#yyyy.mm.dd                          <- "20190211"
##   F12
#flight.num                          <- 12 
#yyyy.mm.dd                          <- "20190212"
##   F13
#flight.num                          <- 13 
#yyyy.mm.dd                          <- "20190212"
##   F14
#flight.num                          <- 14 
#yyyy.mm.dd                          <- "20190214"
##   F15
#flight.num                          <- 15 
#yyyy.mm.dd                          <- "20190215"
##   F16
#flight.num                          <- 16 
#yyyy.mm.dd                          <- "20190216"
##   F17
flight.num                          <- 17 
yyyy.mm.dd                          <- "20190217"
##   F18
#flight.num                          <- 18 
#yyyy.mm.dd                          <- "20190217"
##   F19
#flight.num                          <- 19 
#yyyy.mm.dd                          <- "20190222"
##   F20  RADIA-MOSAIC V@ DATA MISSING
#flight.num                          <- 20 
#yyyy.mm.dd                          <- "20190223"
##   F21
#flight.num                          <- 21 
#yyyy.mm.dd                          <- "20190224"
##   F22
#flight.num                          <- 22 
#yyyy.mm.dd                          <- "20190224"
##   F23
#flight.num                          <- 23 
#yyyy.mm.dd                          <- "20190226"
##   F24
#flight.num                          <- 24
#yyyy.mm.dd                          <- "20190226"
#yyyy.mm.dd                          <- "20190227"
##   F25
#flight.num                          <- 25 
#yyyy.mm.dd                          <- "20190228"
##   F26
#flight.num                          <- 26 
#yyyy.mm.dd                          <- "20190302"
##   F27
#flight.num                          <- 27 
#yyyy.mm.dd                          <- "20190302"
##   F28
#flight.num                          <- 28 
#yyyy.mm.dd                          <- "20190305"
##   F29
#flight.num                          <- 29 
#yyyy.mm.dd                          <- "20190307"

# constants definitions 
radius.earth.km                     <- 6378.1         # [km]
degPERcircle                        <-  360.0         # [deg]

# unit conversion definitions
kmPERm                              <- 0.001          # convert m to km
kmPERrangegate                      <- 0.250          

# flag, set initial value for whether match found for flight/time of NAW nc file
CNV.NAW.zennad.nc.file.match.flag   <- 0

#----------------------------------------------
# define data paths, list data files in path
#----------------------------------------------
# define base path dir
base.path.dir                        <- file.path("/d1/serke/projects/case_studies/")
base.path.ext.dir                    <- file.path("/media/ice_d3/d1/serke/projects/case_studies/")

# define NEXRAD site location csv file path
nexrad.site.dataframetxt.dir         <- file.path(paste(base.path.dir, "SNOWIE_2017/data/RadIA_data/nexrad_site_data/", sep=""))

# define NRC CNV homogeneous IFI conditions ver 1 csv format file path
CNV.homog.IFI.dir                    <- file.path(paste(base.path.dir, "ICICLE_2019/data/CNV_homog_IFI/ver", latest.homog.IFI.ver.num, "/", sep=""))
CNV.homog.IFI.csv.listfiles          <- list.files(path = CNV.homog.IFI.dir , include.dirs = FALSE) 
#############################################################################################
##### testing with F09 App C time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[1]

##### testing with F09 MIXPHA time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[2]

##### testing with F12 MIXPHA time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[3]

##### testing with F14 App C time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[4]

#### testing with F14 ICEONLY time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[5]

##### testing with F15 MIXPHA time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[6]

##### testing with F16 MIXPHA time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[7]

##### testing with F17 ICEONLY time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[8]

##### testing with F17 App O time periods only
CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[10]

##### testing with F18 App C time period only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[9]

##### testing with F21 App C time period only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[11]

##### testing with F22 ICEONLY time period only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[12]

##### testing with F22 3x MIXPHA time periods only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[13]

##### testing with F22 App C time period only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[14]

##### testing with F22 ICEONLY time period only
#CNV.homog.IFI.csv.listfiles          <- CNV.homog.IFI.csv.listfiles[15]

#print(CNV.homog.IFI.csv.listfiles)
#############################################################################################
# define NRC CNV version X csv format file path
CNV.30s.mean.dir                     <- file.path(paste(base.path.dir, field.campaign, "/data/CNV_30s_mean/ver", latest.30s.mean.ver.num, "/", sep=""))
CNV.30s.mean.listfiles               <- list.files(path = CNV.30s.mean.dir, include.dirs = FALSE)

# define NRC CNV NAW zen/nad/side ver 1 nc format file path
CNV.NAW.zennad.dir                   <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/20201029_NAWdata/nc/", sep=""))
CNV.NAW.zennad.nc.listfiles          <- list.files(path = CNV.NAW.zennad.dir , include.dirs = FALSE) 

# define NRC CNV NAX horiz ver 1 nc format file path
CNV.NAX.horiz.dir                    <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/20201110_NAXdata/nc/", sep=""))
CNV.NAX.horiz.nc.listfiles           <- list.files(path = CNV.NAX.horiz.dir , include.dirs = FALSE)

# define NRC CNV lidar ver 1 nc format file path
CNV.LIDAR.dir                        <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/20201029_LIDARdata/nc/", sep=""))
CNV.LIDAR.nc.listfiles               <- list.files(path = CNV.LIDAR.dir , include.dirs = FALSE)

## define RadIA-mosaic version 1 nc format interest file paths
if (latest.mosaic.ver.num == 1) {
#  #radia.mosaic.nc.FZDZ.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/FZDZ/",   substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.FZDZ.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/FZDZ/",   substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.FZDZ.listfiles       <- list.files(path = radia.mosaic.nc.FZDZ.dir ,   include.dirs = FALSE) 
#  #radia.mosaic.nc.SLW.dir              <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/SLW/",  substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.SLW.dir              <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/SLW/",  substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.SLW.listfiles        <- list.files(path = radia.mosaic.nc.SLW.dir ,    include.dirs = FALSE)
#  #radia.mosaic.nc.MIXPHA.dir           <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/MIXPHA/", substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.MIXPHA.dir           <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/MIXPHA/", substr(yyyymmdd,1,4), "-", substr(yyyymmdd,5,6), "-", substr(yyyymmdd,7,8), "/", sep=""))
  radia.mosaic.nc.MIXPHA.listfiles     <- list.files(path = radia.mosaic.nc.MIXPHA.dir , include.dirs = FALSE)
} else if (latest.mosaic.ver.num >= 2) {
  radia.mosaic.INTS.nc.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/main/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.INTS.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/algo_ints/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.INTS.nc.listfiles       <- list.files(path = radia.mosaic.INTS.nc.dir ,   include.dirs = FALSE)
  radia.mosaic.FZDZ.nc.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/frzdrz/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.FZDZ.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/fzdz_flds/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.FZDZ.nc.listfiles       <- list.files(path = radia.mosaic.FZDZ.nc.dir ,   include.dirs = FALSE)
  radia.mosaic.SLW.nc.dir              <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/slw/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.SLW.nc.dir              <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/slw_flds/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.SLW.nc.listfiles        <- list.files(path = radia.mosaic.SLW.nc.dir ,    include.dirs = FALSE)
  radia.mosaic.MIXPHA.nc.dir           <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/mixpha/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.MIXPHA.nc.dir           <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/mpha_flds/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.MIXPHA.nc.listfiles     <- list.files(path = radia.mosaic.MIXPHA.nc.dir , include.dirs = FALSE)
  radia.mosaic.PLATES.nc.dir           <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/plates/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.PLATES.nc.dir           <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/plat_flds/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.PLATES.nc.listfiles     <- list.files(path = radia.mosaic.PLATES.nc.dir , include.dirs = FALSE)
  radia.mosaic.MOMS.nc.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/passthrough/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.MOMS.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/raw_moms/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.MOMS.nc.listfiles       <- list.files(path = radia.mosaic.MOMS.nc.dir ,   include.dirs = FALSE)
} else {
  print("Error")
}
  
# flag (binary) definitions   
#   ...input time periods
#input.use.CNV.homog.IFI.flag         <- 1
input.use.CNV.homog.IFI.flag         <- 0
#   ...outputs
output.matchup.data.flag             <- 1
output.matchup.image.flag            <- 0

# define output matchup data file path
output.matchup.data.dir              <- file.path(paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", sep=""))

## define output matchup plots file path
#output.matchup.image.dir             <- file.path(paste(base.path.dir, field.campaign, "/images/flights_indiv/", as.character(yyyymmdd), "_F", flight.num, "/ver",  latest.30s.mean.ver.num, "/", sep=""))

#---------------------------------------------
# Start writing debugging information to an output file
#---------------------------------------------
#sink(paste(output.matchup.data.dir, "print.output.txt", sep = ""))

# Print some pre-processing info
print("---------------------------------------")
print(paste("Available ", field.campaign, " flight files..."))
print(paste("  CNV 30-s mean files are:",             sep=""))
print(paste("   ", CNV.30s.mean.listfiles,            sep=""))
print(paste("  CNV RadIA FZDZ files are:",            sep=""))
print(paste("   ", radia.mosaic.FZDZ.nc.listfiles,    sep=""))
print(paste("  CNV NAW zenith/nadir/side files are:", sep=""))
print(paste("   ", CNV.NAW.zennad.nc.listfiles,       sep=""))
print(paste("  CNV NAX files are:",                   sep=""))
print(paste("   ", CNV.NAX.horiz.nc.listfiles,        sep=""))
print(paste("  CNV lidar files are:",                 sep=""))
print(paste("   ", CNV.LIDAR.nc.listfiles,            sep="")) 
print("---------------------------------------")
print(paste("Processing parameters, as defined by user..."), sep="")
print(paste("  Field Campaign: ", field.campaign, sep=""))
#print(paste("  Case: ", yyyymmdd, ", F#", flight.num, ", ", hh.min.for.plotting, ":", mm.min.for.plotting, " to ", hh.max.for.plotting, ":", mm.max.for.plotting, " UTC", sep=""))
print(paste("  Flag values:"), sep="")
print(paste("    Output matched data = ",  output.matchup.data.flag,  sep=""))
print(paste("    Output matched image = ", output.matchup.image.flag, sep=""))
print("---------------------------------------")

#---------------------------------------------
# calculate (hh*60)*mm for RadIA-mosaic version 1 nc files
#   use this value later to find closest time to each CNV flight track time
#---------------------------------------------
if (latest.mosaic.ver.num == 1) {
  
  mm.tot.mosaic.FZDZ.file              <- array(data = NA, dim = c(length(radia.mosaic.nc.FZDZ.listfiles), 1))
  #   calc tot mins of each mosaic FZDZ file times (hh*mm)
  for (b in 1:length(radia.mosaic.nc.FZDZ.listfiles)) {
    hh.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.FZDZ.listfiles[b], 10, 11))
    mm.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.FZDZ.listfiles[b], 12, 13))
    mm.tot.mosaic.FZDZ.file[b]   <- (hh.mosaic.file * 60) + mm.mosaic.file
  }  # end of for (b...)
  mm.tot.mosaic.SLW.file               <- array(data = NA, dim = c(length(radia.mosaic.nc.SLW.listfiles), 1))
  #   calc tot mins of each mosaic FZDZ file times (hh*mm)
  for (c in 1:length(radia.mosaic.nc.FZDZ.listfiles)) {
    hh.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.SLW.listfiles[c], 10, 11))
    mm.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.SLW.listfiles[c], 12, 13))
   mm.tot.mosaic.SLW.file[c]    <- (hh.mosaic.file * 60) + mm.mosaic.file
  }  # end of for (c...)
  mm.tot.mosaic.MIXPHA.file            <- array(data = NA, dim = c(length(radia.mosaic.nc.MIXPHA.listfiles), 1))
  #   calc tot mins of each mosaic FZDZ file times (hh*mm)
  for (d in 1:length(radia.mosaic.nc.MIXPHA.listfiles)) {
    hh.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.MIXPHA.listfiles[d], 10, 11))
    mm.mosaic.file               <- as.numeric(substr(radia.mosaic.nc.MIXPHA.listfiles[d], 12, 13))
    mm.tot.mosaic.MIXPHA.file[d] <- (hh.mosaic.file * 60) + mm.mosaic.file
  }  # end of for (d...)
  
} else if (latest.mosaic.ver.num >= 2) {
  
  mm.tot.mosaic.INTS.file              <- array(data = NA, dim = c(length(radia.mosaic.INTS.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.INTS.nc.listfiles)) {
    hh.mosaic.INTS.file          <- as.numeric(substr(radia.mosaic.INTS.nc.listfiles[b], 1, 2))
    mm.mosaic.INTS.file          <- as.numeric(substr(radia.mosaic.INTS.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.INTS.file[b]   <- (hh.mosaic.INTS.file * 60) + mm.mosaic.INTS.file
  }  # end of for (b...)
  mm.tot.mosaic.MOMS.file              <- array(data = NA, dim = c(length(radia.mosaic.MOMS.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.MOMS.nc.listfiles)) {
    hh.mosaic.MOMS.file          <- as.numeric(substr(radia.mosaic.MOMS.nc.listfiles[b], 1, 2))
    mm.mosaic.MOMS.file          <- as.numeric(substr(radia.mosaic.MOMS.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.MOMS.file[b]   <- (hh.mosaic.MOMS.file * 60) + mm.mosaic.MOMS.file
  }  # end of for (b...)
  mm.tot.mosaic.FZDZ.file              <- array(data = NA, dim = c(length(radia.mosaic.FZDZ.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.FZDZ.nc.listfiles)) {
    hh.mosaic.FZDZ.file          <- as.numeric(substr(radia.mosaic.FZDZ.nc.listfiles[b], 1, 2))
    mm.mosaic.FZDZ.file          <- as.numeric(substr(radia.mosaic.FZDZ.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.FZDZ.file[b]   <- (hh.mosaic.FZDZ.file * 60) + mm.mosaic.FZDZ.file
  }  # end of for (b...)
  mm.tot.mosaic.SLW.file               <- array(data = NA, dim = c(length(radia.mosaic.SLW.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.SLW.nc.listfiles)) {
    hh.mosaic.SLW.file          <- as.numeric(substr(radia.mosaic.SLW.nc.listfiles[b], 1, 2))
    mm.mosaic.SLW.file          <- as.numeric(substr(radia.mosaic.SLW.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.SLW.file[b]   <- (hh.mosaic.SLW.file * 60) + mm.mosaic.SLW.file
  }  # end of for (b...)
  mm.tot.mosaic.MIXPHA.file            <- array(data = NA, dim = c(length(radia.mosaic.MIXPHA.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.MIXPHA.nc.listfiles)) {
    hh.mosaic.MIXPHA.file          <- as.numeric(substr(radia.mosaic.MIXPHA.nc.listfiles[b], 1, 2))
    mm.mosaic.MIXPHA.file          <- as.numeric(substr(radia.mosaic.MIXPHA.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.MIXPHA.file[b]   <- (hh.mosaic.MIXPHA.file * 60) + mm.mosaic.MIXPHA.file
  }  # end of for (b...)
  mm.tot.mosaic.PLATES.file            <- array(data = NA, dim = c(length(radia.mosaic.PLATES.nc.listfiles), 1))
  #   calc tot mins of each mosaic file times (hh*mm)
  for (b in 1:length(radia.mosaic.PLATES.nc.listfiles)) {
    hh.mosaic.PLATES.file          <- as.numeric(substr(radia.mosaic.PLATES.nc.listfiles[b], 1, 2))
    mm.mosaic.PLATES.file          <- as.numeric(substr(radia.mosaic.PLATES.nc.listfiles[b], 3, 4))
    mm.tot.mosaic.PLATES.file[b]   <- (hh.mosaic.PLATES.file * 60) + mm.mosaic.PLATES.file
  }  # end of for (b...)
  
} else {
  print("Error")
}

#---------------------------------------------
# calculate (hh*60*60)+(mm*60)+ss for CNV NAW zen/nad version 1 nc files
#   use this value later to find closest time to each CNV flight track time
#---------------------------------------------
yymmdd.start.CNV.NAW.zennad.file       <- array(data = NA, dim = c(length(CNV.NAW.zennad.nc.listfiles), 1))
ss.start.tot.CNV.NAW.zennad.file       <- array(data = NA, dim = c(length(CNV.NAW.zennad.nc.listfiles), 1))
ss.end.tot.CNV.NAW.zennad.file         <- array(data = NA, dim = c(length(CNV.NAW.zennad.nc.listfiles), 1))
#   calc tot ss (sec) of each mosaic FZDZ file times (hh*mm)
for (e in 1:length(CNV.NAW.zennad.nc.listfiles)) {
  yymmdd.start.CNV.NAW.zennad.file[e]        <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 17, 24))
  hh.start.CNV.NAW.zennad.file               <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 26, 27))
  mm.start.CNV.NAW.zennad.file               <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 28, 29))
  ss.start.CNV.NAW.zennad.file               <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 30, 31))
  ss.start.tot.CNV.NAW.zennad.file[e]        <- (hh.start.CNV.NAW.zennad.file * 60 * 60) + (mm.start.CNV.NAW.zennad.file * 60) + ss.start.CNV.NAW.zennad.file
  hh.end.CNV.NAW.zennad.file                 <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 33, 34))
  mm.end.CNV.NAW.zennad.file                 <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 35, 36))
  ss.end.CNV.NAW.zennad.file                 <- as.numeric(substr(CNV.NAW.zennad.nc.listfiles[e], 37, 38))
  ss.end.tot.CNV.NAW.zennad.file[e]          <- (hh.end.CNV.NAW.zennad.file * 60 * 60)   + (mm.end.CNV.NAW.zennad.file * 60)   + ss.end.CNV.NAW.zennad.file
}  # end of for (e...)

#----------------------------------------------
# load ICICLE Campaign-related overview datasets
#----------------------------------------------
print(paste("Manage ICICLE Campaign-related overview datasets...", sep=""))

# load the NEXRAD site location text file
print(paste("  Loading NEXRAD site file from: ", nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep=""))
NEXRAD.site.df                       <- read.csv(paste(nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep = ""), header = FALSE, sep = ",", dec = ".", stringsAsFactors=FALSE)
colnames(NEXRAD.site.df)             <- c("NCDCID", "ICAO", "WBAN", "radname", "COUNTRY", "STATE", "COUNTY", "lat", "lon", "elev", "GMTdiff", "STN_TYPE")
#head(NEXRAD.site.df)

# remove df if it exists
#if (is.data.frame(get(case.time.list.df))) {
#  print("      df exists already, removing")
try(rm(case.time.list.df))

#----------------------------------------------
# load desired info about primary and secondary NEXRAD sites for this case
#----------------------------------------------
## NOTE: MUST MANUALLY LOAD icicle.df from ICICLE.1.flhours.vs.days.R before executing next line of code
#ind.flight.num                     <- which(icicle.df$fl.num == case.time.list.df$flight.num[ppp])

# load case time periods
if (input.use.CNV.homog.IFI.flag == 1) {
  # load CNV homog IFI version 1 csv data file
  print(paste("  Loading CNV homogeneous IFI conditions file named: ", CNV.homog.IFI.dir, CNV.homog.IFI.csv.listfiles, sep=""))
  case.time.list.df           <- read.csv(paste(CNV.homog.IFI.dir, CNV.homog.IFI.csv.listfiles, sep = ""), header = TRUE, sep = ",", dec = ".", stringsAsFactors=FALSE)
  head(case.time.list.df)
} else if (input.use.CNV.homog.IFI.flag == 0) {
  
  # load data for primary and secondary radars
  #for (jj in 2:2) {
  for (jj in 1:2) {
    if (jj == 1) {
      print(paste("  Defining primary NEXRAD location information for F#", flight.num, sep=""))
      ind.fl.num                             <- which(icicle.df$fl.num == flight.num)
      NEXRAD.pri.name                        <- paste(" ", icicle.df$pri.radar.name[ind.fl.num], sep="")
      ind.NEXRAD.pri.site                    <- which(NEXRAD.site.df$ICAO == NEXRAD.pri.name)
      NEXRAD.pri.lat.deg                     <- NEXRAD.site.df$lat[ind.NEXRAD.pri.site]
      NEXRAD.pri.lon.deg                     <- NEXRAD.site.df$lon[ind.NEXRAD.pri.site]
      NEXRAD.pri.elev.m                      <- NEXRAD.site.df$elev[ind.NEXRAD.pri.site]
      NEXRAD.pri.srt.hhmm                    <- icicle.df$pri.radar.srt[ind.fl.num]
      NEXRAD.pri.end.hhmm                    <- icicle.df$pri.radar.end[ind.fl.num]
      NEXRAD.pri.srt.hh                      <- as.numeric(substr(NEXRAD.pri.srt.hhmm, 1, 2))
      NEXRAD.pri.srt.mm                      <- as.numeric(substr(NEXRAD.pri.srt.hhmm, 4, 5))
      NEXRAD.pri.end.hh                      <- as.numeric(substr(NEXRAD.pri.end.hhmm, 1, 2))
      NEXRAD.pri.end.mm                      <- as.numeric(substr(NEXRAD.pri.end.hhmm, 4, 5))
      if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh) {
        mm.diff.for.plotting                   <- ((NEXRAD.pri.end.hh * 60) + NEXRAD.pri.end.mm) + (24*60 - (NEXRAD.pri.srt.hh * 60) + NEXRAD.pri.srt.mm)
      } else {
        mm.diff.for.plotting                   <- ((NEXRAD.pri.end.hh * 60) + NEXRAD.pri.end.mm) - ((NEXRAD.pri.srt.hh * 60) + NEXRAD.pri.srt.mm)
      } 
      case.time.list.df                      <- as.data.frame(cbind(as.character(icicle.df$date.posix[ind.fl.num]), flight.num, NEXRAD.pri.name, 1, NEXRAD.pri.srt.hh, NEXRAD.pri.srt.mm, NEXRAD.pri.end.hh, NEXRAD.pri.end.mm, mm.diff.for.plotting, 1, "NA"), row.names = NULL)
      colnames(case.time.list.df) <- c("yyyymmdd", "flight.num", "pri.radar.name", "truth.cat", "hh.min.for.plotting", "mm.min.for.plotting", "hh.max.for.plotting", "mm.max.for.plotting", "mm.diff.for.plotting", "radia.priority.1to5", "notes.txt")
      #head(case.time.list.df)
      print(paste("    NEXRAD.pri.name     = ", NEXRAD.pri.name,     sep=""))
      print(paste("    NEXRAD.pri.lat.deg  = ", NEXRAD.pri.lat.deg,  sep=""))
      print(paste("    NEXRAD.pri.lon.deg  = ", NEXRAD.pri.lon.deg,  sep=""))
      print(paste("    NEXRAD.pri.elev.m   = ", NEXRAD.pri.elev.m,   sep=""))
      print(paste("    NEXRAD.pri.srt.hhmm = ", NEXRAD.pri.srt.hhmm, sep=""))
      print(paste("    NEXRAD.pri.end.hhmm = ", NEXRAD.pri.end.hhmm, sep=""))
      print(paste("    mm.diff.for.plotting= ", mm.diff.for.plotting,sep=""))
    } else if (jj == 2) {
      print(paste("  Defining secondary NEXRAD location information for F#", flight.num, sep=""))
      ind.fl.num                             <- which(icicle.df$fl.num == flight.num)
      NEXRAD.sec.name                        <- paste(" ", icicle.df$sec.radar.name[ind.fl.num], sep="")
      ind.NEXRAD.sec.site                    <- which(NEXRAD.site.df$ICAO == NEXRAD.sec.name)
      NEXRAD.sec.lat.deg                     <- NEXRAD.site.df$lat[ind.NEXRAD.sec.site]
      NEXRAD.sec.lon.deg                     <- NEXRAD.site.df$lon[ind.NEXRAD.sec.site]
      NEXRAD.sec.elev.m                      <- NEXRAD.site.df$elev[ind.NEXRAD.sec.site]
      NEXRAD.sec.srt.hhmm                    <- icicle.df$sec.radar.srt[ind.fl.num]
      NEXRAD.sec.end.hhmm                    <- icicle.df$sec.radar.end[ind.fl.num]
      NEXRAD.sec.srt.hh                      <- as.numeric(substr(NEXRAD.sec.srt.hhmm, 1, 2))
      NEXRAD.sec.srt.mm                      <- as.numeric(substr(NEXRAD.sec.srt.hhmm, 4, 5))
      NEXRAD.sec.end.hh                      <- as.numeric(substr(NEXRAD.sec.end.hhmm, 1, 2))
      NEXRAD.sec.end.mm                      <- as.numeric(substr(NEXRAD.sec.end.hhmm, 4, 5))
      if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh) {
        mm.diff.for.plotting                   <- ((NEXRAD.sec.end.hh * 60) + NEXRAD.sec.end.mm) + (24*60 - (NEXRAD.sec.srt.hh * 60) + NEXRAD.sec.srt.mm)
      } else {
        mm.diff.for.plotting                   <- ((NEXRAD.sec.end.hh * 60) + NEXRAD.sec.end.mm) - ((NEXRAD.sec.srt.hh * 60) + NEXRAD.sec.srt.mm)
      }
      case.time.list.2.df                    <- as.data.frame(cbind(as.character(icicle.df$date.posix[ind.fl.num]), flight.num, NEXRAD.sec.name, 1, NEXRAD.sec.srt.hh, NEXRAD.sec.srt.mm, NEXRAD.sec.end.hh, NEXRAD.sec.end.mm, mm.diff.for.plotting, 1, "NA"), row.names = NULL)
      colnames(case.time.list.2.df)          <- c("yyyymmdd", "flight.num", "pri.radar.name", "truth.cat", "hh.min.for.plotting", "mm.min.for.plotting", "hh.max.for.plotting", "mm.max.for.plotting", "mm.diff.for.plotting", "radia.priority.1to5", "notes.txt")
      #head(case.time.list.2.df)
      print(paste("    NEXRAD.sec.name     = ", NEXRAD.sec.name,     sep=""))
      print(paste("    NEXRAD.sec.lat.deg  = ", NEXRAD.sec.lat.deg,  sep=""))
      print(paste("    NEXRAD.sec.lon.deg  = ", NEXRAD.sec.lon.deg,  sep=""))
      print(paste("    NEXRAD.sec.elev.m   = ", NEXRAD.sec.elev.m,   sep=""))
      print(paste("    NEXRAD.sec.srt.hhmm = ", NEXRAD.sec.srt.hhmm, sep=""))
      print(paste("    NEXRAD.sec.end.hhmm = ", NEXRAD.sec.end.hhmm, sep=""))
      print(paste("    mm.diff.for.plotting= ", mm.diff.for.plotting,sep=""))
    }  # end of for  (jj in 1:2) ...
  }    # end of if (jj==1) ...
  
  case.time.list.df <- rbind(case.time.list.df, case.time.list.2.df)
  head(case.time.list.df)
  
} else {
  print("  Not a valid input.use.CNV.homog.IFI.flag value (Must be [0,1])")
} #end of if(input.use.CNV.homog.IFI.flag == 1)

#------------------------------------------------------
# loop over 30-s mean case files that the user instructs, 
#------------------------------------------------------
# define min and max flight numbers in the case.time.list.df
flight.num              <- transform(case.time.list.df$flight.num, class=as.numeric(as.character(case.time.list.df$flight.num)))
min.fnum.in.df          <- min(flight.num$class)
max.fnum.in.df          <- max(flight.num$class)

# define start and stop indices of loop ii depending on whether "ALL" flights or individual flights are desired
ind.loop.CNV.file.start <- which(str_match(CNV.30s.mean.listfiles, as.character(min.fnum.in.df)) == min.fnum.in.df)
ind.loop.CNV.file.stop  <- which(str_match(CNV.30s.mean.listfiles, as.character(max.fnum.in.df)) == max.fnum.in.df)

# loop over all CNV 30-s mean single date/flight files in case.time.list.df
print(paste("    Looping from 30-s file number ", ind.loop.CNV.file.start, " to file number ", ind.loop.CNV.file.stop, sep=""))
for (ii in ind.loop.CNV.file.start:ind.loop.CNV.file.stop) {
  
  print(paste("------------------------------", sep=""))
  print(paste("Manage ICICLE datasets for user-defined case...", sep=""))

  #-----------------------------------------------
  # load CNV 30-s mean version X csv data file to data frame
  #-----------------------------------------------
  print(paste("  Loading 30-s mean CNV file number ", ii, " named: ", CNV.30s.mean.dir, CNV.30s.mean.listfiles[ii]), sep="")
  CNV.30s.mean.df                    <- read.csv(paste(CNV.30s.mean.dir, CNV.30s.mean.listfiles[ii], sep = ""), header = TRUE, skip = 3, sep = ",", dec = ".", stringsAsFactors=FALSE)
  colnames(CNV.30s.mean.df)          <- c("yyyy-mm-dd", "hhmmss", "lat.deg", "lon.deg", "alt.m", "flight.num", "seg.num", "seg.type", "hdg.deg", "dhdg.dt.deg", "roll.deg", "pitch.deg", "pres.hpa", "temp.c", "warm.flag", "rh.aimms.per", "MLF.gt.10um.per", "MLF.gt.50um.per", "dmax.85.per.L.um", "dmax.99perc.um", "NEV.1.LWC.gm3", "NEV.1.IWC.gm3", "NEV.2.LWC.gm3", "NEV.2.IWC.gm3", "liq.conc.tot.cm3", "ice.conc.tot.cm3",  "OAP.LWC.gm3", "OAP.IWC.gm3", "MVD.liq.um", "MMD.liq.um", "MVD.eq.ice.um", "MMD.eq.ice.um", "MVD.max.ice.um", "MMD.max.ice.um", "frac.sphere.2DC", "frac.irreg.2DC", "frac.needle.2DC", "frac.dend.2DC", "frac.sphere.HVPS", "frac.irreg.HVPS", "frac.needle.HVPS", "frac.dend.HVPS", "RID1.volt", "RID2.volt", "RID3.volt", "RID.LWC.gm3", "REFL.liq.derive.dbz", "REFL.ice.derive.dbz", "CEP.ext.perkm", "CDP.LWC.prelim.gm3")
  
  #-----------------------------------------------
  # add fields to previously loaded CNV 30-s mean data frame
  #-----------------------------------------------
  # convert yyyy-mm-dd_hhmmss to hh, mm, and ss fields for each line of 30s mean data
  hh                                 <- as.numeric(substr(CNV.30s.mean.df$`hhmmss`, 2, 3))
  mm                                 <- as.numeric(substr(CNV.30s.mean.df$`hhmmss`, 5, 6))
  ss                                 <- as.numeric(substr(CNV.30s.mean.df$`hhmmss`, 8, 9))
  # add hh, mm, and ss to df
  CNV.30s.mean.df                    <- cbind(CNV.30s.mean.df, hh, mm, ss)
  # compute dist.radartoac.km for each line of 30s mean data
  a                 <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
  c                 <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
  dist.radartoac.km <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
  for (j in 1:dim(CNV.30s.mean.df)[1]) {
    #calculate great circle distance along earth's surface between two lat/lon pts
    #a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
    #c = 2 ⋅ atan2( √a, √(1−a) )
    #d = R ⋅ c
    a[j]                 <- sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg) - NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg)-NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) / 2) + (cos(NISTdegTOradian(NEXRAD.pri.lat.deg)) * cos(NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg) - NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg)-NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])) / 2) )
    c[j]                 <- 2 * atan2(sqrt(a[j]), sqrt(1 - a[j]))
    dist.radartoac.km[j] <- radius.earth.km * c[j]
  } # end of for j in 1:...
  # add dist.radartoac.km to df
  CNV.30s.mean.df                    <- cbind(CNV.30s.mean.df, dist.radartoac.km)
  
  # define base truth.cat field
  truth.cat                          <- array(data = 0, dim = c(dim(CNV.30s.mean.df)[1]))
  # add truth.cat to df
  CNV.30s.mean.df                    <- cbind(CNV.30s.mean.df, truth.cat)
  
  # Print some of df for QC purposes
  #head(CNV.30s.mean.df, n = 10L)
  
  #------------------------------------
  # initialize output csv file with column headers
  #------------------------------------
  if (output.matchup.data.flag == 1) {
    
    print(paste("  Writing file and header line to file at: ", output.matchup.data.dir, case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv",   sep=""))
    
    if (latest.mosaic.ver.num == 1) {
      write.table(cbind(" yyyymmdd", " f#", " hh", " mm", " ss", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " dmax.85.per.L.um", " MVD.um", " NEV.LWC.gm3", " NEV.TWC.gm3", " RID.LWC.gm3", " CDP.LWC.gm3", " OAP.LWC.gm3", " NAW.zennad.REFL.30s.mean.zen", " NAW.zennad.REFL.30s.mean.nad", " NAW.zennad.REFL.30s.std.zen", " NAW.zennad.REFL.30s.std.nad", " NAW.zennad.VEL.30s.mean.zen", " NAW.zennad.VEL.30s.mean.nad", " NAW.zennad.VEL.30s.std.zen", " NAW.zennad.VEL.30s.std.nad", " dist.radar2ac.km", " FZDZ.int.max.vol", " FZDZ.meanDBZ.max.vol", " FZDZ.sdevDBZ.max.vol", " FZDZ.TDBZ.max.vol", " SLW.int.max.vol", " SLW.meanZDR.max.vol", " SLW.sdevZDR.max.vol", " SLW.meanKDP.max.vol", " SLW.sdevKDP.max.vol", " MIXPHA.int.max.vol", " MIXPHA.meanZDR.max.vol", " MIXPHA.meanDBZ.max.vol", " MIXPHA.TEMP.max.vol"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.30s.mean.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep=""))
    } else if (latest.mosaic.ver.num == 2.1) {
      write.table(cbind(" yyyymmdd", " f#", " hh", " mm", " ss", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " dmax.85.per.L.um", " MVD.um", " NEV.LWC.gm3", " NEV.TWC.gm3", " RID.LWC.gm3", " CDP.LWC.gm3", " OAP.LWC.gm3", " NAW.zennad.REFL.30s.mean.zen", " NAW.zennad.REFL.30s.mean.nad", " NAW.zennad.REFL.30s.std.zen", " NAW.zennad.REFL.30s.std.nad", " NAW.zennad.VEL.30s.mean.zen", " NAW.zennad.VEL.30s.mean.nad", " NAW.zennad.VEL.30s.std.zen", " NAW.zennad.VEL.30s.std.nad", " dist.radartoac.km", " int.full.FZDZ.max.vol", " int.sdev.DBZ.FZDZ.max.vol", " int.T.DBZ.FZDZ.max.vol", " int.mean.DBZ.FZDZ.max.vol", " int.block.sdev.DBZ.FZDZ.max.vol", " int.block.median.DBZ.FZDZ.max.vol", " sdev.DBZ.FZDZ.max.vol", " T.DBZ.FZDZ.max.vol", " mean.DBZ.FZDZ.max.vol", " block.sdev.DBZ.FZDZ.max.vol", " block.median.DBZ.FZDZ.max.vol", " mask.DBZ.FZDZ.max.vol", " ZDR.MOMS.FZDZ.max.vol", " RHO.MOMS.FZDZ.max.vol", " KDP.MOMS.FZDZ.max.vol", " int.full.SLW.max.vol", " int.mean.ZDR.corr.SLW.max.vol", " int.sdev.ZDR.SLW.max.vol", " int.mean.KDP.SLW.max.vol", " int.sdev.KDP.SLW.max.vol", " mean.ZDR.corr.SLW.max.vol", " sdev.ZDR.SLW.max.vol", " mean.KDP.SLW.max.vol", " sdev.KDP.SLW.max.vol", " DBZ.MOMS.SLW.max.vol", " RHO.MOMS.SLW.max.vol", " int.full.MIXPHA.max.vol", " int.mean.ZDR.MIXPHA.max.vol", " int.mean.DBZ.MIXPHA.max.vol", " int.temp.MIXPHA.max.vol", " mean.ZDR.MIXPHA.max.vol", " mean.DBZ.MIXPHA.max.vol", " RHO.MOMS.MIXPHA.max.vol", " KDP.MOMS.MIXPHA.max.vol", " int.full.PLATES.max.vol", " int.mean.ZDR.corr.PLATES.max.vol", " int.mean.DBZ.PLATES.max.vol", " int.temp.PLATES.max.vol", " mean.ZDR.corr.PLATES.max.vol", " mean.DBZ.PLATES.max.vol", " RHO.MOMS.PLATES.max.vol", " KDP.MOMS.PLATES.max.vol"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep="")) 
    } else if (latest.mosaic.ver.num == 2.2) {
      #write.table(cbind(" yyyymmdd", " f#", " hh", " mm", " ss", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " truth.cat", " MLF.gt.10um.%", " MLF50um.%", " dmax.85.per.L.um", " dmax.99%.um", " NEV.1.LWC.gm3", " NEV.1.IWC.gm3", " NEV.2.LWC.gm3", " NEV.2.IWC.gm3",  " OAP.LWC.gm3", " OAP.IWC.gm3", " CDP.LWC.prelim.gm3", " RID.LWC.gm3", " liq.conc.tot.cm3", " ice.conc.tot.cm3", " MVD.liq.um", " MMD.liq.um", " MVD.eq.ice.um", " MMD.eq.ice.um", " MVD.max.ice.um", " MMD.max.ice.um", " 2DC.sphere.%", " 2DC.irreg.%", " 2DC.needle.%", " 2DC.dend.%", " HVPS.sphere.%", " HVPS.irreg.%", " HVPS.needle.%", " HVPS.dend.%", " REFL.liq.derive.dbz", " REFL.ice.derive.dbz", " CEP.ext.perkm", " NAW.REFL.30s.mean.zen", " NAW.REFL.30s.mean.nad", " NAW.REFL.30s.std.zen", " NAW.REFL.30s.std.nad", " NAW.REFL.range.std.zen", " NAW.REFL.range.std.nad", " NAW.VEL.30s.mean.zen", " NAW.VEL.30s.mean.nad", " NAW.VEL.30s.std.zen", " NAW.VEL.30s.std.nad", " NAW.VEL.range.std.zen", " NAW.VEL.range.std.nad", " dist.radartoac.km", " int.full.FZDZ.max.vol", " int.sdev.DBZ.FZDZ.max.vol", " int.T.DBZ.FZDZ.max.vol", " int.mean.DBZ.FZDZ.max.vol", " int.block.sdev.DBZ.FZDZ.max.vol", " int.block.median.DBZ.FZDZ.max.vol", " sdev.DBZ.FZDZ.max.vol", " T.DBZ.FZDZ.max.vol", " mean.DBZ.FZDZ.max.vol", " block.sdev.DBZ.FZDZ.max.vol", " block.median.DBZ.FZDZ.max.vol", " mask.DBZ.FZDZ.max.vol", " ZDR.MOMS.FZDZ.max.vol", " RHO.MOMS.FZDZ.max.vol", " KDP.MOMS.FZDZ.max.vol", " int.full.SLW.max.vol", " int.mean.ZDR.corr.SLW.max.vol", " int.sdev.ZDR.SLW.max.vol", " int.mean.KDP.SLW.max.vol", " int.sdev.KDP.SLW.max.vol", " mean.ZDR.corr.SLW.max.vol", " sdev.ZDR.SLW.max.vol", " mean.KDP.SLW.max.vol", " sdev.KDP.SLW.max.vol", " DBZ.MOMS.SLW.max.vol", " RHO.MOMS.SLW.max.vol", " int.full.MIXPHA.max.vol", " int.mean.ZDR.MIXPHA.max.vol", " int.mean.DBZ.MIXPHA.max.vol", " int.temp.MIXPHA.max.vol", " mean.ZDR.MIXPHA.max.vol", " mean.DBZ.MIXPHA.max.vol", " RHO.MOMS.MIXPHA.max.vol", " KDP.MOMS.MIXPHA.max.vol", " int.full.PLATES.max.vol", " int.mean.ZDR.corr.PLATES.max.vol", " int.mean.DBZ.PLATES.max.vol", " int.temp.PLATES.max.vol", " mean.ZDR.corr.PLATES.max.vol", " mean.DBZ.PLATES.max.vol", " RHO.MOMS.PLATES.max.vol", " KDP.MOMS.PLATES.max.vol"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep="")) 
      ################################################################################
      ######### VERSION BELOW WORKS, COMMENTED OUT FOR I JK TEST #####################
      ################################################################################
      #write.table(cbind("  yyyymmdd", " f#", " hh", " mm", " ss", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " truth.cat", " MLF.gt.10um.%", " MLF50um.%", " dmax.85.per.L.um", " dmax.99%.um", " NEV.1.LWC.gm3", " NEV.1.IWC.gm3", " NEV.2.LWC.gm3", " NEV.2.IWC.gm3",  " OAP.LWC.gm3", " OAP.IWC.gm3", " CDP.LWC.prelim.gm3", " RID.LWC.gm3", " liq.conc.tot.cm3", " ice.conc.tot.cm3", " MVD.liq.um", " MMD.liq.um", " MVD.eq.ice.um", " MMD.eq.ice.um", " MVD.max.ice.um", " MMD.max.ice.um", " 2DC.sphere.%", " 2DC.irreg.%", " 2DC.needle.%", " 2DC.dend.%", " HVPS.sphere.%", " HVPS.irreg.%", " HVPS.needle.%", " HVPS.dend.%", " REFL.liq.derive.dbz", " REFL.ice.derive.dbz", " CEP.ext.perkm", " NAW.REFL.30s.mean.zen", " NAW.REFL.30s.mean.nad", " NAW.REFL.30s.std.zen", " NAW.REFL.30s.std.nad", " NAW.REFL.range.std.zen", " NAW.REFL.range.std.nad", " NAW.VEL.30s.mean.zen", " NAW.VEL.30s.mean.nad", " NAW.VEL.30s.std.zen", " NAW.VEL.30s.std.nad", " NAW.VEL.range.std.zen", " NAW.VEL.range.std.nad", " dist.radartoac.km", " int.full.FZDZ.pix", " int.sdev.DBZ.FZDZ.pix", " int.T.DBZ.FZDZ.pix", " int.mean.DBZ.FZDZ.pix", " int.block.sdev.DBZ.FZDZ.pix", " int.block.median.DBZ.FZDZ.pix", " sdev.DBZ.FZDZ.pix", " T.DBZ.FZDZ.pix", " mean.DBZ.FZDZ.pix", " block.sdev.DBZ.FZDZ.pix", " block.median.DBZ.FZDZ.pix", " mask.DBZ.FZDZ.pix", " ZDR.MOMS.FZDZ.pix", " RHO.MOMS.FZDZ.pix", " KDP.MOMS.FZDZ.pix", " int.full.SLW.pix", " int.mean.ZDR.corr.SLW.pix", " int.sdev.ZDR.SLW.pix", " int.mean.KDP.SLW.pix", " int.sdev.KDP.SLW.pix", " mean.ZDR.corr.SLW.pix", " sdev.ZDR.SLW.pix", " mean.KDP.SLW.pix", " sdev.KDP.SLW.pix", " DBZ.MOMS.SLW.pix", " RHO.MOMS.SLW.pix", " int.full.MIXPHA.pix", " int.mean.ZDR.MIXPHA.pix", " int.mean.DBZ.MIXPHA.pix", " int.temp.MIXPHA.pix", " mean.ZDR.MIXPHA.pix", " mean.DBZ.MIXPHA.pix", " RHO.MOMS.MIXPHA.pix", " KDP.MOMS.MIXPHA.pix", " int.full.PLATES.pix", " int.mean.ZDR.corr.PLATES.pix", " int.mean.DBZ.PLATES.pix", " int.temp.PLATES.pix", " mean.ZDR.corr.PLATES.pix", " mean.DBZ.PLATES.pix", " RHO.MOMS.PLATES.pix", " KDP.MOMS.PLATES.pix"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep="")) 
      ### CURRENT VERSION, WITHOUT I J K ############
      #write.table(cbind("  yyyymmdd", " f#", " hh", " mm", " ss", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " truth.cat", " MLF.gt.10um.%", " MLF50um.%", " dmax.85.per.L.um", " dmax.99%.um", " NEV.1.LWC.gm3", " NEV.1.IWC.gm3", " NEV.2.LWC.gm3", " NEV.2.IWC.gm3",  " OAP.LWC.gm3", " OAP.IWC.gm3", " CDP.LWC.prelim.gm3", " RID.LWC.gm3", " liq.conc.tot.cm3", " ice.conc.tot.cm3", " MVD.liq.um", " MMD.liq.um", " MVD.eq.ice.um", " MMD.eq.ice.um", " MVD.max.ice.um", " MMD.max.ice.um", " 2DC.sphere.%", " 2DC.irreg.%", " 2DC.needle.%", " 2DC.dend.%", " HVPS.sphere.%", " HVPS.irreg.%", " HVPS.needle.%", " HVPS.dend.%", " REFL.liq.derive.dbz", " REFL.ice.derive.dbz", " CEP.ext.perkm", " NAW.REFL.30s.mean.zen", " NAW.REFL.30s.mean.nad", " NAW.REFL.30s.std.zen", " NAW.REFL.30s.std.nad", " NAW.REFL.range.std.zen", " NAW.REFL.range.std.nad", " NAW.VEL.30s.mean.zen", " NAW.VEL.30s.mean.nad", " NAW.VEL.30s.std.zen", " NAW.VEL.30s.std.nad", " NAW.VEL.range.std.zen", " NAW.VEL.range.std.nad", " dist.radartoac.km", " int.full.FZDZ.pix", " int.sdev.DBZ.FZDZ.pix", " int.T.DBZ.FZDZ.pix", " int.mean.DBZ.FZDZ.pix", " int.block.sdev.DBZ.FZDZ.pix", " int.block.median.DBZ.FZDZ.pix", " sdev.DBZ.FZDZ.pix", " T.DBZ.FZDZ.pix", " mean.DBZ.FZDZ.pix", " block.sdev.DBZ.FZDZ.pix", " block.median.DBZ.FZDZ.pix", " mask.DBZ.FZDZ.pix", " ZDR.MOMS.FZDZ.pix", " RHO.MOMS.FZDZ.pix", " KDP.MOMS.FZDZ.pix", " int.full.SLW.pix", " int.mean.ZDR.corr.SLW.pix", " int.sdev.ZDR.SLW.pix", " int.mean.KDP.SLW.pix", " int.sdev.KDP.SLW.pix", " mean.ZDR.corr.SLW.pix", " sdev.ZDR.SLW.pix", " mean.KDP.SLW.pix", " sdev.KDP.SLW.pix", " DBZ.MOMS.SLW.pix", " RHO.MOMS.SLW.pix", " int.full.MIXPHA.pix", " int.mean.ZDR.MIXPHA.pix", " int.mean.DBZ.MIXPHA.pix", " int.temp.MIXPHA.pix", " mean.ZDR.MIXPHA.pix", " mean.DBZ.MIXPHA.pix", " RHO.MOMS.MIXPHA.pix", " KDP.MOMS.MIXPHA.pix", " int.full.PLATES.pix", " int.mean.ZDR.corr.PLATES.pix", " int.mean.DBZ.PLATES.pix", " int.temp.PLATES.pix", " mean.ZDR.corr.PLATES.pix", " mean.DBZ.PLATES.pix", " RHO.MOMS.PLATES.pix", " KDP.MOMS.PLATES.pix"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep="")) 
      ### ABBREVIATED VERSION, WITH I J K ##########################
      write.table(cbind("  yyyymmdd", " f#", " hh", " mm", " ss", "grid.i", "grid.j", "grid.k", " seg#", " seg.t",  " lat.d", " lon.d", " alt.m", " hdg.d", " roll.d", " pitch.d", " pres.mb", " T.c", " warm.fg", " rh.aimms.%", " truth.cat"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep="")) 
    } # end of if (latest.mosaic.ver.num == 1)
  
  } else {
    print("  Not writing output data to file.  output.matchup.data.flag == 0")
  }  # end of if ...
  
  # determine which case.time.list.df rows occur during the currently loaded CNV 30-s mean data file date
  ind.case.time.list.df.of.30sfile   <- which(case.time.list.df$flight.num == CNV.30s.mean.df$flight.num[1])
                                              
  # loop over all case.time.list.df rows that occur during the currently loaded CNV 30-s mean data file date
  print(paste("    Number of NEXRAD-centric periods to loop over (ppp) = ", length(ind.case.time.list.df.of.30sfile), sep=""))
  #for (ppp in 1:1) {
  for (ppp in ind.case.time.list.df.of.30sfile[1]:ind.case.time.list.df.of.30sfile[length(ind.case.time.list.df.of.30sfile)]) { 
    
    #####################################################################################

    print(paste("      ppp = ", ppp, sep=""))
    
    # define indices of lines (times) in CNV.30s.mean.df that are within times defined by case.time.list.df[ppp, ]  
    if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh <= -2) {
      ind.within.rad.domain.after.srt.ppp  <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])))
      hh.diff                              <- NEXRAD.pri.end.hh - NEXRAD.pri.srt.hh - 1
      for (tt in 1:hh.diff) {
        ind.within.rad.domain.mid.ppp <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) + tt)
        if (tt == 1) {
          ind.within.rad.domain.ppp     <- append(ind.within.rad.domain.after.srt.ppp, ind.within.rad.domain.mid.ppp)
        } else {
          ind.within.rad.domain.ppp     <- append(ind.within.rad.domain.ppp, ind.within.rad.domain.mid.ppp)
        }
      }
      ind.within.rad.domain.before.end.ppp <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])))
      ind.within.rad.domain.ppp            <- append(ind.within.rad.domain.ppp, ind.within.rad.domain.before.end.ppp)
    } else if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh == -1) {
      ind.within.rad.domain.after.srt.ppp  <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])))
      ind.within.rad.domain.before.end.ppp <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])))
      ind.within.rad.domain.ppp            <- append(ind.within.rad.domain.after.srt.ppp, ind.within.rad.domain.before.end.ppp)
      #ind.within.rad.domain.ppp            <- which(CNV.30s.mean.df$hh >= as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])) & CNV.30s.mean.df$hh <= as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])) )
    } else if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh == 0) {
      ind.within.rad.domain.ppp            <- which(CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])) )
    } else if (NEXRAD.pri.srt.hh - NEXRAD.pri.end.hh > 0) {
      ind.within.rad.domain.after.srt.ppp  <- which(CNV.30s.mean.df$hh >= as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])))
      ind.within.rad.domain.before.end.ppp <- which(CNV.30s.mean.df$hh <= as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])))
      ind.within.rad.domain.ppp            <- append(ind.within.rad.domain.after.srt.ppp, ind.within.rad.domain.before.end.ppp)
    } else {
      print("Error")
    } # end of if (NEXRAD.pri.srt.hh - ... )
    
    # index on defined criteria
    temp.c.thresh                      <-   0.00
    MLF.gt.50um.per.DOMLIQ.thresh      <-   0.60
    MLF.gt.50um.per.ICEONLY.thresh     <-   0.05
    NEV.LWC.signif.thresh              <-   0.10
    NEV.IWC.signif.thresh              <-   0.10
    NEV.LWC.min.thresh                 <-   0.01
    NEV.IWC.min.thresh                 <-   0.01
    dmax.85.per.L.um.ANYLIQ.thresh     <-   1.00
    dmax.85.per.L.um.FZDZ.thresh       <- 100.00
    dmax.85.per.L.um.FZRA.thresh       <- 500.00
    #RID.LWC.signif.thresh              <-   0.10
    #RID.LWC.min.thresh                 <-   0.01
    #RID.minus.NEV.thresh               <-   0.00
    #dist.radartoac.km.thresh           <- 100.00
    
    # indices for algo ... 
    #   NOTES: *.wdist refers to having 'distance from NEXRAD' constraint, *.nodist refers to having no distance constraint  
    
    #   FZRA
    #ind.FZRA.nodist                     <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.FZRA.nodist                     <- which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.FZRA.nodist                     <- which(CNV.30s.mean.df$MLF.gt.50um.per>=MLF.gt.50um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZRA.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   FZDZ
    #ind.FZDZ.wdist                      <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.FZDZ.nodist                     <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.FZDZ.hi.lwc.nodist              <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.FZDZ.lo.lwc.nodist              <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.signif.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.signif.thresh ) & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.min.thresh ) & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.FZDZ.hi.lwc.nodist              <- which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh)
    # VERSION FROM PRE DEC2020: ind.FZDZ.lo.lwc.nodist              <- which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.signif.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.signif.thresh ) & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.min.thresh ) & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.FZDZ.hi.lwc.nodist              <- which(CNV.30s.mean.df$MLF.gt.50um.per>=MLF.gt.50um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3<NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.FZDZ.lo.lwc.nodist              <- which(CNV.30s.mean.df$MLF.gt.50um.per>=MLF.gt.50um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZRA.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.signif.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3<NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   SLW
    #ind.SLW.wdist                       <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.SLW.nodist                      <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.SLW.hi.lwc.nodist               <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & (CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh) & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.SLW.lo.lwc.nodist               <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per>=MLF.gt.10um.per.DOMLIQ.thresh  & (CNV.30s.mean.df$RID.LWC.gm3<RID.LWC.signif.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh ) & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh  & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh ) & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    ind.SLW.hi.lwc.nodist               <- which(CNV.30s.mean.df$MLF.gt.50um.per>=MLF.gt.50um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3<NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.SLW.lo.lwc.nodist               <- which(CNV.30s.mean.df$MLF.gt.50um.per>=MLF.gt.50um.per.DOMLIQ.thresh  & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3< NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3<NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   MIXPHA TOT
    #ind.MIXPHA.TOT.wdist               <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.TOT.nodist              <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & (CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh | CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.TOT.nodist              <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: nd.MIXPHA.TOT.nodist              <- which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$RID.LWC.gm3>RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.MIXPHA.TOT.nodist              <- which(CNV.30s.mean.df$MLF.gt.50um.per<MLF.gt.50um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.50um.per>MLF.gt.50um.per.ICEONLY.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   MIXPHA domICE
    #ind.MIXPHA.domICE.wdist            <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & (CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh & CNV.30s.mean.df$RID.LWC.gm3<RID.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domICE.nodist           <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & (CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh & CNV.30s.mean.df$RID.LWC.gm3<RID.LWC.signif.thresh) & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domICE.nodist           <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.min.thresh & CNV.30s.mean.df$RID.LWC.gm3<RID.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.MIXPHA.domICE.nodist           <- which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.min.thresh & CNV.30s.mean.df$RID.LWC.gm3<RID.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.MIXPHA.domICE.nodist           <- which(CNV.30s.mean.df$MLF.gt.50um.per<MLF.gt.50um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.50um.per>MLF.gt.50um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   MIXPHA domLIQsmall
    #ind.MIXPHA.domLIQsml.wdist         <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domLIQsml.nodist        <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domLIQsml.nodist        <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.MIXPHA.domLIQsml.nodist        <- which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.MIXPHA.domLIQsml.nodist        <- which(CNV.30s.mean.df$MLF.gt.50um.per<MLF.gt.50um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.50um.per>MLF.gt.50um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.ANYLIQ.thresh & CNV.30s.mean.df$dmax.85.per.L.um<dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   MIXPHA domLIQlarge
    #ind.MIXPHA.domLIQlrg.wdist         <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domLIQlrg.nodist        <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.MIXPHA.domLIQlrg.nodist        <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.MIXPHA.domLIQlrg.nodist        <- which(CNV.30s.mean.df$MLF.gt.10um.per<MLF.gt.10um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.10um.per>MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$RID.LWC.gm3>=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.MIXPHA.domLIQlrg.nodist        <- which(CNV.30s.mean.df$MLF.gt.50um.per<MLF.gt.50um.per.DOMLIQ.thresh   & CNV.30s.mean.df$MLF.gt.50um.per>MLF.gt.50um.per.ICEONLY.thresh & CNV.30s.mean.df$dmax.85.per.L.um>=dmax.85.per.L.um.FZDZ.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3>=NEV.LWC.signif.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
     #   ICEONLY
    #ind.ICEONLY.wdist                  <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<=MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$RID.LWC.gm3<=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh & CNV.30s.mean.df$dist.radartoac.km<dist.radartoac.km.thresh), ind.within.rad.domain.ppp)
    #ind.ICEONLY.nodist                 <- intersect(which(CNV.30s.mean.df$MLF.gt.10um.per<=MLF.gt.10um.per.ICEONLY.thresh & CNV.30s.mean.df$RID.LWC.gm3<=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    #ind.ICEONLY.nodist                 <- intersect(which(CNV.30s.mean.df$RID.LWC.gm3<=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh), ind.within.rad.domain.ppp)
    # VERSION FROM PRE DEC2020: ind.ICEONLY.nodist                 <- which(CNV.30s.mean.df$RID.LWC.gm3<=RID.LWC.min.thresh & CNV.30s.mean.df$NEV.1.LWC.gm3<=NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.signif.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    ind.ICEONLY.nodist                 <- which(CNV.30s.mean.df$NEV.1.LWC.gm3<NEV.LWC.min.thresh & CNV.30s.mean.df$NEV.1.IWC.gm3>=NEV.IWC.min.thresh & CNV.30s.mean.df$temp.c<temp.c.thresh)
    #   NOTINANY
    #ind.INANY.wdist                    <- union(ind.FZDZ.wdist,  ind.SLW.wdist)
    #ind.INANY.wdist                    <- union(ind.INANY.wdist, ind.MIXPHA.TOT.wdist)
    #ind.INANY.wdist                    <- union(ind.INANY.wdist, ind.ICEONLY.wdist)
    #ind.INANY.wdist                   <- setdiff(seq(from=1, to=length(CNV.30s.mean.df$MLF.gt.10um.per), by=1), ind.INANY.nodist)
    ind.INANY.nodist                   <- union(ind.FZDZ.hi.lwc.nodist, ind.FZDZ.lo.lwc.nodist)
    ind.INANY.nodist                   <- union(ind.INANY.nodist, ind.SLW.hi.lwc.nodist)
    ind.INANY.nodist                   <- union(ind.INANY.nodist, ind.SLW.lo.lwc.nodist)
    ind.INANY.nodist                   <- union(ind.INANY.nodist, ind.FZRA.nodist)
    ind.INANY.nodist                   <- union(ind.INANY.nodist, ind.MIXPHA.TOT.nodist)
    ind.INANY.nodist                   <- union(ind.INANY.nodist, ind.ICEONLY.nodist)
    ind.NOTINANY.nodist                <- setdiff(seq(from=1, to=length(CNV.30s.mean.df$MLF.gt.10um.per), by=1), ind.INANY.nodist)
    #   PLATES
    # none yet
    
    # populate truth.cat fieldd for CNV near radar section 'ppp' with 0-6 category designations, based on CNV instrument criteria
    CNV.30s.mean.df$truth.cat[ind.NOTINANY.nodist]          <- 0
    CNV.30s.mean.df$truth.cat[ind.SLW.hi.lwc.nodist]        <- 1
    CNV.30s.mean.df$truth.cat[ind.SLW.lo.lwc.nodist]        <- 2
    CNV.30s.mean.df$truth.cat[ind.FZDZ.hi.lwc.nodist]       <- 3
    CNV.30s.mean.df$truth.cat[ind.FZDZ.lo.lwc.nodist]       <- 4
    CNV.30s.mean.df$truth.cat[ind.FZRA.nodist]              <- 5
    CNV.30s.mean.df$truth.cat[ind.MIXPHA.domICE.nodist]     <- 6
    CNV.30s.mean.df$truth.cat[ind.MIXPHA.domLIQsml.nodist]  <- 7
    CNV.30s.mean.df$truth.cat[ind.MIXPHA.domLIQlrg.nodist]  <- 8
    CNV.30s.mean.df$truth.cat[ind.ICEONLY.nodist]           <- 9
    
    #browser()
    
    #--------------------------------------------------------------
    # plots 
    #--------------------------------------------------------------
    print("      Plotting by ppp")
    #plot(CNV.30s.mean.df$MLF.gt.10um.per[ind.FZDZ.nodist],    (CNV.30s.mean.df$RID.LWC.gm3[ind.FZDZ.nodist])   - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZDZ.nodist],   col="blue",   type="p", cex=2, lwd=2, xlim=c(0.0, 1.0), ylim=c(-0.4, 0.4), xlab="MLF", ylab="RID-NEV LWC [gm-3]")
    #title("F17 2/17/2019 Criteria for RadIA Algo Comparisons")
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.SLW.nodist],    (CNV.30s.mean.df$RID.LWC.gm3[ind.SLW.nodist])    - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.SLW.nodist],    col="green",  type="p", cex=2, lwd=2)
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.MIXPHA.nodist], (CNV.30s.mean.df$RID.LWC.gm3[ind.MIXPHA.nodist]) - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.nodist], col="orange", type="p", cex=2, lwd=2)
    #segments(0.500, -0.001, 1.100, -0.001, col="green")
    #segments(0.502, -0.500, 0.502, -0.001, col="green")
    #segments(0.500, +0.001, 1.100, +0.001, col="blue" )
    #segments(0.502, +0.001, 0.502, +0.500, col="blue" )
    #abline(v = +0.498, col="orange")
    #text(0.8, +0.3, paste("Large-drop, N=", length(ind.FZDZ.nodist),   sep=""), col="blue")
    #text(0.8, -0.3, paste("Small-drop, N=", length(ind.SLW.nodist),    sep=""), col="green")
    #text(0.2, -0.3, paste("Mix-Phase,  N=", length(ind.MIXPHA.nodist), sep=""), col="orange")
    #grid()
    
    #plot(CNV.30s.mean.df$MLF.gt.10um.per[ind.FZDZ.nodist],    CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZDZ.nodist],   col="blue",   type="p", cex=2, lwd=2, xlim=c(0.0, 1.0), ylim=c(0.0, 0.6), xlab="MLF", ylab="NEV LWC [gm-3]")
    #title("F17 2/17/2019 Criteria for RadIA Algo Comparisons")
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.SLW.nodist],    CNV.30s.mean.df$NEV.1.LWC.gm3[ind.SLW.nodist],    col="green",  type="p", cex=2, lwd=2)
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.MIXPHA.nodist], CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.nodist], col="orange", type="p", cex=2, lwd=2)
    #grid()
    
    #plot(CNV.30s.mean.df$MLF.gt.10um.per[ind.FZDZ.nodist],    CNV.30s.mean.df$RID.LWC.gm3[ind.FZDZ.nodist],   col="blue",   type="p", cex=2, lwd=2, xlim=c(0.0, 1.0), ylim=c(0.0, 0.5), xlab="MLF", ylab="RID LWC [gm-3]")
    #title("F17 2/17/2019 Criteria for RadIA Algo Comparisons")
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.SLW.nodist],    CNV.30s.mean.df$RID.LWC.gm3[ind.SLW.nodist],    col="green",  type="p", cex=2, lwd=2)
    #lines(CNV.30s.mean.df$MLF.gt.10um.per[ind.MIXPHA.nodist], CNV.30s.mean.df$RID.LWC.gm3[ind.MIXPHA.nodist], col="orange", type="p", cex=2, lwd=2)
    #grid()
    
    #plot(CNV.30s.mean.df$dmax.85.per.L.um[ind.MIXPHA.nodist],    (CNV.30s.mean.df$RID.LWC.gm3[ind.MIXPHA.nodist]) - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.nodist], col="orange",   type="p", cex=2, lwd=2, xlim=c(0.0, 600.00), ylim=c(-0.4, 0.4), xlab="Dmax-99", ylab="RID-NEV LWC [gm-3]")
    #title("F17 2/17/2019 Criteria for RadIA Algo Comparisons")
    #lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.SLW.nodist],    (CNV.30s.mean.df$RID.LWC.gm3[ind.SLW.nodist])      - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.SLW.nodist],        col="green",  type="p", cex=2, lwd=2)
    #lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.FZDZ.nodist], (CNV.30s.mean.df$RID.LWC.gm3[ind.FZDZ.nodist])       - CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZDZ.nodist],           col="blue", type="p", cex=2, lwd=2)
    #abline(v = 99,  col="green",  lwd=2, lty=1)
    #abline(v = 101, col="blue",   lwd=2, lty=1)
    #abline(h = 0,   col="black",  lwd=2, lty=2)
    #text(0.8, +0.3, paste("Large-drop, N=", length(ind.FZDZ.nodist),   sep=""), col="blue")
    #text(0.8, -0.3, paste("Small-drop, N=", length(ind.SLW.nodist),    sep=""), col="green")
    #text(0.2, -0.3, paste("Mix-Phase,  N=", length(ind.MIXPHA.nodist), sep=""), col="orange")
    #grid()
    
    # Truth criteria plot: Dmax vs LWC
    par(mfrow = c(1, 1))
    par(mar   = c(5, 5, 5, 5))
    if (length(ind.FZRA.nodist) > 0) {
      plot(CNV.30s.mean.df$dmax.85.per.L.um[ind.MIXPHA.domLIQsml.nodist],  CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.domLIQsml.nodist], col="orange", type="p", cex=2, lwd=2, pch=1, xlim=c(0.00, 1200.00), ylim=c(0.00, 0.60), xlab="Dmax(85/L) [um]", cex.lab=2, ylab=paste("LWC [g/m3]", sep=""), xaxt="n", yaxt="n")
    } else {
      plot(CNV.30s.mean.df$dmax.85.per.L.um[ind.MIXPHA.domLIQsml.nodist],  CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.domLIQsml.nodist], col="orange", type="p", cex=2, lwd=2, pch=1, xlim=c(0.00, 600.00), ylim=c(0.00, 0.60), xlab="Dmax(85/L) [um]", cex.lab=2, ylab=paste("LWC [g/m3]", sep=""), xaxt="n", yaxt="n")
    }
    axis(1, cex.axis=2)
    axis(2, cex.axis=2)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.MIXPHA.domLIQlrg.nodist], CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.domLIQlrg.nodist], col="orange",     type="p", cex=2, lwd=2, pch=2)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.MIXPHA.domICE.nodist],    CNV.30s.mean.df$NEV.1.LWC.gm3[ind.MIXPHA.domICE.nodist],    col="chocolate",  type="p", cex=2, lwd=2, pch=0)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.SLW.hi.lwc.nodist],       CNV.30s.mean.df$NEV.1.LWC.gm3[ind.SLW.hi.lwc.nodist],       col="green",      type="p", cex=2, lwd=2, pch=1)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.SLW.lo.lwc.nodist],       CNV.30s.mean.df$NEV.1.LWC.gm3[ind.SLW.lo.lwc.nodist],       col="green",      type="p", cex=2, lwd=2, pch=1)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.FZDZ.hi.lwc.nodist],      CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZDZ.hi.lwc.nodist],      col="blue",       type="p", cex=2, lwd=2, pch=2)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.FZDZ.lo.lwc.nodist],      CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZDZ.lo.lwc.nodist],      col="blue",       type="p", cex=2, lwd=2, pch=2)
    lines(CNV.30s.mean.df$dmax.85.per.L.um[ind.FZRA.nodist],             CNV.30s.mean.df$NEV.1.LWC.gm3[ind.FZRA.nodist],             col="dodgerblue", type="p", cex=2, lwd=2, pch=2)
    if (ppp == 1) {
      title(paste("F", as.character(icicle.df$fl.num[ind.fl.num]), " ", as.character(case.time.list.df$yyyymmdd[ppp]), " CNV 'truth' points near", NEXRAD.pri.name, sep=""), cex=2.0)
    } else if (ppp == 2) {
      title(paste("F", as.character(icicle.df$fl.num[ind.fl.num]), " ", as.character(case.time.list.df$yyyymmdd[ppp]), " CNV 'truth' points near", NEXRAD.sec.name, sep=""), cex=2.0)
    }
    abline(v =  98.5, col="green",      lwd=2, lty=1)
    abline(v = 101.5, col="blue",       lwd=2, lty=1)
    abline(v = 498.5, col="blue",       lwd=2, lty=1)
    abline(v = 503.5, col="dodgerblue", lwd=2, lty=1)
    text(300, 0.60, paste("Large-drop FZDZ, N=",       length(ind.FZDZ.hi.lwc.nodist)+length(ind.FZDZ.lo.lwc.nodist), sep=""), col="blue",       cex=1.5)
    text(300, 0.58, paste("Large-drop FZRA, N=",       length(ind.FZRA.nodist),                                       sep=""), col="dodgerblue", cex=1.5)
    text(300, 0.56, paste("Small-drop, N=",            length(ind.SLW.hi.lwc.nodist)+length(ind.SLW.lo.lwc.nodist),   sep=""), col="green",      cex=1.5)
    #text(300, 0.56, paste("Mix-Phase(TOT),  N=",       length(ind.MIXPHA.TOT.nodist),                                 sep=""), col="orange",     cex=1.5)
    text(300, 0.54, paste("Mix-Phase(domLIQsml),  N=", length(ind.MIXPHA.domLIQsml.nodist),                           sep=""), col="orange",     cex=1.5)
    text(300, 0.52, paste("Mix-Phase(domLIQlrg),  N=", length(ind.MIXPHA.domLIQlrg.nodist),                           sep=""), col="orange",     cex=1.5)
    text(300, 0.50, paste("Mix-Phase(domICE),  N=",    length(ind.MIXPHA.domICE.nodist),                              sep=""), col="chocolate",  cex=1.5)
    text(300, 0.48, paste("ICEONLY,  N=",              length(ind.ICEONLY.nodist),                                    sep=""), col="grey",       cex=1.5)
    grid()
    
    # Truth criteria plot: mapview near NEXRAD
    par(mfrow = c(1, 1))
    par(mar   = c(5, 5, 5, 5))
    plot(CNV.30s.mean.df$lon.deg[ind.MIXPHA.domLIQlrg.nodist],  CNV.30s.mean.df$lat.deg[ind.MIXPHA.domLIQlrg.nodist], col="orange",    type="p", cex=1.5, lwd=2, pch=2, xlim=c(NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]-1.5, NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]+1.2), ylim=c(NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]-0.8, NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]+0.9), xlab="Lon [deg]", cex.lab=2, ylab="Lat [deg]", xaxt="n", yaxt="n")
    axis(1, cex.axis=2)
    axis(2, cex.axis=2)
    lines(CNV.30s.mean.df$lon.deg[ind.NOTINANY.nodist],         CNV.30s.mean.df$lat.deg[ind.NOTINANY.nodist],         col="black",     type="p", cex=0.5, lwd=2, pch=1)
    lines(CNV.30s.mean.df$lon.deg[ind.MIXPHA.domLIQsml.nodist], CNV.30s.mean.df$lat.deg[ind.MIXPHA.domLIQsml.nodist], col="orange",    type="p", cex=1.5, lwd=2, pch=1)
    lines(CNV.30s.mean.df$lon.deg[ind.MIXPHA.domICE.nodist],    CNV.30s.mean.df$lat.deg[ind.MIXPHA.domICE.nodist],    col="chocolate", type="p", cex=1.5, lwd=2, pch=0)
    lines(CNV.30s.mean.df$lon.deg[ind.SLW.hi.lwc.nodist],       CNV.30s.mean.df$lat.deg[ind.SLW.hi.lwc.nodist],       col="green",     type="p", cex=1.5, lwd=2, pch=1)
    lines(CNV.30s.mean.df$lon.deg[ind.SLW.lo.lwc.nodist],       CNV.30s.mean.df$lat.deg[ind.SLW.lo.lwc.nodist],       col="green",     type="p", cex=1.5, lwd=2, pch=1)
    lines(CNV.30s.mean.df$lon.deg[ind.FZDZ.hi.lwc.nodist],      CNV.30s.mean.df$lat.deg[ind.FZDZ.hi.lwc.nodist],      col="blue",      type="p", cex=1.5, lwd=2, pch=2)
    lines(CNV.30s.mean.df$lon.deg[ind.FZDZ.lo.lwc.nodist],      CNV.30s.mean.df$lat.deg[ind.FZDZ.lo.lwc.nodist],      col="blue",      type="p", cex=1.5, lwd=2, pch=2)
    lines(CNV.30s.mean.df$lon.deg[ind.FZRA.nodist],             CNV.30s.mean.df$lat.deg[ind.FZRA.nodist],             col="dodgerblue",type="p", cex=1.5, lwd=2, pch=2)
    lines(CNV.30s.mean.df$lon.deg[ind.ICEONLY.nodist],          CNV.30s.mean.df$lat.deg[ind.ICEONLY.nodist],          col="grey",      type="p", cex=1.5, lwd=2, pch=0)
    if (ppp == 1) {
      text(NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])],      NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])],                  "*", cex=2.5, lwd=2)
      text(NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]-0.10, NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]+0.05, NEXRAD.pri.name, cex=2.0, lwd=2)
    } else if (ppp == 2) {
      text(NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])],      NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])],                  "*", cex=2.5, lwd=2)
      text(NEXRAD.site.df$lon[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]-0.10, NEXRAD.site.df$lat[NEXRAD.site.df$ICAO==as.character(case.time.list.df$pri.radar.name[ppp])]+0.05, NEXRAD.sec.name, cex=2.0, lwd=2)
    }
    legend("bottomright", legend=c("MPHA(domLIQlrg)", "MPHA(domLIQsml)", "MPHA(domICE)", "SMALL", "FZDZ", "FZRA", "ICEONLY", "NONE"), col=c("orange", "orange", "chocolate", "green", "blue", "dodgerblue", "grey", "black"), pch=c(2,1,0,1,2,2,0,1), cex=c(1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0))
    grid()
    
    ## Truth criteria plot: time/height near NEXRAD
    #par(mfrow = c(1, 1))
    #par(mar   = c(5, 5, 5, 5))
    #plot(CNV.30s.mean.df$alt.m[ind.NOTINANY.nodist],          CNV.30s.mean.df$mm[ind.NOTINANY.nodist], col="black",     type="p", cex=0.5, lwd=2, pch=19, xlim=c(0,400), ylim=c(0,4000), xlab="Time []", cex.lab=2, ylab="ALT [m MSL]", xaxt="n", yaxt="n")
    #lines(CNV.30s.mean.df$alt.m[ind.MIXPHA.domLIQlrg.nodist], CNV.30s.mean.df$mm[ind.MIXPHA.domLIQlrg.nodist], col="orange",    type="p", cex=1.5, lwd=2, pch=2)
    #lines(CNV.30s.mean.df$alt.m[ind.MIXPHA.domICE.nodist],    CNV.30s.mean.df$mm[ind.MIXPHA.domICE.nodist], col="chocolate", type="p", cex=1.5, lwd=2, pch=0)
    #lines(CNV.30s.mean.df$alt.m[ind.SLW.nodist],              CNV.30s.mean.df$mm[ind.SLW.nodist], col="green",     type="p", cex=1.5, lwd=2, pch=1)
    #lines(CNV.30s.mean.df$alt.m[ind.FZDZ.nodist],             CNV.30s.mean.df$mm[ind.FZDZ.nodist], col="blue",      type="p", cex=1.5, lwd=2, pch=2)
    #lines(CNV.30s.mean.df$alt.m[ind.ICEONLY.nodist],          CNV.30s.mean.df$mm[ind.ICEONLY.nodist], col="grey",      type="p", cex=1.5, lwd=2, pch=0)
    #axis(1, cex.axis=2)
    #axis(2, cex.axis=2)
    #grid()
    
    # Histogram of Dmax for whole flight
    hist(CNV.30s.mean.df$dmax.85.per.L.um[CNV.30s.mean.df$dmax.85.per.L.um>=0], breaks=16, col="skyblue3", xlab="Dmax(85/L) [um]", main=paste("F", icicle.df$fl.num[ind.fl.num], " ", case.time.list.df$yyyymmdd[ppp], " Convair 30-s mean periods", sep=""), xlim=c(0, max(CNV.30s.mean.df$dmax.85.per.L.um)+100))
    abline(v=100, cex=5)
    abline(v=500, cex=5)
    text(150, 50, "App O,",      col="blue")
    text(150, 45, "FZDZ",        col="blue")
    text( 50, 50, "App C",       col="green")
    text(550, 50, "App O,",      col="dodgerblue")
    text(550, 45, "FZRA",        col="dodgerblue")
    grid()
    
    ## Histogram of LWC-Nev for whole flight
    #hist(CNV.30s.mean.df$NEV.1.LWC.gm3, breaks=5, col="skyblue3", xlab="LWC [g/m3]", main=paste("F", icicle.df$fl.num[ind.fl.num], " ", case.time.list.df$yyyymmdd[ppp], " Convair 30-s mean periods", sep=""), xlim=c(0, max(CNV.30s.mean.df$NEV.1.LWC.gm3)))
    #grid()
    
    #plot(CNV.30s.mean.df$NEV.1.LWC.gm3, ylim=c(0.0, 0.2), type="b")
    
    ## MLF and IWC
    #plot(CNV.30s.mean.df$NEV.1.IWC.gm3, ylim=c(0.0, 1.0), type="b")
    #lines(CNV.30s.mean.df$MLF.gt.10um.per,                type="p", col="red")
    
    ## MLF timeseries
    #plot( CNV.30s.mean.df$MLF.gt.10um.per, ylim=c(0,1), type="p", col="red")
    #lines(CNV.30s.mean.df$NEV.1.IWC.gm3,                type="p", col="black", pch=19)
    #lines(CNV.30s.mean.df$NEV.1.LWC.gm3,                type="p", col="grey",  pch=19)
            
    #ind.MLF.le.0 <- which(CNV.30s.mean.df$MLF.gt.10um.per <= 0)
    #CNV.30s.mean.df$MLF.gt.10um.per[ind.MLF.le.0]
    
    #plot(CNV.30s.mean.df$dmax.85.per.L.um[86:124], ylim=c(0, 500), xlab="Time", ylab="Dmax")
    #lines(CNV.30s.mean.df$dmax.85.per.L.um[86:124], lty="p", pch=16)
  
    print("      End of ppp plots")
    
    #browser()
    
    ############################################################################################################################
    
    # create index of desired times to loop over, from user inputted hh/mm min to hh/mm max
    # check if hh min equals hh max, if not then bind ind values from separate hours together
    if (case.time.list.df$hh.min.for.plotting[ppp] != as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp]))) {
      ind.hhmm.first  <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])))
      ind.hhmm.last   <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.max.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])))
      ind.hhmm.total  <- c(ind.hhmm.first, ind.hhmm.last)
    } else {
      ind.hhmm.total  <- which(CNV.30s.mean.df$hh == as.numeric(as.character(case.time.list.df$hh.min.for.plotting[ppp])) & CNV.30s.mean.df$mm >= as.numeric(as.character(case.time.list.df$mm.min.for.plotting[ppp])) & CNV.30s.mean.df$mm <= as.numeric(as.character(case.time.list.df$mm.max.for.plotting[ppp])) )
    } # end of if (hh.min.for.plotting != ...)
  
    # print some data based on ind.hhmm.total
    CNV.30s.mean.df$seg.num[ind.hhmm.total]
    CNV.30s.mean.df$seg.type[ind.hhmm.total]
  
    # initialize an index number of the mosaic algorithm file that was used last, to be assigned the current index number after matched files are loaded
    ind.CNV.NAW.zennad.file.closest.last  <- 0
    if (latest.mosaic.ver.num == 1) {
      ind.mosaic.FZDZ.file.closest.last     <- 0
      ind.mosaic.SLW.file.closest.last      <- 0
      ind.mosaic.MIXPHA.file.closest.last   <- 0
    } else if (latest.mosaic.ver.num >= 2) {
      ind.mosaic.INTS.file.closest.last     <- 0
      ind.mosaic.MOMS.file.closest.last     <- 0
      ind.mosaic.FZDZ.file.closest.last     <- 0
      ind.mosaic.SLW.file.closest.last      <- 0
      ind.mosaic.MIXPHA.file.closest.last   <- 0
      ind.mosaic.PLATES.file.closest.last   <- 0
      #ind.mosaic.file.closest.last          <- 0
    } else {
      print("Error")
    } # end of if (latest.mosaic.ver.num == 1)
    
    ## define a list sequence of all segment numbers based on user-define time period
    #user.defined.seg.number.first       <- CNV.30s.mean.df$seg.num[ind.hhmm.total[1]]
    #if (user.defined.seg.number.first < 1) {
    #  min(CNV.30s.mean.df$seg.num[CNV.30s.mean.df$seg.num[ind.hhmm.total]]>0)
    #  user.defined.seg.number.first <- 1
    #}
    #user.defined.seg.number.last        <- CNV.30s.mean.df$seg.num[ind.hhmm.total[length(ind.hhmm.total)]]
    #user.defined.seg.number.list        <- seq(from = user.defined.seg.number.first, to = user.defined.seg.number.last, by = 1)
  
    ## loop over all user-defined hhmm times within case ii
    #ppp<-2
    print(paste("  Looping from user-defined start hhmm to stop hhmm.  Total of ", length(ind.hhmm.total), " 30-s mean aircraft points will be matched to RadIA", sep=""))
    #for (j in 193:193) {
    #for (j in 115:ind.hhmm.total[length(ind.hhmm.total)]) {
    #for (j in 205:445) {  
    #for (j in ind.hhmm.total[1]:192) {
    for (j in ind.hhmm.total[1]:ind.hhmm.total[length(ind.hhmm.total)]) {
  
    ## loop over all flight segments within a user-defined hhmm time bounds within case ii
    ##for (j in 1:length(user.defined.seg.number.list)) {
    
      ## find all other ind.hhmm.total the same as first to a new index, including looking outside the user-defined time
      ##ind.seg.number.j <- which(CNV.30s.mean.df$seg.num == user.defined.seg.number.list[j])
  
      # calc tot mins of flight track time (hh*mm)
      print(paste("-----------   ", sep=""))
      print(paste("    j = ",  j, sep=""))
      hh.flight.track              <- CNV.30s.mean.df$hh[j]
      mm.flight.track              <- CNV.30s.mean.df$mm[j]
      ss.flight.track              <- CNV.30s.mean.df$ss[j]
      hhmmss.closest               <- paste(hh.flight.track, ":", mm.flight.track, ":", ss.flight.track, sep="")
      print(paste("    Closest CNV 30-s mean hh:mm:ss = ", hhmmss.closest, " Z", sep=""))
      ss.tot.flight.track          <- (hh.flight.track * 60 * 60) + (mm.flight.track * 60) + ss.flight.track
      if (ss.tot.flight.track >= (((23*60)+59) * 60) + 59) {
        ss.tot.flight.track <- ss.tot.flight.track - 24*60*60
      } # end of if ...
    
      #browser()
      
      #-----------------------------------------------
      # load CNV NAW zen/nad version 1 nc data files that is closest in time
      #-----------------------------------------------
      # find the closest hh*mm time for CNV NAW
      print(paste("    Searching for representative CNV NAW data files...", sep=""))
      ind.CNV.NAW.zennad.file.closest.by.date <- which(as.numeric(substr(CNV.NAW.zennad.nc.listfiles, 17, 24)) == paste(substr(as.character(case.time.list.df$yyyymmdd[1]), 1, 4), substr(as.character(case.time.list.df$yyyymmdd[1]), 6, 7), substr(as.character(case.time.list.df$yyyymmdd[1]), 9, 10), sep="") )
      #ind.CNV.NAW.zennad.file.closest.by.fnum <- which(as.numeric(substr(CNV.NAW.zennad.nc.listfiles, 2, 3)) == as.numeric(as.character(case.time.list.df$flight.num[1])))
      ind.CNV.NAW.zennad.file.closest.by.ss   <- which((ss.start.tot.CNV.NAW.zennad.file <= ss.tot.flight.track) & (ss.end.tot.CNV.NAW.zennad.file >= ss.tot.flight.track))
      ind.CNV.NAW.zennad.file.closest.by.ss   <- intersect(ind.CNV.NAW.zennad.file.closest.by.date, ind.CNV.NAW.zennad.file.closest.by.ss)
      
      if (length(ind.CNV.NAW.zennad.file.closest.by.date) == 1 & length(ind.CNV.NAW.zennad.file.closest.by.ss) > 1) {
        
        # find the min ss diff and set to the closest
        ss.avetime.tot.CNV.NAW.zennad.file <- (ss.start.tot.CNV.NAW.zennad.file + ss.end.tot.CNV.NAW.zennad.file) / 2
        ss.diff.zennad.from.avetime        <- abs(ss.avetime.tot.C, NV.NAW.zennad.file - ss.tot.flight.track)
        ind.CNV.NAW.zennad.file.closest    <- which.min(abs(ss.diff.zennad.from.avetime - ss.tot.flight.track))
        
        # set NAW file match flag to 1 since flight/time match found
        CNV.NAW.zennad.nc.file.match.flag  <- 1
        
      } else if (length(ind.CNV.NAW.zennad.file.closest.by.date) > 1 & length(ind.CNV.NAW.zennad.file.closest.by.ss) == 1) {
        
        # find the intersect of closest.by.fnum and closest.by.ss and set to the closest
        ind.CNV.NAW.zennad.file.closest    <- intersect(ind.CNV.NAW.zennad.file.closest.by.date, ind.CNV.NAW.zennad.file.closest.by.ss)
        
        # set NAW file match flag to 1 since flight/time match found
        CNV.NAW.zennad.nc.file.match.flag  <- 1
        
      } else if (length(ind.CNV.NAW.zennad.file.closest.by.date) == 0) {
      
        # set NAW zen/nad values to NaN in no representative NAW nc file
        print("      No representative NAW file found...")
        print("        Setting appropriate NAW values to NaN")
        # fields for internal computations
        CNV.NAW.zennad.TIME.s       <- NaN
        CNV.NAW.zennad.ALTRANGE.m   <- NaN
        CNV.NAW.zennad.REFL.dbz     <- NaN
        CNV.NAW.zennad.VEL.mps      <- NaN
        # fields for output file
        CNV.NAW.REFL.30s.mean.zen   <- NaN
        CNV.NAW.REFL.30s.mean.nad   <- NaN
        CNV.NAW.REFL.30s.std.zen    <- NaN
        CNV.NAW.REFL.30s.std.nad    <- NaN
        CNV.NAW.REFL.range.std.zen  <- NaN
        CNV.NAW.REFL.range.std.nad  <- NaN
        CNV.NAW.VEL.30s.mean.zen    <- NaN
        CNV.NAW.VEL.30s.mean.nad    <- NaN
        CNV.NAW.VEL.30s.std.zen     <- NaN
        CNV.NAW.VEL.30s.std.nad     <- NaN
        CNV.NAW.VEL.range.std.zen   <- NaN
        CNV.NAW.VEL.range.std.nad   <- NaN
        
        # set NAW file match flag to 0 since flight/time match not found
        CNV.NAW.zennad.nc.file.match.flag  <- 0
        
        # ind closest set to -1 since does not exist
        ind.CNV.NAW.zennad.file.closest    <- -1
      
      } else if (length(ind.CNV.NAW.zennad.file.closest.by.date) == 1 & length(ind.CNV.NAW.zennad.file.closest.by.ss) == 1) {
        
        # set NAW file match flag to 0 since flight/time match not found
        CNV.NAW.zennad.nc.file.match.flag  <- 1
        
      } # end of if (length(ind.CNV.NAW.zennad.file.closest.by.fnum) > 1) 
      
      # load CNV NAW zen/nad version 1 interest files, if not already loaded
      if (CNV.NAW.zennad.nc.file.match.flag == 1 & length(ind.CNV.NAW.zennad.file.closest.by.ss) > 0) {
        if (ind.CNV.NAW.zennad.file.closest.by.ss != ind.CNV.NAW.zennad.file.closest.last) {  
          CNV.NAW.zennad.nc.filename       <- paste(CNV.NAW.zennad.dir, CNV.NAW.zennad.nc.listfiles[ind.CNV.NAW.zennad.file.closest.by.ss], sep="")
          print(paste("      Loading: ", CNV.NAW.zennad.nc.filename, sep=""))
          if (substr(CNV.NAW.zennad.nc.filename, nchar(CNV.NAW.zennad.nc.filename)-1, nchar(CNV.NAW.zennad.nc.filename)) == "gz") {
            print("      File is gzipped. Unzipping...")
            untar(CNV.NAW.zennad.nc.filename)
            CNV.NAW.zennad.nc.filename <- substr(CNV.NAW.zennad.nc.filename, 1, nchar(CNV.NAW.zennad.nc.filename)-3)
          }  # end of if ()...
          CNV.NAW.zennad.nc                <- nc_open(CNV.NAW.zennad.nc.filename, write = FALSE, verbose = FALSE)
          print(paste("      The file has", CNV.NAW.zennad.nc$nvars, "variables"))
          CNV.NAW.zennad.var.num           <- seq(1, CNV.NAW.zennad.nc$nvars, by=1)
          for (s in 1:length(CNV.NAW.zennad.var.num)) {
            CNV.NAW.zennad.nam <- paste("v", CNV.NAW.zennad.var.num[s], sep = "")
            assign(CNV.NAW.zennad.nam, CNV.NAW.zennad.nc$var[[CNV.NAW.zennad.var.num[s]]])
          }  # end of for ()...
          if (CNV.NAW.zennad.nc$nvars >= 19) {
            CNV.NAW.zennad.TIME.s       <- ncvar_get( CNV.NAW.zennad.nc, v1 )
            CNV.NAW.zennad.ALTRANGE.m   <- ncvar_get( CNV.NAW.zennad.nc, v2 )
            CNV.NAW.zennad.REFL.dbz     <- ncvar_get( CNV.NAW.zennad.nc, v3 )
            CNV.NAW.zennad.VEL.mps      <- ncvar_get( CNV.NAW.zennad.nc, v5 )
            CNV.NAW.zennad.ALT.m        <- ncvar_get( CNV.NAW.zennad.nc, v10)
            CNV.NAW.zennad.LAT.deg      <- ncvar_get( CNV.NAW.zennad.nc, v12)
            CNV.NAW.zennad.LON.deg      <- ncvar_get( CNV.NAW.zennad.nc, v13)
            CNV.NAW.zennad.TAS.mps      <- ncvar_get( CNV.NAW.zennad.nc, v14)
            CNV.NAW.zennad.SIDERANGE.m  <- ncvar_get( CNV.NAW.zennad.nc, v15)
            CNV.NAW.zennad.SIDEREFL.dbz <- ncvar_get( CNV.NAW.zennad.nc, v16)
            CNV.NAW.zennad.SIDEVEL.mps  <- ncvar_get( CNV.NAW.zennad.nc, v17)
          } else {
            print("ERROR")
          }  # end of if ()...
          #print(paste("V1 has name", v1$name))
          #browser()
          ind.CNV.NAW.zennad.file.closest.last <- ind.CNV.NAW.zennad.file.closest.by.ss
          nc_close(CNV.NAW.zennad.nc)
          #CNV.NAW.zennad.df <- data.frame(CNV.NAW.zennad[ , , ind.nssl.mosaic.alt])
          #head(FZDZ.mosaic.df)
        
        } else if (ind.CNV.NAW.zennad.file.closest.by.ss == ind.CNV.NAW.zennad.file.closest.last) {
          print("     Closest NAW time is same as the last, which is already loaded")
        }    # end of if (ind.mosaic.file.closest......)   
        
        #----------------------------------
        # Manipulate CNV.NAW data
        #----------------------------------
        #   convert CNV.NAW.TIME.s secs since 1970 to UTC, and hh:mm
        CNV.NAW.zennad.TIME.utc              <- as.POSIXct(CNV.NAW.zennad.TIME.s, origin = "1970-01-01", tz = "UTC")
        CNV.NAW.zennad.TIME.hh               <- hour(CNV.NAW.zennad.TIME.utc)
        CNV.NAW.zennad.TIME.mm               <- minute(CNV.NAW.zennad.TIME.utc)
        CNV.NAW.zennad.TIME.ss               <- second(CNV.NAW.zennad.TIME.utc)
        CNV.NAW.zennad.TIME.hhmm             <- CNV.NAW.zennad.TIME.hh + CNV.NAW.zennad.TIME.mm / 60
        #CNV.NAW.zennad.TIME.hhmm             <- paste(CNV.NAW.zennad.TIME.hh, CNV.NAW.zennad.TIME.mm, sep=":")
        
        #   find indices of CNV.NAW at start and stop time hhmmss[j] of 30-s mean period 
        ind.CNV.NAW.zennad.TIME              <- which(CNV.NAW.zennad.TIME.hh == CNV.30s.mean.df$hh[j] & CNV.NAW.zennad.TIME.mm == CNV.30s.mean.df$mm[j] & round(CNV.NAW.zennad.TIME.ss, digits=0) == CNV.30s.mean.df$ss[j])
        if (length(ind.CNV.NAW.zennad.TIME) > 1) {
          ind.CNV.NAW.zennad.TIME <- round(mean(ind.CNV.NAW.zennad.TIME), digits=0)
        }  # end of if (length(ind.CNV.NAW.zennad.TIME) > 1)
        # find the indices of the start (15 seconds before 30-s mean time) and end (15 seconds after to 30-s mean time) times 
        ind.CNV.NAW.zennad.TIME.30s.start    <- which(as.character(CNV.NAW.zennad.TIME.utc) == as.character(CNV.NAW.zennad.TIME.utc[ind.CNV.NAW.zennad.TIME] - 15))
        ind.CNV.NAW.zennad.TIME.30s.start    <- ind.CNV.NAW.zennad.TIME.30s.start[1]
        ind.CNV.NAW.zennad.TIME.30s.end      <- which(as.character(CNV.NAW.zennad.TIME.utc) == as.character(CNV.NAW.zennad.TIME.utc[ind.CNV.NAW.zennad.TIME] + 15))
        ind.CNV.NAW.zennad.TIME.30s.end      <- ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]
        # find the index of the closest MSL altitude range to the jth CNV 30-s mean altitude
        ind.CNV.NAW.zennad.ALTRANGE.m.clos   <- which.min(abs(CNV.30s.mean.df$alt.m[j] - CNV.NAW.zennad.ALTRANGE.m))
        
        # define some NAW-related parameters
        NAW.range.offset                     <- 2  # number of range gates away from NAW to start using
        NAW.num.ranges                       <- 20 # number of ranges away from CNV to include in sd(REFL) in nad/zen range
        
        if (is.na(ind.CNV.NAW.zennad.TIME.30s.start[1]) | length(ind.CNV.NAW.zennad.TIME.30s.end) == 0) {
          CNV.NAW.REFL.30s.mean.zen            <- NaN
          CNV.NAW.REFL.30s.mean.nad            <- NaN
          CNV.NAW.REFL.30s.std.zen             <- NaN
          CNV.NAW.REFL.30s.std.nad             <- NaN
          CNV.NAW.REFL.range.std.zen           <- NaN
          CNV.NAW.REFL.range.std.nad           <- NaN
          CNV.NAW.VEL.30s.mean.zen             <- NaN
          CNV.NAW.VEL.30s.mean.nad             <- NaN
          CNV.NAW.VEL.30s.std.zen              <- NaN
          CNV.NAW.VEL.30s.std.nad              <- NaN
          CNV.NAW.VEL.range.std.zen            <- NaN
          CNV.NAW.VEL.range.std.nad            <- NaN
        } else {
          #  calculate some statistical values from NAW zenith REFL/VEL returns  
          if (ind.CNV.NAW.zennad.ALTRANGE.m.clos + NAW.range.offset <= dim(CNV.NAW.zennad.REFL.dbz)[1]) {
            CNV.NAW.REFL.30s.mean.zen  <- round(mean(CNV.NAW.zennad.REFL.dbz[  ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.REFL.30s.std.zen   <- round(sd(  CNV.NAW.zennad.REFL.dbz[  ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.30s.mean.zen   <- round(mean(CNV.NAW.zennad.VEL.mps[   ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.30s.std.zen    <- round(sd(  CNV.NAW.zennad.VEL.mps[   ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
          } else {
            CNV.NAW.REFL.30s.mean.zen  <- NaN
            CNV.NAW.REFL.30s.std.zen   <- NaN
            CNV.NAW.VEL.30s.mean.zen   <- NaN
            CNV.NAW.VEL.30s.std.zen    <- NaN
          }
          if (ind.CNV.NAW.zennad.ALTRANGE.m.clos + NAW.range.offset + NAW.num.ranges <= dim(CNV.NAW.zennad.REFL.dbz)[1]) {
            CNV.NAW.REFL.range.std.zen <- round(sd(  CNV.NAW.zennad.REFL.dbz[ (ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset):(ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset+NAW.num.ranges), ind.CNV.NAW.zennad.TIME], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.range.std.zen  <- round(sd(  CNV.NAW.zennad.VEL.mps[  (ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset):(ind.CNV.NAW.zennad.ALTRANGE.m.clos+NAW.range.offset+NAW.num.ranges), ind.CNV.NAW.zennad.TIME], na.rm = TRUE), digits = 2)
           } else {
            CNV.NAW.REFL.range.std.zen <- NaN 
            CNV.NAW.VEL.range.std.zen  <- NaN
          }
            
          #  calculate some statistical values from NAW nadir REFL/VEL returns
          if (ind.CNV.NAW.zennad.ALTRANGE.m.clos - NAW.range.offset >= 0) {
            CNV.NAW.REFL.30s.mean.nad  <- round(mean(CNV.NAW.zennad.REFL.dbz[  ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.REFL.30s.std.nad   <- round(sd(  CNV.NAW.zennad.REFL.dbz[  ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.30s.mean.nad   <- round(mean(CNV.NAW.zennad.VEL.mps[   ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.30s.std.nad    <- round(sd(  CNV.NAW.zennad.VEL.mps[   ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset,                                                                       ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], na.rm = TRUE), digits = 2)
          } else {
            CNV.NAW.REFL.30s.mean.zen  <- NaN
            CNV.NAW.REFL.30s.std.zen   <- NaN
            CNV.NAW.VEL.30s.mean.zen   <- NaN
            CNV.NAW.VEL.30s.std.zen    <- NaN
          }
          if (ind.CNV.NAW.zennad.ALTRANGE.m.clos - NAW.range.offset - NAW.num.ranges >= 0) {
            CNV.NAW.REFL.range.std.nad <- round(sd(  CNV.NAW.zennad.REFL.dbz[(ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset-NAW.num.ranges):(ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset),  ind.CNV.NAW.zennad.TIME], na.rm = TRUE), digits = 2)
            CNV.NAW.VEL.range.std.nad  <- round(sd(  CNV.NAW.zennad.VEL.mps[ (ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset-NAW.num.ranges):(ind.CNV.NAW.zennad.ALTRANGE.m.clos-NAW.range.offset),  ind.CNV.NAW.zennad.TIME], na.rm = TRUE), digits = 2)
          } else {
            CNV.NAW.REFL.range.std.nad <- NaN
            CNV.NAW.VEL.range.std.nad  <- NaN         
          }
            
            
        } # end of if (length(ind.CNV.NAW.zennad.TIME.30s.end) == 0) ...
          
        #print(paste("NAW zen/nad index seq for this 30s:", ind.CNV.NAW.zennad.TIME.30s.start[1]:ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)], sep=""))
        # print NAW zen/nad some values
        #print(paste("    j = ", j, ", hh:mm:ss[j] = ", CNV.30s.mean.df$hh[j], ":", CNV.30s.mean.df$mm[j], ":", CNV.30s.mean.df$ss[j], ", alt.m[j] = ", CNV.30s.mean.df$alt.m[j],sep=""))
        print(paste("    Printing NAW zen/nad matchup values to GUI:", sep=""))
        print(paste("      NAW zen/nad matched time = ",     CNV.NAW.zennad.TIME.utc[ind.CNV.NAW.zennad.TIME], sep=""))
        print(paste("      NAW zen/nad matched index = ",    ind.CNV.NAW.zennad.TIME, sep=""))
        print(paste("      NAW zen/nad REFL matrix size = ", dim(CNV.NAW.zennad.REFL.dbz), sep=""))
        print(paste("      NAW zen/nad start time = ",       CNV.NAW.zennad.TIME.utc[ind.CNV.NAW.zennad.TIME.30s.start[1]], sep=""))
        print(paste("      NAW zen/nad end time = ",         CNV.NAW.zennad.TIME.utc[ind.CNV.NAW.zennad.TIME.30s.end[length(ind.CNV.NAW.zennad.TIME.30s.end)]], sep=""))
        print(paste("      NAW REFL 30s mean (zen):",        CNV.NAW.REFL.30s.mean.zen))
        print(paste("      NAW REFL 30s mean (nad):",        CNV.NAW.REFL.30s.mean.nad))
        print(paste("      NAW REFL 30s std (zen):",         CNV.NAW.REFL.30s.std.zen))
        print(paste("      NAW REFL 30s std (nad):",         CNV.NAW.REFL.30s.std.nad))
        print(paste("      NAW REFL range std (zen):",       CNV.NAW.REFL.range.std.zen))
        print(paste("      NAW REFL range std (nad):",       CNV.NAW.REFL.range.std.nad))
        print(paste("      NAW VEL 30s mean (zen):",         CNV.NAW.VEL.30s.mean.zen))
        print(paste("      NAW VEL 30s mean (nad):",         CNV.NAW.VEL.30s.mean.nad))
        print(paste("      NAW VEL 30s std (zen):",          CNV.NAW.VEL.30s.std.zen))
        print(paste("      NAW VEL 30s std (nad):",          CNV.NAW.VEL.30s.std.nad))
        print(paste("      NAW VEL range std (zen):",        CNV.NAW.VEL.range.std.zen))
        print(paste("      NAW VEL range std (nad):",        CNV.NAW.VEL.range.std.nad))
        
      } else {
        
        CNV.NAW.REFL.30s.mean.zen            <- NaN
        CNV.NAW.REFL.30s.mean.nad            <- NaN
        CNV.NAW.REFL.30s.std.zen             <- NaN
        CNV.NAW.REFL.30s.std.nad             <- NaN
        CNV.NAW.REFL.range.std.zen           <- NaN
        CNV.NAW.REFL.range.std.nad           <- NaN
        CNV.NAW.VEL.30s.mean.zen             <- NaN
        CNV.NAW.VEL.30s.mean.nad             <- NaN
        CNV.NAW.VEL.30s.std.zen              <- NaN
        CNV.NAW.VEL.30s.std.nad              <- NaN
        CNV.NAW.VEL.range.std.zen            <- NaN
        CNV.NAW.VEL.range.std.nad            <- NaN
        
      } # end of if (CNV.NAW.zennad.nc.file.match.flag == 1) ...
     
      #browser()
      
      #-----------------------------------------------
      # load RadIA-mosaic nc format data files that is closest in time
      #-----------------------------------------------
      print(paste("    Searching for representative RadIA-mosaic data files...", sep=""))
      
      # initialize arrays for each field, one entry per value in flight track 
      length.flight.track                 <- dim(CNV.30s.mean.df)[1]
      ind.closest.mosaic.lat.pixel        <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      ind.closest.mosaic.lon.pixel        <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      ind.closest.mosaic.alt.pixel        <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      ind.closest.mosaic.pres.pixel       <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      
      if (latest.mosaic.ver.num == 1) {
        FZDZ.closest.mosaic.pixel           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanDBZ.FZDZ.closest.mosaic.pixel   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevDBZ.FZDZ.closest.mosaic.pixel   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TDBZ.FZDZ.closest.mosaic.pixel      <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        SLW.closest.mosaic.pixel            <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanZDR.SLW.closest.mosaic.pixel    <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevZDR.SLW.closest.mosaic.pixel    <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanKDP.SLW.closest.mosaic.pixel    <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevKDP.SLW.closest.mosaic.pixel    <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        MIXPHA.closest.mosaic.pixel         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanZDR.MIXPHA.closest.mosaic.pixel <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanDBZ.MIXPHA.closest.mosaic.pixel <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TEMP.MIXPHA.closest.mosaic.pixel    <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
      
        FZDZ.closest.mosaic.volume          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanDBZ.FZDZ.closest.mosaic.volume  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevDBZ.FZDZ.closest.mosaic.volume  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TDBZ.FZDZ.closest.mosaic.volume     <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        SLW.closest.mosaic.volume           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanZDR.SLW.closest.mosaic.volume   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevZDR.SLW.closest.mosaic.volume   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanKDP.SLW.closest.mosaic.volume   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdevKDP.SLW.closest.mosaic.volume   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        MIXPHA.closest.mosaic.volume        <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanZDR.MIXPHA.closest.mosaic.volume<- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        meanDBZ.MIXPHA.closest.mosaic.volume<- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TEMP.MIXPHA.closest.mosaic.volume   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        
      } else if (latest.mosaic.ver.num >= 2) {
        grid.i                        <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        grid.j                        <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        grid.k                        <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TEMP.CELSIUS.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        FZDZ.INT.pix                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        SLW.INT.pix                   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        MIXPHA.INT.pix                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        PLATES.INT.pix                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        RADIA2.VAL.pix                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        RHO.MOMS.pix                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        DBZ.MOMS.pix                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        ZDR.MOMS.pix                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        KDP.MOMS.pix                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.DBZ.FZDZ.pix             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.DBZ.FZDZ.pix             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        T.DBZ.FZDZ.pix                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.DBZ.FZDZ.pix             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        block.sdev.DBZ.FZDZ.pix       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        block.median.DBZ.FZDZ.pix     <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.DBZ.FZDZ.pix         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.T.DBZ.FZDZ.pix            <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.FZDZ.pix         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.block.sdev.DBZ.FZDZ.pix   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.block.median.DBZ.FZDZ.pix <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.FZDZ.pix             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.ZDR.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.ZDR.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.ZDR.corr.SLW.pix         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.ZDR.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.KDP.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.KDP.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.KDP.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.corr.SLW.pix     <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.ZDR.SLW.pix          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.KDP.SLW.pix          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.KDP.SLW.pix          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.SLW.pix              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.MIXPHA.pix       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.MIXPHA.pix       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.temp.MIXPHA.pix           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.MIXPHA.pix           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.PLATES.pix       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.PLATES.pix       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.temp.PLATES.pix           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.PLATES.pix           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        TEMP_CELSIUS.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        FZDZ.INT.vol                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        SLW.INT.vol                   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        MIXPHA.INT.vol                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        PLATES.INT.vol                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        RADIA2.VAL.vol                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        RHO.MOMS.vol                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        DBZ.MOMS.vol                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        ZDR.MOMS.vol                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        KDP.MOMS.vol                  <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.DBZ.FZDZ.vol             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.DBZ.FZDZ.vol             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        T.DBZ.FZDZ.vol                <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.DBZ.FZDZ.vol             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        block.sdev.DBZ.FZDZ.vol       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        block.median.DBZ.FZDZ.vol     <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.DBZ.FZDZ.vol         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.T.DBZ.FZDZ.vol            <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.FZDZ.vol         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.block.sdev.DBZ.FZDZ.vol   <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.block.median.DBZ.FZDZ.vol <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.FZDZ.vol             <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.ZDR.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.ZDR.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.ZDR.corr.SLW.vol         <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.ZDR.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mask.KDP.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        sdev.KDP.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        mean.KDP.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.corr.SLW.vol     <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.ZDR.SLW.vol          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.KDP.SLW.vol          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.sdev.KDP.SLW.vol          <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.SLW.vol              <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.MIXPHA.vol       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.MIXPHA.vol       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.temp.MIXPHA.vol           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.MIXPHA.vol           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.ZDR.PLATES.vol       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.mean.DBZ.PLATES.vol       <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.temp.PLATES.vol           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
        int.full.PLATES.vol           <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
      }
        
      hhmm.closest                        <- array(data = NaN, dim = c(dim(CNV.30s.mean.df)[1]))
      X.rad                               <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      Y.rad                               <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      bearing.radartoac.deg               <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      bearing.radartoac.ind               <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      a                                   <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      c                                   <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      dist.radartoac.km                   <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      alpha.deg                           <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      range.radartoac.km                  <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      range.radartoac.ind                 <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      theta.radartoac.deg                 <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      theta.radartoac.ind                 <- array(data = 0L, dim = c(dim(CNV.30s.mean.df)[1]))
      
      if (latest.mosaic.ver.num == 1) {
        
        # find the closest RadIA-mosaic alt to the CNV alt at time j
        ind.nssl.mosaic.alt                 <- which.min(abs(mosaic.alt.array - (CNV.30s.mean.df$alt.m[j] * kmPERm)))
        
        # find the closest hh*mm time for RadIA-mosaic
        ind.mosaic.FZDZ.file.closest        <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.FZDZ.file*60)))
        ind.mosaic.SLW.file.closest         <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.SLW.file*60)))
        ind.mosaic.MIXPHA.file.closest      <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.MIXPHA.file*60)))

        #----------------------------------------------
        # load RadIA-mosaic version 1 interest files, if not already loaded
        #----------------------------------------------
        # mosaic file containing FZDZ
        if (ind.mosaic.FZDZ.file.closest != ind.mosaic.FZDZ.file.closest.last) {  
          radia.mosaic.nc.FZDZ.filename       <- paste(radia.mosaic.nc.FZDZ.dir, radia.mosaic.nc.FZDZ.listfiles[ind.mosaic.FZDZ.file.closest], sep="")
          print(paste("      Loading: ", radia.mosaic.nc.FZDZ.filename, sep=""))
          if (substr(radia.mosaic.nc.FZDZ.filename, nchar(radia.mosaic.nc.FZDZ.filename)-1, nchar(radia.mosaic.nc.FZDZ.filename)) == "gz") {
            print("      File is gzipped. Unzipping...")
            untar(radia.mosaic.nc.FZDZ.filename)
            radia.mosaic.nc.FZDZ.filename <- substr(radia.mosaic.nc.FZDZ.filename, 1, nchar(radia.mosaic.nc.FZDZ.filename)-3)
          }  # end of if ()...
          radia.mosaic.nc.FZDZ                <- nc_open(radia.mosaic.nc.FZDZ.filename, write = FALSE, verbose = FALSE)
          #print(paste("The file has", radia.mosaic.nc.FZDZ$nvars, "variables"))
          radia.mosaic.FZDZ.var.num           <- seq(1, radia.mosaic.nc.FZDZ$nvars, by=1)
          for (s in 1:length(radia.mosaic.FZDZ.var.num)) {
            radia.mosaic.FZDZ.nam <- paste("v", radia.mosaic.FZDZ.var.num[s], sep = "")
            assign(radia.mosaic.FZDZ.nam, radia.mosaic.nc.FZDZ$var[[radia.mosaic.FZDZ.var.num[s]]])
          }  # end of for ()...
          if (radia.mosaic.nc.FZDZ$nvars >= 9) {
            FZDZ.mosaic                                <- ncvar_get( radia.mosaic.nc.FZDZ, v6 )
            meanDBZ.FZDZ.mosaic                        <- ncvar_get( radia.mosaic.nc.FZDZ, v7 )
            sdevDBZ.FZDZ.mosaic                        <- ncvar_get( radia.mosaic.nc.FZDZ, v8 )
            TDBZ.FZDZ.mosaic                           <- ncvar_get( radia.mosaic.nc.FZDZ, v9 )
          } else if (radia.mosaic.nc.FZDZ$nvars == 4) {
            FZDZ.mosaic                                <- ncvar_get( radia.mosaic.nc.FZDZ, v1 )
            meanDBZ.FZDZ.mosaic                        <- ncvar_get( radia.mosaic.nc.FZDZ, v2 )
            sdevDBZ.FZDZ.mosaic                        <- ncvar_get( radia.mosaic.nc.FZDZ, v3 )
            TDBZ.FZDZ.mosaic                           <- ncvar_get( radia.mosaic.nc.FZDZ, v4 )
          } else if (radia.mosaic.nc.FZDZ$nvars == 1) {
            FZDZ.mosaic                                <- ncvar_get( radia.mosaic.nc.FZDZ, v1 )
          }  # end of if ()...
          #print(paste("V1 has name", v1$name))
          nc_close(radia.mosaic.nc.FZDZ)
          FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
          #head(FZDZ.mosaic.df)
        } else {
          print("      Closest mosaic time is same as the last, which is already loaded")
        }    # end of if (ind.mosaic.file.closest...)
  
        # mosaic file containing SLW
        if (ind.mosaic.SLW.file.closest != ind.mosaic.SLW.file.closest.last) {
          radia.mosaic.nc.SLW.filename       <- paste(radia.mosaic.nc.SLW.dir, radia.mosaic.nc.SLW.listfiles[ind.mosaic.SLW.file.closest], sep="")
          print(paste("        Loading: ", radia.mosaic.nc.SLW.filename, sep=""))
          if (substr(radia.mosaic.nc.SLW.filename, nchar(radia.mosaic.nc.SLW.filename)-1, nchar(radia.mosaic.nc.SLW.filename)) == "gz") {
            print("          File is gzipped. Unzipping...")
            untar(radia.mosaic.nc.SLW.filename)
            radia.mosaic.nc.SLW.filename <- substr(radia.mosaic.nc.SLW.filename, 1, nchar(radia.mosaic.nc.SLW.filename)-3)
          }  # end of if () ...
          radia.mosaic.nc.SLW                     <- nc_open(radia.mosaic.nc.SLW.filename, write = FALSE, verbose = FALSE)
          #print(paste("The file has", radia.mosaic.nc.SLW$nvars, "variables"))
          radia.mosaic.SLW.var.num                <- seq(1, radia.mosaic.nc.SLW$nvars, by=1)
          for (s in 1:length(radia.mosaic.SLW.var.num)) {
            radia.mosaic.SLW.nam <- paste("v", radia.mosaic.SLW.var.num[s], sep = "")
            assign(radia.mosaic.SLW.nam, radia.mosaic.nc.SLW$var[[radia.mosaic.SLW.var.num[s]]])
          }  # end of for () ...
          if (radia.mosaic.nc.SLW$nvars >= 10) {
            SLW.mosaic                                <- ncvar_get( radia.mosaic.nc.SLW, v6 )
            meanZDR.SLW.mosaic                        <- ncvar_get( radia.mosaic.nc.SLW, v7 )
            sdevZDR.SLW.mosaic                        <- ncvar_get( radia.mosaic.nc.SLW, v8 )
            meanKDP.SLW.mosaic                        <- ncvar_get( radia.mosaic.nc.SLW, v9 )
            sdevKDP.SLW.mosaic                        <- ncvar_get( radia.mosaic.nc.SLW, v10)
          } else if (radia.mosaic.nc.SLW$nvars == 1) {
            SLW.mosaic                                <- ncvar_get( radia.mosaic.nc.SLW, v1 )
          }  # end of if ()...
          #print(paste("V1 has name", v1$name))
          nc_close(radia.mosaic.nc.SLW)
          SLW.mosaic.df <- data.frame(SLW.mosaic[ , , ind.nssl.mosaic.alt])
        }    # end of if (ind.mosaic.SLW.file.closest ~= ...)
  
        # mosaic file containing MIXPHA
        if (ind.mosaic.MIXPHA.file.closest != ind.mosaic.MIXPHA.file.closest.last) {
          radia.mosaic.nc.MIXPHA.filename       <- paste(radia.mosaic.nc.MIXPHA.dir, radia.mosaic.nc.MIXPHA.listfiles[ind.mosaic.MIXPHA.file.closest], sep="")
          print(paste("        Loading: ", radia.mosaic.nc.MIXPHA.filename, sep=""))
          if (substr(radia.mosaic.nc.MIXPHA.filename, nchar(radia.mosaic.nc.MIXPHA.filename)-1, nchar(radia.mosaic.nc.MIXPHA.filename)) == "gz") {
            print("          File is gzipped. Unzipping...")
            untar(radia.mosaic.nc.MIXPHA.filename)
            radia.mosaic.nc.MIXPHA.filename <- substr(radia.mosaic.nc.MIXPHA.filename, 1, nchar(radia.mosaic.nc.MIXPHA.filename)-3)
          }  # end of if () ...
          radia.mosaic.nc.MIXPHA                <- nc_open(radia.mosaic.nc.MIXPHA.filename, write = FALSE, verbose = FALSE)
          #print(paste("The file has", radia.mosaic.nc.MIXPHA$nvars, "variables"))
          radia.mosaic.MIXPHA.var.num           <- seq(1, radia.mosaic.nc.MIXPHA$nvars, by=1)
          for (s in 1:length(radia.mosaic.MIXPHA.var.num)) {
            radia.mosaic.MIXPHA.nam <- paste("v", radia.mosaic.MIXPHA.var.num[s], sep = "")
            assign(radia.mosaic.MIXPHA.nam, radia.mosaic.nc.MIXPHA$var[[radia.mosaic.MIXPHA.var.num[s]]])
          }  # end of for ()...
          if (radia.mosaic.nc.MIXPHA$nvars >= 9) {
            MIXPHA.mosaic                               <- ncvar_get( radia.mosaic.nc.MIXPHA, v6 )
            meanZDR.MIXPHA.mosaic                       <- ncvar_get( radia.mosaic.nc.MIXPHA, v7 )
            meanDBZ.MIXPHA.mosaic                       <- ncvar_get( radia.mosaic.nc.MIXPHA, v8 )
            TEMP.MIXPHA.mosaic                          <- ncvar_get( radia.mosaic.nc.MIXPHA, v9 )
          } else if (radia.mosaic.nc.MIXPHA$nvars == 1) {
            MIXPHA.mosaic                               <- ncvar_get( radia.mosaic.nc.MIXPHA, v1 )
          }  # end of if ()...
          #print(paste("V1 has name", v1$name))
          nc_close(radia.mosaic.nc.MIXPHA)
          MIXPHA.mosaic.df <- data.frame(MIXPHA.mosaic[ , , ind.nssl.mosaic.alt])
        }    # end of if (ind.mosaic.MIXPHA.file.closest ~= ...)
  
        # define indices of the last closest file, to see if a new nc file needs to be loaded or not
        ind.mosaic.FZDZ.file.closest.last      <- ind.mosaic.FZDZ.file.closest
        ind.mosaic.SLW.file.closest.last       <- ind.mosaic.SLW.file.closest
        ind.mosaic.MIXPHA.file.closest.last    <- ind.mosaic.MIXPHA.file.closest
        
        #----------------------------------------------------------------------------------
        # Manipulate the RadIA-mosaic version 1 moment stats, feature fields, and INTs data
        #----------------------------------------------------------------------------------
        # find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
        ind.closest.mosaic.lat.pixel[j]        <- which.min(abs(CNV.30s.mean.df$lat.deg[j] - mosaic.lat.array))
        ind.closest.mosaic.lon.pixel[j]        <- which.min(abs(CNV.30s.mean.df$lon.deg[j] - mosaic.lon.array))
        ind.closest.mosaic.alt.pixel[j]        <- which.min(abs(CNV.30s.mean.df$alt.m[j]   - (mosaic.alt.array / kmPERm)))  # both values in [m]
        
        # define and save the single pixel INT value for each of the three RadIA-mosaic algorithms that are closest to the aircraft at the closest time 
        #   for FZDZ
        FZDZ.closest.mosaic.pixel[j]           <- FZDZ.mosaic[           ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        meanDBZ.FZDZ.closest.mosaic.pixel[j]   <- meanDBZ.FZDZ.mosaic[   ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        sdevDBZ.FZDZ.closest.mosaic.pixel[j]   <- sdevDBZ.FZDZ.mosaic[   ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        TDBZ.FZDZ.closest.mosaic.pixel[j]      <- TDBZ.FZDZ.mosaic[      ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        #   for SLW
        SLW.closest.mosaic.pixel[j]            <- SLW.mosaic[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        meanZDR.SLW.closest.mosaic.pixel[j]    <- meanZDR.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        sdevZDR.SLW.closest.mosaic.pixel[j]    <- sdevZDR.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        meanKDP.SLW.closest.mosaic.pixel[j]    <- meanKDP.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        sdevKDP.SLW.closest.mosaic.pixel[j]    <- sdevKDP.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        #   for MIXPHA
        MIXPHA.closest.mosaic.pixel[j]         <- MIXPHA.mosaic[         ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        meanZDR.MIXPHA.closest.mosaic.pixel[j] <- meanZDR.MIXPHA.mosaic[ ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        meanDBZ.MIXPHA.closest.mosaic.pixel[j] <- meanDBZ.MIXPHA.mosaic[ ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        TEMP.MIXPHA.closest.mosaic.pixel[j]    <- TEMP.MIXPHA.mosaic[    ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.alt.pixel[j]]
        
        # define and save the user-defined volume INT values for each of the three RadIA-mosaic algorithms that are closest to the aircraft at the closest time
        #   for FZDZ
        FZDZ.closest.mosaic.volume             <- FZDZ.mosaic[           ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        meanDBZ.FZDZ.closest.mosaic.volume     <- meanDBZ.FZDZ.mosaic[   ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        sdevDBZ.FZDZ.closest.mosaic.volume     <- sdevDBZ.FZDZ.mosaic[   ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        TDBZ.FZDZ.closest.mosaic.volume        <- TDBZ.FZDZ.mosaic[      ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        #   for SLW
        SLW.closest.mosaic.volume              <- SLW.mosaic[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        meanZDR.SLW.closest.mosaic.volume      <- meanZDR.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        sdevZDR.SLW.closest.mosaic.volume      <- sdevZDR.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        meanKDP.SLW.closest.mosaic.volume      <- meanKDP.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        sdevKDP.SLW.closest.mosaic.volume      <- sdevKDP.SLW.mosaic[    ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        #   for MIXPHA
        MIXPHA.closest.mosaic.volume           <- MIXPHA.mosaic[         ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        meanZDR.MIXPHA.closest.mosaic.volume   <- meanZDR.MIXPHA.mosaic[ ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        meanDBZ.MIXPHA.closest.mosaic.volume   <- meanDBZ.MIXPHA.mosaic[ ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        TEMP.MIXPHA.closest.mosaic.volume      <- TEMP.MIXPHA.mosaic[    ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.alt.pixel[j]+ind.pixels.around.closest.alt]
        
        # FZDZ calculate some statistics
        meanDBZ.FZDZ.closest.mosaic.mean.volume   <- mean(meanDBZ.FZDZ.closest.mosaic.volume, na.rm=TRUE)
        sdevDBZ.FZDZ.closest.mosaic.mean.volume   <- mean(sdevDBZ.FZDZ.closest.mosaic.volume, na.rm=TRUE)
        TDBZ.FZDZ.closest.mosaic.mean.volume      <- mean(TDBZ.FZDZ.closest.mosaic.volume,    na.rm=TRUE)
        FZDZ.closest.mosaic.mean.volume           <- mean(FZDZ.closest.mosaic.volume,         na.rm=TRUE)
        #meanDBZ.FZDZ.closest.mosaic.max.volume    <- max(meanDBZ.FZDZ.closest.mosaic.volume,  na.rm=TRUE)
        #sdevDBZ.FZDZ.closest.mosaic.max.volume    <- max(sdevDBZ.FZDZ.closest.mosaic.volume,  na.rm=TRUE)
        #TDBZ.FZDZ.closest.mosaic.max.volume       <- max(TDBZ.FZDZ.closest.mosaic.volume,     na.rm=TRUE)
        FZDZ.closest.mosaic.max.volume            <- max(FZDZ.closest.mosaic.volume,          na.rm=TRUE)
        ind.FZDZ.closest.mosaic.max.volume        <- which(FZDZ.closest.mosaic.volume == FZDZ.closest.mosaic.max.volume)
        meanDBZ.FZDZ.closest.mosaic.max.volume    <- meanDBZ.FZDZ.closest.mosaic.volume[ind.FZDZ.closest.mosaic.max.volume]
        sdevDBZ.FZDZ.closest.mosaic.max.volume    <- sdevDBZ.FZDZ.closest.mosaic.volume[ind.FZDZ.closest.mosaic.max.volume]
        TDBZ.FZDZ.closest.mosaic.max.volume       <- TDBZ.FZDZ.closest.mosaic.volume[ind.FZDZ.closest.mosaic.max.volume]
        
        # SLW calculate some statistics
        meanZDR.SLW.closest.mosaic.mean.volume    <- mean(meanZDR.SLW.closest.mosaic.volume, na.rm=TRUE)
        sdevZDR.SLW.closest.mosaic.mean.volume    <- mean(sdevZDR.SLW.closest.mosaic.volume, na.rm=TRUE)
        meanKDP.SLW.closest.mosaic.mean.volume    <- mean(meanKDP.SLW.closest.mosaic.volume, na.rm=TRUE)
        sdevKDP.SLW.closest.mosaic.mean.volume    <- mean(sdevKDP.SLW.closest.mosaic.volume, na.rm=TRUE)
        SLW.closest.mosaic.mean.volume            <- mean(SLW.closest.mosaic.volume,         na.rm=TRUE)
        #meanZDR.SLW.closest.mosaic.max.volume     <- max(meanZDR.SLW.closest.mosaic.volume,  na.rm=TRUE)
        #sdevZDR.SLW.closest.mosaic.max.volume     <- max(sdevZDR.SLW.closest.mosaic.volume,  na.rm=TRUE)
        #meanKDP.SLW.closest.mosaic.max.volume     <- max(meanKDP.SLW.closest.mosaic.volume,  na.rm=TRUE)
        #sdevKDP.SLW.closest.mosaic.max.volume     <- max(sdevKDP.SLW.closest.mosaic.volume,  na.rm=TRUE)
        SLW.closest.mosaic.max.volume             <- max(SLW.closest.mosaic.volume,          na.rm=TRUE)
        ind.SLW.closest.mosaic.max.volume         <- which(SLW.closest.mosaic.volume == SLW.closest.mosaic.max.volume)
        meanZDR.SLW.closest.mosaic.max.volume     <- meanZDR.SLW.closest.mosaic.volume[ind.SLW.closest.mosaic.max.volume]
        sdevZDR.SLW.closest.mosaic.max.volume     <- sdevZDR.SLW.closest.mosaic.volume[ind.SLW.closest.mosaic.max.volume]
        meanKDP.SLW.closest.mosaic.max.volume     <- meanKDP.SLW.closest.mosaic.volume[ind.SLW.closest.mosaic.max.volume]
        sdevKDP.SLW.closest.mosaic.max.volume     <- sdevKDP.SLW.closest.mosaic.volume[ind.SLW.closest.mosaic.max.volume]
        
        # MIXPHA calculate some statistics
        meanZDR.MIXPHA.closest.mosaic.mean.volume <- mean(meanZDR.MIXPHA.closest.mosaic.volume, na.rm=TRUE)
        meanDBZ.MIXPHA.closest.mosaic.mean.volume <- mean(meanDBZ.MIXPHA.closest.mosaic.volume, na.rm=TRUE)
        TEMP.MIXPHA.closest.mosaic.mean.volume    <- mean(TEMP.MIXPHA.closest.mosaic.volume,    na.rm=TRUE)
        MIXPHA.closest.mosaic.mean.volume         <- mean(MIXPHA.closest.mosaic.volume,         na.rm=TRUE)
        #meanZDR.MIXPHA.closest.mosaic.max.volume  <- max(meanZDR.MIXPHA.closest.mosaic.volume,  na.rm=TRUE)
        #meanDBZ.MIXPHA.closest.mosaic.max.volume  <- max(meanDBZ.MIXPHA.closest.mosaic.volume,  na.rm=TRUE)
        #TEMP.MIXPHA.closest.mosaic.max.volume     <- max(TEMP.MIXPHA.closest.mosaic.volume,     na.rm=TRUE)
        MIXPHA.closest.mosaic.max.volume          <- max(MIXPHA.closest.mosaic.volume,          na.rm=TRUE)
        ind.MIXPHA.closest.mosaic.max.volume      <- which(MIXPHA.closest.mosaic.volume == MIXPHA.closest.mosaic.max.volume)
        meanZDR.MIXPHA.closest.mosaic.max.volume  <- meanZDR.MIXPHA.closest.mosaic.volume[ind.MIXPHA.closest.mosaic.max.volume]
        meanDBZ.MIXPHA.closest.mosaic.max.volume  <- meanDBZ.MIXPHA.closest.mosaic.volume[ind.MIXPHA.closest.mosaic.max.volume]
        TEMP.MIXPHA.closest.mosaic.max.volume     <- TEMP.MIXPHA.closest.mosaic.volume[ind.MIXPHA.closest.mosaic.max.volume]
        
        # print these values to GUI
        print(      '    Printing RadIA-mosaic matchup values to GUI:')
        print(      '      FZDZ:')
        print(paste('        meanDBZ.FZDZ.closest.mosaic.mean.volume= ',   round(meanDBZ.FZDZ.closest.mosaic.mean.volume,   digits=2), sep=""))
        print(paste('        sdevDBZ.FZDZ.closest.mosaic.mean.volume= ',   round(sdevDBZ.FZDZ.closest.mosaic.mean.volume,   digits=2), sep=""))
        print(paste('        TDBZ.FZDZ.closest.mosaic.mean.volume= ',      round(TDBZ.FZDZ.closest.mosaic.mean.volume,      digits=2), sep=""))
        print(paste('        FZDZ.closest.mosaic.mean.volume= ',           round(FZDZ.closest.mosaic.mean.volume,           digits=2), sep=""))
        print(paste('        meanDBZ.FZDZ.closest.mosaic.max.volume= ',    round(meanDBZ.FZDZ.closest.mosaic.max.volume,    digits=2), sep=""))
        print(paste('        sdevDBZ.FZDZ.closest.mosaic.max.volume= ',    round(sdevDBZ.FZDZ.closest.mosaic.max.volume,    digits=2), sep=""))
        print(paste('        TDBZ.FZDZ.closest.mosaic.max.volume= ',       round(TDBZ.FZDZ.closest.mosaic.max.volume,       digits=2), sep=""))
        print(paste('        FZDZ.closest.mosaic.max.volume= ',            round(FZDZ.closest.mosaic.max.volume,            digits=2), sep=""))
        print(      '      SLW:')
        print(paste('        meanZDR.SLW.closest.mosaic.mean.volume= ',    round(meanZDR.SLW.closest.mosaic.mean.volume,    digits=2), sep=""))
        print(paste('        sdevZDR.SLW.closest.mosaic.mean.volume= ',    round(sdevZDR.SLW.closest.mosaic.mean.volume,    digits=2), sep=""))
        print(paste('        meanKDP.SLW.closest.mosaic.mean.volume= ',    round(meanKDP.SLW.closest.mosaic.mean.volume,    digits=2), sep=""))
        print(paste('        sdevKDP.SLW.closest.mosaic.mean.volume= ',    round(sdevKDP.SLW.closest.mosaic.mean.volume,    digits=2), sep=""))
        print(paste('        SLW.closest.mosaic.mean.volume= ',            round(SLW.closest.mosaic.mean.volume,            digits=2), sep=""))
        print(paste('        meanZDR.SLW.closest.mosaic.max.volume= ',     round(meanZDR.SLW.closest.mosaic.max.volume,     digits=2), sep=""))
        print(paste('        sdevZDR.SLW.closest.mosaic.max.volume= ',     round(sdevZDR.SLW.closest.mosaic.max.volume,     digits=2), sep=""))
        print(paste('        meanKDP.SLW.closest.mosaic.max.volume= ',     round(meanKDP.SLW.closest.mosaic.max.volume,     digits=2), sep=""))
        print(paste('        sdevKDP.SLW.closest.mosaic.max.volume= ',     round(sdevKDP.SLW.closest.mosaic.max.volume,     digits=2), sep=""))
        print(paste('        SLW.closest.mosaic.max.volume= ',             round(SLW.closest.mosaic.max.volume,             digits=2), sep=""))
        print(      '      MIXPHA:')
        print(paste('        meanZDR.MIXPHA.closest.mosaic.mean.volume= ', round(meanZDR.MIXPHA.closest.mosaic.mean.volume, digits=2), sep=""))
        print(paste('        meanDBZ.MIXPHA.closest.mosaic.mean.volume= ', round(meanDBZ.MIXPHA.closest.mosaic.mean.volume, digits=2), sep=""))
        print(paste('        TEMP.MIXPHA.closest.mosaic.mean.volume= ',    round(TEMP.MIXPHA.closest.mosaic.mean.volume,    digits=2), sep=""))
        print(paste('        MIXPHA.closest.mosaic.mean.volume= ',         round(MIXPHA.closest.mosaic.mean.volume,         digits=2), sep=""))
        print(paste('        meanZDR.MIXPHA.closest.mosaic.max.volume= ',  round(meanZDR.MIXPHA.closest.mosaic.max.volume,  digits=2), sep=""))
        print(paste('        meanDBZ.MIXPHA.closest.mosaic.max.volume= ',  round(meanDBZ.MIXPHA.closest.mosaic.max.volume,  digits=2), sep=""))
        print(paste('        TEMP.MIXPHA.closest.mosaic.max.volume= ',     round(TEMP.MIXPHA.closest.mosaic.max.volume,     digits=2), sep=""))
        print(paste('        MIXPHA.closest.mosaic.max.volume= ',          round(MIXPHA.closest.mosaic.max.volume,          digits=2), sep=""))
        
      } else if (latest.mosaic.ver.num >= 2) {
        
        # find the closest RadIA-mosaic/HRRR pressure level to the CNV pressure at time j
        ind.nssl.mosaic.pres                   <- which.min(abs(abs(HRRR.pres.mb) - CNV.30s.mean.df$pres.hpa[j]))
        
        # find the closest hh*mm time for RadIA-mosaic files
        ind.mosaic.INTS.file.closest           <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.INTS.file   * 60)))
        ind.mosaic.FZDZ.file.closest           <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.FZDZ.file   * 60)))
        ind.mosaic.SLW.file.closest            <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.SLW.file    * 60)))
        ind.mosaic.MIXPHA.file.closest         <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.MIXPHA.file * 60)))
        ind.mosaic.PLATES.file.closest         <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.PLATES.file * 60)))
        ind.mosaic.MOMS.file.closest           <- which.min(abs(ss.tot.flight.track - (mm.tot.mosaic.MOMS.file   * 60)))
        
        # find seconds time diff of the closest RadIA-mosaic files
        ss.diff.INTS.file.closest              <- abs(ss.tot.flight.track - (mm.tot.mosaic.INTS.file   * 60))[ind.mosaic.INTS.file.closest]
        ss.diff.FZDZ.file.closest              <- abs(ss.tot.flight.track - (mm.tot.mosaic.FZDZ.file   * 60))[ind.mosaic.FZDZ.file.closest]
        ss.diff.SLW.file.closest               <- abs(ss.tot.flight.track - (mm.tot.mosaic.SLW.file    * 60))[ind.mosaic.SLW.file.closest]
        ss.diff.MIXPHA.file.closest            <- abs(ss.tot.flight.track - (mm.tot.mosaic.MIXPHA.file * 60))[ind.mosaic.MIXPHA.file.closest]
        ss.diff.PLATES.file.closest            <- abs(ss.tot.flight.track - (mm.tot.mosaic.PLATES.file * 60))[ind.mosaic.PLATES.file.closest]
        ss.diff.MOMS.file.closest              <- abs(ss.tot.flight.track - (mm.tot.mosaic.MOMS.file   * 60))[ind.mosaic.MOMS.file.closest]
        
        #browser()
        
        #-------------------------------------------------------------
        # load RadIA-mosaic version 2 files, if not already loaded
        #------------------------------------------------------------
        # mosaic file containing FZDZ, SLW, PLATES, and MIXPHA INTs
        if (ss.diff.INTS.file.closest > 1800) {
          print("    Time diff CNV-closest INTS file > 30min, too far away.  No file loaded")
          INTS.file.within.30mm.flag <- 0
        } else {
          INTS.file.within.30mm.flag <- 1
          if (ind.mosaic.INTS.file.closest != ind.mosaic.INTS.file.closest.last) {  
            radia.mosaic.INTS.nc.filename       <- paste(radia.mosaic.INTS.nc.dir, radia.mosaic.INTS.nc.listfiles[ind.mosaic.INTS.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.INTS.nc.filename, sep=""))
            if (substr(radia.mosaic.INTS.nc.filename, nchar(radia.mosaic.INTS.nc.filename)-1, nchar(radia.mosaic.INTS.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.INTS.nc.filename)
              radia.mosaic.INTS.nc.filename <- substr(radia.mosaic.INTS.nc.filename, 1, nchar(radia.mosaic.INTS.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.INTS.nc           <- nc_open(radia.mosaic.INTS.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("        The file has", radia.mosaic.INTS.nc$nvars, "variables"))
            radia.mosaic.INTS.var.num      <- seq(1, radia.mosaic.INTS.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.INTS.var.num)) {
              radia.mosaic.INTS.nam <- paste("v", radia.mosaic.INTS.var.num[s], sep = "")
              assign(radia.mosaic.INTS.nam, radia.mosaic.INTS.nc$var[[radia.mosaic.INTS.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.INTS.nc$nvars >= 14) {
              lat0.INT                              <- ncvar_get( radia.mosaic.INTS.nc, v5  )
              lon0.INT                              <- ncvar_get( radia.mosaic.INTS.nc, v6  )         
              TEMP.CELSIUS                          <- ncvar_get( radia.mosaic.INTS.nc, v9  )
              FZDZ.INT                              <- ncvar_get( radia.mosaic.INTS.nc, v10 )
              SLW.INT                               <- ncvar_get( radia.mosaic.INTS.nc, v11 )
              MIXPHA.INT                            <- ncvar_get( radia.mosaic.INTS.nc, v12 )
              PLATES.INT                            <- ncvar_get( radia.mosaic.INTS.nc, v13 )
              RADIA2.VAL                            <- ncvar_get( radia.mosaic.INTS.nc, v14 )
            } else if (radia.mosaic.nc$nvars == 4) {
            }
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.INTS.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest INTS mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.INTS.file.closest.last           <- ind.mosaic.INTS.file.closest
        }      # end of if (ss.diff.INTS.file.closest > 1800)
          
        # mosaic file containing raw moments (MOMs)
        if (ss.diff.MOMS.file.closest > 1800) {
          print("    Time diff CNV-closest MOMS file > 30min, too far away.  No file loaded")
          MOMS.file.within.30mm.flag <- 0
        } else {
          MOMS.file.within.30mm.flag <- 1
          if (ind.mosaic.MOMS.file.closest != ind.mosaic.MOMS.file.closest.last) {  
            radia.mosaic.MOMS.nc.filename       <- paste(radia.mosaic.MOMS.nc.dir, radia.mosaic.MOMS.nc.listfiles[ind.mosaic.MOMS.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.MOMS.nc.filename, sep=""))
            if (substr(radia.mosaic.MOMS.nc.filename, nchar(radia.mosaic.MOMS.nc.filename)-1, nchar(radia.mosaic.MOMS.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.MOMS.nc.filename)
              radia.mosaic.MOMS.nc.filename <- substr(radia.mosaic.MOMS.nc.filename, 1, nchar(radia.mosaic.MOMS.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.MOMS.nc           <- nc_open(radia.mosaic.MOMS.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("        The file has", radia.mosaic.MOMS.nc$nvars, "variables"))
            radia.mosaic.MOMS.var.num      <- seq(1, radia.mosaic.MOMS.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.MOMS.var.num)) {
              radia.mosaic.MOMS.nam <- paste("v", radia.mosaic.MOMS.var.num[s], sep = "")
              assign(radia.mosaic.MOMS.nam, radia.mosaic.MOMS.nc$var[[radia.mosaic.MOMS.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.MOMS.nc$nvars >= 12) {
              lat0.MOMS                             <- ncvar_get( radia.mosaic.MOMS.nc, v5  )
              lon0.MOMS                             <- ncvar_get( radia.mosaic.MOMS.nc, v6  )         
              RHO.MOMS                              <- ncvar_get( radia.mosaic.MOMS.nc, v9  )
              DBZ.MOMS                              <- ncvar_get( radia.mosaic.MOMS.nc, v10 )
              ZDR.MOMS                              <- ncvar_get( radia.mosaic.MOMS.nc, v11 )
              KDP.MOMS                              <- ncvar_get( radia.mosaic.MOMS.nc, v12 )
            } else if (radia.mosaic.MOMS.nc$nvars == 4) {
            }
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.MOMS.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest MOMS mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.MOMS.file.closest.last           <- ind.mosaic.MOMS.file.closest
        }      # end of if (ss.diff.MOMS.closest.file > 1800)
          
        # mosaic file containing FZDZ meta-fields
        if (ss.diff.FZDZ.file.closest > 1800) {
          print("    Time diff CNV-closest FZDZ file > 30min, too far away.  No file loaded")
          FZDZ.file.within.30mm.flag <- 0
        } else {
          FZDZ.file.within.30mm.flag <- 1
          if (ind.mosaic.FZDZ.file.closest != ind.mosaic.FZDZ.file.closest.last) {  
            radia.mosaic.FZDZ.nc.filename       <- paste(radia.mosaic.FZDZ.nc.dir, radia.mosaic.FZDZ.nc.listfiles[ind.mosaic.FZDZ.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.FZDZ.nc.filename, sep=""))
            if (substr(radia.mosaic.FZDZ.nc.filename, nchar(radia.mosaic.FZDZ.nc.filename)-1, nchar(radia.mosaic.FZDZ.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.FZDZ.nc.filename)
              radia.mosaic.FZDZ.nc.filename <- substr(radia.mosaic.FZDZ.nc.filename, 1, nchar(radia.mosaic.FZDZ.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.FZDZ.nc           <- nc_open(radia.mosaic.FZDZ.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("      The file has", radia.mosaic.FZDZ.nc$nvars, "variables"))
            radia.mosaic.FZDZ.var.num      <- seq(1, radia.mosaic.FZDZ.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.FZDZ.var.num)) {
              radia.mosaic.FZDZ.nam <- paste("v", radia.mosaic.FZDZ.var.num[s], sep = "")
              assign(radia.mosaic.FZDZ.nam, radia.mosaic.FZDZ.nc$var[[radia.mosaic.FZDZ.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.FZDZ.nc$nvars >= 21) {
              lat0.FZDZ                             <- ncvar_get( radia.mosaic.FZDZ.nc, v5  )
              lon0.FZDZ                             <- ncvar_get( radia.mosaic.FZDZ.nc, v6  )         
              mask.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v9  )
              mean.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v10 )
              T.DBZ.FZDZ                            <- ncvar_get( radia.mosaic.FZDZ.nc, v11 )
              sdev.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v12 )
              block.sdev.DBZ.FZDZ                   <- ncvar_get( radia.mosaic.FZDZ.nc, v13 )
              block.median.DBZ.FZDZ                 <- ncvar_get( radia.mosaic.FZDZ.nc, v14 )
              int.sdev.DBZ.FZDZ                     <- ncvar_get( radia.mosaic.FZDZ.nc, v15 )
              int.T.DBZ.FZDZ                        <- ncvar_get( radia.mosaic.FZDZ.nc, v16 )
              int.mean.DBZ.FZDZ                     <- ncvar_get( radia.mosaic.FZDZ.nc, v17 )
              int.block.sdev.DBZ.FZDZ               <- ncvar_get( radia.mosaic.FZDZ.nc, v18 )
              int.block.median.DBZ.FZDZ             <- ncvar_get( radia.mosaic.FZDZ.nc, v19 )
              int.full.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v20 )
              int.FZDZ                              <- ncvar_get( radia.mosaic.FZDZ.nc, v21 )
            } else if (radia.mosaic.FZDZ.nc$nvars == 20) {
              lat0.FZDZ                             <- ncvar_get( radia.mosaic.FZDZ.nc, v5  )
              lon0.FZDZ                             <- ncvar_get( radia.mosaic.FZDZ.nc, v6  )         
              mask.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v9  )
              mean.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v10 )
              T.DBZ.FZDZ                            <- ncvar_get( radia.mosaic.FZDZ.nc, v11 )
              sdev.DBZ.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v12 )
              block.sdev.DBZ.FZDZ                   <- ncvar_get( radia.mosaic.FZDZ.nc, v13 )
              block.median.DBZ.FZDZ                 <- ncvar_get( radia.mosaic.FZDZ.nc, v14 )
              int.sdev.DBZ.FZDZ                     <- ncvar_get( radia.mosaic.FZDZ.nc, v15 )
              int.T.DBZ.FZDZ                        <- ncvar_get( radia.mosaic.FZDZ.nc, v16 )
              int.mean.DBZ.FZDZ                     <- ncvar_get( radia.mosaic.FZDZ.nc, v17 )
              int.block.sdev.DBZ.FZDZ               <- ncvar_get( radia.mosaic.FZDZ.nc, v18 )
              int.block.median.DBZ.FZDZ             <- ncvar_get( radia.mosaic.FZDZ.nc, v19 )
              int.full.FZDZ                         <- ncvar_get( radia.mosaic.FZDZ.nc, v20 )
            } else if (radia.mosaic.FZDZ.nc$nvars == 4) {
            } #  end of if (radia.mosaic.FZDZ.nc$ncvars >= 20)
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.FZDZ.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest FZDZ mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.FZDZ.file.closest.last           <- ind.mosaic.FZDZ.file.closest
        }      # end of if (ss.diff.FZDZ.file.closest)
          
        # mosaic file containing SLW meta-fields
        if (ss.diff.SLW.file.closest > 1800) {
          print("    Time diff CNV-closest SLW file > 30min, too far away.  No file loaded")
          SLW.file.within.30mm.flag <- 0
        } else {
          SLW.file.within.30mm.flag <- 1
          if (ind.mosaic.SLW.file.closest != ind.mosaic.SLW.file.closest.last) {  
            radia.mosaic.SLW.nc.filename       <- paste(radia.mosaic.SLW.nc.dir, radia.mosaic.SLW.nc.listfiles[ind.mosaic.SLW.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.SLW.nc.filename, sep=""))
            if (substr(radia.mosaic.SLW.nc.filename, nchar(radia.mosaic.SLW.nc.filename)-1, nchar(radia.mosaic.SLW.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.SLW.nc.filename)
              radia.mosaic.SLW.nc.filename <- substr(radia.mosaic.SLW.nc.filename, 1, nchar(radia.mosaic.SLW.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.SLW.nc           <- nc_open(radia.mosaic.SLW.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("        The file has", radia.mosaic.SLW.nc$nvars, "variables"))
            radia.mosaic.SLW.var.num      <- seq(1, radia.mosaic.SLW.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.SLW.var.num)) {
              radia.mosaic.SLW.nam <- paste("v", radia.mosaic.SLW.var.num[s], sep = "")
              assign(radia.mosaic.SLW.nam, radia.mosaic.SLW.nc$var[[radia.mosaic.SLW.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.SLW.nc$nvars >= 21) {
              lat0.SLW                             <- ncvar_get( radia.mosaic.SLW.nc, v5  )
              lon0.SLW                             <- ncvar_get( radia.mosaic.SLW.nc, v6  )         
              mask.ZDR.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v9  )
              mean.ZDR.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v10 )
              mean.ZDR.corr.SLW                    <- ncvar_get( radia.mosaic.SLW.nc, v11 )
              sdev.ZDR.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v12 )
              mask.KDP.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v13 )
              sdev.KDP.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v14 )
              mean.KDP.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v15 )
              int.mean.ZDR.corr.SLW                <- ncvar_get( radia.mosaic.SLW.nc, v16 )
              int.sdev.ZDR.SLW                     <- ncvar_get( radia.mosaic.SLW.nc, v17 )
              int.mean.KDP.SLW                     <- ncvar_get( radia.mosaic.SLW.nc, v18 )
              int.sdev.KDP.SLW                     <- ncvar_get( radia.mosaic.SLW.nc, v19 )
              int.full.SLW                         <- ncvar_get( radia.mosaic.SLW.nc, v20 )
            } else if (radia.mosaic.SLW.nc$nvars == 4) {
            } # end of if (radia.mosaic.SLW.nc$ncvars >= 21)
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.SLW.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest SLW mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.SLW.file.closest.last           <- ind.mosaic.SLW.file.closest
        }      # end of if (ss.diff.SLW.file.closest > 1800)
          
        # mosaic file containing MIXPHA meta-fields
        if (ss.diff.MIXPHA.file.closest > 1800) {
          print("    Time diff CNV-closest MIXPHA file > 30min, too far away.  No file loaded")
          MIXPHA.file.within.30mm.flag <- 0
        } else {
          MIXPHA.file.within.30mm.flag <- 1
          if (ind.mosaic.MIXPHA.file.closest != ind.mosaic.MIXPHA.file.closest.last) {  
            radia.mosaic.MIXPHA.nc.filename       <- paste(radia.mosaic.MIXPHA.nc.dir, radia.mosaic.MIXPHA.nc.listfiles[ind.mosaic.MIXPHA.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.MIXPHA.nc.filename, sep=""))
            if (substr(radia.mosaic.MIXPHA.nc.filename, nchar(radia.mosaic.MIXPHA.nc.filename)-1, nchar(radia.mosaic.MIXPHA.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.MIXPHA.nc.filename)
              radia.mosaic.MIXPHA.nc.filename <- substr(radia.mosaic.MIXPHA.nc.filename, 1, nchar(radia.mosaic.MIXPHA.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.MIXPHA.nc           <- nc_open(radia.mosaic.MIXPHA.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("        The file has", radia.mosaic.MIXPHA.nc$nvars, "variables"))
            radia.mosaic.MIXPHA.var.num      <- seq(1, radia.mosaic.MIXPHA.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.MIXPHA.var.num)) {
              radia.mosaic.MIXPHA.nam <- paste("v", radia.mosaic.MIXPHA.var.num[s], sep = "")
              assign(radia.mosaic.MIXPHA.nam, radia.mosaic.MIXPHA.nc$var[[radia.mosaic.MIXPHA.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.MIXPHA.nc$nvars >= 13) {
              lat0.MIXPHA                             <- ncvar_get( radia.mosaic.MIXPHA.nc, v5  )
              lon0.MIXPHA                             <- ncvar_get( radia.mosaic.MIXPHA.nc, v6  )         
              int.mean.ZDR.MIXPHA                     <- ncvar_get( radia.mosaic.MIXPHA.nc, v9  )
              int.mean.DBZ.MIXPHA                     <- ncvar_get( radia.mosaic.MIXPHA.nc, v10 )
              int.temp.MIXPHA                         <- ncvar_get( radia.mosaic.MIXPHA.nc, v11 )
              int.full.MIXPHA                         <- ncvar_get( radia.mosaic.MIXPHA.nc, v12 )
            } else if (radia.mosaic.MIXPHA.nc$nvars == 4) {
            } # end of if (radia.mosaic.MIXPHA.nc$ncvars >= 21)
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.MIXPHA.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest MIXPHA mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.MIXPHA.file.closest.last           <- ind.mosaic.MIXPHA.file.closest
        }      # end of if (ss.diff.MIXPHA.file.closest > 1800)
          
        # mosaic file containing PLATES meta-fields
        if (ss.diff.PLATES.file.closest > 1800) {
          print("    Time diff CNV-closest PLATES file > 30min, too far away.  No file loaded")
          PLATES.file.within.30mm.flag <- 0
        } else {
          PLATES.file.within.30mm.flag <- 1
          if (ind.mosaic.PLATES.file.closest != ind.mosaic.PLATES.file.closest.last) {  
            radia.mosaic.PLATES.nc.filename       <- paste(radia.mosaic.PLATES.nc.dir, radia.mosaic.PLATES.nc.listfiles[ind.mosaic.PLATES.file.closest], sep="")
            print(paste("      Loading: ", radia.mosaic.PLATES.nc.filename, sep=""))
            if (substr(radia.mosaic.PLATES.nc.filename, nchar(radia.mosaic.PLATES.nc.filename)-1, nchar(radia.mosaic.PLATES.nc.filename)) == "gz") {
              print("      File is gzipped. Unzipping...")
              untar(radia.mosaic.PLATES.nc.filename)
              radia.mosaic.PLATES.nc.filename <- substr(radia.mosaic.PLATES.nc.filename, 1, nchar(radia.mosaic.PLATES.nc.filename)-3)
            }  # end of if ()...
            radia.mosaic.PLATES.nc           <- nc_open(radia.mosaic.PLATES.nc.filename, write = FALSE, verbose = FALSE)
            print(paste("        The file has", radia.mosaic.PLATES.nc$nvars, "variables"))
            radia.mosaic.PLATES.var.num      <- seq(1, radia.mosaic.PLATES.nc$nvars, by=1)
            for (s in 1:length(radia.mosaic.PLATES.var.num)) {
              radia.mosaic.PLATES.nam <- paste("v", radia.mosaic.PLATES.var.num[s], sep = "")
              assign(radia.mosaic.PLATES.nam, radia.mosaic.PLATES.nc$var[[radia.mosaic.PLATES.var.num[s]]])
            }  # end of for ()...
            if (radia.mosaic.PLATES.nc$nvars >= 12) {
              lat0.PLATES                             <- ncvar_get( radia.mosaic.PLATES.nc, v5  )
              lon0.PLATES                             <- ncvar_get( radia.mosaic.PLATES.nc, v6  )         
              int.mean.ZDR.PLATES                     <- ncvar_get( radia.mosaic.PLATES.nc, v9  )
              int.mean.DBZ.PLATES                     <- ncvar_get( radia.mosaic.PLATES.nc, v10 )
              int.temp.PLATES                         <- ncvar_get( radia.mosaic.PLATES.nc, v11 )
              int.full.PLATES                         <- ncvar_get( radia.mosaic.PLATES.nc, v12 )
            } else if (radia.mosaic.PLATES.nc$nvars == 4) {
            } # end of if (radia.mosaic.PLATES.nc$ncvars >= 21)
            #print(paste("V1 has name", v1$name))
            nc_close(radia.mosaic.PLATES.nc)
            #FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
            #head(FZDZ.mosaic.df)
          } else {
            print("      Closest PLATES mosaic time is same as the last, which is already loaded")
          }    # end of if (ind.mosaic.file.closest...)
          # define indices of the last closest file, to see if a new nc file needs to be loaded or not
          ind.mosaic.PLATES.file.closest.last           <- ind.mosaic.PLATES.file.closest
        }      # end of if (ss.diff.PLATES.closest.file > 1800)
        
        #----------------------------------------------------------------------------------
        # Manipulate the RadIA-mosaic version 2 moment stats, feature fields, and INTs data
        #----------------------------------------------------------------------------------
        
        #if (j == 232) {
        #  browser()
        #}
        
        # make vector from matrix
        lon0.INT.array                   <- as.vector(lon0.INT)
        lat0.INT.array                   <- as.vector(lat0.INT)
        # make lon/lat df
        lon0.lat0.INT.df                 <- as.data.frame(cbind(lon0.INT.array, lat0.INT.array))
        # process through nearest neighbor function
        nearest                          <- nn2(lon0.lat0.INT.df, t(replicate(dim(lon0.lat0.INT.df)[1], c(CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$lat.deg[j]))), searchtype="radius", radius=0.0245)
        max(nearest$nn.idx)
        # find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
        ind.closest.mosaic.lat.pixel[j]  <- max(nearest$nn.idx)%/%dim(lon0.INT)[1] + 1 # the next column (+1) after the whole number divisor
        ind.closest.mosaic.lon.pixel[j]  <- max(nearest$nn.idx)%%dim(lon0.INT)[1]
        ind.closest.mosaic.pres.pixel[j] <- which.min(abs(CNV.30s.mean.df$pres.hpa[j] - abs(HRRR.pres.mb))) 
        
        # define new i/j/k/fields for 
        grid.i[j]                        <- ind.closest.mosaic.lon.pixel[j]
        grid.j[j]                        <- ind.closest.mosaic.lat.pixel[j]
        grid.k[j]                        <- ind.closest.mosaic.pres.pixel[j]
        
        rm(nearest)
        # print some nn and actual CNV lat/lon as a crosscheck
        print("")
        
        # define the single pixel value that is closest to the aircraft at the closest time 
        #   from INTS
        TEMP.CELSIUS.pix              <- round(TEMP.CELSIUS[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        FZDZ.INT.pix                  <- round(FZDZ.INT[                ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        SLW.INT.pix                   <- round(SLW.INT[                 ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        MIXPHA.INT.pix                <- round(MIXPHA.INT[              ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        PLATES.INT.pix                <- round(PLATES.INT[              ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        RADIA2.VAL.pix                <- round(RADIA2.VAL[              ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2) 
        #   from MOMS
        RHO.MOMS.pix                  <- round(RHO.MOMS[                ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        DBZ.MOMS.pix                  <- round(DBZ.MOMS[                ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        ZDR.MOMS.pix                  <- round(ZDR.MOMS[                ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        KDP.MOMS.pix                  <- round(KDP.MOMS[                ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        #   for FZDZ
        mask.DBZ.FZDZ.pix             <- round(mask.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        mean.DBZ.FZDZ.pix             <- round(mean.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        T.DBZ.FZDZ.pix                <- round(T.DBZ.FZDZ[              ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        sdev.DBZ.FZDZ.pix             <- round(sdev.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        block.sdev.DBZ.FZDZ.pix       <- round(block.sdev.DBZ.FZDZ[     ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        block.median.DBZ.FZDZ.pix     <- round(block.median.DBZ.FZDZ[   ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.sdev.DBZ.FZDZ.pix         <- round(int.sdev.DBZ.FZDZ[       ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.T.DBZ.FZDZ.pix            <- round(int.T.DBZ.FZDZ[          ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.mean.DBZ.FZDZ.pix         <- round(int.mean.DBZ.FZDZ[       ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.block.sdev.DBZ.FZDZ.pix   <- round(int.block.sdev.DBZ.FZDZ[ ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.block.median.DBZ.FZDZ.pix <- round(int.block.median.DBZ.FZDZ[ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.full.FZDZ.pix             <- round(int.full.FZDZ[           ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        #   for SLW
        mask.ZDR.SLW.pix              <- round(mask.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        mean.ZDR.SLW.pix              <- round(mean.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        mean.ZDR.corr.SLW.pix         <- round(mean.ZDR.corr.SLW[       ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        sdev.ZDR.SLW.pix              <- round(sdev.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        mask.KDP.SLW.pix              <- round(mask.KDP.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        sdev.KDP.SLW.pix              <- round(sdev.KDP.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        mean.KDP.SLW.pix              <- round(mean.KDP.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.mean.ZDR.corr.SLW.pix     <- round(int.mean.ZDR.corr.SLW[   ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.sdev.ZDR.SLW.pix          <- round(int.sdev.ZDR.SLW[        ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.mean.KDP.SLW.pix          <- round(int.mean.KDP.SLW[        ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.sdev.KDP.SLW.pix          <- round(int.sdev.KDP.SLW[        ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.full.SLW.pix              <- round(int.full.SLW[            ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        #   for MIXPHA
        int.mean.ZDR.MIXPHA.pix       <- round(int.mean.ZDR.MIXPHA[     ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.mean.DBZ.MIXPHA.pix       <- round(int.mean.DBZ.MIXPHA[     ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.temp.MIXPHA.pix           <- round(int.temp.MIXPHA[         ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.full.MIXPHA.pix           <- round(int.full.MIXPHA[         ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        #   for PLATES
        int.mean.ZDR.PLATES.pix       <- round(int.mean.ZDR.PLATES[     ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.mean.DBZ.PLATES.pix       <- round(int.mean.DBZ.PLATES[     ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.temp.PLATES.pix           <- round(int.temp.PLATES[         ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
        int.full.PLATES.pix           <- round(int.full.PLATES[         ind.closest.mosaic.lon.pixel[j], ind.closest.mosaic.lat.pixel[j], ind.closest.mosaic.pres.pixel[j]], digits=2)
          
        # define the volume that is closest to the aircraft at the closest time 
        #   from INTS
        TEMP.CELSIUS.vol                 <- TEMP.CELSIUS[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        FZDZ.INT.vol                     <- FZDZ.INT[                ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        SLW.INT.vol                      <- SLW.INT[                 ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        MIXPHA.INT.vol                   <- MIXPHA.INT[              ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        PLATES.INT.vol                   <- PLATES.INT[              ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        RADIA2.VAL.vol                   <- RADIA2.VAL[              ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres] 
        #   from MOMS
        RHO.MOMS.vol                     <- RHO.MOMS[                ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        DBZ.MOMS.vol                     <- DBZ.MOMS[                ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        ZDR.MOMS.vol                     <- ZDR.MOMS[                ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        KDP.MOMS.vol                     <- KDP.MOMS[                ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        #   for FZDZ
        mask.DBZ.FZDZ.vol                <- mask.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        mean.DBZ.FZDZ.vol                <- mean.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        T.DBZ.FZDZ.vol                   <- T.DBZ.FZDZ[              ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        sdev.DBZ.FZDZ.vol                <- sdev.DBZ.FZDZ[           ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        block.sdev.DBZ.FZDZ.vol          <- block.sdev.DBZ.FZDZ[     ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        block.median.DBZ.FZDZ.vol        <- block.median.DBZ.FZDZ[   ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.sdev.DBZ.FZDZ.vol            <- int.sdev.DBZ.FZDZ[       ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.T.DBZ.FZDZ.vol               <- int.T.DBZ.FZDZ[          ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.mean.DBZ.FZDZ.vol            <- int.mean.DBZ.FZDZ[       ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.block.sdev.DBZ.FZDZ.vol      <- int.block.sdev.DBZ.FZDZ[ ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.block.median.DBZ.FZDZ.vol    <- int.block.median.DBZ.FZDZ[ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.full.FZDZ.vol                <- int.full.FZDZ[           ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        #   for SLW
        mask.ZDR.SLW.vol                 <- mask.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        mean.ZDR.SLW.vol                 <- mean.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        mean.ZDR.corr.SLW.vol            <- mean.ZDR.corr.SLW[       ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        sdev.ZDR.SLW.vol                 <- sdev.ZDR.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        mask.KDP.SLW.vol                 <- mask.KDP.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        sdev.KDP.SLW.vol                 <- sdev.KDP.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        mean.KDP.SLW.vol                 <- mean.KDP.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.mean.ZDR.corr.SLW.vol        <- int.mean.ZDR.corr.SLW[   ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.sdev.ZDR.SLW.vol             <- int.sdev.ZDR.SLW[        ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.mean.KDP.SLW.vol             <- int.mean.KDP.SLW[        ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.sdev.KDP.SLW.vol             <- int.sdev.KDP.SLW[        ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.full.SLW.vol                 <- int.full.SLW[            ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        #   for MIXPHA
        int.mean.ZDR.MIXPHA.vol          <- int.mean.ZDR.MIXPHA[     ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.mean.DBZ.MIXPHA.vol          <- int.mean.DBZ.MIXPHA[     ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.temp.MIXPHA.vol              <- int.temp.MIXPHA[         ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.full.MIXPHA.vol              <- int.full.MIXPHA[         ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        #   for PLATES
        int.mean.ZDR.PLATES.vol          <- int.mean.ZDR.PLATES[     ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.mean.DBZ.PLATES.vol          <- int.mean.DBZ.PLATES[     ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.temp.PLATES.vol              <- int.temp.PLATES[         ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        int.full.PLATES.vol              <- int.full.PLATES[         ind.closest.mosaic.lon.pixel[j]+ind.pixels.around.closest.lon, ind.closest.mosaic.lat.pixel[j]+ind.pixels.around.closest.lat, ind.closest.mosaic.pres.pixel[j]+ind.pixels.around.closest.pres]
        
        # FZDZ calculate some statistics [ 16 fields to pass to output ]
        #   FZDZ max in vol
        FZDZ.INT.max.vol                  <- max(FZDZ.INT.vol, na.rm=TRUE)
        #   ind of FZDZ max in vol
        ind.FZDZ.INT.max.vol              <- round(median(which(FZDZ.INT.vol == FZDZ.INT.max.vol)),     digits=0)
        #   FZDZ-related fields at ind of FZDZ max in vol
        ZDR.MOMS.FZDZ.max.vol             <- round(ZDR.MOMS.vol[ind.FZDZ.INT.max.vol],                  digits=2)
        RHO.MOMS.FZDZ.max.vol             <- round(RHO.MOMS.vol[ind.FZDZ.INT.max.vol],                  digits=2)
        KDP.MOMS.FZDZ.max.vol             <- round(KDP.MOMS.vol[ind.FZDZ.INT.max.vol],                  digits=3)
        mask.DBZ.FZDZ.max.vol             <- round(mask.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],             digits=2)
        mean.DBZ.FZDZ.max.vol             <- round(mean.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],             digits=2)
        T.DBZ.FZDZ.max.vol                <- round(T.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],                digits=2)
        sdev.DBZ.FZDZ.max.vol             <- round(sdev.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],             digits=2)
        block.sdev.DBZ.FZDZ.max.vol       <- round(block.sdev.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],       digits=2)
        block.median.DBZ.FZDZ.max.vol     <- round(block.median.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],     digits=2)
        int.sdev.DBZ.FZDZ.max.vol         <- round(int.sdev.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],         digits=2)
        int.T.DBZ.FZDZ.max.vol            <- round(int.T.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],            digits=2)
        int.mean.DBZ.FZDZ.max.vol         <- round(int.mean.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],         digits=2)
        int.block.sdev.DBZ.FZDZ.max.vol   <- round(int.block.sdev.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol],   digits=2)
        int.block.median.DBZ.FZDZ.max.vol <- round(int.block.median.DBZ.FZDZ.vol[ind.FZDZ.INT.max.vol], digits=2)
        int.full.FZDZ.max.vol             <- round(int.full.FZDZ.vol[ind.FZDZ.INT.max.vol],             digits=2)
        
        # SLW calculate some statistics [ 15 fields to pass to output ]
        #   SLW max in vol
        SLW.INT.max.vol                   <- max(SLW.INT.vol, na.rm=TRUE)
        #   ind of SLW max in vol
        ind.SLW.INT.max.vol               <- round(median(which(SLW.INT.vol == SLW.INT.max.vol)),  digits=0)
        #   SLW-related fields at ind of SLW max in vol
        DBZ.MOMS.SLW.max.vol              <- round(DBZ.MOMS.vol[             ind.SLW.INT.max.vol], digits=2)
        RHO.MOMS.SLW.max.vol              <- round(RHO.MOMS.vol[             ind.SLW.INT.max.vol], digits=2)
        mask.ZDR.SLW.max.vol              <- round(mask.ZDR.SLW.vol[         ind.SLW.INT.max.vol], digits=2)
        mean.ZDR.SLW.max.vol              <- round(mean.ZDR.SLW.vol[         ind.SLW.INT.max.vol], digits=2)
        mean.ZDR.corr.SLW.max.vol         <- round(mean.ZDR.corr.SLW.vol[    ind.SLW.INT.max.vol], digits=2)
        sdev.ZDR.SLW.max.vol              <- round(sdev.ZDR.SLW.vol[         ind.SLW.INT.max.vol], digits=2)
        mask.KDP.SLW.max.vol              <- round(mask.KDP.SLW.vol[         ind.SLW.INT.max.vol], digits=3)
        sdev.KDP.SLW.max.vol              <- round(sdev.KDP.SLW.vol[         ind.SLW.INT.max.vol], digits=3)
        mean.KDP.SLW.max.vol              <- round(mean.KDP.SLW.vol[         ind.SLW.INT.max.vol], digits=3)
        int.mean.ZDR.corr.SLW.max.vol     <- round(int.mean.ZDR.corr.SLW.vol[ind.SLW.INT.max.vol], digits=2)
        int.sdev.ZDR.SLW.max.vol          <- round(int.sdev.ZDR.SLW.vol[     ind.SLW.INT.max.vol], digits=2)
        int.mean.KDP.SLW.max.vol          <- round(int.mean.KDP.SLW.vol[     ind.SLW.INT.max.vol], digits=2)
        int.sdev.KDP.SLW.max.vol          <- round(int.sdev.KDP.SLW.vol[     ind.SLW.INT.max.vol], digits=2)
        int.full.SLW.max.vol              <- round(int.full.SLW.vol[         ind.SLW.INT.max.vol], digits=2)
        
        # MIXPHA calculate some statistics [ 9 fields to pass to output ]
        #   MIXPHA max in vol
        MIXPHA.INT.max.vol                <- max(MIXPHA.INT.vol, na.rm=TRUE)
        #   ind of MIXPHA max in vol
        ind.MIXPHA.INT.max.vol            <- round(median(which(MIXPHA.INT.vol == MIXPHA.INT.max.vol)),  digits=0)
        #   MIXPHA-related fields at ind of MIXPHA max in vol
        KDP.MOMS.MIXPHA.max.vol           <- round(KDP.MOMS.vol[                 ind.MIXPHA.INT.max.vol],     digits=3)
        RHO.MOMS.MIXPHA.max.vol           <- round(RHO.MOMS.vol[                 ind.MIXPHA.INT.max.vol],     digits=2)
        mean.ZDR.corr.MIXPHA.max.vol      <- round(mean.ZDR.corr.SLW.vol[        ind.MIXPHA.INT.max.vol],     digits=2)
        mean.DBZ.MIXPHA.max.vol           <- round(mean.DBZ.FZDZ.vol[            ind.MIXPHA.INT.max.vol],     digits=2)
        int.mean.ZDR.MIXPHA.max.vol       <- round(int.mean.ZDR.MIXPHA.vol[      ind.MIXPHA.INT.max.vol],     digits=2)
        int.mean.DBZ.MIXPHA.max.vol       <- round(int.mean.DBZ.MIXPHA.vol[      ind.MIXPHA.INT.max.vol],     digits=2)
        int.temp.MIXPHA.max.vol           <- round(int.temp.MIXPHA.vol[          ind.MIXPHA.INT.max.vol],     digits=2)
        int.full.MIXPHA.max.vol           <- round(int.full.MIXPHA.vol[          ind.MIXPHA.INT.max.vol],     digits=2)
        
        # PLATES calculate some statistics [ 9 fields to pass to output ]
        #   PLATES max in vol
        PLATES.INT.max.vol                <- max(PLATES.INT.vol, na.rm=TRUE)
        #   ind of PLATES max in vol
        ind.PLATES.INT.max.vol            <- round(median(which(PLATES.INT.vol == PLATES.INT.max.vol)), digits=0)
        #   PLATES-related fields at ind of PLATES max in vol
        KDP.MOMS.PLATES.max.vol           <- round(KDP.MOMS.vol[            ind.PLATES.INT.max.vol],    digits=3)
        RHO.MOMS.PLATES.max.vol           <- round(RHO.MOMS.vol[            ind.PLATES.INT.max.vol],    digits=2)
        mean.ZDR.corr.PLATES.max.vol      <- round(mean.ZDR.corr.SLW.vol[   ind.PLATES.INT.max.vol],    digits=2)
        mean.DBZ.PLATES.max.vol           <- round(mean.DBZ.FZDZ.vol[       ind.PLATES.INT.max.vol],    digits=2)
        int.mean.ZDR.PLATES.max.vol       <- round(int.mean.ZDR.PLATES.vol[ ind.PLATES.INT.max.vol],    digits=2)
        int.mean.DBZ.PLATES.max.vol       <- round(int.mean.DBZ.PLATES.vol[ ind.PLATES.INT.max.vol],    digits=2)
        int.temp.PLATES.max.vol           <- round(int.temp.PLATES.vol[     ind.PLATES.INT.max.vol],    digits=2)
        int.full.PLATES.max.vol           <- round(int.full.PLATES.vol[     ind.PLATES.INT.max.vol],    digits=2)
        
        # print these values to GUI
        print(      '    Printing RadIA-mosaic matchup values to GUI:')
        print(      '      FZDZ:')
        print(paste('        FZDZ.INT.max.vol      = ',       round(FZDZ.INT.max.vol, digits=2),       sep=""))
        print(paste('        ZDR.MOMS.FZDZ.max.vol = ',             ZDR.MOMS.FZDZ.max.vol,             sep=""))
        print(paste('        RHO.MOMS.FZDZ.max.vol = ',             RHO.MOMS.FZDZ.max.vol,             sep=""))
        print(paste('        KDP.MOMS.FZDZ.max.vol = ',             KDP.MOMS.FZDZ.max.vol,             sep=""))
        print(paste('        mask.DBZ.FZDZ.max.vol = ',             mask.DBZ.FZDZ.max.vol,             sep=""))
        print(paste('        mean.DBZ.FZDZ.max.vol = ',             mean.DBZ.FZDZ.max.vol,             sep=""))
        print(paste('        T.DBZ.FZDZ.max.vol    = ',             T.DBZ.FZDZ.max.vol,                sep=""))
        print(paste('        sdev.DBZ.FZDZ.max.vol = ',             sdev.DBZ.FZDZ.max.vol,             sep=""))
        print(paste('        block.sdev.DBZ.FZDZ.max.vol = ',       block.sdev.DBZ.FZDZ.max.vol,       sep=""))
        print(paste('        block.median.DBZ.FZDZ.max.vol = ',     block.median.DBZ.FZDZ.max.vol,     sep=""))
        print(paste('        int.sdev.DBZ.FZDZ.max.vol = ',         int.sdev.DBZ.FZDZ.max.vol,         sep=""))
        print(paste('        int.T.DBZ.FZDZ.max.vol = ',            int.T.DBZ.FZDZ.max.vol,            sep=""))
        print(paste('        int.mean.DBZ.FZDZ.max.vol = ',         int.mean.DBZ.FZDZ.max.vol,         sep=""))
        print(paste('        int.block.sdev.DBZ.FZDZ.max.vol = ',   int.block.sdev.DBZ.FZDZ.max.vol,   sep=""))
        print(paste('        int.block.median.DBZ.FZDZ.max.vol = ', int.block.median.DBZ.FZDZ.max.vol, sep=""))
        print(paste('        int.full.FZDZ.max.vol = ',             int.full.FZDZ.max.vol,             sep=""))
        print(      '      SLW:')
        print(paste('        SLW.INT.max.vol = ',             round(SLW.INT.max.vol, digits=2),        sep=""))
        print(paste('        DBZ.MOMS.SLW.max.vol = ',              DBZ.MOMS.SLW.max.vol,              sep=""))
        print(paste('        RHO.MOMS.SLW.max.vol = ',              RHO.MOMS.SLW.max.vol,              sep=""))
        print(paste('        mask.ZDR.SLW.max.vol = ',              mask.ZDR.SLW.max.vol,              sep=""))
        print(paste('        mean.ZDR.SLW.max.vol = ',              mean.ZDR.SLW.max.vol,              sep=""))
        print(paste('        mean.ZDR.corr.SLW.max.vol = ',         mean.ZDR.corr.SLW.max.vol,         sep=""))
        print(paste('        sdev.ZDR.SLW.max.vol = ',              sdev.ZDR.SLW.max.vol,              sep=""))
        print(paste('        mask.KDP.SLW.max.vol = ',              mask.KDP.SLW.max.vol,              sep=""))
        print(paste('        sdev.KDP.SLW.max.vol = ',              sdev.KDP.SLW.max.vol,              sep=""))
        print(paste('        mean.KDP.SLW.max.vol = ',              mean.KDP.SLW.max.vol,              sep=""))
        print(paste('        int.mean.ZDR.corr.SLW.max.vol = ',     int.mean.ZDR.corr.SLW.max.vol,     sep=""))
        print(paste('        int.sdev.ZDR.SLW.max.vol = ',          int.sdev.ZDR.SLW.max.vol,          sep=""))
        print(paste('        int.mean.KDP.SLW.max.vol = ',          int.mean.KDP.SLW.max.vol,          sep=""))
        print(paste('        int.sdev.KDP.SLW.max.vol = ',          int.sdev.KDP.SLW.max.vol,          sep=""))
        print(paste('        int.full.SLW.max.vol = ',              int.full.SLW.max.vol,              sep=""))
        print(      '      MIXPHA:')
        print(paste('        MIXPHA.INT.max.vol = ',          round(MIXPHA.INT.max.vol, digits=2),     sep=""))
        print(paste('        KDP.MOMS.MIXPHA.max.vol = ',           KDP.MOMS.MIXPHA.max.vol,           sep=""))
        print(paste('        RHO.MOMS.MIXPHA.max.vol = ',           RHO.MOMS.MIXPHA.max.vol,           sep=""))
        print(paste('        mean.ZDR.corr.MIXPHA.max.vol = ',      mean.ZDR.corr.MIXPHA.max.vol,      sep=""))
        print(paste('        mean.DBZ.MIXPHA.max.vol = ',           mean.DBZ.MIXPHA.max.vol,           sep=""))
        print(paste('        int.mean.ZDR.MIXPHA.max.vol = ',       int.mean.ZDR.MIXPHA.max.vol,       sep=""))
        print(paste('        int.mean.DBZ.MIXPHA.max.vol = ',       int.mean.DBZ.MIXPHA.max.vol,       sep=""))
        print(paste('        int.temp.MIXPHA.max.vol = ',           int.temp.MIXPHA.max.vol,           sep=""))
        print(paste('        int.full.MIXPHA.max.vol = ',           int.full.MIXPHA.max.vol,           sep=""))
        print(      '      PLATES:')
        print(paste('        PLATES.INT.max.vol = ',          round(PLATES.INT.max.vol, digits=2),     sep=""))
        print(paste('        KDP.MOMS.PLATES.max.vol = ',           KDP.MOMS.PLATES.max.vol,           sep=""))
        print(paste('        RHO.MOMS.PLATES.max.vol = ',           RHO.MOMS.PLATES.max.vol,           sep=""))
        print(paste('        mean.ZDR.corr.PLATES.max.vol = ',      mean.ZDR.corr.PLATES.max.vol,      sep=""))
        print(paste('        mean.DBZ.PLATES.max.vol = ',           mean.DBZ.PLATES.max.vol,           sep=""))
        print(paste('        int.mean.ZDR.PLATES.max.vol = ',       int.mean.ZDR.PLATES.max.vol,       sep=""))
        print(paste('        int.mean.DBZ.PLATES.max.vol = ',       int.mean.DBZ.PLATES.max.vol,       sep=""))
        print(paste('        int.full.PLATES.max.vol = ',           int.full.PLATES.max.vol,           sep=""))
        
      } else {
        print("Error")
      }        # end of if (latest.mosaic.ver.num == 1)
    
      ##browser()# find range/azimuth indices of closest SRPC lat/lon to each AC lat/lon
      ## CALCULATE BEARING, HORIZ DISTANCE, ALPHA, AND RANGE FROM RADAR TO AC
    
      ## calculate bearing (degrees) between two lat/lon points
      ##β = atan2(X,Y) is bearing from X to Y
      ##X = cos θb * sin ∆L
      ##Y = cos θa * sin θb – sin θa * cos θb * cos ∆L
      #X.rad[j]                          <- cos(NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg)-NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])))
      #Y.rad[j]                          <- cos(NISTdegTOradian(NEXRAD.pri.lat.deg)) * sin(NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) - sin(NISTdegTOradian(NEXRAD.pri.lat.deg)) * cos(NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) * cos(abs(NISTdegTOradian(NEXRAD.pri.lon.deg)-NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])))
      #bearing.radartoac.deg[j]          <- 360 - NISTradianTOdeg(atan2(X.rad[j], Y.rad[j]))
    
      ## calculate bearing index of ac to radar
      #bearing.radartoac.ind[j] <- round(bearing.radartoac.deg[j] / radar.azimuth.resolution.0.5.deg)
      #if (bearing.radartoac.ind[j] == 0) {
      #  bearing.radartoac.ind[j] <- 720
      #}  # end of if (bearing.radartoac.ind[m] )
    
      #calculate great circle distance along earth's surface between two lat/lon pts
      #a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
      #c = 2 ⋅ atan2( √a, √(1−a) )
      #d = R ⋅ c
      a[j]                        <- sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg) - NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg)-NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) / 2) + (cos(NISTdegTOradian(NEXRAD.pri.lat.deg)) * cos(NISTdegTOradian(CNV.30s.mean.df$lat.deg[j])) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg) - NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg)-NISTdegTOradian(CNV.30s.mean.df$lon.deg[j])) / 2) )
      c[j]                        <- 2 * atan2(sqrt(a[j]), sqrt(1 - a[j]))
      dist.radartoac.km[j]        <- radius.earth.km * c[j]
    
      #------------------------------------
      # output arrays as csv files
      #------------------------------------
      if (output.matchup.data.flag == 1) {
        print(paste("      Writing row of output data to file at: ", output.matchup.data.dir, case.time.list.df$yyyymmdd[ppp], "_F", case.time.list.df$flight.num[ppp], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "_RadIA-m_v", latest.mosaic.ver.num,  "_CNV_v", latest.30s.mean.ver.num,  ".csv",   sep=""))
        
        if (latest.mosaic.ver.num == 1) {
          #write.table(cbind(        " yyyymmdd"," flight.num",           " hh",             " mm",             " ss",                      " seg.num",                      " seg.type",                       " lat.deg",                      " lon.deg",                      " alt.m",                      " hdg.deg",                      " roll.deg",                      " pitch.deg",                      " pres.hpa",                      " temp.c",                      " warm.flag",                      " rh.aimms.per",                      " dmax.85.per.L.um",                      " MVD.um",                      " NEV.LWC.gm3",                      " NEV.TWC.gm3",                      " RID.LWC.gm3",                      " CDP.LWC.gm3",                      " OAP.LWC.gm3", "CNV.NAW..zennadREFL.30s.mean.zen", "CNV.NAW.zennad.REFL.30s.mean.nad", "CNV.NAW.zennad.REFL.30s.std.zen", "CNV.NAW.zennad.REFL.30s.std.nad", "CNV.NAW.zennad.VEL.30s.mean.zen", "CNV.NAW..zennadVEL.30s.mean.nad", "CNV.NAW.zennad.VEL.30s.std.zen", "CNV.NAW.zennad.VEL.30s.std.nad",                  " dist.radartoac.km",                              " FZDZ.int.max.vol",                                 " FZDZ.meanDBZ.max.vol",                                 " FZDZ.sdevDBZ.max.vol",                                 " FZDZ.TDBZ.max.vol",                             " SLW.int.max.vol",                                 " SLW.meanZDR.max.vol",                                 " SLW.sdevZDR.max.vol",                                 " SLW.meanKDP.max.vol",                                 " SLW.sdevKDP.max.vol",                             " MIXPHA.int.max.vol",                                 " MIXPHA.meanZDR.max.vol",                                 " MIXPHA.meanDBZ.max.vol",                                   " MIXPHA.TEMP.max.vol"
          write.table(cbind(as.numeric(yyyymmdd), flight.num, as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(FZDZ.closest.mosaic.max.volume, digits=2), round(meanDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(sdevDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(TDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(SLW.closest.mosaic.max.volume, digits=2), round(meanZDR.SLW.closest.mosaic.max.volume, digits=2), round(sdevZDR.SLW.closest.mosaic.max.volume, digits=2), round(meanKDP.SLW.closest.mosaic.max.volume, digits=2), round(sdevKDP.SLW.closest.mosaic.max.volume, digits=2), round(MIXPHA.closest.mosaic.max.volume, digits=2), round(meanZDR.MIXPHA.closest.mosaic.max.volume, digits=2), round(meanDBZ.MIXPHA.closest.mosaic.max.volume, digits=2), round(TEMP.MIXPHA.closest.mosaic.max.volume, digits=2)), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver2/", yyyymmdd, "_F", flight.num, "_", hh.min.for.plotting, mm.min.for.plotting, "to", hh.max.for.plotting, mm.max.for.plotting, "Z_RadIA-m_v",  latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          print(      cbind(as.numeric(yyyymmdd), flight.num, as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(FZDZ.closest.mosaic.max.volume, digits=2), round(meanDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(sdevDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(TDBZ.FZDZ.closest.mosaic.max.volume, digits=2), round(SLW.closest.mosaic.max.volume, digits=2), round(meanZDR.SLW.closest.mosaic.max.volume, digits=2), round(sdevZDR.SLW.closest.mosaic.max.volume, digits=2), round(meanKDP.SLW.closest.mosaic.max.volume, digits=2), round(sdevKDP.SLW.closest.mosaic.max.volume, digits=2), round(MIXPHA.closest.mosaic.max.volume, digits=2), round(meanZDR.MIXPHA.closest.mosaic.max.volume, digits=2), round(meanDBZ.MIXPHA.closest.mosaic.max.volume, digits=2), round(TEMP.MIXPHA.closest.mosaic.max.volume, digits=2)))
        } else if (latest.mosaic.ver.num == 2.1) {
          ## NEXT FEW LINES OUTPUT int.full.{ALGO}.max.vol
          #write.table(cbind(                               " yyyymmdd",                     " flight.num",             " hh",             " mm",             " ss",                      " seg.num",                      " seg.type",                       " lat.deg",                      " lon.deg",                      " alt.m",                      " hdg.deg",                      " roll.deg",                      " pitch.deg",                      " pres.hpa",                      " temp.c",                      " warm.flag",                      " rh.aimms.per",                      " dmax.85.per.L.um",                      " MVD.um",                      " NEV.LWC.gm3",                      " NEV.TWC.gm3",                      " RID.LWC.gm3",                      " CDP.LWC.gm3",                      " OAP.LWC.gm3", "CNV.NAW..zennadREFL.30s.mean.zen", "CNV.NAW.zennad.REFL.30s.mean.nad", "CNV.NAW.zennad.REFL.30s.std.zen", "CNV.NAW.zennad.REFL.30s.std.nad", "CNV.NAW.zennad.VEL.30s.mean.zen", "CNV.NAW..zennadVEL.30s.mean.nad", "CNV.NAW.zennad.VEL.30s.std.zen", "CNV.NAW.zennad.VEL.30s.std.nad",                  " dist.radartoac.km",                " int.full.FZDZ.max.vol",               " int.sdev.DBZ.FZDZ.max.vol",               " int.T.DBZ.FZDZ.max.vol",               " int.mean.DBZ.FZDZ.max.vol",               " int.block.sdev.DBZ.FZDZ.max.vol",               " int.block.median.DBZ.FZDZ.max.vol",               " sdev.DBZ.FZDZ.max.vol",               " T.DBZ.FZDZ.max.vol",               " mean.DBZ.FZDZ.max.vol",               " block.sdev.DBZ.FZDZ.max.vol",               " block.median.DBZ.FZDZ.max.vol",               " mask.DBZ.FZDZ.max.vol",               " ZDR.MOMS.FZDZ.max.vol",               " RHO.MOMS.FZDZ.max.vol",               " KDP.MOMS.FZDZ.max.vol",               " int.full.SLW.max.vol",               " int.mean.ZDR.corr.SLW.max.vol",               " int.sdev.ZDR.SLW.max.vol",               " int.mean.KDP.SLW.max.vol",               " int.sdev.KDP.SLW.max.vol",               " mean.ZDR.corr.SLW.max.vol",               " sdev.ZDR.SLW.max.vol",               " mean.KDP.SLW.max.vol",               " sdev.KDP.SLW.max.vol",               " DBZ.MOMS.SLW.max.vol",               " RHO.MOMS.SLW.max.vol",               " int.full.MIXPHA.max.vol",          " int.mean.ZDR.corr.MIXPHA.max.vol",               " int.mean.DBZ.MIXPHA.max.vol",               " int.temp.MIXPHA.max.vol",               " mean.ZDR.corr.MIXPHA.max.vol",               " mean.DBZ.MIXPHA.max.vol",               " RHO.MOMS.MIXPHA.max.vol",               " KDP.MOMS.MIXPHA.max.vol",               " int.full.PLATES.max.vol",               " int.mean.ZDR.PLATES.max.vol",                " int.mean.DBZ.PLATES.max.vol",              " int.temp.PLATES.max.vol",               " mean.ZDR.corr.PLATES.max.vol",               " mean.DBZ.PLATES.max.vol",               " RHO.MOMS.PLATES.max.vol",                " KDP.MOMS.PLATES.max.vol"                 
          write.table(cbind(as.numeric(case.time.list.df$yyyymmdd[ppp]), case.time.list.df$flight.num[ppp], as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(int.full.SLW.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(int.full.MIXPHA.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(int.full.PLATES.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, na = "NaN", file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/",  case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v",  latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          print(      cbind(as.numeric(case.time.list.df$yyyymmdd[ppp]), case.time.list.df$flight.num[ppp], as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(int.full.SLW.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(int.full.MIXPHA.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(int.full.PLATES.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)))
          ## NEXT FEW LINES OUTPUT {ALGO}.INT.max.vol
          ##write.table(cbind(                               " yyyymmdd",                     " flight.num",             " hh",             " mm",             " ss",                      " seg.num",                      " seg.type",                       " lat.deg",                      " lon.deg",                      " alt.m",                      " hdg.deg",                      " roll.deg",                      " pitch.deg",                      " pres.hpa",                      " temp.c",                      " warm.flag",                      " rh.aimms.per",                      " dmax.85.per.L.um",                      " MVD.um",                      " NEV.LWC.gm3",                      " NEV.TWC.gm3",                      " RID.LWC.gm3",                      " CDP.LWC.gm3",                      " OAP.LWC.gm3", "CNV.NAW..zennadREFL.30s.mean.zen", "CNV.NAW.zennad.REFL.30s.mean.nad", "CNV.NAW.zennad.REFL.30s.std.zen", "CNV.NAW.zennad.REFL.30s.std.nad", "CNV.NAW.zennad.VEL.30s.mean.zen", "CNV.NAW..zennadVEL.30s.mean.nad", "CNV.NAW.zennad.VEL.30s.std.zen", "CNV.NAW.zennad.VEL.30s.std.nad",                  " dist.radartoac.km",                " FZDZ.INT.max.vol",               " int.sdev.DBZ.FZDZ.max.vol",               " int.T.DBZ.FZDZ.max.vol",               " int.mean.DBZ.FZDZ.max.vol",               " int.block.sdev.DBZ.FZDZ.max.vol",               " int.block.median.DBZ.FZDZ.max.vol",               " sdev.DBZ.FZDZ.max.vol",               " T.DBZ.FZDZ.max.vol",               " mean.DBZ.FZDZ.max.vol",               " block.sdev.DBZ.FZDZ.max.vol",               " block.median.DBZ.FZDZ.max.vol",               " mask.DBZ.FZDZ.max.vol",               " ZDR.MOMS.FZDZ.max.vol",               " RHO.MOMS.FZDZ.max.vol",               " KDP.MOMS.FZDZ.max.vol",               " SLW.INT.max.vol",               " int.mean.ZDR.corr.SLW.max.vol",               " int.sdev.ZDR.SLW.max.vol",               " int.mean.KDP.SLW.max.vol",               " int.sdev.KDP.SLW.max.vol",               " mean.ZDR.corr.SLW.max.vol",               " sdev.ZDR.SLW.max.vol",               " mean.KDP.SLW.max.vol",               " sdev.KDP.SLW.max.vol",               " DBZ.MOMS.SLW.max.vol",               " RHO.MOMS.SLW.max.vol",               " MIXPHA.INT.max.vol",          " int.mean.ZDR.corr.MIXPHA.max.vol",               " int.mean.DBZ.MIXPHA.max.vol",               " int.temp.MIXPHA.max.vol",               " mean.ZDR.corr.MIXPHA.max.vol",               " mean.DBZ.MIXPHA.max.vol",               " RHO.MOMS.MIXPHA.max.vol",               " KDP.MOMS.MIXPHA.max.vol",               " PLATES.INT.max.vol",               " int.mean.ZDR.PLATES.max.vol",                " int.mean.DBZ.PLATES.max.vol",              " int.temp.PLATES.max.vol",               " mean.ZDR.corr.PLATES.max.vol",               " mean.DBZ.PLATES.max.vol",               " RHO.MOMS.PLATES.max.vol",                " KDP.MOMS.PLATES.max.vol"                 
          #write.table(cbind(as.numeric(case.time.list.df$yyyymmdd[ppp]), case.time.list.df$flight.num[ppp], as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(FZDZ.INT.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(SLW.INT.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(MIXPHA.INT.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(PLATES.INT.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver2/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v",  latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          #print(      cbind(as.numeric(case.time.list.df$yyyymmdd[ppp]), case.time.list.df$flight.num[ppp], as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$MVD.um[j], CNV.30s.mean.df$NEV.LWC.gm3[j], CNV.30s.mean.df$NEV.TWC.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$CDP.LWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j],   CNV.NAW.zennad.REFL.30s.mean.zen,   CNV.NAW.zennad.REFL.30s.mean.nad,   CNV.NAW.zennad.REFL.30s.std.zen,   CNV.NAW.zennad.REFL.30s.std.nad,   CNV.NAW.zennad.VEL.30s.mean.zen,   CNV.NAW.zennad.VEL.30s.mean.nad,   CNV.NAW.zennad.VEL.30s.std.zen,   CNV.NAW.zennad.VEL.30s.std.nad, round(dist.radartoac.km[j], digits=2),  round(FZDZ.INT.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(SLW.INT.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(MIXPHA.INT.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(PLATES.INT.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)))
        } else if (latest.mosaic.ver.num == 2.2) {
          print("Into ver 2.2")
          # using max in volume as match to radar
          #write.table(cbind(as.character(case.time.list.df$yyyymmdd[ppp]), as.numeric(as.character(case.time.list.df$flight.num[ppp])), as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j],  CNV.30s.mean.df$truth.cat[j], CNV.30s.mean.df$MLF.gt.10um.per[j], CNV.30s.mean.df$MLF.gt.50um.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$dmax.99perc.um[j], CNV.30s.mean.df$NEV.1.LWC.gm3[j], CNV.30s.mean.df$NEV.1.IWC.gm3[j], CNV.30s.mean.df$NEV.2.LWC.gm3[j], CNV.30s.mean.df$NEV.2.IWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j], CNV.30s.mean.df$OAP.IWC.gm3[j], CNV.30s.mean.df$CDP.LWC.prelim.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$liq.conc.tot.cm3[j], CNV.30s.mean.df$ice.conc.tot.cm3[j], CNV.30s.mean.df$MVD.liq.um[j], CNV.30s.mean.df$MMD.liq.um[j], CNV.30s.mean.df$MVD.eq.ice.um[j], CNV.30s.mean.df$MMD.eq.ice.um[j], CNV.30s.mean.df$MVD.max.ice.um[j], CNV.30s.mean.df$MMD.max.ice.um[j], CNV.30s.mean.df$frac.sphere.2DC[j], CNV.30s.mean.df$frac.irreg.2DC[j], CNV.30s.mean.df$frac.needle.2DC[j], CNV.30s.mean.df$frac.dend.2DC[j], CNV.30s.mean.df$frac.sphere.HVPS[j], CNV.30s.mean.df$frac.irreg.HVPS[j], CNV.30s.mean.df$frac.needle.HVPS[j], CNV.30s.mean.df$frac.dend.HVPS[j], CNV.30s.mean.df$REFL.liq.derive.dbz[j], CNV.30s.mean.df$REFL.ice.derive.dbz[j], CNV.30s.mean.df$CEP.ext.perkm[j], CNV.NAW.REFL.30s.mean.zen,   CNV.NAW.REFL.30s.mean.nad,   CNV.NAW.REFL.30s.std.zen,   CNV.NAW.REFL.30s.std.nad,   CNV.NAW.REFL.range.std.zen,   CNV.NAW.REFL.range.std.nad, CNV.NAW.VEL.30s.mean.zen,   CNV.NAW.VEL.30s.mean.nad,   CNV.NAW.VEL.30s.std.zen,   CNV.NAW.VEL.30s.std.nad, CNV.NAW.VEL.range.std.zen,   CNV.NAW.VEL.range.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(int.full.SLW.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(int.full.MIXPHA.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(int.full.PLATES.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, na = "NaN", file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/",  case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v",  latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          #print(      cbind(as.character(case.time.list.df$yyyymmdd[ppp]), as.numeric(as.character(case.time.list.df$flight.num[ppp])), as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j],  CNV.30s.mean.df$truth.cat[j], CNV.30s.mean.df$MLF.gt.10um.per[j], CNV.30s.mean.df$MLF.gt.50um.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$dmax.99perc.um[j], CNV.30s.mean.df$NEV.1.LWC.gm3[j], CNV.30s.mean.df$NEV.1.IWC.gm3[j], CNV.30s.mean.df$NEV.2.LWC.gm3[j], CNV.30s.mean.df$NEV.2.IWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j], CNV.30s.mean.df$OAP.IWC.gm3[j], CNV.30s.mean.df$CDP.LWC.prelim.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$liq.conc.tot.cm3[j], CNV.30s.mean.df$ice.conc.tot.cm3[j], CNV.30s.mean.df$MVD.liq.um[j], CNV.30s.mean.df$MMD.liq.um[j], CNV.30s.mean.df$MVD.eq.ice.um[j], CNV.30s.mean.df$MMD.eq.ice.um[j], CNV.30s.mean.df$MVD.max.ice.um[j], CNV.30s.mean.df$MMD.max.ice.um[j], CNV.30s.mean.df$frac.sphere.2DC[j], CNV.30s.mean.df$frac.irreg.2DC[j], CNV.30s.mean.df$frac.needle.2DC[j], CNV.30s.mean.df$frac.dend.2DC[j], CNV.30s.mean.df$frac.sphere.HVPS[j], CNV.30s.mean.df$frac.irreg.HVPS[j], CNV.30s.mean.df$frac.needle.HVPS[j], CNV.30s.mean.df$frac.dend.HVPS[j], CNV.30s.mean.df$REFL.liq.derive.dbz[j], CNV.30s.mean.df$REFL.ice.derive.dbz[j], CNV.30s.mean.df$CEP.ext.perkm[j], CNV.NAW.REFL.30s.mean.zen,   CNV.NAW.REFL.30s.mean.nad,   CNV.NAW.REFL.30s.std.zen,   CNV.NAW.REFL.30s.std.nad,   CNV.NAW.REFL.range.std.zen,   CNV.NAW.REFL.range.std.nad, CNV.NAW.VEL.30s.mean.zen,   CNV.NAW.VEL.30s.mean.nad,   CNV.NAW.VEL.30s.std.zen,   CNV.NAW.VEL.30s.std.nad, CNV.NAW.VEL.range.std.zen,   CNV.NAW.VEL.range.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.max.vol, digits=2), round(int.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.T.DBZ.FZDZ.max.vol, digits=2), round(int.mean.DBZ.FZDZ.max.vol, digits=2), round(int.block.sdev.DBZ.FZDZ.max.vol, digits=2), round(int.block.median.DBZ.FZDZ.max.vol, digits=2), round(sdev.DBZ.FZDZ.max.vol, digits=2), round(T.DBZ.FZDZ.max.vol, digits=2), round(mean.DBZ.FZDZ.max.vol, digits=2), round(block.sdev.DBZ.FZDZ.max.vol, digits=2), round(block.median.DBZ.FZDZ.max.vol, digits=2), round(mask.DBZ.FZDZ.max.vol, digits=2), round(ZDR.MOMS.FZDZ.max.vol, digits=2), round(RHO.MOMS.FZDZ.max.vol, digits=2), round(KDP.MOMS.FZDZ.max.vol, digits=2), round(int.full.SLW.max.vol, digits=2), round(int.mean.ZDR.corr.SLW.max.vol, digits=2), round(int.sdev.ZDR.SLW.max.vol, digits=2), round(int.mean.KDP.SLW.max.vol, digits=2), round(int.sdev.KDP.SLW.max.vol, digits=2), round(mean.ZDR.corr.SLW.max.vol, digits=2), round(sdev.ZDR.SLW.max.vol, digits=2), round(mean.KDP.SLW.max.vol, digits=2), round(sdev.KDP.SLW.max.vol, digits=2), round(DBZ.MOMS.SLW.max.vol, digits=2), round(RHO.MOMS.SLW.max.vol, digits=2), round(int.full.MIXPHA.max.vol, digits=2), round(int.mean.ZDR.MIXPHA.max.vol, digits=2), round(int.mean.DBZ.MIXPHA.max.vol, digits=2), round(int.temp.MIXPHA.max.vol, digits=2), round(mean.ZDR.corr.MIXPHA.max.vol, digits=2), round(mean.DBZ.MIXPHA.max.vol, digits=2), round(RHO.MOMS.MIXPHA.max.vol, digits=2), round(KDP.MOMS.MIXPHA.max.vol, digits=2), round(int.full.PLATES.max.vol, digits=2), round(int.mean.ZDR.PLATES.max.vol, digits=2), round(int.mean.DBZ.PLATES.max.vol, digits=2), round(int.temp.PLATES.max.vol, digits=2), round(mean.ZDR.corr.PLATES.max.vol, digits=2), round(mean.DBZ.PLATES.max.vol, digits=2), round(RHO.MOMS.PLATES.max.vol, digits=2), round(KDP.MOMS.PLATES.max.vol, digits=2)))
          #write.table(cbind(                                 " yyyymmdd",                                                      " f#",             " hh",             " mm",             " ss",                    " seg#",                    " seg.t",                    " lat.d",                   " lon.d",                 " alt.m",                   " hdg.d",                   " roll.d",                   " pitch.d",                  " pres.mb",                    " T.c",                   " warm.fg",                   " rh.aimms.%",                   " truth.cat",                   " MLF.gt.10um.%",                   " MLF.gt.50um.%",                 " dmax.85.per.L.um",                    " dmax.99%.um",                 " NEV.1.LWC.gm3",                 " NEV.1.IWC.gm3",                 " NEV.2.LWC.gm3",                 " NEV.2.IWC.gm3",                 " OAP.LWC.gm3", "                 OAP.IWC.gm3",                 " CDP.LWC.prelim.gm3",                 " RID.LWC.gm3",                 " liq.conc.tot.cm3",                 " ice.conc.tot.cm3",                 " MVD.liq.um",                 " MMD.liq.um",                 " MVD.eq.ice.um",                 " MMD.eq.ice.um",                 " MVD.max.ice.um",                 " MMD.max.ice.um",                    " 2DC.sphere.%",                    " 2DC.irreg.%",                    " 2DC.needle.%",                    " 2DC.dend.%",                    " HVPS.sphere.%",                    " HVPS.irreg.%",                    " HVPS.needle.%",                    " HVPS.dend.%",                 " REFL.liq.derive.dbz",                 " REFL.ice.derive.dbz",                 " CEP.ext.perkm",  " NAW.REFL.30s.mean.zen",    " NAW.REFL.30s.mean.nad",    " NAW.REFL.30s.std.zen",    " NAW.REFL.30s.std.nad",    " NAW.REFL.range.std.zen",    " NAW.REFL.range.std.nad",  " NAW.VEL.30s.mean.zen",    " NAW.VEL.30s.mean.nad",    " NAW.VEL.30s.std.zen",    " NAW.VEL.30s.std.nad",  " NAW.VEL.range.std.zen",    " NAW.VEL.range.std.nad",                  " dist.radartoac.km",            " int.full.FZDZ.max.vol",               " int.sdev.DBZ.FZDZ.max.vol",               " int.T.DBZ.FZDZ.max.vol",               " int.mean.DBZ.FZDZ.max.vol",               " int.block.sdev.DBZ.FZDZ.max.vol",               " int.block.median.DBZ.FZDZ.max.vol",               " sdev.DBZ.FZDZ.max.vol",               " T.DBZ.FZDZ.max.vol",               " mean.DBZ.FZDZ.max.vol",               " block.sdev.DBZ.FZDZ.max.vol",               " block.median.DBZ.FZDZ.max.vol",               " mask.DBZ.FZDZ.max.vol",               " ZDR.MOMS.FZDZ.max.vol",               " RHO.MOMS.FZDZ.max.vol",               " KDP.MOMS.FZDZ.max.vol",               " int.full.SLW.max.vol",               " int.mean.ZDR.corr.SLW.max.vol",               " int.sdev.ZDR.SLW.max.vol",               " int.mean.KDP.SLW.max.vol",               " int.sdev.KDP.SLW.max.vol",               " mean.ZDR.corr.SLW.max.vol",               " sdev.ZDR.SLW.max.vol",               " mean.KDP.SLW.max.vol",               " sdev.KDP.SLW.max.vol",               " DBZ.MOMS.SLW.max.vol",               " RHO.MOMS.SLW.max.vol",               " int.full.MIXPHA.max.vol",               " int.mean.ZDR.MIXPHA.max.vol",               " int.mean.DBZ.MIXPHA.max.vol",               " int.temp.MIXPHA.max.vol",                    " mean.ZDR.MIXPHA.max.vol",               " mean.DBZ.MIXPHA.max.vol",               " RHO.MOMS.MIXPHA.max.vol",               " KDP.MOMS.MIXPHA.max.vol",               " int.full.PLATES.max.vol",          " int.mean.ZDR.corr.PLATES.max.vol",               " int.mean.DBZ.PLATES.max.vol",               " int.temp.PLATES.max.vol",               " mean.ZDR.corr.PLATES.max.vol",               " mean.DBZ.PLATES.max.vol",               " RHO.MOMS.PLATES.max.vol",               " KDP.MOMS.PLATES.max.vol"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep=""))
          # using single closest pixel as match to radar
          ############# NEXT LINE WORKS UNCOMMENT ##############################################################
          #write.table(cbind(as.character(case.time.list.df$yyyymmdd[ppp]), as.numeric(as.character(case.time.list.df$flight.num[ppp])), as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]), CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j],  CNV.30s.mean.df$truth.cat[j], CNV.30s.mean.df$MLF.gt.10um.per[j], CNV.30s.mean.df$MLF.gt.50um.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$dmax.99perc.um[j], CNV.30s.mean.df$NEV.1.LWC.gm3[j], CNV.30s.mean.df$NEV.1.IWC.gm3[j], CNV.30s.mean.df$NEV.2.LWC.gm3[j], CNV.30s.mean.df$NEV.2.IWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j], CNV.30s.mean.df$OAP.IWC.gm3[j], CNV.30s.mean.df$CDP.LWC.prelim.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$liq.conc.tot.cm3[j], CNV.30s.mean.df$ice.conc.tot.cm3[j], CNV.30s.mean.df$MVD.liq.um[j], CNV.30s.mean.df$MMD.liq.um[j], CNV.30s.mean.df$MVD.eq.ice.um[j], CNV.30s.mean.df$MMD.eq.ice.um[j], CNV.30s.mean.df$MVD.max.ice.um[j], CNV.30s.mean.df$MMD.max.ice.um[j], CNV.30s.mean.df$frac.sphere.2DC[j], CNV.30s.mean.df$frac.irreg.2DC[j], CNV.30s.mean.df$frac.needle.2DC[j], CNV.30s.mean.df$frac.dend.2DC[j], CNV.30s.mean.df$frac.sphere.HVPS[j], CNV.30s.mean.df$frac.irreg.HVPS[j], CNV.30s.mean.df$frac.needle.HVPS[j], CNV.30s.mean.df$frac.dend.HVPS[j], CNV.30s.mean.df$REFL.liq.derive.dbz[j], CNV.30s.mean.df$REFL.ice.derive.dbz[j], CNV.30s.mean.df$CEP.ext.perkm[j], CNV.NAW.REFL.30s.mean.zen,   CNV.NAW.REFL.30s.mean.nad,   CNV.NAW.REFL.30s.std.zen,   CNV.NAW.REFL.30s.std.nad,   CNV.NAW.REFL.range.std.zen,   CNV.NAW.REFL.range.std.nad, CNV.NAW.VEL.30s.mean.zen,   CNV.NAW.VEL.30s.mean.nad,   CNV.NAW.VEL.30s.std.zen,   CNV.NAW.VEL.30s.std.nad, CNV.NAW.VEL.range.std.zen,   CNV.NAW.VEL.range.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.pix, digits=2), round(int.sdev.DBZ.FZDZ.pix, digits=2), round(int.T.DBZ.FZDZ.pix, digits=2), round(int.mean.DBZ.FZDZ.pix, digits=2), round(int.block.sdev.DBZ.FZDZ.pix, digits=2), round(int.block.median.DBZ.FZDZ.pix, digits=2), round(sdev.DBZ.FZDZ.pix, digits=2), round(T.DBZ.FZDZ.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(block.sdev.DBZ.FZDZ.pix, digits=2), round(block.median.DBZ.FZDZ.pix, digits=2), round(mask.DBZ.FZDZ.pix, digits=2), round(ZDR.MOMS.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2), round(int.full.SLW.pix, digits=2), round(int.mean.ZDR.corr.SLW.pix, digits=2), round(int.sdev.ZDR.SLW.pix, digits=2), round(int.mean.KDP.SLW.pix, digits=2), round(int.sdev.KDP.SLW.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(sdev.ZDR.SLW.pix, digits=2), round(mean.KDP.SLW.pix, digits=2), round(sdev.KDP.SLW.pix, digits=2), round(DBZ.MOMS.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(int.full.MIXPHA.pix, digits=2), round(int.mean.ZDR.MIXPHA.pix, digits=2), round(int.mean.DBZ.MIXPHA.pix, digits=2), round(int.temp.MIXPHA.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2), round(int.full.PLATES.pix, digits=2), round(int.mean.ZDR.PLATES.pix, digits=2), round(int.mean.DBZ.PLATES.pix, digits=2), round(int.temp.PLATES.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2)), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, na = "NaN", file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/",  case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          ####write.table(cbind(                                 " yyyymmdd",                                                      " f#",             " hh",             " mm",             " ss",                     " seg#",                    " seg.t",                    " lat.d",                   " lon.d",                 " alt.m",                   " hdg.d",                   " roll.d",                   " pitch.d",                  " pres.mb",                    " T.c",                   " warm.fg",                   " rh.aimms.%",                   " truth.cat",                   " MLF.gt.10um.%",                   " MLF.gt.50um.%",                 " dmax.85.per.L.um",                    " dmax.99%.um",                 " NEV.1.LWC.gm3",                 " NEV.1.IWC.gm3",                 " NEV.2.LWC.gm3",                 " NEV.2.IWC.gm3",                 " OAP.LWC.gm3", "                 OAP.IWC.gm3",                 " CDP.LWC.prelim.gm3",                 " RID.LWC.gm3",                 " liq.conc.tot.cm3",                 " ice.conc.tot.cm3",                 " MVD.liq.um",                 " MMD.liq.um",                 " MVD.eq.ice.um",                 " MMD.eq.ice.um",                 " MVD.max.ice.um",                 " MMD.max.ice.um",                    " 2DC.sphere.%",                    " 2DC.irreg.%",                    " 2DC.needle.%",                    " 2DC.dend.%",                    " HVPS.sphere.%",                    " HVPS.irreg.%",                    " HVPS.needle.%",                    " HVPS.dend.%",                 " REFL.liq.derive.dbz",                 " REFL.ice.derive.dbz",                 " CEP.ext.perkm",  " NAW.REFL.30s.mean.zen",    " NAW.REFL.30s.mean.nad",    " NAW.REFL.30s.std.zen",    " NAW.REFL.30s.std.nad",    " NAW.REFL.range.std.zen",    " NAW.REFL.range.std.nad",  " NAW.VEL.30s.mean.zen",    " NAW.VEL.30s.mean.nad",    " NAW.VEL.30s.std.zen",    " NAW.VEL.30s.std.nad",  " NAW.VEL.range.std.zen",    " NAW.VEL.range.std.nad",                  " dist.radartoac.km",            " int.full.FZDZ.pix",               " int.sdev.DBZ.FZDZ.pix",               " int.T.DBZ.FZDZ.pix",               " int.mean.DBZ.FZDZ.pix",               " int.block.sdev.DBZ.FZDZ.pix",               " int.block.median.DBZ.FZDZ.pix",               " sdev.DBZ.FZDZ.pix",               " T.DBZ.FZDZ.pix",               " mean.DBZ.FZDZ.pix",               " block.sdev.DBZ.FZDZ.pix",               " block.median.DBZ.FZDZ.pix",               " mask.DBZ.FZDZ.pix",               " ZDR.MOMS.pix",               " RHO.MOMS.pix",               " KDP.MOMS.pix",               " int.full.SLW.pix",               " int.mean.ZDR.corr.SLW.pix",               " int.sdev.ZDR.SLW.pix",               " int.mean.KDP.SLW.pix",               " int.sdev.KDP.SLW.pix",               " mean.ZDR.corr.SLW.pix",               " sdev.ZDR.SLW.pix",               " mean.KDP.SLW.pix",               " sdev.KDP.SLW.pix",               " DBZ.MOMS.pix",               " RHO.MOM.pix",               " int.full.MIXPHA.pix",               " int.mean.ZDR.MIXPHA.pix",               " int.mean.DBZ.MIXPHA.pix",               " int.temp.MIXPHA.pix",                    " mean.ZDR.corr.SLW.pix",               " mean.DBZ.FZDZ.pix",               " RHO.MOMS.pix",               " KDP.MOMS.pix",               " int.full.PLATES.pix",          " int.mean.ZDR.corr.PLATES.pix",               " int.mean.DBZ.PLATES.pix",               " int.temp.PLATES.pix",               " mean.ZDR.corr.SLW.pix",               " mean.DBZ.FZDZ.pix",               " RHO.MOMS.pix",               " KDP.MOMS.pix"), sep = ",", col.names = FALSE, quote = FALSE, append = TRUE, file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v",  latest.30s.mean.ver.num, ".csv", sep=""))
          write.table(cbind(as.character(case.time.list.df$yyyymmdd[ppp]), as.numeric(as.character(case.time.list.df$flight.num[ppp])), as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]),  grid.i[j],   grid.j[j],   grid.k[j], CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j],  CNV.30s.mean.df$truth.cat[j]), sep = ", ", col.names = FALSE, quote = FALSE, append = TRUE, na = "NaN", file = paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/",  case.time.list.df$yyyymmdd[1], "_F", case.time.list.df$flight.num[1], "_", case.time.list.df$hh.min.for.plotting[1], case.time.list.df$mm.min.for.plotting[1], "to", case.time.list.df$hh.max.for.plotting[dim(case.time.list.df)[1]], case.time.list.df$mm.max.for.plotting[dim(case.time.list.df)[1]], "Z_RadIA-m_v", latest.mosaic.ver.num, "_CNV_v", latest.30s.mean.ver.num, ".csv", sep=""))
          print(      cbind(as.character(case.time.list.df$yyyymmdd[ppp]), as.numeric(as.character(case.time.list.df$flight.num[ppp])), as.numeric(hh[j]), as.numeric(mm[j]), as.numeric(ss[j]),  grid.i[j],   grid.j[j],   grid.k[j], CNV.30s.mean.df$seg.num[j], CNV.30s.mean.df$seg.type[j],  CNV.30s.mean.df$lat.deg[j], CNV.30s.mean.df$lon.deg[j], CNV.30s.mean.df$alt.m[j], CNV.30s.mean.df$hdg.deg[j], CNV.30s.mean.df$roll.deg[j], CNV.30s.mean.df$pitch.deg[j], CNV.30s.mean.df$pres.hpa[j], CNV.30s.mean.df$temp.c[j], CNV.30s.mean.df$warm.flag[j], CNV.30s.mean.df$rh.aimms.per[j],  CNV.30s.mean.df$truth.cat[j], CNV.30s.mean.df$MLF.gt.10um.per[j], CNV.30s.mean.df$MLF.gt.50um.per[j], CNV.30s.mean.df$dmax.85.per.L.um[j], CNV.30s.mean.df$dmax.99perc.um[j], CNV.30s.mean.df$NEV.1.LWC.gm3[j], CNV.30s.mean.df$NEV.1.IWC.gm3[j], CNV.30s.mean.df$NEV.2.LWC.gm3[j], CNV.30s.mean.df$NEV.2.IWC.gm3[j], CNV.30s.mean.df$OAP.LWC.gm3[j], CNV.30s.mean.df$OAP.IWC.gm3[j], CNV.30s.mean.df$CDP.LWC.prelim.gm3[j], CNV.30s.mean.df$RID.LWC.gm3[j], CNV.30s.mean.df$liq.conc.tot.cm3[j], CNV.30s.mean.df$ice.conc.tot.cm3[j], CNV.30s.mean.df$MVD.liq.um[j], CNV.30s.mean.df$MMD.liq.um[j], CNV.30s.mean.df$MVD.eq.ice.um[j], CNV.30s.mean.df$MMD.eq.ice.um[j], CNV.30s.mean.df$MVD.max.ice.um[j], CNV.30s.mean.df$MMD.max.ice.um[j], CNV.30s.mean.df$frac.sphere.2DC[j], CNV.30s.mean.df$frac.irreg.2DC[j], CNV.30s.mean.df$frac.needle.2DC[j], CNV.30s.mean.df$frac.dend.2DC[j], CNV.30s.mean.df$frac.sphere.HVPS[j], CNV.30s.mean.df$frac.irreg.HVPS[j], CNV.30s.mean.df$frac.needle.HVPS[j], CNV.30s.mean.df$frac.dend.HVPS[j], CNV.30s.mean.df$REFL.liq.derive.dbz[j], CNV.30s.mean.df$REFL.ice.derive.dbz[j], CNV.30s.mean.df$CEP.ext.perkm[j], as.character(CNV.NAW.REFL.30s.mean.zen),   CNV.NAW.REFL.30s.mean.nad,   CNV.NAW.REFL.30s.std.zen,   CNV.NAW.REFL.30s.std.nad,   CNV.NAW.REFL.range.std.zen,   CNV.NAW.REFL.range.std.nad, CNV.NAW.VEL.30s.mean.zen,   CNV.NAW.VEL.30s.mean.nad,   CNV.NAW.VEL.30s.std.zen,   CNV.NAW.VEL.30s.std.nad, CNV.NAW.VEL.range.std.zen,   CNV.NAW.VEL.range.std.nad, round(dist.radartoac.km[j], digits=2),  round(int.full.FZDZ.pix, digits=2), round(int.sdev.DBZ.FZDZ.pix, digits=2), round(int.T.DBZ.FZDZ.pix, digits=2), round(int.mean.DBZ.FZDZ.pix, digits=2), round(int.block.sdev.DBZ.FZDZ.pix, digits=2), round(int.block.median.DBZ.FZDZ.pix, digits=2), round(sdev.DBZ.FZDZ.pix, digits=2), round(T.DBZ.FZDZ.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(block.sdev.DBZ.FZDZ.pix, digits=2), round(block.median.DBZ.FZDZ.pix, digits=2), round(mask.DBZ.FZDZ.pix, digits=2), round(ZDR.MOMS.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2), round(int.full.SLW.pix, digits=2), round(int.mean.ZDR.corr.SLW.pix, digits=2), round(int.sdev.ZDR.SLW.pix, digits=2), round(int.mean.KDP.SLW.pix, digits=2), round(int.sdev.KDP.SLW.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(sdev.ZDR.SLW.pix, digits=2), round(mean.KDP.SLW.pix, digits=2), round(sdev.KDP.SLW.pix, digits=2), round(DBZ.MOMS.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(int.full.MIXPHA.pix, digits=2), round(int.mean.ZDR.MIXPHA.pix, digits=2), round(int.mean.DBZ.MIXPHA.pix, digits=2), round(int.temp.MIXPHA.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2), round(int.full.PLATES.pix, digits=2), round(int.mean.ZDR.PLATES.pix, digits=2), round(int.mean.DBZ.PLATES.pix, digits=2), round(int.temp.PLATES.pix, digits=2), round(mean.ZDR.corr.SLW.pix, digits=2), round(mean.DBZ.FZDZ.pix, digits=2), round(RHO.MOMS.pix, digits=2), round(KDP.MOMS.pix, digits=2)))
          } # end of if (latest.mosaic.ver.num == 1) 
        
      } else {
        print("      Not writing output data to file.  Flag = 0")
      }    # end of if ...
    
      #browser()
      
    }      # end of for (j in ind.hhmm.total[1]:ind.hhmm.total[length(ind.hhmm.total)]))
    
    #print("out of j loop")
    
    browser()
    
  }      # end of for (ppp in ...)  
    
  # Start writing to an output file
  #sink(paste(output.matchup.data.dir, "print.output.txt", sep = ""), append = TRUE)
  
  print("-----------")
    
  # add new matchup fields to CNV 30-s mean dataframe
  CNV.30s.mean.df                 <- cbind(CNV.30s.mean.df, dist.radartoac.km, bearing.radartoac.deg, range.radartoac.km, theta.radartoac.deg, FZDZ.closest.mosaic.mean.volume, meanDBZ.FZDZ.closest.mosaic.mean.volume, sdevDBZ.FZDZ.closest.mosaic.mean.volume, TDBZ.FZDZ.closest.mosaic.mean.volume, SLW.closest.mosaic.mean.volume, meanZDR.SLW.closest.mosaic.mean.volume, sdevZDR.SLW.closest.mosaic.mean.volume, meanKDP.SLW.closest.mosaic.mean.volume, sdevKDP.SLW.closest.mosaic.mean.volume, MIXPHA.closest.mosaic.mean.volume, meanZDR.MIXPHA.closest.mosaic.mean.volume, meanDBZ.MIXPHA.closest.mosaic.mean.volume, TEMP.MIXPHA.closest.mosaic.mean.volume)
    
  #----------------------------------------------
  #
  # plotting
  #
  #----------------------------------------------
  print("-----------")
  print(paste("  Plotting image(s) to GUI", sep=""))
  
  if (CNV.NAW.zennad.nc.file.match.flag  == 1) {
    # plot timeseries of CNV NAW
    #dev.new()
    image.plot(CNV.NAW.TIME.hhmm, CNV.NAW.ALTRANGE.m, flipud(t(CNV.NAW.REFL.dbz)), xlab="Time [s]", ylab="Alt [m]")
    grid(NA, 5, lwd = 2)
  }
  
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
  
  #-------------------------------------------------
  # save off image to a yyyymmdd_F## directory
  #  this block outputs plot for above current image
  #  copy/paste/edit this block for every image
  #-------------------------------------------------
  print("-----------")
  if (output.matchup.image.flag == 1) {
    print("  Checking if output image directory exists...")
    if (dir.exists(paste(output.matchup.image.dir, sep="")) == TRUE) {
      print("    Output image dir does exist")
    } else {
      print("    Output image dir does not exist.  Creating...")
      dir.create(file.path(paste(output.matchup.image.dir, as.character(yyyymmdd), "_", as.character(flight.num), "/ver", latest.30s.mean.ver.num,  sep="")))
    }
    
    print(paste("    Writing output image to file: ", output.matchup.image.dir, "/", as.character(yyyymmdd), "_", as.character(flight.num), "_", hh.min.for.plotting, mm.min.for.plotting, "to", hh.max.for.plotting, mm.max.for.plotting, "Z_2x2_track_LWCs_diffs_DmaxMVD.png", sep = ""))
    dev.copy(png,                               paste(output.matchup.image.dir, "/", as.character(yyyymmdd), "_", as.character(flight.num), "_", hh.min.for.plotting, mm.min.for.plotting, "to", hh.max.for.plotting, mm.max.for.plotting, "Z_2x2_track_LWCs_diffs_DmaxMVD.png", sep = ""))
    dev.off()
  } else {
    print("  Not writing output image to file.  Flag = 0")
  }  # end of if ...
  print("-----------")

  print(paste("Done processing through CNV 30-s mean flight file number ", ii, sep=""))
  
  print("-----------")
  
  #sink()
  closeAllConnections()
  
} # end of for ii in 1: length(CNV......)


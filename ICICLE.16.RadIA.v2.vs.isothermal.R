#--------------------------------------------
#
# Name:    ICICLE.16.RadIA.v2.vs.isothermal.R
#
# Purpose: 1) 
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
latest.mosaic.ver.num               <- 2.2            # as of 09.21.2020, produced by Dave A., but science team should be able to process as of this date
#latest.mosaic.ver.num               <- 3             # as of ??.??.????, produced by Dave S. and Scott E., first outputted version produced by science team

# aircraft CNV 30-s mean input data information
#   latest data version number
#latest.30s.mean.ver.num            <- 1               # as of 04.01.2020, original output from Allyson
#latest.30s.mean.ver.num            <- 2               # as of 06.10.2020, fixed column headers, Dmax pass through corrected
latest.30s.mean.ver.num             <- 3               # as of 10.22.2020, when ECCC sends SNDI-like 2DS field data

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

# params for single case processing
##   F17
#flight.num                          <- 17 
#yyyy.mm.dd                          <- "20190217"
##   F20
#flight.num                          <- 15 
#yyyy.mm.dd                          <- "20190215"
##   F21
#flight.num                          <- 21 
#yyyy.mm.dd                          <- "20190224"
##   F22
#flight.num                          <- 22 
#yyyy.mm.dd                          <- "20190224"
#   F24
flight.num                          <- 24
#yyyy.mm.dd                          <- "20190226"
yyyy.mm.dd                          <- "2019-02-27"
NEXRAD.site.name                    <- " KDVN"

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
base.path.ext.dir                    <- file.path("/media/ice_d3/projects/case_studies/")

# define NEXRAD site location csv file path
nexrad.site.dataframetxt.dir         <- file.path(paste(base.path.dir, "SNOWIE_2017/data/RadIA_data/nexrad_site_data/", sep=""))

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

# define RAP temp prof txt format file path
RAP.temp.txt.dir                     <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA_temp_profs_ascii/", yyyy.mm.dd, "/", sep=""))
RAP.temp.txt.listfiles               <- list.files(path = RAP.temp.txt.dir , include.dirs = FALSE)

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
  radia.mosaic.INTS.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v2.2/algo_ints/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.INTS.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/algo_ints/",  yyyy.mm.dd, "/", sep=""))
  radia.mosaic.INTS.nc.listfiles       <- list.files(path = radia.mosaic.INTS.nc.dir ,   include.dirs = FALSE)
  radia.mosaic.INTS.nc.filename        <- radia.mosaic.INTS.nc.listfiles[13]
  print(radia.mosaic.INTS.nc.filename)
  #radia.mosaic.FZDZ.nc.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/frzdrz/",  yyyy.mm.dd, "/", sep=""))
  ##radia.mosaic.FZDZ.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/fzdz_flds/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.FZDZ.nc.listfiles       <- list.files(path = radia.mosaic.FZDZ.nc.dir ,   include.dirs = FALSE)
  #radia.mosaic.SLW.nc.dir              <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/slw/",  yyyy.mm.dd, "/", sep=""))
  ##radia.mosaic.SLW.nc.dir              <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/slw_flds/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.SLW.nc.listfiles        <- list.files(path = radia.mosaic.SLW.nc.dir ,    include.dirs = FALSE)
  #radia.mosaic.MIXPHA.nc.dir           <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/mixpha/",  yyyy.mm.dd, "/", sep=""))
  ##radia.mosaic.MIXPHA.nc.dir           <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/mpha_flds/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.MIXPHA.nc.listfiles     <- list.files(path = radia.mosaic.MIXPHA.nc.dir , include.dirs = FALSE)
  #radia.mosaic.PLATES.nc.dir           <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/plates/",  yyyy.mm.dd, "/", sep=""))
  ##radia.mosaic.PLATES.nc.dir           <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/plat_flds/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.PLATES.nc.listfiles     <- list.files(path = radia.mosaic.PLATES.nc.dir , include.dirs = FALSE)
  #radia.mosaic.MOMS.nc.dir             <- file.path(paste(base.path.ext.dir, "ICICLE_2019/data/FINAL/RadIA-mosaic_v2_data/debug/passthrough/",  yyyy.mm.dd, "/", sep=""))
  ##radia.mosaic.MOMS.nc.dir             <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/v", latest.mosaic.ver.num, "/raw_moms/",  yyyy.mm.dd, "/", sep=""))
  #radia.mosaic.MOMS.nc.listfiles       <- list.files(path = radia.mosaic.MOMS.nc.dir ,   include.dirs = FALSE)
} else {
  print("Error")
}

# Also define path of RadIA-m rerun output for isothermal T=-12, T=-2, T=+2
radia.mosaic.INTS.Tneg12C.nc.dir       <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/rerun/output4d_-12C_rerun/main/2019-02-27/", sep=""))
radia.mosaic.INTS.Tneg02C.nc.dir       <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/rerun/output4d_-02C_rerun/main/2019-02-27/", sep=""))
radia.mosaic.INTS.Tpos02C.nc.dir       <- file.path(paste(base.path.dir, "ICICLE_2019/data/nc_radia_MCCC/rerun/output4d_02C_rerun/main/2019-02-27/",  sep=""))

# Also define filenames of RadIA-m rerun output for isothermal T=-12, T=-2, T=+2
radia.mosaic.INTS.Tneg12C.nc.listfiles <- list.files(path = radia.mosaic.INTS.Tneg12C.nc.dir ,   include.dirs = FALSE)
radia.mosaic.INTS.Tneg02C.nc.listfiles <- list.files(path = radia.mosaic.INTS.Tneg02C.nc.dir ,   include.dirs = FALSE)
radia.mosaic.INTS.Tpos02C.nc.listfiles <- list.files(path = radia.mosaic.INTS.Tpos02C.nc.dir ,   include.dirs = FALSE)
radia.mosaic.INTS.Tneg12C.nc.filename  <- radia.mosaic.INTS.Tneg12C.nc.listfiles[1]
radia.mosaic.INTS.Tneg02C.nc.filename  <- radia.mosaic.INTS.Tneg02C.nc.listfiles[1]
radia.mosaic.INTS.Tpos02C.nc.filename  <- radia.mosaic.INTS.Tpos02C.nc.listfiles[1]
print(paste(radia.mosaic.INTS.Tneg12C.nc.dir, radia.mosaic.INTS.Tneg12C.nc.filename, sep=""))

# define output matchup data file path
output.matchup.data.dir              <- file.path(paste(base.path.dir, field.campaign, "/data/ICICLE.7_output/ver", latest.mosaic.ver.num, "/", sep=""))

#---------------------------------------------
# Start writing debugging information to an output file
#---------------------------------------------
# Print some pre-processing info
print("---------------------------------------")
print(paste("Available ", field.campaign, " flight files..."))
print(paste("  CNV 30-s mean files are:", sep=""))
print(paste("   ", CNV.30s.mean.listfiles, sep=""))
print(paste("  CNV NAW zenith/nadir/side files are:", sep=""))
print(paste("   ", CNV.NAW.zennad.nc.listfiles, sep=""))
print(paste("  CNV NAX files are:", sep=""))
print(paste("   ", CNV.NAX.horiz.nc.listfiles, sep=""))
print(paste("  CNV lidar files are:", sep=""))
print(paste("   ", CNV.LIDAR.nc.listfiles, sep="")) 
print("---------------------------------------")
print(paste("Processing parameters, as defined by user..."), sep="")
print(paste("  Field Campaign: ", field.campaign, sep=""))
#print(paste("  Case: ", yyyymmdd, ", F#", flight.num, ", ", hh.min.for.plotting, ":", mm.min.for.plotting, " to ", hh.max.for.plotting, ":", mm.max.for.plotting, " UTC", sep=""))
print(paste("  Flag values:"), sep="")
print(paste("    Output matched data = ",  output.matchup.data.flag,  sep=""))
print(paste("    Output matched image = ", output.matchup.image.flag, sep=""))
print("---------------------------------------")

#----------------------------------------------
# load ICICLE Campaign-related overview datasets
#----------------------------------------------
print(paste("Manage ICICLE Campaign-related overview datasets...", sep=""))

# load the NEXRAD site location text file
print(paste("  Loading NEXRAD site file from: ", nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep=""))
NEXRAD.site.df                       <- read.csv(paste(nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep = ""), header = FALSE, sep = ",", dec = ".", stringsAsFactors=FALSE)
colnames(NEXRAD.site.df)             <- c("NCDCID", "ICAO", "WBAN", "radname", "COUNTRY", "STATE", "COUNTY", "lat", "lon", "elev", "GMTdiff", "STN_TYPE")
#head(NEXRAD.site.df)

#-----------------------------------------------
# load RAP temp prof txt data files 
#-----------------------------------------------
print(paste("    Loading RAP temp prof txt data files...", sep=""))
RAP.temp.KDVN.02Z.df                 <- read.csv(paste(RAP.temp.txt.dir, RAP.temp.txt.listfiles[6], sep = ""), header = TRUE, sep = " ", dec = ".", stringsAsFactors=FALSE)
RAP.temp.KDMX.02Z.df                 <- read.csv(paste(RAP.temp.txt.dir, RAP.temp.txt.listfiles[5], sep = ""), header = TRUE, sep = " ", dec = ".", stringsAsFactors=FALSE)
colnames(RAP.temp.KDVN.02Z.df)       <- c(" ", "alt.msl.m", "pres.hpa", "temp.c")
colnames(RAP.temp.KDMX.02Z.df)       <- c(" ", "alt.msl.m", "pres.hpa", "temp.c")
head(RAP.temp.KDVN.02Z.df)

#-----------------------------------------------
# load RadIA-mosaic nc format data files that is closest in time
#-----------------------------------------------
print(paste("    Searching for representative RadIA-mosaic data files...", sep=""))
      
#-------------------------------------------------------------
# load RadIA-mosaic version 2 files
#------------------------------------------------------------
# mosaic file containing FZDZ, SLW, PLATES, and MIXPHA INTs
print(paste("      Loading: ", radia.mosaic.INTS.nc.filename, sep=""))
radia.mosaic.v2.INTS.nc.filename <- paste(radia.mosaic.INTS.nc.dir, radia.mosaic.INTS.nc.filename, sep="")
radia.mosaic.v2.INTS.nc          <- nc_open(radia.mosaic.v2.INTS.nc.filename, write = FALSE, verbose = FALSE)
print(paste("        The file has", radia.mosaic.v2.INTS.nc$nvars, "variables"))
radia.mosaic.v2.INTS.var.num     <- seq(1, radia.mosaic.v2.INTS.nc$nvars, by=1)
for (s in 1:length(radia.mosaic.v2.INTS.var.num)) {
  radia.mosaic.v2.INTS.nam <- paste("v", radia.mosaic.v2.INTS.var.num[s], sep = "")
  assign(radia.mosaic.v2.INTS.nam, radia.mosaic.v2.INTS.nc$var[[radia.mosaic.v2.INTS.var.num[s]]])
}  # end of for ()...
if (radia.mosaic.v2.INTS.nc$nvars >= 14) {
  lat0.INT                              <- ncvar_get( radia.mosaic.v2.INTS.nc, v5  )
  lon0.INT                              <- ncvar_get( radia.mosaic.v2.INTS.nc, v6  )         
  TEMP.CELSIUS                          <- ncvar_get( radia.mosaic.v2.INTS.nc, v9  )
  FZDZ.INT                              <- ncvar_get( radia.mosaic.v2.INTS.nc, v10 )
  SLW.INT                               <- ncvar_get( radia.mosaic.v2.INTS.nc, v11 )
  MIXPHA.INT                            <- ncvar_get( radia.mosaic.v2.INTS.nc, v12 )
  PLATES.INT                            <- ncvar_get( radia.mosaic.v2.INTS.nc, v13 )
  RADIA2.VAL                            <- ncvar_get( radia.mosaic.v2.INTS.nc, v14 )
} else if (radia.mosaic.v2.INTS.nc$nvars == 4) {
} # end of if (radia.mosaic.v2.INTS.nc$nvars >= 14) ...
#print(paste("V1 has name", v1$name))
nc_close(radia.mosaic.v2.INTS.nc)
#FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
#head(FZDZ.mosaic.df)
          
#-------------------------------------------------------------
# load RadIA-mosaic version 2 file, isothermal T=-12C
#------------------------------------------------------------
# mosaic file containing FZDZ, SLW, PLATES, and MIXPHA INTs
print(paste("      Loading: ", radia.mosaic.INTS.Tneg12C.nc.filename, sep=""))
radia.mosaic.v2.INTS.Tneg12C.nc.filename <- paste(radia.mosaic.INTS.Tneg12C.nc.dir, radia.mosaic.INTS.Tneg12C.nc.filename, sep="")
radia.mosaic.v2.INTS.Tneg12C.nc          <- nc_open(radia.mosaic.v2.INTS.Tneg12C.nc.filename, write = FALSE, verbose = FALSE)
print(paste("        The file has", radia.mosaic.v2.INTS.Tneg12C.nc$nvars, "variables"))
radia.mosaic.v2.INTS.Tneg12C.var.num     <- seq(1, radia.mosaic.v2.INTS.Tneg12C.nc$nvars, by=1)
for (s in 1:length(radia.mosaic.v2.INTS.Tneg12C.var.num)) {
  radia.mosaic.v2.INTS.Tneg12C.nam <- paste("v", radia.mosaic.v2.INTS.Tneg12C.var.num[s], sep = "")
  assign(radia.mosaic.v2.INTS.Tneg12C.nam, radia.mosaic.v2.INTS.Tneg12C.nc$var[[radia.mosaic.v2.INTS.Tneg12C.var.num[s]]])
}  # end of for ()...
if (radia.mosaic.v2.INTS.Tneg12C.nc$nvars >= 14) {
  lat0.INT.Tneg12C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v5  )
  lon0.INT.Tneg12C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v6  )         
  TEMP.CELSIUS.Tneg12C                          <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v9  )
  FZDZ.INT.Tneg12C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v10 )
  SLW.INT.Tneg12C                               <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v11 )
  MIXPHA.INT.Tneg12C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v12 )
  PLATES.INT.Tneg12C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v13 )
  RADIA2.VAL.Tneg12C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg12C.nc, v14 )
} else if (radia.mosaic.v2.INTS.Tneg12C.nc$nvars == 4) {
} # end of if (radia.mosaic.v2.INTS.nc$nvars >= 14) ...
#print(paste("V1 has name", v1$name))
nc_close(radia.mosaic.v2.INTS.Tneg12C.nc)
#FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
#head(FZDZ.mosaic.df)

#-------------------------------------------------------------
# load RadIA-mosaic version 2 file, isothermal T=-02C
#------------------------------------------------------------
# mosaic file containing FZDZ, SLW, PLATES, and MIXPHA INTs
print(paste("      Loading: ", radia.mosaic.INTS.Tneg02C.nc.filename, sep=""))
radia.mosaic.v2.INTS.Tneg02C.nc.filename <- paste(radia.mosaic.INTS.Tneg02C.nc.dir, radia.mosaic.INTS.Tneg02C.nc.filename, sep="")
radia.mosaic.v2.INTS.Tneg02C.nc          <- nc_open(radia.mosaic.v2.INTS.Tneg02C.nc.filename, write = FALSE, verbose = FALSE)
print(paste("        The file has", radia.mosaic.v2.INTS.Tneg02C.nc$nvars, "variables"))
radia.mosaic.v2.INTS.Tneg02C.var.num     <- seq(1, radia.mosaic.v2.INTS.Tneg02C.nc$nvars, by=1)
for (s in 1:length(radia.mosaic.v2.INTS.Tneg02C.var.num)) {
  radia.mosaic.v2.INTS.Tneg02C.nam <- paste("v", radia.mosaic.v2.INTS.Tneg02C.var.num[s], sep = "")
  assign(radia.mosaic.v2.INTS.Tneg02C.nam, radia.mosaic.v2.INTS.Tneg02C.nc$var[[radia.mosaic.v2.INTS.Tneg02C.var.num[s]]])
}  # end of for ()...
if (radia.mosaic.v2.INTS.Tneg02C.nc$nvars >= 14) {
  lat0.INT.Tneg02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v5  )
  lon0.INT.Tneg02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v6  )         
  TEMP.CELSIUS.Tneg02C                          <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v9  )
  FZDZ.INT.Tneg02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v10 )
  SLW.INT.Tneg02C                               <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v11 )
  MIXPHA.INT.Tneg02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v12 )
  PLATES.INT.Tneg02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v13 )
  RADIA2.VAL.Tneg02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tneg02C.nc, v14 )
} else if (radia.mosaic.v2.INTS.Tneg02C.nc$nvars == 4) {
} # end of if (radia.mosaic.v2.INTS.nc$nvars >= 14) ...
#print(paste("V1 has name", v1$name))
nc_close(radia.mosaic.v2.INTS.Tneg02C.nc)
#FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
#head(FZDZ.mosaic.df)

#-------------------------------------------------------------
# load RadIA-mosaic version 2 file, isothermal T=+02C
#------------------------------------------------------------
# mosaic file containing FZDZ, SLW, PLATES, and MIXPHA INTs
print(paste("      Loading: ", radia.mosaic.INTS.Tpos02C.nc.filename, sep=""))
radia.mosaic.v2.INTS.Tpos02C.nc.filename <- paste(radia.mosaic.INTS.Tpos02C.nc.dir, radia.mosaic.INTS.Tpos02C.nc.filename, sep="")
radia.mosaic.v2.INTS.Tpos02C.nc          <- nc_open(radia.mosaic.v2.INTS.Tpos02C.nc.filename, write = FALSE, verbose = FALSE)
print(paste("        The file has", radia.mosaic.v2.INTS.Tpos02C.nc$nvars, "variables"))
radia.mosaic.v2.INTS.Tpos02C.var.num     <- seq(1, radia.mosaic.v2.INTS.Tpos02C.nc$nvars, by=1)
for (s in 1:length(radia.mosaic.v2.INTS.Tpos02C.var.num)) {
  radia.mosaic.v2.INTS.Tpos02C.nam <- paste("v", radia.mosaic.v2.INTS.Tpos02C.var.num[s], sep = "")
  assign(radia.mosaic.v2.INTS.Tpos02C.nam, radia.mosaic.v2.INTS.Tpos02C.nc$var[[radia.mosaic.v2.INTS.Tpos02C.var.num[s]]])
}  # end of for ()...
if (radia.mosaic.v2.INTS.Tpos02C.nc$nvars >= 14) {
  lat0.INT.Tpos02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v5  )
  lon0.INT.Tpos02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v6  )         
  TEMP.CELSIUS.Tpos02C                          <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v9  )
  FZDZ.INT.Tpos02C                              <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v10 )
  SLW.INT.Tpos02C                               <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v11 )
  MIXPHA.INT.Tpos02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v12 )
  PLATES.INT.Tpos02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v13 )
  RADIA2.VAL.Tpos02C                            <- ncvar_get( radia.mosaic.v2.INTS.Tpos02C.nc, v14 )
} else if (radia.mosaic.v2.INTS.Tpos02C.nc$nvars == 4) {
} # end of if (radia.mosaic.v2.INTS.nc$nvars >= 14) ...
#print(paste("V1 has name", v1$name))
nc_close(radia.mosaic.v2.INTS.Tpos02C.nc)
#FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
#head(FZDZ.mosaic.df)

##################################################################
# LEFT OFF HERE
#################################################################

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

#------------------------------------------------        
#
# Manipulate data
#
#------------------------------------------------
# NEXRAD site information
print(paste("Loading location info for NEXRAD = ", NEXRAD.site.name, sep=""))
ind.NEXRAD.site.name          <- which(NEXRAD.site.df$ICAO == NEXRAD.site.name)
NEXRAD.site.lat.deg           <- NEXRAD.site.df$lat[ind.NEXRAD.site.name]
NEXRAD.site.lon.deg           <- NEXRAD.site.df$lon[ind.NEXRAD.site.name]
print(paste("  NEXRAD = ", NEXRAD.site.name, " at lat = ", NEXRAD.site.lat.deg, ", lon = ", NEXRAD.site.lon.deg,  sep=""))

# Find closest lat and lon in array
#   make vector from matrix
lon0.INT.array                <- as.vector(lon0.INT)
lat0.INT.array                <- as.vector(lat0.INT)
#   make lon/lat df
lon0.lat0.INT.df              <- as.data.frame(cbind(lon0.INT.array, lat0.INT.array))
#   process through nearest neighbor function
nearest                       <- nn2(lon0.lat0.INT.df, t(replicate(dim(lon0.lat0.INT.df)[1], c(NEXRAD.site.lon.deg , NEXRAD.site.lat.deg ))), searchtype="radius", radius=0.0245)
max(nearest$nn.idx)
#   find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
ind.closest.mosaic.lat.pixel  <- max(nearest$nn.idx)%/%dim(lon0.INT)[1] + 1 # the next column (+1) after the whole number divisor
ind.closest.mosaic.lon.pixel  <- max(nearest$nn.idx)%%dim(lon0.INT)[1]
#ind.closest.mosaic.pres.pixel <- which.min(abs(CNV.30s.mean.df$pres.hpa[j] - abs(HRRR.pres.mb)))

#----------------------------------------------
#
# plotting
#
#----------------------------------------------
print("-----------")
print(paste("  Plotting image(s) to GUI", sep=""))
  
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
image.plot(lon0.INT, lat0.INT, FZDZ.INT[           , , 10], xlim=c(-92, -90), ylim=c(40, 44))
text(NEXRAD.site.lon.deg, NEXRAD.site.lat.deg, "*", cex=2)
grid()

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
  


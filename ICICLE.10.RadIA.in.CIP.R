#---------------------------------------
#
# ICICILE.10.RadIA.in.CIP.R
#
#---------------------------------------

#---------------------------------------
# add libraries
#---------------------------------------
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

# data input format
library(ncdf4)

# unit conversion
library(NISTunits)

# color pallettes
library(RColorBrewer)

#----------------------------------------------
# define x & y coordinates
#----------------------------------------------
HRRR.pres.mb                         <- seq(from = -1000, to = -25, by = 25)
x.coord                              <- seq(from =  -120, to = 120, by = 3)

#----------------------------------------------
# define matrices
#----------------------------------------------
sample.xsect                         <- array(data = -1L, dim = c(81, 40))

#----------------------------------------------
# define data paths, list data files in path
#----------------------------------------------
# define base path dir
base.path.dir                        <- file.path("/d1/serke/projects/case_studies/")

# define NEXRAD site location csv file path
nexrad.site.dataframetxt.dir         <- file.path(paste(base.path.dir, "SNOWIE_2017/data/RadIA_data/nexrad_site_data/", sep=""))
#----------------------------------------------
# load input data files
#----------------------------------------------
#   load the NEXRAD site location text file
print(paste("  Loading NEXRAD site file from: ", nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep=""))
NEXRAD.site.df                       <- read.csv(paste(nexrad.site.dataframetxt.dir, "nexrad_site.csv", sep = ""), header = FALSE, sep = ",", dec = ".", stringsAsFactors=FALSE)
colnames(NEXRAD.site.df)             <- c("NCDCID", "ICAO", "WBAN", "radname", "COUNTRY", "STATE", "COUNTY", "lat", "lon", "elev", "GMTdiff", "STN_TYPE")
head(NEXRAD.site.df)

# mosaic file containing FZDZ meta-fields
print(paste("      Loading: /d1/serke/projects/case_studies/ICICLE_2019/data/nc_radia_MCCC/v2.2/fzdz_flds/2019-02-17/124000.mdv.nc", sep=""))
radia.mosaic.FZDZ.nc                <- nc_open("/d1/serke/projects/case_studies/ICICLE_2019/data/nc_radia_MCCC/v2.2/fzdz_flds/2019-02-17/124000.mdv.nc", write = FALSE, verbose = FALSE)
print(paste("      The file has", radia.mosaic.FZDZ.nc$nvars, "variables"))
radia.mosaic.FZDZ.var.num           <- seq(1, radia.mosaic.FZDZ.nc$nvars, by=1)
for (s in 1:length(radia.mosaic.FZDZ.var.num)) {
  radia.mosaic.FZDZ.nam <- paste("v", radia.mosaic.FZDZ.var.num[s], sep = "")
  assign(radia.mosaic.FZDZ.nam, radia.mosaic.FZDZ.nc$var[[radia.mosaic.FZDZ.var.num[s]]])
}  # end of for ()...
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
nc_close(radia.mosaic.FZDZ.nc)
#FZDZ.mosaic.df <- data.frame(FZDZ.mosaic[ , , ind.nssl.mosaic.alt])
#head(FZDZ.mosaic.df)

# file containing CIP SLD meta-fields
print(paste("      Loading: /d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/SLD/2019-02-17/ncfdata20190217_130800.nc", sep=""))
CIP.SLD.nc                <- nc_open("/d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/SLD/2019-02-17/ncfdata20190217_130800.nc", write = FALSE, verbose = FALSE)
print(paste("      The file has", CIP.SLD.nc$nvars, "variables"))
CIP.SLD.var.num           <- seq(1, CIP.SLD.nc$nvars, by=1)
for (s in 1:length(CIP.SLD.var.num)) {
  CIP.SLD.nam <- paste("v", CIP.SLD.var.num[s], sep = "")
  assign(CIP.SLD.nam, CIP.SLD.nc$var[[CIP.SLD.var.num[s]]])
}  # end of for ()...
lat0.CIP.SLD                             <- ncvar_get( CIP.SLD.nc, v3  )
lon0.CIP.SLD                             <- ncvar_get( CIP.SLD.nc, v4  )         
CIP.SLD                                  <- ncvar_get( CIP.SLD.nc, v6  )
nc_close(CIP.SLD.nc)

# file containing CIP ICE PROB meta-fields
print(paste("      Loading: /d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/ICE_PROB/2019-02-17/ncfdata20190217_130800.nc", sep=""))
CIP.IPROB.nc                <- nc_open("/d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/ICE_PROB/2019-02-17/ncfdata20190217_130800.nc", write = FALSE, verbose = FALSE)
print(paste("      The file has", CIP.IPROB.nc$nvars, "variables"))
CIP.IPROB.var.num           <- seq(1, CIP.IPROB.nc$nvars, by=1)
for (s in 1:length(CIP.IPROB.var.num)) {
  CIP.IPROB.nam <- paste("v", CIP.IPROB.var.num[s], sep = "")
  assign(CIP.IPROB.nam, CIP.IPROB.nc$var[[CIP.IPROB.var.num[s]]])
}  # end of for ()...
lat0.CIP.IPROB                             <- ncvar_get( CIP.IPROB.nc, v3  )
lon0.CIP.IPROB                             <- ncvar_get( CIP.IPROB.nc, v4  )         
CIP.IPROB                                  <- ncvar_get( CIP.IPROB.nc, v6  )
nc_close(CIP.IPROB.nc)

# file containing CIP ICE SEV meta-fields
print(paste("      Loading: /d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/ICE_SEV/2019-02-17/ncfdata20190217_130800.nc", sep=""))
CIP.ISEV.nc                <- nc_open("/d1/serke/projects/case_studies/ICICLE_2019/data/CIP/v1/ICE_SEV/2019-02-17/ncfdata20190217_130800.nc", write = FALSE, verbose = FALSE)
print(paste("      The file has", CIP.ISEV.nc$nvars, "variables"))
CIP.ISEV.var.num           <- seq(1, CIP.ISEV.nc$nvars, by=1)
for (s in 1:length(CIP.ISEV.var.num)) {
  CIP.ISEV.nam <- paste("v", CIP.ISEV.var.num[s], sep = "")
  assign(CIP.ISEV.nam, CIP.ISEV.nc$var[[CIP.ISEV.var.num[s]]])
}  # end of for ()...
lat0.CIP.ISEV                             <- ncvar_get( CIP.ISEV.nc, v3  )
lon0.CIP.ISEV                             <- ncvar_get( CIP.ISEV.nc, v4  )         
CIP.ISEV                                  <- ncvar_get( CIP.ISEV.nc, v6  )
nc_close(CIP.ISEV.nc)

#------------------------------------------------------------------
# manipulate fields from input data files
#------------------------------------------------------------------
# for primary radar
NEXRAD.pri.name                      <- paste(" KDVN", sep="")
ind.NEXRAD.pri.site                  <- which(NEXRAD.site.df$ICAO == NEXRAD.pri.name)
NEXRAD.pri.lat.deg                   <- NEXRAD.site.df$lat[ind.NEXRAD.pri.site]
NEXRAD.pri.lon.deg                   <- NEXRAD.site.df$lon[ind.NEXRAD.pri.site]
NEXRAD.pri.elev.m                    <- NEXRAD.site.df$elev[ind.NEXRAD.pri.site]
print(paste("    NEXRAD.pri.name     = ", NEXRAD.pri.name,     sep=""))
print(paste("    NEXRAD.pri.lat.deg  = ", NEXRAD.pri.lat.deg,  sep=""))
print(paste("    NEXRAD.pri.lon.deg  = ", NEXRAD.pri.lon.deg,  sep=""))
print(paste("    NEXRAD.pri.elev.m   = ", NEXRAD.pri.elev.m,   sep=""))

# for FZDZ
#   make vector from matrix
lon0.FZDZ.array                       <- as.vector(lon0.FZDZ)
lat0.FZDZ.array                       <- as.vector(lat0.FZDZ)
#   make lon/lat df
lon0.lat0.FZDZ.df                     <- as.data.frame(cbind(lon0.FZDZ.array, lat0.FZDZ.array))
#   process through nearest neighbor function
nearest.FZDZ                          <- nn2(lon0.lat0.FZDZ.df, t(replicate(dim(lon0.lat0.FZDZ.df)[1], c(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg))), searchtype="radius", radius=0.0245)
max(nearest.FZDZ$nn.idx)
#   find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
ind.closest.FZDZ.lat.pixel            <- max(nearest.FZDZ$nn.idx)%/%dim(lon0.FZDZ)[1] + 1 # the next column (+1) after the whole number divisor
ind.closest.FZDZ.lon.pixel            <- max(nearest.FZDZ$nn.idx)%%dim(lon0.FZDZ)[1]

int.full.FZDZ[int.full.FZDZ >= 1.00]  <- NaN

# for CIP.SLD

CIP.SLD[CIP.SLD <= 0]                 <- 0

#   make vector from matrix
lon0.CIP.SLD.array                    <- as.vector(lon0.CIP.SLD)
lat0.CIP.SLD.array                    <- as.vector(lat0.CIP.SLD)
#   make lon/lat df
lon0.lat0.CIP.SLD.df                  <- as.data.frame(cbind(lon0.CIP.SLD.array, lat0.CIP.SLD.array))
#   process through nearest neighbor function
nearest.CIP.SLD                       <- nn2(lon0.lat0.CIP.SLD.df, t(replicate(dim(lon0.lat0.CIP.SLD.df)[1], c(NEXRAD.pri.lon.deg, NEXRAD.pri.lat.deg))), searchtype="radius", radius=0.0245)
max(nearest.CIP.SLD$nn.idx)
#   find indices of closest RadIA-mosaic lat/lon to each AC lat/lon
ind.closest.CIP.SLD.lat.pixel         <- max(nearest.CIP.SLD$nn.idx)%/%dim(lon0.CIP.SLD)[1] + 1 # the next column (+1) after the whole number divisor
ind.closest.CIP.SLD.lon.pixel         <- max(nearest.CIP.SLD$nn.idx)%%dim(lon0.CIP.SLD)[1]

##----------------------------------------------
##calculate great circle distance along earth's surface between two lat/lon pts
##----------------------------------------------
##a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
##c = 2 ⋅ atan2( √a, √(1−a) )
##d = R ⋅ c
#a[j]                        <- sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg) - NISTdegTOradian(CNV.30s.mean.ver2.df$lat.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lat.deg)-NISTdegTOradian(CNV.30s.mean.ver2.df$lat.deg[j])) / 2) + (cos(NISTdegTOradian(NEXRAD.pri.lat.deg)) * cos(NISTdegTOradian(CNV.30s.mean.ver2.df$lat.deg[j])) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg) - NISTdegTOradian(CNV.30s.mean.ver2.df$lon.deg[j])) / 2) * sin(abs(NISTdegTOradian(NEXRAD.pri.lon.deg)-NISTdegTOradian(CNV.30s.mean.ver2.df$lon.deg[j])) / 2) )
#c[j]                        <- 2 * atan2(sqrt(a[j]), sqrt(1 - a[j]))
#dist.radartoac.km[j]        <- radius.earth.km * c[j]

# define CNV 30-s mean values from flight time
CNV.lon.1230to1255          <-      c(-89.728, -89.759, -89.789, -89.818, -89.848, -89.878, -89.908, -89.939,-89.97,-90.003,-90.036,-90.067,-90.096,-90.122,-90.146,-90.168,-90.189,-90.212,-90.234,-90.256,-90.279,-90.302,-90.328,-90.354,-90.381,-90.408,-90.435,-90.459,-90.481,-90.502,-90.52,-90.536,-90.547,-90.556,-90.561,-90.561,-90.553,-90.541,-90.516,-90.519,-90.535,-90.562,-90.591,-90.62,-90.65,-90.681,-90.712,-90.743,-90.772,-90.779,-90.786, -90.79)
CNV.lat.1230to1255          <-      c( 41.737,  41.739,  41.742,  41.747,  41.752,  41.755,  41.759,  41.762,41.765,41.768,41.77,41.768,41.763,41.754,41.744,41.731,41.717,41.703,41.689,41.674,41.659,41.645,41.634,41.625,41.618,41.608,41.599,41.586,41.569,41.551,41.533,41.513,41.494,41.473,41.452,41.43,41.408,41.388,41.389,41.41,41.431,41.447,41.463,41.476,41.49,41.503,41.516,41.528,41.521,41.501,41.48,41.47)
CNV.pres.1230to1255         <- -1 * c(726.3,   717,     707.7,   701.1,   697.3,   701.6,   708.7,   716.3, 726.2, 735.6, 734.3, 725.9, 714.2, 704.2, 694.9, 689.4, 682.7, 674.3, 671.3, 670.4, 668.3, 673.8, 683.9, 695.2, 708.6, 719.6, 728.6, 741.7, 754.6, 766, 779.2, 791.2, 803.1, 816.5, 829.4, 841.2, 855.4, 866.7, 881.4, 892.9, 904.2, 915.1, 925.7, 928.3, 927.6, 927.1, 928, 925.8, 927.2, 927.7, 927.5, 927)
CNV.NEV.LWC.1230to1255      <-      c(  0.10,   0.11,    0.25,    0.23,    0.21,    0.21,    0.2,     0.09,    0.09,  0.16,  0.08,  0.09,  0.17,  0.16,  0.22,  0.3,   0.36,  0.27, 0.01, 0, 0, 0, 0, 0, 0.12, 0.43, 0.29, 0.2, 0.18, 0.14, 0.14, 0.21, 0.25, 0.22, 0.2, 0.18, 0.15, 0.1, 0.06, 0.05, 0.05, 0.04, 0.05, 0.04, 0.04, 0.04, 0.05, 0.04, 0.03, 0.03, 0.04, 0.04)
CNV.NEV.LWC.1230to1255[CNV.NEV.LWC.1230to1255 == -999] <- NaN
CNV.RID.LWC.1230to1255      <-      c(  0.10,   0.10,    0.28,    0.29,    0.27,    0.26,    0.25,    0.15,    0.08,  0.15,  0.07,  0.07,  0.25,  0.19,  0.25,  0.37,  0.45,  0.41,  0.04, -999, -999, -999, -999, -999, 0.36, 0.38, 0.32, 0.18, 0.18, 0.12, 0.08, 0.18, 0.21, 0.16, 0.16, 0.16, 0.1, 0.07, 0.02, 0.03, 0.03, -999, 0.02, 0.01, 0.01, 0.01, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01)
CNV.RID.LWC.1230to1255[CNV.RID.LWC.1230to1255 == -999] <- NaN
CNV.RmN.LWC.1230to1255      <-      CNV.RID.LWC.1230to1255 - CNV.NEV.LWC.1230to1255
#CNV.RmN.LWC.1230to1255[CNV.RmN.LWC.1230to1255 < 0] <- 0
CNV.Dmax.1230to1255         <-      c(310,     280,     270,     320,     330,     320,     260,   230,   220,   260,  -999,  -999,  -999,   280,   230,   210,   190,    80,  -999,  -999,  -999,  -999,  -999,130,200,230,190,240,-999,-999,-999,-999,260,290,320,340,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999,-999)
CNV.Dmax.1230to1255[CNV.Dmax.1230to1255 < 0] <- NaN

#----------------------------------------------
# CIP.RadIA v2.0 fields
#----------------------------------------------
# SLD
CIP.RadIA.V2.SLD.INT                        <- array(data = NaN, dim = c(dim(int.full.FZDZ)[1], dim(int.full.FZDZ)[2], dim(int.full.FZDZ)[3]))
CIP.RadIA.V2.SLD.FILL.INT                   <- array(data = NaN, dim = c(dim(int.full.FZDZ)[1],                     1, dim(int.full.FZDZ)[3]))
#   1. is GE zero where CIP.SLD GE zero and RADIA.FZDZ GE zero

#   2. is EQ THRESH(RadIA.FZDZ) where exist(RadIA.FZDZ)
CIP.RadIA.V2.SLD.INT[int.full.FZDZ >= 0.68] <- 1.0
CIP.RadIA.V2.SLD.INT[int.full.FZDZ <  0.68] <- 0.5
CIP.RadIA.V2.SLD.INT[int.full.FZDZ <  0.60] <- 0.0
#   3. is expanded horizontally where NEXRAD has no return within 'cone-of-silence'
#start with radar lon, step up from sfc thru each HRRR level looking for NA, count left/right until first number expand inward with filled number
CIP.RadIA.V2.SLD.FILL.2D.INT                <- CIP.RadIA.V2.SLD.INT[ , ind.closest.FZDZ.lat.pixel, ]
ind.closest.NEXRAD.lon.pixel                <- which.min(abs(lon0.FZDZ[ , ind.closest.FZDZ.lat.pixel] - NEXRAD.pri.lon.deg))
# loop over every HRRR pres alt in profile
for (kkk in 1:dim(CIP.RadIA.V2.SLD.FILL.2D.INT)[2]) {
  if (is.nan(CIP.RadIA.V2.SLD.FILL.2D.INT[ind.closest.NEXRAD.lon.pixel, kkk])) {
    # loop to the east
    ind.nan.east <- 1
    ind.nan.west <- 1
    for (lll in 1:12) {
      # look east
      if (is.nan(CIP.RadIA.V2.SLD.FILL.2D.INT[ind.closest.NEXRAD.lon.pixel + lll, kkk])) {
        ind.nan.east <- c(ind.nan.east, lll)
      } else {
        
      }
      # look west
      if (is.nan(CIP.RadIA.V2.SLD.FILL.2D.INT[ind.closest.NEXRAD.lon.pixel - lll, kkk])) {
        
      }
    }
    
  } # end of if(is.nan()...)
}   # end of for (kkk ...)
#   4. is expanded horizontally where NEXRAD has no return and GOES has cloud-top texture polygons
#   5. is EQ CIP.SLD where exist(CIP.SLD) and where ~exist(RadIA.FZDZ)
  
# ICE PROB
CIP.RadIA.V2.ICE.PROB.INT                   <-
  
# ICE SEV
CIP.RADIA.V2.ICE.SEV.INT                    <-


#----------------------------------------------
# plotting
#----------------------------------------------
## who knows where this came from
#image.plot(x.coord, HRRR.pres.mb, sample.xsect, xlab="E-W dist from NEXRAD [km]", ylab="Pres [mb]")
#text(0, -980, "NEXRAD")
#text(0, -1000, "*", cex=2)
#grid()

#display.brewer.pal( n = 9, name = 'YlOrRd')
#brewer.pal(         n = 9, name = 'YlOrRd')

# plot CIP SLD INTs and RadIA FZDZ INTs
#par(mfrow = c(3, 1))
par(mfrow = c(2, 1))
par(mar   = c(4, 4, 2, 2))
image.plot(lon0.CIP.SLD[, ind.closest.CIP.SLD.lat.pixel], HRRR.pres.mb, CIP.SLD[, ind.closest.CIP.SLD.lat.pixel, ], xlim=c(-91.0, -88.6), ylim=c(-1000, -400), zlim=c(0, 1), xlab="", ylab="Pres [mb]")
title(expression("CIP-v1, large-drop INT, 2019/02/17, F17, 3-hr Fx valid 13:08 UTC"), col.main = "black", cex.main=1.5)
text(NEXRAD.pri.lon.deg,      -980, "KDVN", cex = 1.0, col = "black")
text(NEXRAD.pri.lon.deg,     -1000,    "*", cex = 2.0, col = "black")
grid()
image.plot(lon0.FZDZ[, ind.closest.FZDZ.lat.pixel-0], HRRR.pres.mb, int.full.FZDZ[, ind.closest.FZDZ.lat.pixel-0, ], xlim=c(-91.0, -88.6), ylim=c(-1000, -400), zlim=c(0, 1), xlab="Lon [deg]", ylab="Pres [mb]")
title(expression("RadIA-m-v2.2, large-drop INT vs CNV LWCs, 2019/02/17, F17, 12:40 UTC"), col.main = "black", cex.main=1.5) 
for (iii in 1:length(CNV.Dmax.1230to1255)) {
  # plot RID-Nev LWC color-scaled circles 
  if (is.nan(CNV.RmN.LWC.1230to1255[iii])) {
    lines(CNV.lon.1230to1255[iii]-0.00,    CNV.pres.1230to1255[iii], type="p", pch=19, col = "black", cex=1.8)
  } else {
    ind.CNV.RmN.col <- 5 - round(CNV.RmN.LWC.1230to1255[iii] / (0.30 / length(brewer.pal(n = 9, name = "RdBu"))))
    lines(CNV.lon.1230to1255[iii]-0.00,    CNV.pres.1230to1255[iii], type="p", pch=19, col = brewer.pal(n = 9, name = "RdBu")[ind.CNV.RmN.col], cex=1.8)
  }
  # plot RID LWC color-scaled circles
  if (is.nan(CNV.RID.LWC.1230to1255[iii])) {
    lines(CNV.lon.1230to1255[iii]-0.00,    CNV.pres.1230to1255[iii], type="p", pch=19, col = "black", cex=1.8)
  } else {
    ind.CNV.RID.col <- length(brewer.pal(n = 9, name = "RdBu")) - round(CNV.RID.LWC.1230to1255[iii] / (0.40 / length(brewer.pal(n = 9, name = "RdBu"))))
    lines(CNV.lon.1230to1255[iii]-0.00,    CNV.pres.1230to1255[iii]+28, type="p", pch=19, col = brewer.pal(n = 9, name = "RdBu")[ind.CNV.RID.col], cex=1.8)
  }
  # overplot black circles around each color-scaled circle
  lines(  CNV.lon.1230to1255[iii]-0.00,    CNV.pres.1230to1255[iii], type="p", pch=21, col = "black", cex=1.8)
  lines(  CNV.lon.1230to1255[iii]-0.00, CNV.pres.1230to1255[iii]+28, type="p", pch=21, col = "black", cex=1.8)
}
text( CNV.lon.1230to1255[1],       CNV.pres.1230to1255[1]+55,        "12:30", cex = 1.0)
text(CNV.lon.1230to1255[11],      CNV.pres.1230to1255[11]+55,        "12:35", cex = 1.0)
text(CNV.lon.1230to1255[21],      CNV.pres.1230to1255[21]+50,        "12:40", cex = 1.0)
text(CNV.lon.1230to1255[31]-0.08, CNV.pres.1230to1255[31]+40,        "12:45", cex = 1.0)
text(CNV.lon.1230to1255[41]-0.08, CNV.pres.1230to1255[41]+40,        "12:50", cex = 1.0)
text(CNV.lon.1230to1255[51]-0.05, CNV.pres.1230to1255[51]+50,        "12:55", cex = 1.0)
text(                     -90.88,                       -908, "LWC(RID-NEV)", cex = 0.7, col = "black")
text(                     -90.88,                       -935,     "LWC(RID)", cex = 0.7, col = "black")
text(    NEXRAD.pri.lon.deg-0.05,                     -980+3,         "KDVN", cex = 1.0, col = "black")
text(         NEXRAD.pri.lon.deg,                      -1000,            "*", cex = 2.0, col = "black")
grid()
image.plot(lon0.FZDZ[, ind.closest.FZDZ.lat.pixel], HRRR.pres.mb, CIP.RadIA.V2.SLD.INT[, ind.closest.FZDZ.lat.pixel-0, ], xlim=c(-91.0, -88.6), ylim=c(-1000, -400), zlim=c(0, 1), xlab="", ylab="Pres [mb]")
title(expression("CIP/RadIA-v2, large-drop INT, 2019/02/17, F17, 3-hr Fx valid 13:08 UTC"), col.main = "black", cex.main=1.5)
text(NEXRAD.pri.lon.deg,      -980, "KDVN", cex = 1.0, col = "white")
text(NEXRAD.pri.lon.deg,     -1000,    "*", cex = 2.0, col = "white")
abline(v=NEXRAD.pri.lon.deg, col="black", type="l")
grid()

# plot CIP SLD, IPROB, and ISEV INTs
par(mfrow = c(3, 1))
par(mar   = c(4, 4, 2, 2))
image.plot(lon0.CIP.SLD[, ind.closest.CIP.SLD.lat.pixel], HRRR.pres.mb, CIP.IPROB[, ind.closest.CIP.SLD.lat.pixel, ], xlim=c(-91.0, -89.7), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]")
title(expression("CIP-v1 Icing Prob INT, 2019/02/17, F17, 3-hr Fx valid 13:08 UTC"), col.main = "black", cex.main=1.5)
text(NEXRAD.pri.lon.deg,      -970, "KDVN", cex = 1.0, col = "white")
text(NEXRAD.pri.lon.deg,     -1000,    "*", cex = 2.0, col = "black")
grid()
image.plot(lon0.CIP.SLD[, ind.closest.CIP.SLD.lat.pixel], HRRR.pres.mb, CIP.ISEV[, ind.closest.CIP.SLD.lat.pixel, ], xlim=c(-91.0, -89.7), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]")
title(expression("CIP-v1 Icing Severity INT, 2019/02/17, F17, 3-hr Fx valid 13:08 UTC"), col.main = "black", cex.main=1.5)
text(NEXRAD.pri.lon.deg,      -970, "KDVN", cex = 1.0, col = "white")
text(NEXRAD.pri.lon.deg,     -1000,    "*", cex = 2.0, col = "black")
grid()
image.plot(lon0.CIP.SLD[, ind.closest.CIP.SLD.lat.pixel], HRRR.pres.mb, CIP.SLD[, ind.closest.CIP.SLD.lat.pixel, ], xlim=c(-91.0, -89.7), ylim=c(-1000, -300), zlim=c(0,1), xlab="Lon [deg]", ylab="Pres [mb]")
title(expression("CIP-v1 large-drop INT, 2019/02/17, F17, 3-hr Fx valid 13:08 UTC"), col.main = "black", cex.main=1.5)
text(NEXRAD.pri.lon.deg,      -970, "KDVN", cex = 1.0, col = "white")
text(NEXRAD.pri.lon.deg,     -1000,    "*", cex = 2.0, col = "black")
grid()


# plot colorbar for RID LWC 0.0-0.40
color.bar(colorRampPalette(rev(c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")))(100), 0.0, 0.40)

# plot colorbar for RID-NEV LWC -0.15 - 0.15
color.bar(colorRampPalette(rev(c("#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC")))(100), -0.15, 0.15)

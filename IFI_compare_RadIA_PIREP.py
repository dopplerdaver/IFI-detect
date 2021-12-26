
#-------------------------------------------
#
# FILENAME:	IFI_compare_RadIA_PIREP.py
# 		
# CREATED: 	12.15.2021 - dserke
#
# PURPOSE:	1) ingest matched RadIA and PIREPs csv file, 2) manipulate and plot the data
#
#-------------------------------------------

#-------------------------------------------
# IMPORT LIBRARIES
#-------------------------------------------
import pandas                  as pd
import geopandas               as gpd
import numpy                   as np
import csv

import wradlib as wrl

import matplotlib              as mpl
from   matplotlib              import pyplot as plt
from   matplotlib.colors       import ListedColormap
from   mpl_toolkits.axes_grid1 import make_axes_locatable

import warnings
warnings.filterwarnings('ignore')

#-------------------------------------------
# DEFINE INPUT PATHS
#-------------------------------------------
# ... define raduis (r) of earth in km
r_km               = 6378.1

ft_TO_m            = 0.3048

nbins              = 1832.0
range_res_m        =  250.0
bw_deg             =    1.0    # half power beam width (deg)
vol_deg            = [0.5, 1.5, 2.5, 3.5, 4.5]
lat_KBBX           =   39.4969580 
lon_KBBX           = -121.6316557
alt_KBBX_m         =  221.0 * ft_TO_m
sitecoords         = (lon_KBBX, lat_KBBX, alt_KBBX_m)

# ... define base path dir
base_path_dir      = '/d1/serke/projects/'

# ... paths to Rv3 INTs and PIRP csv data files
Rv3PIR_dir         = base_path_dir+'RADIA_FAA/data/RadIAv3PIREPs/'
# ... names of Rv3 INTs and PIRP csv data files
# ... NOTE: currently, these files just represent ICICLE F17
Rv3PIR_FZDZ_name   = 'exfout_MrmsPostProcessor_fzdz_interest.csv'
Rv3PIR_SSLW_name   = 'exfout_MrmsPostProcessor_slw_interest.csv'
Rv3PIR_PIRP_name   = 'exmatch_MrmsPostProcessor.csv'

# ... path to NEXRAD site location csv 
nexrad_sites_dir   = base_path_dir+'case_studies/SNOWIE_2017/data/RadIA_data/nexrad_site_data/'
nexrad_sites_name  = 'nexrad_site_whdr.csv'

#-------------------------------------------
# LOAD INPUT DATASETS
#-------------------------------------------
# ... radar data into radar object
Rv3PIR_FZDZ        = pd.read_csv(Rv3PIR_dir+Rv3PIR_FZDZ_name, header=0, index_col=0)
Rv3PIR_SSLW        = pd.read_csv(Rv3PIR_dir+Rv3PIR_SSLW_name, header=0, index_col=0)
Rv3PIR_PIRP        = pd.read_csv(Rv3PIR_dir+Rv3PIR_PIRP_name, header=0, index_col=0)

# ... radar site location dataset
nexrad_sites       = pd.read_csv(nexrad_sites_dir+nexrad_sites_name, header=0, index_col=1)

# ... low res countries dataset
countries          = gpd.read_file(gpd.datasets.get_path("naturalearth_lowres"))

#-------------------------------------------
# MANIPULATE INPUT DATA
#-------------------------------------------
# Data from full month Feb2019 ICICLE have a few missing RadIA matchups (rows)
# ... find missing integers in RadIA FZDZ/SSLW lists
def find_missing(input):
    return [x for x in range(input[0], input[-1]+1)
	    if x not in input]
missing_SSLW_inds = find_missing(Rv3PIR_SSLW.index)
missing_FZDZ_inds = find_missing(Rv3PIR_FZDZ.index)

# ... exclude the inds missing from FZDZ/SSLW dfs from the PIRP df
Rv3PIR_PIRP.drop(Rv3PIR_PIRP.index[[missing_SSLW_inds]], inplace=True)
# ... exclude ind 0 from the PIRP df
#Rv3PIR_PIRP.drop(Rv3PIR_PIRP.index[[0]], inplace=True)

Rv3PIR_FZDZ.index = Rv3PIR_FZDZ.index-1
Rv3PIR_SSLW.index = Rv3PIR_SSLW.index-1

# ... define function for distance between two lat/lon points
def haversine_distance(lat1, lon1, lat2, lon2):
   phi1         = np.radians(lat1)
   phi2         = np.radians(lat2)
   delta_phi    = np.radians(lat2 - lat1)
   delta_lambda = np.radians(lon2 - lon1)
   a            = np.sin(delta_phi / 2)**2 + np.cos(phi1) * np.cos(phi2) *   np.sin(delta_lambda / 2)**2
   res          = r_km * (2 * np.arctan2(np.sqrt(a), np.sqrt(1 - a)))
   return np.round(res, 2)

# ... calculate distance between Rv3PIR_PIRP lon/lat and nexrad_sites LAT_DEG/LON_DEG
nexrad_distfromPIRPmin_km = []
for index_PIRP, row_PIRP in enumerate(range(Rv3PIR_PIRP.shape[0])):
    dist_from_nexrads_km = []
    for index, row in enumerate(range(nexrad_sites.shape[0])):
        dist_from_nexrads_km.append(haversine_distance(Rv3PIR_PIRP[' lat'][index_PIRP], Rv3PIR_PIRP[' lon'][index_PIRP], nexrad_sites[' LAT_DEG'][index], nexrad_sites[' LON_DEG'][index]))
        #print(index, dist_from_nexrads_km[index])
    # ... add DistFromPIRP to sites df
    nexrad_sites['DistFromPIRP'] = dist_from_nexrads_km
    # ... find min dist of PIRP from all sites and save to list
    nexrad_distfromPIRPmin_km.append(nexrad_sites['DistFromPIRP'].min())
# ... concat closest nexrad site dist to PIRP to Rv3PIR_PIRP df
Rv3PIR_PIRP['Distfromnexrad_min_km'] = nexrad_distfromPIRPmin_km

# ... concatenate Rv3 algo INT outputs and PIRP input pandas dfs into one df
Rv3PIR_ALL         = pd.concat([Rv3PIR_FZDZ, Rv3PIR_SSLW, Rv3PIR_PIRP], axis=1)
#Rv3PIR_MAXint      = Rv3PIR_ALL[[' fzdz_interestmax', ' slw_interestmax']]

# ... create new Rv3/PIRP pandas df containing only Rv3 INT=NaN values
Rv3PIR_RNAN        = Rv3PIR_ALL.loc[ (Rv3PIR_ALL[' fzdz_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' slw_interestmax'].astype(np.float).isna()) ]

# ... create new Rv3/PIRP pandas df containing only (Rv3 INT=NaN & PIRP sev > 0) values
Rv3PIR_RNAN_Sg0    = Rv3PIR_ALL.loc[ (Rv3PIR_ALL[' fzdz_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' slw_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' iint1'] > 0) ]

# ... create new Rv3/PIRP pandas df containing only (Rv3 INT=NaN & PIRP sev > 0) values
Rv3PIR_RVAL_Sg0    = Rv3PIR_ALL.loc[ ~(Rv3PIR_ALL[' fzdz_interestmax'].astype(np.float).isna()) & ~(Rv3PIR_ALL[' slw_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' iint1'] > 0) ]

# ... indexing/filtering of dataframe values
PIRP_tot_num       = np.array(Rv3PIR_ALL[' iint1'])[np.array(Rv3PIR_ALL[' iint1'])].shape[0]
PIRP_pos_num       = np.array(Rv3PIR_ALL[' iint1'])[np.array(Rv3PIR_ALL[' iint1']) > 0.0].shape[0]
PIRP_neg_num       = np.array(Rv3PIR_ALL[' iint1'])[np.array(Rv3PIR_ALL[' iint1']) < 0.0].shape[0]
SSLW_pos_num       = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) >= 0.5]).shape[0]
SSLW_neg_num       = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) < 0.5]).shape[0]
SSLW_neg           = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) < 0.5])
FZDZ               = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) >= 0.0])
FZDZ_pos_num       = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) >= 0.5]).shape[0]
FZDZ_neg_num       = (np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) < 0.5]).shape[0]
FZDZ_neg           = [np.array(Rv3PIR_ALL[' iint1'])[(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) < 0.5]]

Rv3_pos_ind        = [(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) >= 0.5] or [(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) >= 0.5]
Rv3_neg_ind        = [(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) < 0.5] and [(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) < 0.5]
Rv3_pos_num        = sum(sum(Rv3_pos_ind))
Rv3_neg_num        = sum(sum(Rv3_neg_ind))

ranges              = np.arange(nbins) * range_res_m

#-------------------------------------------
# PLOTS
#-------------------------------------------
#def mscatter(x,y,ax=None, m=None, **kw):
#    import matplotlib.markers as mmarkers
#    if not ax: ax=plt.gca()
#    sc = ax.scatter(x,y,**kw)
#    if (m is not None) and (len(m)==len(x)):
#        paths = []
#        for marker in m:
#            if isinstance(marker, mmarkers.MarkerStyle):
#                marker_obj = marker
#            else:
#                marker_obj = mmarkers.MarkerStyle(marker)
#            path = marker_obj.get_path().transformed(
#                        marker_obj.get_transform())
#            paths.append(path)
#        sc.set_paths(paths)
#    return sc

cMap    = 'viridis'

# SCATTER PLOTS
# ... for R-v3 FZDZ/SSLW ints with PIREP sev color-coded points
fig, ax = plt.subplots(figsize = (15, 15))
m       = ['*','o','o','o','o','o','o','o','o','o']
#scatter = mscatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_ALL[' slw_interestmax']).astype(np.float), c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), s=75, m=m, ax=ax)
#plt.show()
ax.scatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_ALL[' slw_interestmax']).astype(np.float), c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
ax.grid(color='grey', linestyle='--', linewidth=1)
ax.set_title('RadIA-v3 INT(MAX) near PIREPs for ICICLE F17', fontsize=20)
ax.text(0.02, 0.61, 'N(P-TOT) = '+str(PIRP_tot_num),          c='black', fontsize = 20)
ax.text(0.02, 0.56, 'N(P-POS) = '+str(PIRP_pos_num),          c='green', fontsize = 20)
ax.text(0.07, 0.51, 'N(R-G50) = '+str(sum(sum(Rv3_pos_ind))), c='green', fontsize = 20)
ax.text(0.02, 0.46, 'N(P-NEG) = '+str(PIRP_neg_num),          c='red',   fontsize = 20)
ax.text(0.07, 0.41, 'N(R-L50) = '+str(sum(sum(Rv3_neg_ind))), c='red',   fontsize = 20)
ax.set_xlabel('FZDZ INT', fontsize = 16)
ax.set_ylabel('SSLW INT', fontsize = 16)
ax.plot([0.0, 0.5], [0.5, 0.5], 'r--', label='test')
ax.plot([0.5, 0.5], [0.5, 0.0], 'r--', label='test')
plt.xlim(0.0, 1.02)
plt.ylim(0.0, 1.02)
plt.show()

# ... for R-v3 SSLW ints vs PIREP sev 
m, b    = np. polyfit(np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float), np.array(Rv3PIR_RVAL_Sg0[' iint1']).astype(np.float), 1)
fig, ax = plt.subplots(figsize = (15, 15))
ax.scatter(np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float), np.array(Rv3PIR_RVAL_Sg0[' iint1']).astype(np.float), c='grey', cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
ax.plot(np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float), m*np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float) + b)
ax.grid(color='grey', linestyle='--', linewidth=1)
plt.show()

# ... for R-v3 FZDZ ints vs PIREP sev 
m, b    = np. polyfit(np.array(Rv3PIR_RVAL_Sg0[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_RVAL_Sg0[' iint1']).astype(np.float), 1)
fig, ax = plt.subplots(figsize = (15, 15))
ax.scatter(np.array(Rv3PIR_RVAL_Sg0[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_RVAL_Sg0[' iint1']).astype(np.float), c='grey', cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
ax.plot(np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float), m*np.array(Rv3PIR_RVAL_Sg0[' slw_interestmax']).astype(np.float) + b)
ax.grid(color='grey', linestyle='--', linewidth=1)
plt.show()

## SCATTER PLOTS
## ...for R-v3 FZDZ ints versus PIREP reporting height with PIREP sev color-coded points
#fig, ax = plt.subplots(figsize = (15, 15))
#ax.scatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), Rv3PIR_ALL[' flvl'], c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
#ax.set_xlabel('FZDZ INT', fontsize = 16)
#ax.set_ylabel('F-lvl [kft]', fontsize = 16)
#plt.grid(b=True, alpha=0.5)
#plt.show()

# 3 PLOTS: RANGE/ALT FOR 1) CLEAR AIR VCP VOLUME, 2) PIREP SEV WHEN Rv3=NaN, 3) PIREP SEV WHEN Rv3=VAL
wrl.vis.plot_scan_strategy(ranges, vol_deg, sitecoords, beamwidth=bw_deg, vert_res=1000., maxalt=15000., range_res=5000., maxrange=200000., units='km', cmap='viridis')
fig, ax = plt.subplots(figsize = (16, 8))
plt.scatter(np.array(Rv3PIR_RNAN_Sg0['Distfromnexrad_min_km']).astype(np.float), np.array(Rv3PIR_RNAN_Sg0[' flvl']).astype(np.float)*(ft_TO_m/10), c=np.array(Rv3PIR_RNAN_Sg0[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=5, s=75, marker='o')
plt.plot([10, 22, 42, 105, 153], [1.0, 2.0, 4.0, 10.0, 15.0])
plt.plot([20, 40, 60, 80, 120, 160, 193], [0.3, 0.5, 0.9, 1.1, 2.0, 3.0, 4.0])
plt.xlim(0.0, 200.0)
plt.ylim(0.0,  15.0)
plt.xlabel('Range [km]')
plt.ylabel('Altitude [km]')
plt.show()
fig, ax = plt.subplots(figsize = (16, 8))
plt.scatter(np.array(Rv3PIR_RVAL_Sg0['Distfromnexrad_min_km']).astype(np.float), np.array(Rv3PIR_RVAL_Sg0[' flvl']).astype(np.float)*(ft_TO_m/10), c=np.array(Rv3PIR_RVAL_Sg0[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=5, s=75, marker='o')
plt.plot([10, 22, 42, 105, 153], [1.0, 2.0, 4.0, 10.0, 15.0])
plt.plot([20, 40, 60, 80, 120, 160, 193], [0.3, 0.5, 0.9, 1.1, 2.0, 3.0, 4.0])
plt.xlim(0.0, 200.0)
plt.ylim(0.0,  15.0)
plt.xlabel('Range [km]')
plt.ylabel('Altitude [km]')
plt.show()

# MAPVIEW PLOTS OF PIREP LOCATIONS/SEVERITIES
#............................................
# ...for Rv3PIR_ALL df
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries['name'] == 'United States of America'].plot(color='lightgrey', ax=ax)
# ...... plot NEXRAD locs
nexrad_sites.plot(x=' LON_DEG', y=' LAT_DEG', marker='x', linestyle='', c='red', ax=ax)
# ......plot points
Rv3PIR_ALL.plot(x=' lon', y=' lat', kind='scatter', s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.title('PIREP location/severity during Feb of 2019', fontsize = 30)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.legend(['NEXRAD'], fontsize = 15, labelcolor='red')
plt.show()
#............................................
# ...for Rv3PIR_RNAN df
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ......plot points
Rv3PIR_RNAN.plot(x=" lon", y=" lat", kind="scatter", s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.title('PIREP location/severity when Rv3 INTs=NaN during Feb of 2019', fontsize = 30)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.legend(['NEXRAD'], fontsize = 15, labelcolor='red')
plt.show()
#............................................
# ...for Rv3PIR_RNAN_Sg0 df
# ... This case is both Rv3 INT values are NaN (RNAN) and PIRP sev > 0
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ...... plot NEXRAD locs
nexrad_sites.plot(x=' LON_DEG', y=' LAT_DEG', marker='x', linestyle='', c='red', ax=ax)
#plt.text(np.array(nexrad_sites[' LON_DEG']), np.array(nexrad_sites[' LAT_DEG'])+0.02, 't')
# ......plot points
Rv3PIR_RNAN_Sg0.plot(x=" lon", y=" lat", kind="scatter", s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.title('PIREP location/severity when (Rv3 INTs=NaN & SEV>0) during Feb of 2019', fontsize = 30)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.legend(['NEXRAD'], fontsize = 15, labelcolor='red')
plt.show()

# HISTOGRAMS OF PIRP HEIGHTS
#   for PIRP when Rv3 val is pos (left) and when Rv3 val is NaN (right)
# ...for Rv3PIR_RVAL_Sg0 df
fig = plt.subplots(figsize=(25, 18))
ax1 = plt.subplot(121)
Rv3PIR_RVAL_Sg0.hist(column=' flvl', bins=45, ax=ax1, orientation="horizontal")
plt.xlabel('N of PIREP', fontsize = 30)
plt.ylabel('Flight level [kft x 10]', fontsize = 30)
plt.xlim(0, 750)
plt.ylim(0, 350)
# ...for Rv3PIR_RNAN_Sg0 df
ax2 = plt.subplot(122)
Rv3PIR_RNAN_Sg0.hist(column=' flvl', bins=45, ax=ax2, orientation="horizontal")
plt.xlabel('N of PIREP', fontsize = 30)
plt.ylabel('Flight level [kft x 10]', fontsize = 30)
plt.xlim(0, 750)
plt.ylim(0, 350)


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

import matplotlib              as mpl
from   matplotlib              import pyplot as plt
from   matplotlib.colors       import ListedColormap
from   mpl_toolkits.axes_grid1 import make_axes_locatable

import warnings
warnings.filterwarnings('ignore')

#-------------------------------------------
# DEFINE INPUT PATHS
#-------------------------------------------
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
# ... concatenate Rv3 and PIRP input pandas dfs into one df
Rv3PIR_ALL         = pd.concat([Rv3PIR_FZDZ, Rv3PIR_SSLW, Rv3PIR_PIRP], axis=1)
#Rv3PIR_MAXint      = Rv3PIR_ALL[[' fzdz_interestmax', ' slw_interestmax']]

# ... create new Rv3/PIRP pandas df containing only Rv3 INT=NaN values
Rv3PIR_RNAN        = Rv3PIR_ALL.loc[ (Rv3PIR_ALL[' fzdz_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' slw_interestmax'].astype(np.float).isna()) ]

# ... create new Rv3/PIRP pandas df containing only (Rv3 INT=NaN & PIRP sev > 0) values
Rv3PIR_RNAN_Sg0    = Rv3PIR_ALL.loc[ (Rv3PIR_ALL[' fzdz_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' slw_interestmax'].astype(np.float).isna()) & (Rv3PIR_ALL[' iint1'] > 0) ]

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
fig, ax = plt.subplots(figsize = (15, 15))
m       = ['*','o','o','o','o','o','o','o','o','o']
#scatter = mscatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_ALL[' slw_interestmax']).astype(np.float), c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), s=75, m=m, ax=ax)
#plt.show()
# ... scatter plot of R-v3 FZDZ/SSLW ints with PIREP sev color-coded points
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
#divider = make_axes_locatable(ax)
#cax     = divider.append_axes("right", size="5%", pad=0.75)
#plt.colorbar(cax=cax)
plt.show()

# ... scatter plot of R-v3 FZDZ ints versus PIREP reporting height with PIREP sev color-coded points
fig, ax = plt.subplots(figsize = (15, 15))
ax.scatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), Rv3PIR_ALL[' flvl'], c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
ax.set_xlabel('FZDZ INT', fontsize = 16)
ax.set_ylabel('F-lvl [kft]', fontsize = 16)
plt.grid(b=True, alpha=0.5)
plt.show()

# mapview plot of PIREP locations
# ...for Rv3PIR_ALL df
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ......plot points
Rv3PIR_ALL.plot(x=" lon", y=" lat", kind="scatter", s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='All PIREP location/severity in CONUS during 2/17/2019', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.show()
# ...for Rv3PIR_RNAN df
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ......plot points
Rv3PIR_RNAN.plot(x=" lon", y=" lat", kind="scatter", s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='PIREP location/severity when Rv3 INTs=NaN in CONUS during 2/17/2019', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.show()
# ...for Rv3PIR_RNAN_Sg0 df
# ... This case is both Rv3 INT values are NaN (RNAN) and PIRP sev > 0
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ...... plot NEXRAD locs
nexrad_sites.plot(x=' LON_DEG', y=' LAT_DEG', marker='x', linestyle='', c='red', ax=ax)
# ......plot points
Rv3PIR_RNAN_Sg0.plot(x=" lon", y=" lat", kind="scatter", s=75, marker='o', 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='PIREP location/severity when (Rv3 INTs=NaN & SEV>0) in CONUS during 2/17/2019', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.legend(['NEXRAD'])
plt.show()

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
# ... paths to data files
Rv3PIR_dir         = '/d1/serke/projects/RADIA_FAA/data/RadIAv3PIREPs/'
# ... names of data files
# ... NOTE: currently, these files just represent ICICLE F17
Rv3PIR_FZDZ_name   = 'exfout_MrmsPostProcessor_fzdz_interest.csv'
Rv3PIR_SSLW_name   = 'exfout_MrmsPostProcessor_slw_interest.csv'
Rv3PIR_PIRP_name   = 'exmatch_MrmsPostProcessor.csv'

#-------------------------------------------
# LOAD INPUT FILES
#-------------------------------------------
# ... radar data into radar object
Rv3PIR_FZDZ        = pd.read_csv(Rv3PIR_dir+Rv3PIR_FZDZ_name, header=0, index_col=0)
Rv3PIR_SSLW        = pd.read_csv(Rv3PIR_dir+Rv3PIR_SSLW_name, header=0, index_col=0)
Rv3PIR_PIRP        = pd.read_csv(Rv3PIR_dir+Rv3PIR_PIRP_name, header=0, index_col=0)

countries          = gpd.read_file(gpd.datasets.get_path("naturalearth_lowres"))

#-------------------------------------------
# MANIPULATE INPUT DATA
#-------------------------------------------
# ... concatenate input pandas dfs
Rv3PIR_ALL         = pd.concat([Rv3PIR_FZDZ, Rv3PIR_SSLW, Rv3PIR_PIRP], axis=1)
#Rv3PIR_MAXint      = Rv3PIR_ALL[[' fzdz_interestmax', ' slw_interestmax']]

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
Rv3_neg_ind        = [(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float) < 0.5] or [(Rv3PIR_ALL[' slw_interestmax']).astype(np.float) < 0.5]
print(sum(sum(Rv3_pos_ind)))
print(sum(sum(Rv3_neg_ind)))

#-------------------------------------------
# PLOTS
#-------------------------------------------
cMap = 'viridis'
# ... scatter of R-v3 FZDZ/SSLW ints with PIREP sev color-coded points
fig, ax = plt.subplots(figsize = (15, 15))
#for i, ind in enumerate(np.array(Rv3PIR_ALL[' iint1']).astype(np.float)):
#    if np.array(Rv3PIR_ALL[' iint1']).astype(np.float) >= 1:
ax.scatter(np.array(Rv3PIR_ALL[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_ALL[' slw_interestmax']).astype(np.float), c=np.array(Rv3PIR_ALL[' iint1']).astype(np.float), cmap=cMap, vmin=-1, vmax=8, s=75, marker='o')
#    else:
#       ax.scatter(np.array(Rv3PIR_ALL[' fzdz_interestmax'][i]).astype(np.float), np.array(Rv3PIR_ALL[' slw_interestmax'][i]).astype(np.float), c=np.array(Rv3PIR_ALL[' iint1'][i]).astype(np.float), s=75, marker='x')
ax.grid(color='grey', linestyle='--', linewidth=1)
ax.set_title('RadIA-v3 INT(MAX) near PIREPs for ICICLE F17', fontsize=20)
ax.text(0.75, 0.60, 'N(P-TOT) = '+str(PIRP_tot_num), fontsize = 20)
ax.text(0.75, 0.55, 'N(P-POS) = '+str(PIRP_pos_num), fontsize = 20)
ax.text(0.80, 0.50, 'N(R-G50) = '+str(sum(sum(Rv3_pos_ind))), fontsize = 20)
ax.text(0.75, 0.45, 'N(P-NEG) = '+str(PIRP_neg_num), fontsize = 20)
ax.text(0.80, 0.40, 'N(R-L50) = '+str(sum(sum(Rv3_neg_ind))), fontsize = 20)
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

# ... mapview plot of PIREP locations
# ......initialize an axis
fig, ax = plt.subplots(figsize=(25,18))
# ......plot map on axis
countries[countries["name"] == "United States of America"].plot(color="lightgrey", ax=ax)
# ......plot points
Rv3PIR_ALL.plot(x=" lon", y=" lat", kind="scatter", 
                c=' iint1', colormap=cMap, vmin=-1, vmax=8, 
                title='PIREP location/severity in CONUS during 2/17/2019', 
                ax=ax)
plt.xlim(-125, -66)
plt.ylim(  25,  50)
plt.grid(b=True, alpha=0.5)
plt.xlabel('Lon [deg]', fontsize = 30)
plt.ylabel('Lat [deg]', fontsize = 30)
plt.show()


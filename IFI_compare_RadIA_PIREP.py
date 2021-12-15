
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
import pandas               as pd
import numpy                as np
import csv

import matplotlib           as mpl
from   matplotlib           import pyplot as plt
from   matplotlib.colors    import ListedColormap

import warnings
warnings.filterwarnings('ignore')

#-------------------------------------------
# DEFINE INPUT PATHS
#-------------------------------------------
# ... paths to data files
Rv3PIR_dir        = '/d1/serke/projects/RADIA_FAA/data/RadIAv3PIREPs/'
# ... names of data files
# ... NOTE: currently, these files just represent ICICLE F17
Rv3PIR_FZDZ_name  = 'exfout_MrmsPostProcessor_fzdz_interest.csv'
Rv3PIR_SSLW_name  = 'exfout_MrmsPostProcessor_slw_interest.csv'
Rv3PIR_PIRP_name  = 'exmatch_MrmsPostProcessor.csv'

#-------------------------------------------
# LOAD INPUT FILES
#-------------------------------------------
# ... radar data into radar object
Rv3PIR_FZDZ        = pd.read_csv(Rv3PIR_dir+Rv3PIR_FZDZ_name, header=0, index_col=0)
Rv3PIR_SSLW        = pd.read_csv(Rv3PIR_dir+Rv3PIR_SSLW_name, header=0, index_col=0)
Rv3PIR_PIRP        = pd.read_csv(Rv3PIR_dir+Rv3PIR_PIRP_name,           index_col=0)

#-------------------------------------------
# MANIPULATE INPUT DATA
#-------------------------------------------
# ... concatenate input pandas dfs
Rv3PIR_ALL         = pd.concat([Rv3PIR_FZDZ, Rv3PIR_SSLW, Rv3PIR_PIRP], axis=1)
Rv3PIR_MAXint      = Rv3PIR_ALL[[' fzdz_interestmax', ' slw_interestmax']]

#-------------------------------------------
# PLOTS
#-------------------------------------------
# ... scatter of Rv3 FZDZ/SSLW ints
fig, ax = plt.subplots(figsize = (15, 15))
ax.scatter(np.array(Rv3PIR_MAXint[' fzdz_interestmax']).astype(np.float), np.array(Rv3PIR_MAXint[' slw_interestmax']).astype(np.float))
ax.grid(color='grey', linestyle='--', linewidth=1)
ax.set_title('RadIA-v3 INT(MAX) near PIREPs for F17', fontsize=20)
ax.text(0.8, 0.6, 'N = ', fontsize = 30)
ax.set_xlabel('FZDZ INT', fontsize = 16)
ax.set_ylabel('SSLW INT', fontsize = 16)
#ax.xlim([0.0, 1.0])
#ax.ylim([0.0, 1.0])
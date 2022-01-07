
#-------------------------------------------
#
# FILENAME:	MRMS_CONUS_mosaic.py
# 		
# CREATED: 	01.06.2022 - dserke
#
# SOURCES:	https://github.com/nasa/MMM-Py
#
# TO DO:	Need to find path to dual-pol mosaics for ICICLE (ask Paul)
#
#-------------------------------------------

#-------------------------------------------
# IMPORT LIBRARIES
#-------------------------------------------
from __future__             import print_function
import numpy                as     np
import matplotlib.pyplot    as     plt
import datetime             as     dt
import pandas               as     pd
import glob
import cartopy.crs          as     ccrs
import cartopy.feature      as     cfeature
from   cartopy.io.img_tiles import StamenTerrain
import pygrib
import os
import pyart
%matplotlib inline

import mmmpy

#-------------------------------------------
# DEFINE CONSTANTS
#-------------------------------------------
# NOTE: UPDATE THESE FOR IFI PURPOSES
BASE_PATH   = '/Users/tjlang/Documents/Python/DataForTesting'
TMPDIR      = BASE_PATH+'/tmpdir/'
WGRIB2_PATH = BASE_PATH+'/MRMSupdates2015/MRMS_modified_wgrib2_v2.0.1/wgrib2/'
GRIB_PATH   = BASE_PATH+'/MRMSupdates2015/MRMS_GRIB2_SAMPLE-reflectivity_3d_plus/'
gf          = glob.glob(GRIB_PATH+'*.grib2.gz')

#-------------------------------------------
# DEFINE FUNCTIONS
#-------------------------------------------
def download_files(input_dt, max_seconds=300):
    """
    This function takes an input datetime object, and will try to match with the closest mosaics in time
    that are available at NCEP. Note that NCEP does not archive much beyond 24 hours of data.
    
    Parameters
    ----------
    input_dt : datetime.datetime object
        input datetime object, will try to find closest file in time on NCEP server
    
    Other Parameters
    ----------------
    max_seconds : int or float
        Maximum number of seconds difference tolerated between input and selected datetimes,
        before file matching will fail
    
    Returns
    -------
    files : 1-D ndarray of strings
        Array of mosaic file names, ready for ingest into MMM-Py
    """
    baseurl = 'http://mrms.ncep.noaa.gov/data/3DReflPlus/'
    page1 = pd.read_html(baseurl)
    directories = np.array(page1[0][0][3:-1])  # May need to change indices depending on pandas version
    urllist = []
    files = []
    for i, d in enumerate(directories):
        print(baseurl + d)
        page2 = pd.read_html(baseurl + d)
        filelist = np.array(page2[0][0][3:-1])  # May need to change indices depending on pandas version
        dts = []
        for filen in filelist:
            # Will need to change in event of a name change
            dts.append(dt.datetime.strptime(filen[32:47], '%Y%m%d-%H%M%S'))
        dts = np.array(dts)
        diff = np.abs((dts - input_dt))
        if np.min(diff).total_seconds() <= max_seconds:
            urllist.append(baseurl + d + filelist[np.argmin(diff)])
            files.append(filelist[np.argmin(diff)])
    for url in urllist:
        print(url)
        os.system('wget ' + url)
    return np.array(files)

#-------------------------------------------
# DEFINE INPUT FILES
#-------------------------------------------
files      = download_files(dt.datetime.utcnow())

#-------------------------------------------
# LOAD INPUT FILES
#-------------------------------------------
# Direct ingest of grib into MMM-Py
# ... from: https://github.com/nasa/MMM-Py/blob/master/notebooks/Direct_Grib_Read_Demo.ipynb
mosaic     = mmmpy.MosaicTile(files)

# Local ingest of grib2, and convert to nc format
# ... from: https://github.com/nasa/MMM-Py/blob/master/notebooks/Grib_Read_Demo.ipynb
tile       = mmmpy.MosaicTile(filename=gf, verbose=True, wgrib2_path=WGRIB2_PATH, nc_path=TMPDIR, wgrib2_name='wgrib2')
# read the intermediary netCDFs if they've already been created and kept.  You can also just pass MosaicTile a single file, so you just get a single vertical level.
ncfiles    = glob.glob(TMPDIR + '*.nc')

#-------------------------------------------
# EXPLORE INFO IN DATA
#-------------------------------------------
print(tile.Height)

print(ncfiles[0])
test       = mmmpy.MosaicTile(ncfiles[0])

#-------------------------------------------
# PLOTTING
#-------------------------------------------
mosaic.diag()  

# Plotting using cartopy
tiler      = StamenTerrain()
ext        = [-130, -65, 20, 50]
fig        = plt.figure(figsize=(12, 6))
projection = ccrs.PlateCarree()  # ShadedReliefESRI().crs
ax         = plt.axes(projection=projection)
ax.set_extent(ext)
ax.add_image(tiler, 3)
# Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
states_provinces = cfeature.NaturalEarthFeature(
    category='cultural',
    name='admin_1_states_provinces_lines',
    scale='50m',
    facecolor='none')
ax.add_feature(states_provinces, edgecolor='gray')
# Create a feature for Countries 0 regions at 1:10m from Natural Earth
countries = cfeature.NaturalEarthFeature(
    category='cultural',
    name='admin_0_boundary_lines_land',
    scale='50m',
    facecolor='none')
ax.add_feature(countries, edgecolor='k')
ax.coastlines(resolution='50m')
mosaic.get_comp()
valmask = np.ma.masked_where(mosaic.mrefl3d_comp <= 0, mosaic.mrefl3d_comp)
cs      = plt.pcolormesh(mosaic.Longitude, mosaic.Latitude, valmask, vmin=0, vmax=55,
                    cmap='pyart_Carbone42', transform=projection)
plt.colorbar(cs, label='Composite Reflectivity (dBZ)',
             orientation='horizontal', pad=0.05, shrink=0.75, fraction=0.05, aspect=30)
plt.title(dt.datetime.utcfromtimestamp(mosaic.Time).strftime('%m/%d/%Y %H:%M UTC'))

# all plotting methods consolidated to MosaicDisplay class
# ... plot from grib, full CONUS
display = mmmpy.MosaicDisplay(tile)
fig     = plt.figure(figsize=(8, 14))
ax1     = fig.add_subplot(211)
display.plot_horiz()
ax2     = fig.add_subplot(212)
display.plot_horiz(level=4)
display.plot_vert(lat=45, xrange=[-95, -85])
display.three_panel_plot(lat=42.9, lon=-89, latrange=[40,50], lonrange=[-92, -82], 
                         parallels=2, meridians=2)

# ... plot from grib, lat/lon subsection
tile    = mmmpy.MosaicTile(filename=gf, latrange=[35,50], lonrange=[-95,-80])
display = mmmpy.MosaicDisplay(tile)
display.plot_horiz()

# ... plot from nc, a single vertical level
dis = mmmpy.MosaicDisplay(test)
dis.plot_horiz(level=5, verbose=True)

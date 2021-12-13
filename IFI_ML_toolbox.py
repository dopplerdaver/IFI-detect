
#-------------------------------------------
#
# FILENAME:	IFI_ML_toolbox.py
# 		
# CREATED: 	11.27.2021 - dserke
#
# NOTE:		8 methods overview at: https://medium.com/@rinu.gour123/8-machine-learning-algorithms-in-python-you-must-learn-e9b79b361f49
#
#-------------------------------------------

#-------------------------------------------
# IMPORT LIBRARIES
#-------------------------------------------
import sklearn

# ... K-means, unsupervised ML, solves clustering problem, classifies data using a number of clusters
from sklearn.datasets       import make_blobs
from sklearn.cluster        import KMeans

# ... Linear Regression, for continuous data
from   sklearn.datasets     import load_iris
from   sklearn.datasets     import load_boston
from   sklearn.linear_model import LinearRegression
from   sklearn              import neighbors, datasets

import pandas               as pd
import numpy                as np
import csv

import matplotlib           as mpl
from   matplotlib           import pyplot as plt
from   matplotlib.colors    import ListedColormap

import warnings
warnings.filterwarnings('ignore')

#-------------------------------------------
# DEFINE CONSTANTS
#-------------------------------------------
# ... paths to data files
CNV_rad_match_path        = '/d1/serke/projects/case_studies/ICICLE_2019/data/ICICLE.7_output/ver2.5/full_flight_clo_pix/CNV_v4/'
CNV_rad_match_name        = 'ALLFLIGHTS_RadIA-m_v2.5_CNV_v4.csv'
# ... concatenate path and file names
CNV_rad_match_path_name   = CNV_rad_match_path + CNV_rad_match_name

#-------------------------------------------
# LOAD INPUT FILES
#-------------------------------------------
# ... sample iris data
#iris                      = load_iris()

# ... sample Boston housing data
#data                      = load_boston()

# ... radar data into radar object
#CNV_rad_match             = pd.read_csv(CNV_rad_match_path_name, header=0, index_col=0)
CNV_rad_match             = pd.read_csv(CNV_rad_match_path_name, header=0, index_col='truth.cat2')
CNV_rad_match             = CNV_rad_match.apply(pd.to_numeric, errors='coerce')
#CNV_rad_match_F17         = pd.read_csv(CNV_rad_match_path_name, header=0, index_col=' f#')

#-------------------------------------------
# K-MEANS METHOD
#-------------------------------------------
# ... example found at: https://towardsdatascience.com/machine-learning-algorithms-part-9-k-means-example-in-python-f2ad05ed5203
X, y                      = make_blobs(n_samples=300, centers=4, cluster_std=0.60, random_state=0)
plt.scatter(X[:,0], X[:,1])
X.shape
y.shape

# ... follow same process using CNV_rad_match dataframe fields 
# ......use only index 1 and 9
CNV_rad_match = CNV_rad_match.loc[[1,9], :]
# ......drop all remaining comuns that have NaNs
CNV_rad_match = CNV_rad_match.dropna()
# save as np.array
X_IFI_0   = np.array(CNV_rad_match.loc[[1,9], ' mean.DBZ.FZDZ.pix'])
# save as np.array
X_IFI_1   = np.array(CNV_rad_match.loc[[1,9], ' mean.ZDR.corr.SLW.pix'])
# stack columns to make 2D np.array
X_IFI     = np.column_stack((np.array(CNV_rad_match.loc[[1,9], ' mean.DBZ.FZDZ.pix']), np.array(CNV_rad_match.loc[[1,9], ' mean.ZDR.corr.SLW.pix'])))
# save truth.cat2 index column as np.array
y_IFI     = np.array(CNV_rad_match.loc[[1,9],].index)
y_IFI[y_IFI == 1] = 10

# ..... use elbow method to identify optimal number of clusters
wcss      = []
for i in range(1, 11):
    kmeans = KMeans(n_clusters=i, init='k-means++', max_iter=300, n_init=10, random_state=0)
    kmeans.fit(X_IFI)
    wcss.append(kmeans.inertia_)
plt.plot(range(1, 11), wcss)
plt.title('Elbow Method')
plt.xlabel('Number of clusters')
plt.ylabel('WCSS')
plt.show()
# ...... run KMeans using predetermined optimal number of clusters
kmeans     = KMeans(n_clusters=3, init='k-means++', max_iter=300, n_init=10, random_state=0)
pred_y     = kmeans.fit_predict(X_IFI)
plt.scatter(X_IFI[:,0], X_IFI[:,1], c=y_IFI)
plt.scatter(kmeans.cluster_centers_[:, 0], kmeans.cluster_centers_[:, 1], s=200, c='red')
plt.show()

#-------------------------------------------
# SUPPORT VECTOR MACHINES (SVM) METHOD
#-------------------------------------------

##############################################################################################

# ... for ICICLE data
ICICLE_target_names = np.asarray(['NONE', 'FZDZ-H', 'FZDZ-L', 'SLW-H', 'SLW-L', 'FZRA', 'MPHA-DZ', 'MPHA-S', 'MPHA-I', 'ICEONLY'])
formatter           = plt.FuncFormatter(lambda i, *args: ICICLE_target_names[int(i)])
cmap                = mpl.cm.viridis
bounds              = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
norm                = mpl.colors.BoundaryNorm(bounds, cmap.N, extend='both')
plt.figure(figsize=(15, 15))
plt.scatter(CNV_rad_match.loc[:, ' NEV.LWC.gm3'], CNV_rad_match.loc[:, ' dmax.85.per.L.um'], c=CNV_rad_match.index)
plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap), format=formatter)
plt.xlim([0, 1])
plt.ylim([0, 1000])
plt.xlabel(' NEV.LWC.gm3')
plt.ylabel(' dmax.85.per.L.um')
plt.tight_layout()
plt.show()

#fig, ax = plt.subplots()
#colors  = {'NONE':'white', 'FZDZ-H':'blue', 'FZDZ-L':'lightblue', 'SLW-H':'darkgreen', 'SLW-L':'green', 'FZRA':'darkblue', 'MPHA':'orange', 'MPHA':'orange', 'MPHA':'orange', 'ICEONLY':'grey'}
#grouped = CNV_rad_match.groupby('truth.cat2')
#for key, group in grouped:
#    group.plot(ax=ax, kind='scatter', x=' ZDR.MOMS.FZDZ.pix', y=' mean.DBZ.FZDZ.pix', label=key, color=colors[key])
#plt.show()

plt.figure(figsize=(10, 10))
plt.scatter(CNV_rad_match.loc[[5], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[5], ' mean.DBZ.FZDZ.pix'].astype(np.float), c='darkblue')
plt.scatter(CNV_rad_match.loc[[2], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[2], ' mean.DBZ.FZDZ.pix'].astype(np.float), c='lightblue')
plt.scatter(CNV_rad_match.loc[[1], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[1], ' mean.DBZ.FZDZ.pix'].astype(np.float), c='blue')
plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap), format=formatter)
plt.xlim([-2, 2])
plt.ylim([-30, 40])
plt.xlabel(' ZDR.MOMS.FZDZ.pix')
plt.ylabel(' mean.DBZ.FZDZ.pix')
plt.tight_layout()
plt.show()

plt.figure(figsize=(10, 10))
plt.scatter(CNV_rad_match.loc[[4], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[4], ' DBZ.MOMS.SLW.pix'].astype(np.float), c='lightgreen')
plt.scatter(CNV_rad_match.loc[[3], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[3], ' DBZ.MOMS.SLW.pix'].astype(np.float), c='green')
plt.scatter(CNV_rad_match.loc[[5], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[5], ' DBZ.MOMS.SLW.pix'].astype(np.float), c='darkblue')
plt.scatter(CNV_rad_match.loc[[2], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[2], ' DBZ.MOMS.SLW.pix'].astype(np.float), c='lightblue')
plt.scatter(CNV_rad_match.loc[[1], ' ZDR.MOMS.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[1], ' DBZ.MOMS.SLW.pix'].astype(np.float), c='blue')
plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap), format=formatter)
plt.xlim([-2, 2])
plt.ylim([-30, 40])
plt.xlabel(' ZDR.MOMS.FZDZ.pix')
plt.ylabel(' mean.DBZ.FZDZ.pix')
plt.tight_layout()
plt.show()

plt.figure(figsize=(10, 10))
plt.scatter(CNV_rad_match.loc[[4], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[4], ' int.full.SLW.pix'].astype(np.float), c='lightgreen')
plt.scatter(CNV_rad_match.loc[[3], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[3], ' int.full.SLW.pix'].astype(np.float), c='green')
plt.scatter(CNV_rad_match.loc[[5], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[5], ' int.full.SLW.pix'].astype(np.float), c='darkblue')
plt.scatter(CNV_rad_match.loc[[2], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[2], ' int.full.SLW.pix'].astype(np.float), c='lightblue')
plt.scatter(CNV_rad_match.loc[[1], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[1], ' int.full.SLW.pix'].astype(np.float), c='blue')
plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap), format=formatter)
plt.xlim([0, 1.1])
plt.ylim([0, 1.1])
plt.xlabel(' int.full.FZDZ.pix')
plt.ylabel(' int.full.SLW.pix')
plt.tight_layout()
plt.show()

plt.figure(figsize=(10, 10))
plt.scatter(CNV_rad_match.loc[[9], ' int.full.FZDZ.pix'].astype(np.float), CNV_rad_match.loc[[9], ' int.full.SLW.pix'].astype(np.float), c='grey')
plt.colorbar(mpl.cm.ScalarMappable(norm=norm, cmap=cmap), format=formatter)
plt.xlim([0, 1.1])
plt.ylim([0, 1.1])
plt.xlabel(' int.full.FZDZ.pix')
plt.ylabel(' int.full.SLW.pix')
plt.tight_layout()
plt.show()

# 
plt.figure(figsize=(10, 10))
plt.contour(CNV_rad_match_F17.loc[[17], 'truth.cat2'].values, CNV_rad_match_F17.loc[[17], ' int.full.FZDZ.pix'].values.astype(np.float), c='blue', marker='.')
plt.colorbar()
plt.xlim([0, 9])
plt.ylim([0, 1.1])
plt.xlabel(' truth.cat2')
plt.ylabel(' int.full.FZDZ.pix')
plt.tight_layout()
plt.show()

#RadIA_int_names     = np.asarray(['LOW: 0.0-0.3', 'MEDIUM: 0.3-0.5', 'HIGH: 0.5-1.0'])
#formatter           = plt.FuncFormatter(lambda i, *args: RadIA_int_names[int(i)])
#cmap                = mpl.cm.rainbow
#bounds              = [0.1, 0.4, 0.7]
#norm                = mpl.colors.BoundaryNorm(bounds, cmap.N, extend='both')

# mapview plots of RadIA-m-V2 .....
# ... int.full.FZDZ.pix
plt.figure(figsize=(10, 8))
plt.scatter(CNV_rad_match_F17.loc[[17], ' lon.d'].astype(np.float), CNV_rad_match_F17.loc[[17], ' lat.d'].astype(np.float), c='grey', marker='.', s=1)
plt.scatter(CNV_rad_match_F17.loc[[17], ' lon.d'].astype(np.float), CNV_rad_match_F17.loc[[17], ' lat.d'].astype(np.float), c=CNV_rad_match_F17.loc[[17], ' int.full.FZDZ.pix'].astype(np.float), marker='o', s=50)
plt.xlabel('Lon [deg]')
plt.ylabel('Lat [deg]')
plt.colorbar()
plt.tight_layout()
plt.show()

# mapview plots of RadIA-m-V2 .....
# ... int.full.SLW.pix
plt.figure(figsize=(10, 8))
plt.scatter(CNV_rad_match_F17.loc[[17], ' lon.d'].astype(np.float), CNV_rad_match_F17.loc[[17], ' lat.d'].astype(np.float), c='grey', marker='.', s=1)
plt.scatter(CNV_rad_match_F17.loc[[17], ' lon.d'].astype(np.float), CNV_rad_match_F17.loc[[17], ' lat.d'].astype(np.float), c=CNV_rad_match_F17.loc[[17], ' int.full.SLW.pix'].astype(np.float), marker='o', s=50)
plt.xlabel('Lon [deg]')
plt.ylabel('Lat [deg]')
plt.colorbar()
plt.tight_layout()
plt.show()

plt.figure(figsize=(20, 10))
plt.plot(CNV_rad_match_F17.loc[[17], 'truth.cat2'].values,           marker='.', linestyle='None', c='black')
#plt.plot(CNV_rad_match_F17.loc[[17], ' int.full.FZDZ.pix'].values*4, marker='.', c='blue')
plt.show()

#---------------------------------------
# SUPERVISED LEARNING: A. CLASSIFICATION AND B. REGRESSION
#   USING NEAREST NEIGHBOR PREDICTION
#---------------------------------------
# A. CLASSIFICATION
# ... Create color maps for 3-class classification problem, as with iris
cmap_light   = ListedColormap(['#FFAAAA', '#AAFFAA', '#AAAAFF'])
cmap_bold    = ListedColormap(['#FF0000', '#00FF00', '#0000FF'])
# ... supervised learning: classification and regression
iris         = datasets.load_iris()
X            = iris.data[:, :2]  # we only take the first two features. We could
                               # avoid this ugly slicing by using a two-dim dataset
y            = iris.target
x_min, x_max = X[:, 0].min() - .1, X[:, 0].max() + .1
y_min, y_max = X[:, 1].min() - .1, X[:, 1].max() + .1
xx, yy       = np.meshgrid(np.linspace(x_min, x_max, 100),
                           np.linspace(y_min, y_max, 100))
# ... calc for n=1
knn_n1       = neighbors.KNeighborsClassifier(n_neighbors=1)
knn_n1.fit(X, y)
Z_n1         = knn_n1.predict(np.c_[xx.ravel(), yy.ravel()])
Z_n1         = Z_n1.reshape(xx.shape)
# ... calc for n=3
knn_n3       = neighbors.KNeighborsClassifier(n_neighbors=3)
knn_n3.fit(X, y)
Z_n3         = knn_n3.predict(np.c_[xx.ravel(), yy.ravel()])
Z_n3         = Z_n3.reshape(xx.shape)

# ... plotting
plt.figure()
plt.pcolormesh(xx, yy, Z_n1, cmap=cmap_light)
# ... Plot also the training points
plt.scatter(X[:, 0], X[:, 1], c=y, cmap=cmap_bold)
plt.xlabel('sepal length (cm)')
plt.ylabel('sepal width (cm)')
plt.axis('tight')
plt.figure()
plt.pcolormesh(xx, yy, Z_n3, cmap=cmap_light)
# ... Plot also the training points
plt.scatter(X[:, 0], X[:, 1], c=y, cmap=cmap_bold)
plt.xlabel('sepal length (cm)')
plt.ylabel('sepal width (cm)')
plt.axis('tight')
# ... What kind of iris has 3cm x 5cm sepal and 4cm x 2cm petal?
print(iris.target_names[knn.predict([[3, 5, 4, 2]])])

# B. REGRESSION
# ... of iris data
# ...... x from 0 to 30
x     = 30 * np.random.random((20, 1))
# ...... y = a*x + b with noise
y     = 0.5 * x + 1.0 + np.random.normal(size=x.shape)
# ...... create a linear regression model
model = LinearRegression()
# ...... FIT TRAINING DATA.  for SL, accepts data (x) and labels (y). for UL, accept data (x) only
model.fit(x, y)  				
# ...... predict y from the data
x_new = np.linspace(0, 30, 100)
y_new = model.predict(x_new[:, np.newaxis])	# used for SL.  given trained model, predict label of x_new data, returns y_new
# also model.predict_proba() method returns probability that new obs has categorical label. label with highest prob returned by model.predict()
# for UL, model.transform() and model.fit_transform() are used
# ...... plot the results
plt.figure(figsize=(4, 3))
ax    = plt.axes()
ax.scatter(x, y)
ax.plot(x_new, y_new)
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.axis('tight')
plt.show()

# ... of Boston housing data
for index, feature_name in enumerate(data.feature_names):
    plt.figure(figsize=(4, 3))
    plt.scatter(data.data[:, index], data.target)
    plt.ylabel('Price', size=15)
    plt.xlabel(feature_name, size=15)
    plt.tight_layout()
    
####################################################################
############################  LEFT OFF HERE 
####################################################################

# ... of ICICLE data
rad_match_feature_names = np.asarray([' T.c', 
			              ' mean.DBZ.FZDZ.pix', ' mean.ZDR.corr.SLW.pix', ' RHO.MOMS.FZDZ.pix',
			              ' sdev.DBZ.FZDZ.pix'])
CNV_match_feature_names = np.asarray([' NEV.LWC.gm3', ' NEV.IWC.gm3', ' T.DBZ.FZDZ.pix', ' T.c', 
			       ' dmax.85.per.L.um', ' NAW.zennad.REFL.30s.mean.updown', ' NAW.zennad.VEL.30s.mean.updown'])
CNV_rad_match2          = CNV_rad_match.dropna()
for index, feature_name in enumerate(rad_match_feature_names):
    print(index)
    plt.figure(figsize=(10, 5))
    if feature_name == str(' T.c'):
#        plt.scatter(CNV_rad_match.loc[9, rad_match_feature_names[index]], CNV_rad_match.loc[9, rad_match_feature_names[index+1]], c='black')
        plt.scatter(CNV_rad_match2.loc[1, rad_match_feature_names[index]], CNV_rad_match2.loc[1, rad_match_feature_names[index+1]], c='red')
        plt.xlim(-40, 20)
    else:
#        plt.scatter(CNV_rad_match.loc[9, feature_name], CNV_rad_match.loc[9, rad_match_feature_names[index+1]], c='black')
        plt.scatter(CNV_rad_match.loc[1, feature_name], CNV_rad_match.loc[1, rad_match_feature_names[index+1]], c='red')
#    plt.scatter(CNV_rad_match.loc[:, CNV_rad_match_feature_names[index]], CNV_rad_match.loc[:, 'truth.cat2'])
    plt.ylabel(rad_match_feature_names[index+1], size=15)
    plt.xlabel(feature_name, size=15)
#    plt.xticks(np.arange(min(x), max(x), (max(x)-min(x))/2))
#    plt.yaxis.set_major_locator(MaxNLocator(5))
#    plt.locator_params(axis='y', nbins=6)
    plt.tight_layout()

####################################################################
############################  LEFT OFF HERE 
####################################################################







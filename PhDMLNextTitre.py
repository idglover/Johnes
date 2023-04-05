###IMPORT PACKAGES###

import pandas as pd
import numpy as np
import math
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression as alg_linreg, ElasticNet as alg_enet
from sklearn.neighbors import KNeighborsRegressor as alg_knn
from sklearn.ensemble import GradientBoostingRegressor as alg_gbm, RandomForestRegressor as alg_rf
from sklearn.svm import SVR as alg_svm
from sklearn.neural_network import MLPRegressor as alg_nnet

from sklearn.model_selection import cross_val_score, GroupKFold, RandomizedSearchCV, GridSearchCV
from sklearn.preprocessing import StandardScaler
from sklearn.inspection import permutation_importance

###READ DATA###

dat = pd.read_csv('y:/ian/johnesthresholds/johnesproper/data/data_brms_slct_wide_complete.csv')

###DEFINE Y###

y = dat['titre_cor']

###DEFINE X AND PREPROCESSING###

std_scaler = StandardScaler()

x_toscale = dat[['age','titre_corm1','titre_corm2','titre_corm3','titre_corm4','titre_corm5','monthsagom1','monthsagom5','meantitre_corm','sdtitre_corm','ratio_latesttwo']]

x_scaled = std_scaler.fit_transform(x_toscale.to_numpy())

x_scaled = pd.DataFrame(x_scaled, columns=['age','titre_corm1','titre_corm2','titre_corm3','titre_corm4','titre_corm5','monthsagom1','monthsagom5','meantitre_corm','sdtitre_corm','ratio_latesttwo'])

###ADD ANIMAL ID###
 
#x_scaled['calfeartag'] = dat['calfeartag']


###CROSS VALIDATED AND APPARENT ALGORITHM PERFORMANCE###

groups = dat['calfeartag']

cv = GroupKFold(n_splits = 10)

cv_rmse_linreg = cross_val_score(alg_linreg(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_enet = cross_val_score(alg_enet(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_knn = cross_val_score(alg_knn(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_svm = cross_val_score(alg_svm(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_rf = cross_val_score(alg_rf(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_gbm = cross_val_score(alg_gbm(), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()

cv_rmse_nnet = cross_val_score(alg_nnet(max_iter = 1000), x_scaled, y, scoring='neg_root_mean_squared_error', groups = groups, cv = cv).mean()


fit_linreg = alg_linreg().fit(x_scaled, y)
fit_enet = alg_enet().fit(x_scaled, y)
fit_knn = alg_knn().fit(x_scaled, y)
fit_svm = alg_svm().fit(x_scaled, y)
fit_rf = alg_rf().fit(x_scaled, y)
fit_gbm = alg_gbm().fit(x_scaled, y)
fit_nnet = alg_nnet().fit(x_scaled, y)

pred_linreg = fit_linreg.predict(x_scaled)
pred_enet = fit_enet.predict(x_scaled)
pred_knn = fit_knn.predict(x_scaled)
pred_svm = fit_svm.predict(x_scaled)
pred_rf = fit_rf.predict(x_scaled)
pred_gbm = fit_gbm.predict(x_scaled)
pred_nnet = fit_nnet.predict(x_scaled)

resid_linreg = y - pred_linreg
resid_enet = y - pred_enet
resid_knn = y - pred_knn
resid_svm = y - pred_svm
resid_rf = y - pred_rf
resid_gbm = y - pred_gbm
resid_nnet = y - pred_nnet

se_linreg = resid_linreg**2
se_enet = resid_enet**2
se_knn = resid_knn**2
se_svm = resid_svm**2
se_rf = resid_rf**2
se_gbm = resid_gbm**2
se_nnet = resid_nnet**2

mse_linreg = se_linreg.mean()
mse_enet = se_enet.mean()
mse_knn = se_knn.mean()
mse_svm = se_svm.mean()
mse_rf = se_rf.mean()
mse_gbm = se_gbm.mean()
mse_nnet = se_nnet.mean()

rmse_linreg = math.sqrt(mse_linreg)
rmse_enet = math.sqrt(mse_enet)
rmse_knn = math.sqrt(mse_knn)
rmse_svm = math.sqrt(mse_svm)
rmse_rf = math.sqrt(mse_rf)
rmse_gbm = math.sqrt(mse_gbm)
rmse_nnet = math.sqrt(mse_nnet)

cv_rmse_vec = [cv_rmse_linreg, cv_rmse_enet, cv_rmse_knn, cv_rmse_svm, cv_rmse_rf, cv_rmse_gbm, cv_rmse_nnet]
app_rmse_vec = [rmse_linreg, rmse_enet, rmse_knn, rmse_svm, rmse_rf, rmse_gbm, rmse_nnet]
cv_rslts = pd.DataFrame({'model': ['LinearRegression','ElasticNet','KNN','SVM', 'RF', 'GBM', 'NNET'], 'cv_rmse':cv_rmse_vec, 'app_rmse':app_rmse_vec})
cv_rslts

###PLOT ALGORITHM PERFORMANCE###


fig, ax1 = plt.subplots()

color = 'tab:red'
ax1.set_xlabel('Model')
ax1.set_ylabel('RMSE', color=color)
ax1.set_title('Blue = CV, Red = Apparent')
ax1.scatter(cv_rslts['model'], cv_rslts['cv_rmse'], color= 'blue')
ax1.scatter(cv_rslts['model'], -cv_rslts['app_rmse'], color = 'red')
ax1.tick_params(axis='y', labelcolor=color)
plt.show()

###TUNE MODELS###

####ENET####

fit_enet.get_params()

random_grid_enet = {'alpha': [0.1, 0.3, 0.5, 0.7, 1], 'l1_ratio': [0.1,0.3,0.5,0.7,0.9]}


gridsearch = GridSearchCV(estimator = alg_enet(), param_grid = random_grid_enet, cv = 3, verbose=2)

gridsearch.fit(x_scaled, y)

gridsearch.best_params_

####NNET####

fit_nnet.get_params()

random_grid_nnet = {'hidden_layer_sizes': [(50,50,50), (50,100,50), (100,)], 'activation': ['tanh', 'relu'], 'solver': ['sgd', 'adam'],'alpha': [0.0001, 0.001, 0.05],'learning_rate': ['constant','adaptive'],}


gridsearch = GridSearchCV(estimator = alg_nnet(max_iter = 1000), param_grid = random_grid_nnet, cv = 3, verbose=2)

gridsearch.fit(x_scaled, y)

gridsearch.best_params_


###FIT TUNED MODELS###

fit_enet_tuned = alg_enet(alpha = 0.1, l1_ratio = 0.9.fit(x_scaled, y))
fit_nnet_tuned = alg_nnet(, max_iter = 1000).fit(x_scaled, y)

###PREDICT ON TRAINING DATA###

dat['pred_enet'] = fit_enet_tuned.predict(x_scaled)
dat['pred_nnet'] = fit_nnet_tuned.predict(x_scaled)

###APPARENT PERFORMANCE OF TUNED ALGORITHMS###

pred_enet_tuned = fit_enet_tuned.predict(x_scaled)

pred_nnet_tuned = fit_nnet_tuned.predict(x_scaled)

resid_enet_tuned = y - pred_enet
resid_nnet_tuned = y - pred_nnet


se_enet_tuned = resid_enet_tuned**2
se_nnet_tuned = resid_nnet_tuned**2



mse_enet_tuned = se_enet_tuned.mean()
mse_nnet_tuned = se_nnet_tuned.mean()

rmse_enet_tuned = math.sqrt(mse_enet_tuned)
rmse_nnet_tuned = math.sqrt(mse_nnet_tuned)


app_rmse_vec_tuned = [rmse_enet_tuned, rmse_nnet_tuned]
rslts = pd.DataFrame({'model': ['ElasticNet', 'NNET'], 'app_rmse':app_rmse_vec_tuned})
rslts

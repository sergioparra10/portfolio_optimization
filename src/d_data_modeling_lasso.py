### Portfolio Optimization
### Data modeling Lasso
### Sergio Parra 
### sparrap@itam.mx
"""
Created on Tue Oct 25 11:49:45 2022

@author: sergioparra
"""
# References: 
    #https://colab.research.google.com/drive/1bZAwb6_X4UjdqDPqKMNJFxfBliN1HZXY#scrollTo=cNPfJcEMF45o

path="/Users/sergioparra/Documents/ITAM/Tesis/"
import os
os.chdir(path)

from pathlib import Path
import pandas as pd
import numpy as np

### Data


df_model = pd.read_csv("03_Output/data_lasso_final.csv").set_index('date')


# Filter train and test data
# Use the model only in test data 

### previous
# N = 1000
# P = 200
N = 354
P = 89
x_train = x[-(N+P):-P]
x_test = x[-P:]


from sklearn.linear_model import LinearRegression
df_model["label"] = 1 
reg = LinearRegression(fit_intercept=False).fit(df_model.drop(["label"],axis=1), df_model["label"])

df_coeff = pd.DataFrame(df_model.drop(["label"],axis=1).columns)
df_coeff.columns = ["TICKER"]
df_coeff["coeff"] = reg.coef_
df_coeff["weight"] = (df_coeff["coeff"].abs() / df_coeff["coeff"].abs().sum())*100

print("Normal Regression Weights")
df_coeff[df_coeff["weight"]>0.1].head(5)   ### Stocks where portfolio weights are more thn 0.1%

### Lasso 

from sklearn.linear_model import Lasso
df_model["label"] = 1 
reg = Lasso(fit_intercept=False,alpha=0.0005, positive=True).fit(df_model.drop(["label"],axis=1), df_model["label"])

df_coeff = pd.DataFrame(df_model.drop(["label"],axis=1).columns)
df_coeff.columns = ["TICKER"]
df_coeff["coeff"] = reg.coef_
df_coeff["weight"] = (df_coeff["coeff"].abs() / df_coeff["coeff"].abs().sum())*100


print("Lasso Regression Weights, alpha=0.0005")
df_coeff[df_coeff["weight"]>1].head(5) ### Stocks where portfolio weights are more thn 1%


df_coeff.to_csv("03_Output/df_coeff_lasso.csv", encoding='utf-8', index=True)







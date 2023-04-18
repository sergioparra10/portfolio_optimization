### Portfolio Optimization
### Data modeling Lasso, three month training
### Sergio Parra 
### sparrap@itam.mx

# References: 
    #https://colab.research.google.com/drive/1bZAwb6_X4UjdqDPqKMNJFxfBliN1HZXY#scrollTo=cNPfJcEMF45o

path="/Users/sergio.parra/Documents/ITAM/Tesis/"
import os
os.chdir(path)

from pathlib import Path
import pandas as pd
import numpy as np

### Data


df_model = pd.read_csv("03_Output/data_lasso_final.csv").set_index('date')


from sklearn.linear_model import Lasso
from sklearn.linear_model import LinearRegression

#years_lookback = 1 
reg_periods = df_model.shape[0]-(30)


reg_type="Lasso"  
#reg_type = "Normal"

print(reg_type)

# Change the way the model is trained to filter just one month
# Keep only one month and the next one in the data frame 
# convert date column to datetime format
df_model['date'] = pd.to_datetime(df_model.index)
df_model['month'] = df_model['date'].dt.strftime('%Y-%m')
months = df_model['date'].dt.strftime('%Y-%m').unique()# get unique months as YYYY-MM strings
# drop NaN values
#months = months[:-1]
months = months[2:48]


for n in range(len(months)-2):
    
    previous_two_month = months[n-2]
    previous_month = months[-1]
    current_month = months[n]
    
    df_reg = df_model.loc[df_model['month'].isin([previous_two_month,previous_month,current_month])].copy()
    
    remove_variables = ['date','month']
    df_reg=df_reg.drop(remove_variables, axis=1) # axis 1 means remove colums, axis 0 means remove rows
    #weight_date=weight_date.drop(remove_variables, axis=1)

    df_reg["label"] = 1 
    if reg_type == "Lasso":
        reg = Lasso(fit_intercept=False,alpha=0.0005, positive=True,random_state=22).fit(df_reg.drop(["label"],axis=1), df_reg["label"])
    elif reg_type == "Normal":
        reg = LinearRegression(fit_intercept=False).fit(df_model.drop(["label"],axis=1), df_model["label"])
    else:
        print("please set reg = to Lasso or Normal")
        break
    
    df_coeff = pd.DataFrame(df_reg.drop(["label"],axis=1).columns)
    df_coeff.columns = ["TICKER"]
    df_coeff["coeff"] = reg.coef_
    df_coeff["weight"] = (df_coeff["coeff"].abs() / df_coeff["coeff"].abs().sum())*100
    df_coeff["weight_date"] = df_coeff["weight"]/100

    df_coeff = df_coeff.set_index("TICKER")

    df_coeff = df_coeff[["weight_date"]].T
    df_coeff = df_coeff.replace(0., np.nan)
    df_coeff[["training_month"]] = current_month

    if n ==0:
        df_all = df_coeff
    else:
        df_all = pd.concat((df_all,df_coeff),axis=0)

df_all.head()


df_all.to_csv("03_Output/df_coeff_lasso_threemonth.csv", encoding='utf-8', index=True)





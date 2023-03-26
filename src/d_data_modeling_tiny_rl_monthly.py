### Portfolio Optimization
### Data modeling Tiny RL (Monthly basis)
### Sergio Parra 
### sparrap@itam.mx

# Reference: 
    # https://colab.research.google.com/drive/1LeFexDnQjwnIlJ-5LWWILySZOHzvmwfy#scrollTo=izohtmKZfz3Q
    # https://teddykoker.com/2019/06/trading-with-reinforcement-learning-in-python-part-ii-application/
    # https://teddykoker.com/2019/05/trading-with-reinforcement-learning-in-python-part-i-gradient-ascent/
    # http://cs229.stanford.edu/proj2006/Molina-StockTradingWithRecurrentReinforcementLearning.pdf
    

path="/Users/sergioparra/Documents/ITAM/Tesis/"
import os
os.chdir(path)

# Here we estimate a sharpe ratio from the return time series 
def sharpe_ratio(rets):
    return rets.mean() / rets.std()

import numpy as np
import pandas as pd
from datetime import datetime
# This function will generate a value between -1 and 1, 
# which will tell us what percentage of the portfolio should buy or short 
# the asset θ. Something important to highlight is that the series starts 
# estimating the positions after 8 data points, those are essential in the model
# to at least be able to capture a gradient. This is essential, as in the final 
# return plot we will see a flat line the first 8 days. 
# This is because I gave the model a look-back window of 8 ticks.

# Theta uses a tanget function that is why its values must pass throug thus function 

def positions(x, theta):
    M = len(theta) - 2
    T = len(x)
    Ft = np.zeros(T)
    for t in range(M, T):
        xt = np.concatenate([[1], x[t - M:t], [Ft[t - 1]]])
        Ft[t] = np.tanh(np.dot(theta, xt))
    return Ft

### Calculating returns 
# These returns can then be used to calculate our Sharpe ratio. It is multiplied 
# by the model's "commission" In this case δ is our transaction cost rate. 

def returns(Ft, x, delta):
    T = len(x)
    rets = Ft[0:T - 1] * x[1:T] - delta * np.abs(Ft[1:T] - Ft[0:T - 1])
    return np.concatenate([[0], rets])

### Determining the Gradient 
# At least 8 observations to start determining the gradient. 

def gradient(x, theta, delta):
    Ft = positions(x, theta)
    R = returns(Ft, x, delta)
    T = len(x)
    M = len(theta) - 2
    
    A = np.mean(R)
    B = np.mean(np.square(R))
    S = A / np.sqrt(B - A ** 2)

    dSdA = S * (1 + S ** 2) / A
    dSdB = -S ** 3 / 2 / A ** 2
    dAdR = 1. / T
    dBdR = 2. / T * R
    
    grad = np.zeros(M + 2)  # initialize gradient
    dFpdtheta = np.zeros(M + 2)  # for storing previous dFdtheta
    
    for t in range(M, T):
        xt = np.concatenate([[1], x[t - M:t], [Ft[t-1]]])
        dRdF = -delta * np.sign(Ft[t] - Ft[t-1])
        dRdFp = x[t] + delta * np.sign(Ft[t] - Ft[t-1])
        dFdtheta = (1 - Ft[t] ** 2) * (xt + theta[-1] * dFpdtheta)
        dSdtheta = (dSdA * dAdR + dSdB * dBdR[t]) * (dRdF * dFdtheta + dRdFp * dFpdtheta)
        grad = grad + dSdtheta
        dFpdtheta = dFdtheta

        
    return grad, S

### Training

def train(x, epochs=2000, M=8, commission=0.0025, learning_rate = 0.3):
    theta = np.random.rand(M + 2)
    sharpes = np.zeros(epochs) # store sharpes over time
    for i in range(epochs):
        grad, sharpe = gradient(x, theta, commission)
        theta = theta + grad * learning_rate

        sharpes[i] = sharpe
    
    
    print("finished training")
    return theta, sharpes


#%matplotlib inline
import matplotlib.pyplot as plt
plt.rcParams["figure.figsize"] = (5, 3) # (w, h)
plt.rcParams["figure.dpi"] = 150
import pandas as pd

###  Change monthly data frame 
stocks_bmv = pd.read_csv("03_Output/FinalDataSet.csv").set_index('date')

# Create empty data bases to store results
df_train_returns = pd.DataFrame(columns=['train_returns', 'symbol','train_month','test_month'])
df_train_hold = pd.DataFrame(columns=['hold_returns', 'symbol','train_month','test_month'])
df_test_returns = pd.DataFrame(columns=['test_returns', 'symbol','train_month','test_month'])
df_test_hold = pd.DataFrame(columns=['hold_returns', 'symbol','train_month','test_month'])
df_sharpes = pd.DataFrame(columns=['sharpe', 'symbol','train_month','test_month'])
df_thetas = pd.DataFrame(columns=['theta', 'symbol','train_month','test_month'])

### Keep only a month and test it in the next month

# Create unique id fopr each month: YYYY-MM
# First short-term strategy: filter in one month and testing in the next month 
# Second short-term strategy: use all historical data and test in the next month 

stocks_bmv.index[0:6]

all_stocks = stocks_bmv.drop_duplicates(subset = ["symbol"])
all_stocks = all_stocks.symbol.tolist()
del all_stocks[61]
del all_stocks[81]
# Delere SRE stock, float division by zero in gradient


#aux_stocks = all_stocks[1:3]
#aux_stocks_bmv = stocks_bmv

# Keep only one month and the next one in the data frame 
# convert date column to datetime format
stocks_bmv['date'] = pd.to_datetime(stocks_bmv.index)
#aux_stocks_bmv = aux_stocks_bmv.loc[aux_stocks_bmv['date'] <= '2019-02-28']
stocks_bmv['month'] = stocks_bmv['date'].dt.strftime('%Y-%m')
#aux_stocks_bmv['month_seq'] = aux_stocks_bmv['date'].dt.month
months = stocks_bmv['date'].dt.strftime('%Y-%m').unique()# get unique months as YYYY-MM strings
# drop NaN values
months = months[:-1]


for i in all_stocks:
    symbol = i
    
    df_stock = stocks_bmv.loc[stocks_bmv['symbol'] == symbol]
    df_stock = df_stock.filter(items=['close','volume','month'])
    df_stock.index = pd.to_datetime(df_stock.index)
    df_stock["rets"] = df_stock['close'].diff()[1:]
    
    for i in range(len(months) - 1):
       current_month = months[i]
       next_month = months[i+1]

       x_train = df_stock.loc[df_stock['month'] == current_month].rets
       x_test = df_stock.loc[df_stock['month'] == next_month].rets
       
    
       x_train = np.array(x_train)
       x_test = np.array(x_test)
    
       x_train=x_train[~np.isnan(x_train)]
       x_test=x_test[~np.isnan(x_test)]
    
       std = np.std(x_train)
       mean = np.mean(x_train)
    
       x_train = (x_train - mean) / std
       x_test = (x_test - mean) / std
    
       np.random.seed(123)
       theta, sharpes = train(x_train, epochs=800, M=8, commission=0.0025, learning_rate=0.3)
    
       train_returns = returns(positions(x_train, theta), x_train, 0.0025)
       test_returns = returns(positions(x_test, theta), x_test, 0.0025)
    
    # initialize data of lists.
       data_train_returns = {'train_returns': train_returns,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
       data_train_hold = {'hold_returns': x_train,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
       data_test_returns = {'test_returns': test_returns,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
       data_test_hold = {'hold_returns': x_test,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
       data_sharpes = {'sharpe': sharpes,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
       data_thetas = {'theta': theta,
        'symbol': symbol,
        'train_month': current_month,
        'test_month':next_month}
    
       df_train_returns_aux = pd.DataFrame(data_train_returns)
       df_train_hold_aux = pd.DataFrame(data_train_hold)
       df_test_returns_aux = pd.DataFrame(data_test_returns)
       df_test_hold_aux = pd.DataFrame(data_test_hold)
       df_sharpes_aux = pd.DataFrame(data_sharpes)
       df_thetas_aux = pd.DataFrame(data_thetas)
    
       df_train_returns = df_train_returns.append(df_train_returns_aux, ignore_index=True)
       df_train_hold = df_train_hold.append(df_train_hold_aux, ignore_index=True)
       df_test_returns = df_test_returns.append(df_test_returns_aux, ignore_index=True)
       df_test_hold = df_test_hold.append(df_test_hold_aux, ignore_index=True)
       df_sharpes = df_sharpes.append(df_sharpes_aux, ignore_index=True)
       df_thetas = df_thetas.append(df_thetas_aux, ignore_index=True)
    
    
    
df_train_returns.to_csv("03_Output/df_train_returns_monthly.csv", encoding='utf-8', index=True)
df_train_hold.to_csv("03_Output/df_train_hold_monthly.csv", encoding='utf-8', index=True)
df_test_returns.to_csv("03_Output/df_test_returns_monthly.csv", encoding='utf-8', index=True)
df_test_hold.to_csv("03_Output/df_test_hold_monthly.csv", encoding='utf-8', index=True)
df_sharpes.to_csv("03_Output/df_sharpes_monthly.csv", encoding='utf-8', index=True)
df_thetas.to_csv("03_Output/df_thetas_monthly.csv", encoding='utf-8', index=True)


#  we can graph the resulting Sharpe ratio over each epoch, 
# and hopefully see it converge to a maximum.

#plt.plot(sharpes)
#plt.xlabel('Epoch Number')
#plt.ylabel('Sharpe Ratio');


#train_returns = returns(positions(x_train, theta), x_train, 0.0025)
#plt.plot((train_returns).cumsum(), label="Reinforcement Learning Model", linewidth=1)
#plt.plot(x_train.cumsum(), label="Buy and Hold", linewidth=1)
#plt.xlabel('Ticks')
#plt.ylabel('Cumulative Returns');
#plt.legend()
#plt.title("RL Model vs. Buy and Hold - Training Data");


#test_returns = returns(positions(x_test, theta), x_test, 0.0025)
#plt.plot((test_returns).cumsum(), label="Reinforcement Learning Model", linewidth=1)
#plt.plot(x_test.cumsum(), label="Buy and Hold", linewidth=1)
#plt.xlabel('Ticks')
#plt.ylabel('Cumulative Returns');
#plt.legend()
#plt.title("RL Model vs. Buy and Hold - Test Data");




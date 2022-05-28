
import pandas as pd
import numpy as np
import statsmodels.api as sm
import random
from statsmodels.sandbox.regression.gmm import IV2SLS
from sklearn.linear_model import LinearRegression
from tqdm import tqdm

random.seed(12345)

ols_model = LinearRegression()

def froll_sum(x): 
    # Convert daily return to monthly return, use fix_rolling_sum function
    tmp = []
    for i in range(int(len(x)/21)): # 21 represents the trading days for one month
        res = np.sum(x[i*21:(i+1)*21])
        tmp.append(res)
    return tmp

"""
Function CAPM_IV_Simulation is applied for automatically running simulation in
camparing the biases of Fama-MacBeth (1973) regression approach under IV and OLS estimation
method, respectively.

Parameters:
 
  
Inputs:
  T = number of years for simulation (years)
  N = number of portfolios/stocks for simulation
  t = rolling estimation window period (years)
  Repetitions = number of simulation times
  mean_beta = the mean of market betas of stocks 
  std_beta = the standard deviation of market betas of stocks
  mean_res_sig = the mean of the standard deviations of residuals
  std_res_sig = the standard deviation of the standard deviations of residuals
  mean_MKT = the mean of market returns
  std_MKT the standard deviation of market returns

Outputs:
  exante_bias_OLS: the ex-ante bias of OLS estimation and the corresponding mean under {Repetitions} simulations
  expost_bias_OLS: the ex-post bias of OLS estimation and the corresponding mean under {Repetitions} simulations
  exante_bias_IV: the ex-ante bias of IV estimation and the corresponding mean under {Repetitions} simulations
  expost_bias_IV: the ex-post bias of IV estimation and the corresponding mean under {Repetitions} simulations
  
  exante_RMSE_OLS: the ex-ante Root-mean-square deviation (RMSE) of OLS estimation
  expost_RMSE_OLS: the ex-post RMSE of OLS estimation
  exante_RMSE_IV: the ex-ante RMSE of IV estimation
  expost_RMSE_IV: the ex-post RMSE of IV estimation
"""


def CAPM_IV_Simulation(mean_MKT, std_MKT, mean_beta, std_beta, mean_res_sig, std_res_sig, N, T, t, Repetitions):
    # how many days in the simulation data
    T = T*12*21
    #Step-1: For each stock, we randomly generate a beta and a standard deviations of return residuals from normal distributions.
    #simulate market betas for each stocks
    sim_betas_tmp = []
    sim_betas_tmp.append(np.random.normal(loc = mean_beta,scale = std_beta, size= N ))
    sim_betas = pd.DataFrame(sim_betas_tmp)

    #simulate the residual return standard deviations (sigmas)
    sim_sigmas_tmp = []
    sim_sigmas_tmp.append(np.random.normal(loc = mean_res_sig,scale = std_res_sig, size= N ))
    sim_sig_res = pd.DataFrame(sim_sigmas_tmp).abs()

    exante_bias_OLS = []
    expost_bias_OLS = []

    exante_bias_IV = []
    expost_bias_IV = []

    for r in tqdm(range(Repetitions)):
        #Step-2: For each day, we randomly draw market excess return from a normal distribution with mean and standard deviation equal to the real mean and standard deviation from the sample data
        sim_MKT_tmp = []
        sim_MKT_tmp.append(np.random.normal(loc = mean_MKT,scale = std_MKT, size= T))
        sim_MKT = pd.DataFrame(sim_MKT_tmp).T

        #Step-3: For each stock, we then randomly generate daily residual return from a normal distribution with mean zero and standard deviation equal to the value generate in step-1
        res_return = pd.DataFrame()
        for j in range(N):
            returns = pd.DataFrame(np.random.normal(loc = 0,scale = sim_sig_res[j], size= T))
            res_return = pd.concat([res_return,returns], axis=1)
        res_return.columns = range(len(res_return.columns))
        #Step-4: For stock i, we compute its excess return as beta,i*MKT,t + episiloni,,t
        sim_SR = pd.DataFrame((np.mat(sim_MKT.iloc[:,0]).T*np.mat(sim_betas.iloc[0,:].values)) + res_return)

        #Calculate the monthly return of simulating stock and market returns
        m_sim_SR = sim_SR.apply(lambda x: froll_sum(x))
        m_sim_MKT = sim_MKT.apply(lambda x: froll_sum(x))

        # FM_OLS Regression
        ## 1st Stage of FM Regression/ Beta Estimation
        holding = 21 # how many trading days for one month
        rollingW = t*12*holding
        periods = int((T - rollingW) / holding)
        # Time-Series Regression
        all_betas = []
        for i in range(periods):
            sim_MKT_tmp = sim_MKT.iloc[i*holding:i*holding+rollingW,:]
            sim_SR_tmp = sim_SR.iloc[i*holding:i*holding+rollingW,:]
            betas = []
            for j in sim_SR.columns:      
                OLS = sm.OLS(sim_SR_tmp.loc[:,j].values.astype(np.float64), sm.add_constant(sim_MKT_tmp.values.astype(np.float64)))
                res = OLS.fit()
                b = list(res.params)
                betas.append(b)
            df_betas = pd.DataFrame(betas).rename(columns={0:'Intercepts',1:'MKT'})
            all_betas.append(df_betas)

        # 2nd Stage of FM Regression/ Risk Premium Estimation
        holding = 1 # the following regression for risk premium estimation is performed monthly
        rollingW = t*12*holding
        lens = T/21
        periods = int((lens - rollingW)/holding)
        # Cross-sectional Regression
        all_lambdas_OLS = []
        all_resids_OLS = []
        lambdas = []
        resids = []
        m_sim_SR_tmp_T = m_sim_SR.iloc[rollingW:rollingW+periods*holding,:].T
        index_times = list(m_sim_SR_tmp_T.columns)
        for k in m_sim_SR_tmp_T.columns:   
            i = int(index_times.index(k)/holding)
            OLS = sm.OLS(m_sim_SR_tmp_T.loc[:,k], sm.add_constant(all_betas[i].loc[:,"MKT"].astype(np.float64)))
            res = OLS.fit()
            l = list(res.params)
            r = list(res.resid)
            lambdas.append(l)
            resids.append(r)        
        all_lambdas_OLS.append(pd.DataFrame(lambdas))
        all_resids_OLS.append(pd.DataFrame(resids))

        exante_bias_ols = ((np.mean(all_lambdas_OLS[0].iloc[:,1]) - mean_MKT*21)/(mean_MKT*21))*100
        expost_bias_ols = ((np.mean(all_lambdas_OLS[0].iloc[:,1]) - np.mean(m_sim_MKT.values))/np.mean(m_sim_MKT.values))*100
        
        exante_bias_OLS.append(exante_bias_ols)
        expost_bias_OLS.append(expost_bias_ols)

         # FM_IV Regression
        ## 1st Stage of FM_IV Regression/ Beta Estimation
        holding = 21
        rollingW = t *12 * holding
        periods = int((T - rollingW) / holding) 
        ## Time-Series Regression 
        all_betas_ev = []
        all_betas_iv = []

        for i in range(periods):
            sim_MKT_tmp = sim_MKT.iloc[i*holding:i*holding+rollingW,:]
            sim_SR_tmp = sim_SR.iloc[i*holding:i*holding+rollingW,:]

            if (i + 1) % 2 == 1: # 0:Jan is odd month
                rw_odd = pd.DataFrame() 
                for k in range(0,t*12,2): # if current month is odd, extract all the past odd months in the rolling window to estimate explanatory betas
                    rw_odd = pd.concat([rw_odd,sim_MKT_tmp.iloc[21*k:21*(k+1),:]])

                betas_ev = []
                for j in sim_SR.columns:
                    df_index = rw_odd.index
                    b = list(ols_model.fit(sim_MKT_tmp.loc[df_index,:].values, sim_SR_tmp.loc[df_index,j].values).coef_)
                    betas_ev.append(b)
                df_betas_ev = pd.DataFrame(np.array(betas_ev))
                all_betas_ev.append(df_betas_ev)
            
                rw_even = pd.DataFrame() 
                for k in range(1,t*12,2):  # if current month is odd, extract all the past even months in the rolling window to estimate instrumental betas (lagged explanatory betas)      
                    rw_even = pd.concat([rw_even,sim_MKT_tmp.iloc[21*k:21*(k+1),:]])
                
                betas_iv = []
                for j in sim_SR.columns:
                    df_index = rw_even.index   
                    b = list(ols_model.fit(sim_MKT_tmp.loc[df_index,:].values, sim_SR_tmp.loc[df_index,j].values).coef_)
                    betas_iv.append(b)      
                df_betas_iv = pd.DataFrame(np.array(betas_iv))
                all_betas_iv.append(df_betas_iv)

            if (i + 1) % 2 == 0: # 1: Feb is even month
                rw_odd = pd.DataFrame()
                for k in range(1,t*12,2): # if current month is even, extract all the past odd months in the rolling window to estimate instrumental betas (lagged explanatory betas)
                    rw_odd = pd.concat([rw_odd,sim_MKT_tmp.iloc[21*k:21*(k+1),:]])

                betas_iv = []
                for j in sim_SR.columns:
                    df_index = rw_odd.index   
                    b = list(ols_model.fit(sim_MKT_tmp.loc[df_index,:].values, sim_SR_tmp.loc[df_index,j].values).coef_)
                    betas_iv.append(b)        
                df_betas_iv = pd.DataFrame(np.array(betas_iv))
                all_betas_iv.append(df_betas_iv)
                
                rw_even = pd.DataFrame()  
                for k in range(0,t*12,2): # if current month is even, extract all the past even months in the rolling window to estimate explanatory betas 
                    rw_even = pd.concat([rw_even,sim_MKT_tmp.iloc[21*k:21*(k+1),:]])

                betas_ev = []
                for j in sim_SR.columns:
                    df_index = rw_even.index         
                    b = list(ols_model.fit(sim_MKT_tmp.loc[df_index,:].values, sim_SR_tmp.loc[df_index,j].values).coef_)
                    betas_ev.append(b)
                df_betas_ev = pd.DataFrame(np.array(betas_ev))
                all_betas_ev.append(df_betas_ev)

        ## 2st Stage of FM Regression/ Risk Premium Estimation
        holding = 1 
        rollingW = t * 12 * holding
        lens = T/21
        periods = int((lens - rollingW) / holding)
        ## Cross-Sectional Regression 
        all_lambdas_IV = []
        all_resids_IV = []
        lambdas = []
        resids = []
        m_sim_SR_tmp_T = m_sim_SR.iloc[rollingW:rollingW+periods*holding,:].T
        index_times = list(m_sim_SR_tmp_T.columns)

        for k in m_sim_SR_tmp_T.columns:
            i = int(index_times.index(k) / holding)
            iv = IV2SLS(endog = m_sim_SR_tmp_T.loc[:,k].values,exog = sm.add_constant(all_betas_ev[i].values.astype(np.float64)),\
                    instrument = sm.add_constant(all_betas_iv[i].values.astype(np.float64)))
            res = iv.fit()
            l = list(res.params)
            r = list(res.resid)
            lambdas.append(l)
            resids.append(r)

        all_lambdas_IV.append(pd.DataFrame(lambdas))
        all_resids_IV.append(pd.DataFrame(resids))

        exante_bias_iv = ((np.mean(all_lambdas_IV[0].iloc[:,1]) - mean_MKT*21)/(mean_MKT*21))*100
        expost_bias_iv = ((np.mean(all_lambdas_IV[0].iloc[:,1]) - np.mean(m_sim_MKT.values))/np.mean(m_sim_MKT.values))*100
        
        exante_bias_IV.append(exante_bias_iv)
        expost_bias_IV.append(expost_bias_iv)

    print(f"The ex-ante bias of OLS: {exante_bias_OLS}",\
        f"The mean of ex-ante bias of OLS among {Repetitions} simulations is: {sum(exante_bias_OLS)/len(exante_bias_OLS)}",\
        f"The ex-post bias of OLS: {expost_bias_OLS}",\
        f"The mean of ex-post bias of OLS among {Repetitions} simulations is: {sum(expost_bias_OLS)/len(expost_bias_OLS)}",\
        f"The ex-ante bias of IV: {exante_bias_IV}",\
        f"The mean of ex-ante bias of V among {Repetitions} simulations is: {sum(exante_bias_IV)/len(exante_bias_IV)}",\
        f"The ex-post bias of IV: {expost_bias_IV}",\
        f"The mean of ex-post bias of IV among {Repetitions} simulations is: {sum(expost_bias_IV)/len(expost_bias_IV)}", sep='\n')

# Example
CAPM_IV_Simulation(mean_MKT=0.00028, std_MKT=0.01155, mean_beta=0.14539, \
                    std_beta=0.20392, mean_res_sig=0.01614, std_res_sig=0.01390, \
                    N=100, T=10, t=3, Repetitions=2)

# It takes about 30 min to finish one repetition/simulation
CAPM_IV_Simulation(mean_MKT=0.00023, std_MKT=0.00966, mean_beta=0.95, \
                    std_beta=0.42, mean_res_sig=0.00233, std_res_sig=0.01499, \
                    N=2000, T=57, t=3, Repetitions=1)
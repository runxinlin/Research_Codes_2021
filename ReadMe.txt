Empirical Asset Pricing with Individual Assets on the JSE: Betas Versus characteristics

####Project Description####

Purpose: The study adopts the instrumental variables (IV) and Shanken’s (1992) bias-adjusted estimator to mitigate the inherent errors-in-variables 
bias in Fama-MacBeth (FM, 1972) regression while allowing the use of individual stocks as test assets to avoid the shortcomings of using portfolios. Further, this study compares the explanatory power of cross-sectional (CS) factors and time-series (TS) factors on asset average returns under the Carhart4-factor model.
 
Context: Portfolios have been widely used as test assets to ease the EIV bias in a two-stage regression. When stocks are sorted into portfolios based on certain characteristics, a strong factor structure is imparted, which may bias regression-based approaches to identifying factors that are actually unrewarded. Additionally, prespecified characteristics may be better proxies for true factor loadings since estimated betas contain measurement errors,
and slope coefficients on characteristics may reflect the underlying factor premiums.
 
Methods: The study adopts two ways to attenuate the EIV bias: 1) The FM-OLS procedure is revised by using the IV estimator, in which the core is to estimate explanatory and instrumental betas from a disjoint data sample, so that measurement errors are not cross-sectionally correlated;
2) The asymptotic EIV-bias-corrected estimator of Shanken (1992) is employed under the Black et al. (1972) regression.
 
Results: In small sample simulations, Shanken's estimator consistently delivers consistent ex-post risk premium estimates, while FM-IV failed to mitigate EIV bias due to serial correlation among the measurement errors of overlapping beta estimates. As a result of empirical asset pricing tests, factors like market, size, value and momentum under the CAPM, the Fama-French 3- and Carhart 4-factor models are significantly priced on the cross-section, while the slope coefficients of their matching characteristics are also statistically significant. It is also shown by using fixed effects estimation that the significance of these characteristics is not caused by their correlation with omitted and potentially unobserved firm-specific factors.  Moreover, a comparison of TS and CS factors under the Carhart4-factor model shows that time-varying characteristics are better proxies for true factor loadings, while their slope coefficients (CS factors) as optimized by the CSR OLS regression provide a better description of cross-sectional average returns than TS factors that are arbitrarily constructed.
 
Key Words: Factor Loadings, Characteristics, Errors-in-Variables Bias, Omitted Variables Problem, Instrumental Variables, Shanken’s Estimator 

####Data Sorting Section####
Data Description
The sample period used in this study spans from Jan 2000 to Dec 2019 (240 months).
A total of 819 distinct stocks entered the sample at different points in time during 
this sample period and the sample comprises 378 stocks per year on average. 
Daily data of stock price, market cap, market return, book-to-market ratio, 
price-to-earnings ratio, operating profitability and change of total asset for 
listed companies on the JSE are obtained from the Bloomberg terminal at Wits Lab, 
while the risk-free is collected from the South African Reserve Bank. The 91-day 
Treasury-bill (T-bill) return rate was obtained by the South African Reserve Bank and 
used as a proxy for the risk-free rate. Because the 91-day T-bill's return was an annualized 
return, it is geometrically divided into daily returns. In addition, J203T was obtained from
the Bloomberg terminal and used to represent the return of the benchmark market. 

***The procedure of data sorting and portfolios construction is done on R.
Portfolios include Market, SMB, HML, RMW and CMA factor realizations.
Please see the details in R_final.R document in master branch.

####Regression section####
1. Simulation
This study first tests whether the instrumental variables (IV) approach as proposed by Jegadeesh et al. (2019) can resolve the
EIV bias, which is inherent to a two-stage regression like Fama-MacBeth (FM, 1973). For that, the simulation
of the performance of IV and OLS are constructed for comparison.

2. Estimation of Factor Premiums
-In this section, the study focus on using the FM OLS and FM IV regression approach to estimate the risk premiums
for the factors under the standard CAPM, the Fama-French three- (FF3) and five-factor (FF5) models, respectively.
-Furthermore, the study includes the firm characteristics as controlled variables for the corresponding factors
like size, value, operatability and investment.
-It is possible that characteries are better proxies for ture factor loadings than estimated betas since beta estimates 
contain measurement errors. This study compares the predictability of characteristics and betas in forecasting future stock
returns of the next period by using the IV and IR metrics.

3. Compare CS and TS Factor models
It is also possible that slope coefficients on characteries (CS factors) have better explanatory power for asset average returns 
than double-sorted factor realizations (TS factors). As motivated by Fama and French (2019), the study use cross-sectional regression
approach of FM to construct CS factors corresponding to the TS factors of Fama and French (2015). The reason of 
choosing this specific FF5 model is because we want to avoid the omitted-variables bias (OVB) by including the potential
significant factors on the JSE as much as possible. 

TS Factor Model I:
〖 R〗_(i,t)- R_(f,t) = α_i+ β_(i,MKT) 〖MKT〗_t+β_(i,SMB) 〖SMB〗_t+β_(i,HML) 〖HML〗_t+β_(i,RMW) 〖RMW〗_t+β_(i,CMA) 〖CMA〗_t+ε_(i,t)

CS Factor Model I:
 R_(i,t)- R_(z,t)= 〖MC〗_(i,t-1) R_(MC,t)+〖PE〗_(i,t-1) R_(PE,t)+〖OP〗_(i,t-1) R_(OP,t)+〖INV〗_(i,t-1) R_(INV,t)+ε_(i,t)

 CS Factor Model II:
 R_(i,t)- R_(f,t)=〖a_i+ β〗_(i,1) R_(MKT,t)+β_(i,2) R_(MC,t)+β_(i,3) R_(PE,t)+β_(i,4) R_(OP,t)+β_(i,5) R_(INV,t)+ε_(i,t)

-The insight here is that the CS factors are constructed from the same portfolios (3 factors * 2*3 double sorts, a total of 18 double-sorted portfoliots)
that are used to produce TS factors. As a result, the CS factor models can be compared with TS factor model in an apple-to-apple case

-The evaluation metrics that this study adopted are a bunch of "Alpha Test" and the different measures of the goodness of fit.

 ***The regression section is completely conducted on Python. As the Python_final file summarises 
 all the components of the above mentioned procedures, each component can be found within the same folder.

 ***The modules used in this study are summarised in the Requirement.txt




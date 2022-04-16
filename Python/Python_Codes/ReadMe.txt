****"!pip3 install tqdm"



****To Do List
4. CAPM simulation

Layout
####Regression Section####

####CAPM Simulation #### 
compare the bias of FM, IV

####OLS Regression####
-CAPM
-FF3-Factor ***Adding firm characteristics as control variables
-Carhart4-Factor 
-FF5-Factor ***Adding firm characteristics as control variables

####IC/ICIR####
Calculate IC/ICIR for FF5-Factor and their corresponding firm characteries

####IV Regression####
-CAPM
-FF3 ***Adding firm characteristics as control variables
-Carhart4-Factor
-FF5 ***Adding firm characteristics as control variables

#### Compare CS and TS model
-FF5-Factor
在学术界关于 empirical asset pricing 的论文中，portfolio test 和 regression test 是检验一个新因子是否有效的两个常见手段。在前者中，使用已有因子的收益率作为 regressors、使用基于新因子构建的投资组合的收益率作为被解释变量，进行时序回归，从而检验新因子组合是否可以获得超额收益 α、以及它在已有因子上的 β。在后者中，新因子和已有因子一起被用来和个股收益率进行截面回归（通常使用 Fama-MacBeth regression），然后考察新因子的预期收益率 E[f] 是否显著不为零。

无论是上面哪种方法，学者们都会对回归分析得到的 α、β 以及 E[f] 给出 t-statistic 从而检验它们的显著性。




从上面的描述不难看出，第二个 CS 模型直接对标了 FF5。这二者唯一的区别就是在四个风格因子的构建上：FF5 使用 18 个 2 × 3 double sort 的投资组合构建；而在 CS 模型二中，使用 FM regression 求解纯因子模型来构建





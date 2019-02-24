
% this is the new programs that modified by Ethan Lin start at 2018/10/15
% source from original programs brcode.m
% 
% original programs:
% version 3, October 11, 2004
% These routines can be used by and distributed for non-profit academic
% purposes without any royalty except that the users must cite:
% Bai, Jushan and Pierre Perron (1998): "Estimating and Testing Linear
% Models with Multiple Structural Changes," Econometrica, vol 66,?@47-78
% and
%  Bai, Jushan and Pierre Perron (2003): "Computation and Analysis of
%  Multiple Structural Change Models," Journal of Applied Econometrics, 18, 1-22.
% For any other commercial use and/or comments, please contact Pierre
% Perron at perron at bu.edu.
% Even though we tried to make this program error-free we cannot be held
% responsible for any consequences that could result from remaining errors.
% Copyright, Pierre Perron (1999, 2004)

clear;
clc;
close all;

%%
table_data_original=readtable('./data/预处理的数据_20181016.xlsx','Sheet','original');  % read data
data_original = table2array(table_data_original(:,3:end));
numEffectiveSample=size(table_data_original,1)-1;                  % set effective sample size
numClassOfCurrency=4;                                      % %%%%%%%%%%%

% weight_c = 0.0;

% set up the data, data_y is the dependent variable data_z is 
                           % the matrix of regressors (numEffectiveSample,numRegressorsZ) whose coefficients 
                           % are allowed to change, data_x is a (numEffectiveSample,numRegressorsX) matrix of 
                           % regressors with coefficients fixed across regimes. 
                           % Note: initialize data_x to [] if numRegressorsX=0.

Delta_log_CNY = calculate_DeltaAndLog(table_data_original.CNY);
Delta_log_USD = calculate_DeltaAndLog(table_data_original.USD);
Delta_log_EUR = calculate_DeltaAndLog(table_data_original.EUR);
Delta_log_JPY = calculate_DeltaAndLog(table_data_original.JPY);
Delta_log_KRW = calculate_DeltaAndLog(table_data_original.KRW);
EMP = calculate_EMP(table_data_original.CNY, table_data_original.ForeignExchangeReserve, table_data_original.ChinaBaseMoney);

data_y = Delta_log_CNY;
data_x = [Delta_log_USD, Delta_log_EUR, Delta_log_JPY, Delta_log_KRW, EMP];
data_z = ones(numEffectiveSample,1);
                                                    

numRegressorsZ=1;                      % number of regressors data_z
numRegressorsX=5;                      % number of regressors data_x 
maxStructuralChanges=5;                      % maximum number of structural changes allowed
eps1=.15;                 % value of the trimming (in percentage) for the construction 
                          % and critical values of the supF type tests (used in the 
                          % supF test, the Dmax, the supF(l+1|l) and the sequential 
                          % procedure). If these tests are used, minSizeOfSegment below should be set 
                          % at int(eps1*numEffectiveSample). But if the tests are not required, 
                          % estimation can be done with an arbitrary minSizeOfSegment. There are five 
                          % options: eps1= .05, .10, .15, .20, or .25. For each option, 
                          % the maximal value of maxStructuralChanges above is: 10 for criterionForConvergence=.05, 8 for 
                          % eps1=.10, 5 for eps1=.15, 3 for eps1=.20, and 2 for eps1=.25.

% minSizeOfSegment=round(eps1*numEffectiveSample);       % minimal length of a segment (minSizeOfSegment >= numRegressorsZ). Note: if isHeterogeneityAndAutocorrelation=1, minSizeOfSegment 
minSizeOfSegment=26;       % minimal length of a segment (minSizeOfSegment >= numRegressorsZ). Note: if isHeterogeneityAndAutocorrelation=1, minSizeOfSegment 
                          % should be set at a larger value. 

% The followings are options if numRegressorsX > 0 -------------------------------                          
isFixedInitialVallueOfBeta=0;                   % set to 1 if use fixed initial values for beta
bataInit=0;                % if isFixedInitialVallueOfBeta=1, load the initial value of beta
maxIteration=20;                  % maximum number of iterations for the nonlinear procedure to 
                          % obtain global minimizers
printd=1;                 % set to 1 if want the output from the iterations to be printed
criterionForConvergence=0.0001;               % criterion for the convergence
% --------------------------------------------------------------------
isHeterogeneityAndAutocorrelation=1;                % set to 1 if want to allow for heterogeneity and autocorrelation 
                         % in the residuals, 0 otherwise. The method used is Andrews(1991) 
                         % automatic bandwidth with AR(1) approximation and the quadratic 
                         % kernel. Note: Do not set to 1 if lagged dependent variables are 
                         % included as regressors.
isApplyAR1=1;               % set to 1 if want to apply AR(1) prewhitening prior to estimating 
                         % the long run covariance matrix.
hetdat=1;                % option for the construction of the F tests. Set to 1 if want to
                         % allow different moment matrices of the regressors across segments. 
                         % If hetdat=0, the same moment matrices are assumed for each segment 
                         % and estimated from the ful sample. It is recommended to set 
                         % hetdat=1 if numRegressorsX>0.
hetvar=1;                % option for the construction of the F tests.Set to 1 if want to allow 
                         % for the variance of the residuals to be different across segments. 
                         % If hetvar=0, the variance of the residuals is assumed constant 
                         % across segments and constructed from the ful sample. This option 
                         % is not available when isHeterogeneityAndAutocorrelation=1.  
hetomega=1;              % used in the construction of the confidence intervals for the break 
                         % dates. If hetomega=0, the long run covariance matrix of zu is 
                         % assumed identical across segments (the variance of the errors u 
                         % if isHeterogeneityAndAutocorrelation=0)
hetq=1;                  % used in the construction of the confidence intervals for the break 
                         % dates. If hetq=0, the moment matrix of the data is assumed identical 
                         % across segments.
isDoGlobal=1;              % set to 1 if want to cal the procedure to obtain global minimizers
isDoTest=1;                % set to 1 if want to construct the supF, UDmax and WDmax tests 
                         % isDoGlobal must be set to 1 to run this procedure.
isDoSuperFTest_l_plus_1=1;              % set to 1 if want to construct the supF(l+1|l) tests where under
                         % the null the l breaks are obtained using global minimizers. 
                         % isDoGlobal must be set to 1 to run this procedure.
doorder=1;               % set to 1 if want to cal the procedure that selects the number of
                         % breaks using information criteria. isDoGlobal must be set to 1 to 
                         % run this procedure.
isEstimateBreaksSequentialy=1;               % set to 1 if want to estimate the breaks sequentialy and estimate 
                         % the number of breaks using supF(l+1|l) test   
isDoRepartition=1;              % set to 1 if want to modify the break dates obtained from the 
                         % sequential method using the repartition method of Bai (1995),
                         % Estimating breaks one at a time. This is needed for the confidence 
                         % intervals obtained with estim below to be valid.
estimbic=1;              % set to 1 if want to estimate the model with the number of breaks 
                         % selected by BIC.
estimlwz=0;              % set to 1 if want to estimate the model with the number of breaks  
                         % selected by LWZ
estimseq=1;              % set to 1 if want to estimate the model with the number of breaks
                         % selected using the sequential procedure
estimrep=0;              % set to 1 if want to estimate the model with the breaks selected
                         % using the repartition method
estimfix=0;              % set to 1 if want to estimate the model with a prespecified number
                         % of breaks equal to fixn set below
fixn=0;

pbreak(numEffectiveSample,data_y,data_z,numRegressorsZ,maxStructuralChanges,minSizeOfSegment,eps1,isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,isDoGlobal,isDoTest,isDoSuperFTest_l_plus_1,doorder,isEstimateBreaksSequentialy,isDoRepartition,estimbic,estimlwz,estimseq,estimrep,estimfix,isFixedInitialVallueOfBeta,data_x,numRegressorsX,criterionForConvergence,maxIteration,bataInit,printd,hetdat,hetvar,fixn)



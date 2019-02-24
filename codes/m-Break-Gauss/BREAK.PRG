@version 4, February 24, 2009.
These routines can be used by and distributed for non-profit academic purposes
without any royalty except that the users must cite:
Bai, Jushan and Pierre Perron (1998): "Estimating and Testing Linear
Models with Multiple Structural Changes," Econometrica, 1998, vol 66, 47-78
and
Bai, J. and P. Perron (2003): "Computation and Analysis of Multiple
Structural Change Models," Journal of Applied Econometrics, 18, 1-22.
For any other commercial use and/or comments,  please contact
Pierre Perron at perron at bu.edu.
Even though we tried to make this program error-free we cannot be held
responsible for any consequences that could result from remaining errors.
Copyrigth, Pierre Perron (1999, 2004, 2009).
@

new;
format /ld 6,4;

output file = real.out reset;   @select your output file@
load yyy[103,1] = real.dat;      @read data@

numEffectiveSample=103;                        @set effective sample size@

data_y=yyy[1:103,1];                @set up the data, data_y is the dependent variable
                                data_z is the matrix of regressors (numEffectiveSample,numRegressorsZ) whose
                                coefficients are allowed to change, x is a
                                (numEffectiveSample,numRegressorsX) matrix of regressors with coefficients
                                fixed across regimes. Note: initialize x to
                                something, say 0, even if numRegressorsX = 0.@
data_z=ones(numEffectiveSample,1);

x=0;

numRegressorsZ=1;                         @number of regressors data_z@
numRegressorsX=0;                         @number of regressors x@
maxStructuralChanges=5;                         @maximum number of structural changes allowed@
eps1=.15;                  @Value of the trimming (in percentage) for the construction
                                  and critical values of the supF ype tests (used in the 
                                 supF test, the Dmax, the supF(l+1|l) and the sequential
                                  procedure). If these test are used, minSizeOfSegment below should be set
                                  at int(eps1*numEffectiveSample). But if the tests are not required, estimation
                                  can be done with an arbitrary minSizeOfSegment.
                                  There are five options: eps1 = .05, .10, .15, .20 or .25.
                                  For each option, the maximal value of maxStructuralChanges above is: 10 for eps1 = .05;
                                  8 for eps1 = .10, 5 for eps1 = .15, 3 for eps1 = .20 and 2 for eps1 = .25.@
minSizeOfSegment=int(eps1*numEffectiveSample);                         @minimal length of a segment (minSizeOfSegment >= numRegressorsX+numRegressorsZ). Note: If
                                  robust=1, minSizeOfSegment should be set at a larger value.@
/* the following are options if numRegressorsX > 0.
----------------------------------- */
fixb=0;                     @set to 1 if use fixed initial values for beta@
betaini=0;                  @if fixb=1, load the initial value of beta.@
maxi=20;                    @maximum number of iterations for the nonlinear
                             procedure to obtain global minimizers.@
printd=1;                   @set to 1 if want the output from the iterations
                             to be printed.@
criterionForConvergence=0.0001;                 @criterion for the convergence.@
/*--------------------------------- */

robust=1;                   @set to 1 if want to allow for heterogeneity
                             and autocorrelation the in residuals, 0 otherwise.
                             The method used is Andrews(1991) automatic
                             bandwidth with AR(1) approximation and the
                             quadratic quernel. Note: Do not set to 1 if
                             lagged dependent variables are included as
                             regressors.@
prewhit=1;                   @set to 1 if want to apply AR(1) prewhitening
                              prior to estimating the long run covariance
                              matrix@
hetdat=1;              @Option for the construction of the F-tests.
                                Set to 1 if want to allow different moment matrices of the 
                               regressors accross segments. If hetdat = 0, the same
                               moment matrices are assumed for each segment and estimated 
                               from the full sample. It is recommended to set hetdat=1.  If numRegressorsX > 0
                                set hetdat = 1.@
hetvar=1;              @Option for the construction of the F-tests.
                              Set to 1 if want to allow for the variance of the residuals
                              to be different across segments. If hetvar=0, the variance
                               of the residuals is assumed constant across segments
                              and constructed from the full sample. This option is not available 
                              when robust = 1.@
hetomega=1;                @Used in the construction of the confidence
                            intervals for the break dates. If hetomega=0,
                            the long run covariance matrix of zu is assumed
                            identical accross segments (the variance of the
                            errors u if robust = 0).@
hetq=1;                    @Used in the construction of the confidence
                            intervals for the break dates. If hetq=0,
                            the moment matrix of the data is assumed
                            identical accross segments.@
doglobal=1;                 @set to 1 if want to call the procedure
                             to obtain global minimizers.@
dotest=1;                   @set to 1 if want to construct the sup F,
                             UDmax and WDmax tests. doglobal must be set
                             to 1 to run this procedure.@
dospflp1=1;                 @set to 1 if want to construct the sup(l+1|l)
                             tests where under the null the l breaks are
                             obtained using global minimizers. doglobal
                             must be set to 1 to run this procedure.@
doorder=1;                  @set to 1 if want to call the procedure that
                             selects the number of breaks using information
                             criteria. doglobal must be set to 1 to run
                             this procedure.@
dosequa=1;                  @set to 1 if want to estimate the breaks
                             sequentially and estimate the number of
                             breaks using the supF(l+1|l) test.@
dorepart=1;                @set to 1 if want to modify the
                             break dates obtained from the sequential
                             method using the repartition method of
                             Bai (1995), Estimating breaks one at a time.
                             This is needed for the confidence intervals
                             obtained with estim below to be valid.@
estimbic=1;                  @set to 1 if want to estimate the model with
                              the number of breaks selected by BIC.@
estimlwz=0;                  @set to 1 if want to estimate the model with
                              the number of breaks selected by LWZ.@
estimseq=1;                 @set to 1 if want to estimate the model with
                              the number of breaks selected using the
                              sequential procedure.@
estimrep=0;                 @set to 1 if want to esimate the model with
                              the breaks selected using the repartition
                              method.@
estimfix=1;                  @set to 1 if want to estimate the model with
                              a prespecified number of breaks equal to fixn
                              set below.@
fixn=2;

call pbreak(numEffectiveSample,data_y,data_z,numRegressorsZ,maxStructuralChanges,minSizeOfSegment,eps1,robust,prewhit,hetomega,
hetq,doglobal,dotest,dospflp1,doorder,dosequa,dorepart,estimbic,estimlwz,
estimseq,estimrep,estimfix,fixb,x,numRegressorsX,criterionForConvergence,maxi,fixb,betaini,printd,hetdat,hetvar,fixn);

#include brcode.src            @set the path to where you store the file brcode.src@

end;

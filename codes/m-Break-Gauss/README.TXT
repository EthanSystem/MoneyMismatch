October 11, 2004


This package contains 3 files:

1) break.prg 

This is the main program where you set up your data and options.
The current setup is to test for and estimate shifts in means 
for the US real interest rate series (see Garcia nd Perron (1996), 
"An Analysis of the Real interest Rate under Regime Shifts,"
Review of Economics and Statistics, 78, 111-125).

There are no real limitations on the length of the data set, the number of breaks,
the number of regressors, etc. It depends on the amount of memory available. 
In many cases, the sample size can be a constraint since the program uses a 
T(T+1)/2 vector as a storage device to compute global optimizers (but the constraint
is binding, in most cases, only if T is greater than 500).

The structure of the program is such that what to do should be self explanatory.
The main part is to set up your data in three parts: 1) the dependent variable (data_y),
2) the regressors whose coefficients are allowed to change (data_z, dimension numRegressorsZ) and 3) the regressors
whose coefficients are not allowed to change (x, dimension numRegressorsX). Note that even if  there are no
x's, you should initialize x to something like x=0. With these you need to specify
numRegressorsX, numRegressorsZ, and numEffectiveSample (the effective sample size).

You also need to specify maxStructuralChanges, the maximal number of breaks allowed (usually 5 if you want to
use the table of critical values in Bai and Perron (1998)). Also minSizeOfSegment, which is the minimal
length of a segment. When constructing the various tests, you should use minSizeOfSegment=int(eps1*T)
where eps1 can take values .05, .10, .15 , .20 and .25 (critical values are available and
printed as output for these cases). If estimation is the sole concern minSizeOfSegment an be set to any 
value greater than numRegressorsZ. If you are using options like robust=1, hetvar=1
hetomega=1 or hetq=1, minSizeOfSegment or eps1 should be larger.

To have the program perform some task or not simply put a 1 or a zero to the corresponding dummy.

2) brcode.src

This is the main code. You should not have to modify it.

3) a file real.dat which contains the US real interest rate series mentioned above. Use it to run
the program once to see if everything is OK.

Note on the October 2004 version: This new version contains a correction for the construction of the 
confidence intervals of the estimates of the break dates as discussed in Zeileis, A. and C. Kleiber (2004),
Validating Multiple Structural Change Models: A Case Study, University of Vienna, Austria.

This program is distributed freely for non-profit academic purposes only. For other uses, please contact
Pierre Perron at perron@bu.edu.
A lot of effort has been put to construct this program and we would appreciate that you acknowledge
using this code in your research and cite the relevant papers on which it is based:

Bai, J. and P. Perron (1998), "Estimating and Testing Linear Models with Multiple Structural Changes,"
Econometrica, 66, 47-78.

and

Bai, J, and P. Perron (1998), "Computation and Analysis of Multiple Structural Change Models," 
Journal of Applied Econometrics, 18, 1-22.

Although a lot of efforts has been put in constructing the program, we cannot be held responsible 
for any consequences that could result from remaining errors.

Comments about errors, possible improvements and so on are most welcomed and should be
directed to Pierre Perron at perron@bu.edu.

Thank you for your interest and good luck with the program.

Pierre Perron
Department of Economics
Boston University
270 Bay State Road
Boston, MA, 02215

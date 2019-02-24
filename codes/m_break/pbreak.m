function []=pbreak(numEffectiveSample,data_y,data_z,numRegressorsZ,maxStructuralChanges,minSizeOfSegment,eps1,isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,isDoGlobal,isDoTest,isDoSuperFTest_l_plus_1,doorder,isEstimateBreaksSequentialy,isDoRepartition,estimbic,estimlwz,estimseq,estimrep,estimfix,isFixedInitialVallueOfBeta,data_x,numRegressorsX,criterionForConvergence,maxi,betaInit,printd,isConstructeForFTest,hetvar,fixn)

%% 设置参数
disp('The option chosen are:')
disp(['minSizeOfSegment = ' num2str(minSizeOfSegment)])
disp(['eps1 = ' num2str(eps1)])
disp(['isConstructeForFTest = ' num2str(isConstructeForFTest)])
disp(['hetvar = ' num2str(hetvar)])
disp(['hetomega = ' num2str(hetomega)])
disp(['hetq = ' num2str(hetq)])
disp(['isHeterogeneityAndAutocorrelation = ' num2str(isHeterogeneityAndAutocorrelation) ' (isApplyAR1 =' num2str(isApplyAR1) ')'])
disp(['hetvar = ' num2str(hetvar)])
disp(['The maximum number of breaks is: ' num2str(maxStructuralChanges)])
disp('***********************************************************')

significanceLevel=[10;5;2.5;1];

%% The following calls the procedure to obtain the break dates and the associated sum of squared residuals for all numbers of breaks between 1 and maxStructuralChanges

if isDoGlobal==1
    % 适用于当使用全局计算的时候
    disp('Output from the global optimization')
    disp('***********************************************************')
    
    if numRegressorsX==0
        % 启动计算纯粹的结构变化模型的情况
        [glob,datevec,bigvec]=dating(data_y,data_z,minSizeOfSegment,maxStructuralChanges,numRegressorsZ,numEffectiveSample);
    else
        % 启动计算部分的结构变化模型的情况
        disp('This is a partial structural change model and the following')
        disp('specifications were used:')
        disp(['The number of regressors with fixed coefficients, numRegressorsX: ' num2str(numRegressorsX)])
        disp(['Whether (1) or not (0) the initial values of beta are fixed: ' num2str(isFixedInitialVallueOfBeta)])
        disp(['If so, at the following values: ' num2str(betaInit)])
        disp(['The convergence criterion is: ' num2str(criterionForConvergence)])
        disp(['Whether (1) or not (0) the iterations are printed: ' num2str(printd)])
        disp('----------------------')
        [glob,datevec,bigvec]=nldat(data_y,data_z,data_x,minSizeOfSegment,maxStructuralChanges,numRegressorsX,numRegressorsZ,numEffectiveSample,isFixedInitialVallueOfBeta,criterionForConvergence,maxi,betaInit,printd);
    end
    disp(' ');
    for i=1:maxStructuralChanges
        disp(['The model with ' num2str(i) ' breaks has SSR : ' num2str(glob(i,1))])
        disp(['The dates of the breaks are: ' num2str(datevec(1:i,i)')])
    end
    disp(' ');
    disp(' ');
    
end

%% The following calls the procedure to estimate and print various tests for the number of breaks. It also returns the UDmax and WDmax tests.

if isDoTest==1
    disp(' ')
    disp('************************************')
    disp('Output from the testing procedures')
    disp(' ')
    disp('************************************')
    disp('a) supF tests against a fixed number of breaks')
    disp('-----------------------------------------')
    
    ftest=zeros(maxStructuralChanges,1);
    wftest=zeros(maxStructuralChanges,1);
    
    for i=1:maxStructuralChanges
        ftest(i,1)=pftest(data_y,data_z,i,numRegressorsZ,numEffectiveSample,datevec,isApplyAR1,isHeterogeneityAndAutocorrelation,data_x,numRegressorsX,isConstructeForFTest,hetvar);
        disp(['The supF test for 0 versus ' num2str(i) ' breaks (scaled by numRegressorsZ) is: ' num2str(ftest(i,1))])
    end
    
    disp('------------------------------------------------')
    for j=1:4
        cv=getCriticalValue1(j,eps1);
        disp(['The critical values at the ' num2str(significanceLevel(j,1)) '% level are (for k=1 to ' num2str(maxStructuralChanges) '): ' num2str(cv(numRegressorsZ,1:maxStructuralChanges))])
    end
    
    disp('---------------------------------------------------------')
    disp('b) Dmax tests against an unknown number of breaks')
    disp('---------------------------------------------------------')
    disp(['The UDmax test is: ' num2str(max(ftest))])
    
    for j=1:4
        cvm=getDMax(j,eps1);
        disp(['(the critical value at the ' num2str(significanceLevel(j,1)) '% level is: ' num2str(cvm(numRegressorsZ,1)) ')'])
    end
    disp('****************************************************************')
    
    for j=1:4
        cv=getCriticalValue1(j,eps1);
        for i=1:maxStructuralChanges
            wftest(i,1)=cv(numRegressorsZ,1)*ftest(i,1)/cv(numRegressorsZ,1);
        end
        
        disp('--------------------------------------')
        disp(['The WDmax test at the ' num2str(significanceLevel(j,1)) '% level is: ' num2str(max(wftest))])
    end
    disp(' ');
    disp(' ');
    
end

%% The following calls the procedure that estimates the supF(l+1|l) tests where the first l breaks are taken from the global minimization
%


if isDoSuperFTest_l_plus_1==1
    disp('**************************************************************')
    disp('supF(l+1|l) tests using global optimizers under the null')
    disp('----------------------------------------------------------------')
    for i=1:maxStructuralChanges-1
        [supfl, ndat]=superF_lPlus1_l(bigvec,datevec(1:i,i),i+1,data_y,data_z,minSizeOfSegment,numRegressorsZ,isApplyAR1,isHeterogeneityAndAutocorrelation,data_x,numRegressorsX,isConstructeForFTest,hetvar);
        disp(['The supF(' num2str(i+1) '|' num2str(i) ') test is ' num2str(supfl)])
        disp(['It corresponds to a new break at: ' num2str(ndat)])
    end
    disp('***************************************************************')
    
    for j=1:4
        cv=getCriticalValue2(j,eps1);
        disp(['The critical values of supF(l+1|l) at the ' num2str(significanceLevel(j,1)) '% level are (for i=1 to ' num2str(maxStructuralChanges) ' are: ' num2str(cv(numRegressorsZ,1:maxStructuralChanges))])
    end
    disp(' ');
    disp(' ');
    
end


%% The following calls the procedure to estimate the number of breaks using the information criteria BIC and LWZ (Liu, Wu and Zidek). It returns the two orderes selected.
%

if doorder==1
    disp('*****************************************************************')
    disp('Output from the application of Information criteria')
    disp('---------------------------------------------------------------------')
    
    if numRegressorsX==0
        zz=data_z;
    else
        zz=[data_z data_x];
    end
    ssrzero=ssrnul(data_y,zz);
    [mbic mlwz]=order(ssrzero,glob,numEffectiveSample,maxStructuralChanges,numRegressorsZ);
    disp(' ');
    disp(' ');
end


%% The following section calls the sequential procedure that estimates each break one at a time. It stops when the supF(l+1|l) test is not significant. It returns the number of breaks found and the break dates. Note that it can be used independently of the other procedures, i.e. global minimizers need not be obtained since it used a method to compute the breaks in O(T) operations.
% 通过序贯检验的方法寻找间断点
if isEstimateBreaksSequentialy==1
    
    nbreak=zeros(4,1);
    dateseq=zeros(4,maxStructuralChanges);
    
    for j=1:4
        disp('********************************************************************')
        disp(['Output from the sequential procedure at significance level ' num2str(significanceLevel(j,1)) '%'])
        disp('-----------------------------------------------------------------------')
        
        [nbr datese]=sequa(maxStructuralChanges,j,numRegressorsZ,minSizeOfSegment,numEffectiveSample,isHeterogeneityAndAutocorrelation,isApplyAR1,data_z,data_y,data_x,numRegressorsX,isConstructeForFTest,hetvar,eps1);
        disp('------------------------------------------------------------')
        disp(['The sequential procedure estimated the number of breaks at: ' num2str(nbr)])
        if nbreak>=1
            disp(['The break dates are: ' num2str(datese)])
        else
        end
        nbreak(j,1)=nbr;
        
        if nbr~=0
            dateseq(j,1:nbreak(j,1))=datese';
        end
    end
    disp(' ');
    disp(' ');
end

%% The following procedure constructs the so-called repartition
% estimates of the breaks obtained by the sequential method (see Bai
% (1995), Estimating Breaks one at a time, Econometric Theory, 13,
% 315-352. It alows estimates that have the same asymptotic
% distribution as those obtained by global minimization. Otherwise, the
% output from the procedure "estim" below do not deliver asymptotically
% correct confidence intervals for the break dates.
% 重新分配间断点的过程。

if isDoRepartition==1
    
    reparv=zeros(4,maxStructuralChanges);
    for j=1:4
        disp('***********************************************')
        disp(['Output from the repartition procedure for the ' num2str(significanceLevel(j,1)) '% significance level'])
        
        if nbreak(j,1)==0
            disp('**************************************************')
            disp('The sequential procedure found no break and ')
            disp('the repartition procedure is skipped.')
            disp('**************************************************')
        else
            repartda=preparti(data_y,data_z,nbreak(j,1),dateseq(j,1:nbreak(j,1))',minSizeOfSegment,data_x,numRegressorsX);
            reparv(j,1:nbreak(j,1))=repartda';
        end
    end
    disp(' ');
    disp(' ');
end

%% The following calls a procedure which estimates the model using as the
% number of breaks the value specified in the first argument. It also calls
% a procedure to calculate standard errors in a way that depends on the
% specification for isHeterogeneityAndAutocorrelation and returns asymptotic confidence intervals for
% the break dates.
% 通过给定的方法得到了间断点，和间断点的方程，而后对该分段方程进行评估，得到一系列指标输出。


if estimbic==1
    disp('******************************************************')
    disp('Output from the estimation of the model selected by BIC')
    disp('------------------------------------------------------------')
    estim(mbic,numRegressorsZ,data_z,data_y,datevec(:,mbic),isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,data_x,numRegressorsX,isConstructeForFTest,hetvar)
end

if estimlwz==1
    disp('******************************************************')
    disp('Output from the estimation of the model selected by LWZ')
    disp('--------------------------------------------------------------')
    estim(mlwz,numRegressorsZ,data_z,data_y,datevec(:,mlwz),isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,data_x,numRegressorsX,isConstructeForFTest,hetvar)
end

if estimseq==1
    disp('******************************************************')
    ii=0;
    j=1;
    while j <= 4
        if ii==0
            if nbreak(j,1)~=0
                disp(['Output from the estimation of the model selected by the sequential method at significance level ' num2str(significanceLevel(j,1)) '%'])
                disp('----------------------------------------------------------')
                estim(nbreak(j,1),numRegressorsZ,data_z,data_y,dateseq(j,1:nbreak(j,1))',isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,data_x,numRegressorsX,isConstructeForFTest,hetvar);
            end
        end
        
        j=j+1;
        
        if j<=4
            if nbreak(j,1)==nbreak(j-1,1)
                if nbreak(j,1)==0
                    disp(['For the ' num2str(significanceLevel(j,1)) '% level, the model is the same as for the ' num2str(significanceLevel(j-1,1)) '% level.'])
                    disp('The estimation is not repeated.')
                    disp('----------------------------------------------------------')
                    ii=1;
                else
                    if dateseq(j,1:nbreak(j,1))==dateseq(j-1,1:nbreak(j-1,1));
                        disp(['For the ' num2str(significanceLevel(j,1)) '% level, the model is the same as for the ' num2str(significanceLevel(j-1,1)) '% level.'])
                        disp('The estimation is not repeated.')
                        disp('-------------------------------------------------------')
                        ii=1;
                    end
                end
            end
        else
            ii=0;
        end
    end
end


if estimrep==1
    ii=0
    disp('*******************************************')
    
    while j<=4
        if ii==0
            if nbreak(j,1)==0
                disp('***********************')
                disp(['The sequential procedure at the significance level ' num2str(significanceLevel(j,1)) '% found no break and '])
                disp('the repartition procedure was skipped.')
                disp('*******************************************')
            else
                disp('Output from the estimation of the model selected by the')
                disp(['repartition method from the sequential procedure at the significance level' num2str(significanceLevel(j,1)) '%'])
                disp('-------------------------------------------------')
                estim(nbreak(j,1),numRegressorsZ,data_z,data_y,reparv(j,1:nbreak(j,1))',isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,data_x,numRegressorsX,isConstructeForFTest,hetvar);
            end
        end
        
        j=j+1;
        if j<=4
            if nbreak(j,1)==nbreak(j-1,1);
                if nbreak(j,1)==0
                    disp(['For the' num2str(significanceLevel(j,1)) '% level, the model is the same as for the ' num2str(significanceLevel(j-1,1)) '% level.'])
                    disp('The estimation is not repeated.')
                    disp('-----------------------------------------------------')
                    ii=1;
                else
                    if dateseq(j,1:nbreak(j,1))==dateseq(j-1,1:nbreak(j-1,1))
                        disp(['For the ' num2str(significanceLevel(j,1)) '% level, the model is the same as for the ' num2str(significanceLevel(j-1,1)) '% level.'])
                        disp('The estimation is not repeated.')
                        disp('---------------------------------------------------------')
                        ii=1;
                    end
                end
            else
                ii=0;
            end
        end
    end
end

if estimfix==1
    disp('**********************************************************')
    disp(['Output from the estimation of the model with ' num2str(fixn) 'breaks'])
    disp('------------------------------------------------------------------')
    estim(fixn,numRegressorsZ,data_z,data_y,datevec(:,fixn),isHeterogeneityAndAutocorrelation,isApplyAR1,hetomega,hetq,data_x,numRegressorsX,isConstructeForFTest,hetvar);
end

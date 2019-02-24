function [mBIC, mLWZ]=order(ssrzero,globl,numEffectiveSample,maxStructuralChanges,numRegressorsZ)

% Determination of the order using BIC and the criterion of Liu, Wu and
% Zidek

glob=zeros(maxStructuralChanges+1,1);
glob(1,1)=ssrzero;
glob(2:maxStructuralChanges+1,1)=globl;

bic=zeros(maxStructuralChanges+1,1);
lwz=zeros(maxStructuralChanges+1,1);

for i= 1:maxStructuralChanges+1
    bic(i,1)=log(glob(i,1)/numEffectiveSample)+log(numEffectiveSample)*(i-1)*(numRegressorsZ+1)/numEffectiveSample;
    lwz(i,1)=log(glob(i,1)/(numEffectiveSample-i*numRegressorsZ-i+1))...
        +((i-1)*(numRegressorsZ+1)*.299*(log(numEffectiveSample))^2.1)/numEffectiveSample;
    
        disp(['With ' num2str(i-1) ' breaks: ']);
        disp(['BIC= ' num2str(bic(i,1))]) ;
        disp(['LWZ= ' num2str(lwz(i,1))]);

end
        

[dummy mbic]=min(bic);
[dummy mlwz]=min(lwz);

disp(['The number of breaks chosen by BIC is : ' num2str(mbic-1)]);
disp(['The number of breaks chosen by LWZ is : ' num2str(mlwz-1)]);

  mBIC=mbic-1;
  mLWZ=mlwz-1;



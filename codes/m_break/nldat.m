function [glb,datevec,bigvec]=nldat(data_y,data_z,data_x,minSizeOfSegment,maxStructuralChanges,numRegressorsX,numRegressorsZ,numEffectiveSample,isFixedInitialVallueOfBeta,criterionForConvergence,maxi,betaInit,printd)

glb=zeros(maxStructuralChanges,1);
globalOfNL=zeros(maxStructuralChanges,1);
datevec=zeros(maxStructuralChanges,maxStructuralChanges);
dateOfNL=zeros(maxStructuralChanges,maxStructuralChanges);

mi=1;
while mi <= maxStructuralChanges
    if printd==1
        disp('Breaks of model');
        disp(mi);
    end
    
    if isFixedInitialVallueOfBeta==0
        qq=numRegressorsX+numRegressorsZ;
        zz=[data_x data_z];
        
        [globalOfNL,dateOfNL,bigvec]=dating(data_y,zz,minSizeOfSegment,mi,qq,numEffectiveSample);
        
        xbar=pzbar(data_x,mi,dateOfNL(1:mi,mi));
        zbar=pzbar(data_z,mi,dateOfNL(1:mi,mi));
        teta=olsqr(data_y,[zbar xbar]);
        delta1=teta(1:numRegressorsZ*(mi+1),1);
        beta1=olsqr(data_y-zbar*delta1,data_x);
        
        % Calculate SSR
        ssr1=(data_y-data_x*beta1-zbar*delta1)'*(data_y-data_x*beta1-zbar*delta1);
        
        if printd==1
            disp('The iterations are initialized with;');
            disp(' ');
            disp('Break date:');
            disp('delta1:');
            disp(delta1);
            disp('beta1:');
            disp(beta1);
        end
    else
        beta1=betaInit;
        ssr1=-5;
    end
    
    % Starting the iterations.
    if printd==1
        disp('Output from the iterations');
    end

    length=99999999.0;
    i=1;
    
    while length > criterionForConvergence
        
        [globalOfNL,dateOfNL,bigvec]=dating(data_y-data_x*beta1,data_z,minSizeOfSegment,mi,numRegressorsZ,numEffectiveSample);
        
        zbar=pzbar(data_z,mi,dateOfNL(1:mi,mi));
        teta1=olsqr(data_y,[data_x zbar]);
        beta1=teta1(1:numRegressorsX,1);
        delta1=teta1(numRegressorsX+1:numRegressorsX+numRegressorsZ*(mi+1),1);
        ssrn=(data_y-[data_x zbar]*teta1)'*(data_y-[data_x zbar]*teta1);
        
        % Calculate overall SRR and check if significantly smaller.
        
        length=abs(ssrn-ssr1);
        
        if printd==1
            disp(['Iteration ' num2str(i)]);
        end
        
        if i >= maxi
            disp('The number of iterations has reached the upper limit');
        else
            i=i+1;
            ssr1=ssrn;
            glb(mi,1)=ssrn;
            datevec(1:mi,mi)=dateOfNL(1:mi,mi);
        end
        
    end
    mi=mi+1;
end

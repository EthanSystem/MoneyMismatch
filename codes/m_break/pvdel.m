function vdel=pvdel(data_y,data_z,i,numRegressorsZ,numEffectiveSample,b,prewhit,robust,x,numRegressorsX,withb,hetdat,hetvar)

% procedure that compute the covariance matrix of the estimates delta.

ev=ones(i+1,1);
zbar=pzbar(data_z,i,b);

if numRegressorsX==0
    delv=olsqr(data_y,zbar);
    res=data_y-zbar*delv;
    reg=zbar;
else
    delv=olsqr(data_y,[zbar x]);
    res=data_y-[zbar x]*delv;
    
    if withb==0
        reg=zbar-x*inv(x'*x)*x'*zbar;
    else
        reg=[x zbar];
    end
end

if robust==0
    % section on testing with no serial correlation in errors
    
    if numRegressorsX==0
        if hetdat==1 && hetvar==0
            sig=res'*res/numEffectiveSample;
            vdel=sig*inv(reg'*reg);
        end
        if hetdat==1 && hetvar==1
            sig=psigmq(res,b,numRegressorsZ,i,numEffectiveSample);
            vdel=kron(sig,eye(numRegressorsZ))*inv(reg'*reg);
        end
        if hetdat==0 && hetvar==0
            lambda=plambda(b,i,numEffectiveSample);
            sig=res'*res/numEffectiveSample;
            vdel=sig*inv(kron(lambda,(data_z'*data_z)));
        end
        if hetdat==0 && hetvar==1
            lambda=plambda(b,i,numEffectiveSample);
            sig=psigmq(res,b,numRegressorsZ,i,numEffectiveSample);
            vdel=kron(sig,eye(numRegressorsZ))*inv(kron(lambda,(data_z'*data_z)));
        end
        
    else
        
        if hetdat==0
            disp('the case hetdat=0 is not allowed.');
            disp('vdel is returned as zeros.');
            vdel=zeros(numRegressorsZ*(i+1),numRegressorsZ*(i+1));
        end
        
        if hetdat==1 && hetvar==0
            sig=res'*res/numEffectiveSample;
            vdel=sig*inv(reg'*reg);
        end
        
        if hetdat==1 && hetvar==1
            wbar=pzbar(reg,i,b);
            ww=wbar'*wbar;
            sig=psigmq(res,b,numRegressorsZ,i,numEffectiveSample);
            gg=zeros((i+1)*numRegressorsZ+numRegressorsX*withb, (i+1)*numRegressorsZ+numRegressorsX*withb);
            ie=1;
            while ie<=i+1
                gg=gg+sig(ie,ie)*ww((ie-1)*((i+1)*numRegressorsZ+numRegressorsX*withb)+1:ie*((i+1)*numRegressorsZ+...
                    numRegressorsX*withb),(ie-1)*((i+1)*numRegressorsZ+numRegressorsX*withb)+1:ie*((i+1)*numRegressorsZ+numRegressorsX*withb));
                ie=ie+1;
            end
            vdel=inv(reg'*reg)*gg*inv(reg'*reg);
        end
    end
    
else
    if hetdat==0
        disp('the case hetdat=0 is not allowed');
        disp('vdel is returned as zeros.');
        vdel=zeros(numRegressorsZ*(i+1),numRegressorsZ*(i+1));
    end
    
    if numRegressorsX==0
        if hetvar==1
            hac=zeros((i+1)*numRegressorsZ,(i+1)*numRegressorsZ);
            vdel=zeros((i+1)*numRegressorsZ,(i+1)*numRegressorsZ);
            hac(1:numRegressorsZ,1:numRegressorsZ)=b(1,1)*correct(data_z(1:b(1,1),:),res(1:b(1,1),1),prewhit);
            for j=2:i
                hac((j-1)*numRegressorsZ+1:j*numRegressorsZ,(j-1)*numRegressorsZ+1:j*numRegressorsZ)=(b(j,1)-b(j-1,1))*correct(data_z(b(j-1,1)+1:b(j,1),:),...
                    res(b(j-1,1)+1:b(j,1),1),prewhit);
            end
            
            hac(i*numRegressorsZ+1:(i+1)*numRegressorsZ,i*numRegressorsZ+1:(i+1)*numRegressorsZ)=(numEffectiveSample-b(i,1))*correct(data_z(b(i,1)+1:numEffectiveSample,:),...
                res(b(i,1)+1:numEffectiveSample,1), prewhit);
            
            vdel=inv(reg'*reg)*hac*inv(reg'*reg);
        else
            hac=correct(data_z,res,prewhit);
            lambda=plambda(b,i,numEffectiveSample);
            vdel=numEffectiveSample*inv(reg'*reg)*kron(lambda,hac)*inv(reg'*reg);
        end
        
    else
        
         hac=correct(reg,res,prewhit);
         vdel=numEffectiveSample*inv(reg'*reg)*hac*inv(reg'*reg);

        
    end
end

        
            
            
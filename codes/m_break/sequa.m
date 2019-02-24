function [nbreak,dv0]=sequa(maxStructuralChanges,signif,numRegressorsZ,minSizeOfSegment,numEffectiveSample,robust,prewhit,data_z,data_y,x,numRegressorsX,hetdat,hetvar,eps1)

% This procedure applies the sequential procedure to obtain the number of
% breaks and the break dates. The current version only allows pure
% structural changes. This will be generalized.
% Here maxStructuralChanges is the maximum number of breaks allowed.

dv=zeros(maxStructuralChanges+2,1);
dv2=zeros(maxStructuralChanges+2,1);
ftestv=zeros(maxStructuralChanges+1,1);

cv=getCriticalValue2(signif,eps1);
dv(1,1)=0;

if numRegressorsX==0
    vssrev=ssr(1,rot90(rot90(data_y)),rot90(rot90(data_z)),minSizeOfSegment,numEffectiveSample);
    vssr=ssr(1,data_y,data_z,minSizeOfSegment,numEffectiveSample);
    [ssrmin,datx]=partione(minSizeOfSegment,numEffectiveSample-minSizeOfSegment,numEffectiveSample,vssr,vssrev);
else
    [ssrmin,datx]=onebp(data_y,data_z,x,minSizeOfSegment,1,numEffectiveSample);
end

dv(2,1)=datx;

ftest=pftest(data_y,data_z,1,numRegressorsZ,numEffectiveSample,dv(2,1),prewhit,robust,x,numRegressorsX,hetdat,hetvar);

if ftest < cv(numRegressorsZ,1)
    nbreak=0;
    dv(2,1)=0;
    dv0=dv(2:nbreak+1,1);
    nseg=1;
else
    disp(['The first break found is at: ' num2str(datx)]);
    nbreak=1;
    nseg=2;
    dv(nseg+1,1)=numEffectiveSample;
end

while nseg <= maxStructuralChanges
    ds=zeros(nseg+1,1);
    ftestv=zeros(nseg+1,1);
    
    is=1;
    
    while is <= nseg
        length=dv(is+1,1)-dv(is,1);
        
        if length >= 2*minSizeOfSegment
            
            if numRegressorsX==0
                vssr=ssr(1,data_y(dv(is,1)+1:dv(is+1,1),1),...
                    data_z(dv(is,1)+1:dv(is+1,1),:),minSizeOfSegment,length);
                vssrev=ssr(1,rot90(rot90(data_y(dv(is,1)+1:dv(is+1,1),1))),...
                    rot90(rot90(data_z(dv(is,1)+1:dv(is+1,1),:))),minSizeOfSegment,length);
                [dum,ds(is,1)]=partione(minSizeOfSegment,length-minSizeOfSegment,length,vssr,vssrev);
                ftestv(is,1)=pftest(data_y(dv(is,1)+1:dv(is+1,1),1),...
                    data_z(dv(is,1)+1:dv(is+1,1),:),1,numRegressorsZ,length,ds(is,1),...
                    prewhit,robust,0,numRegressorsX,hetdat,hetvar);
            else
                [dum,ds(is,1)]=onebp(data_y,data_z,x,minSizeOfSegment,dv(is,1)+1,dv(is+1,1));
                ds(is,1)=ds(is,1)-dv(is,1);
                ftestv(is,1)=pftest(data_y(dv(is,1)+1:dv(is+1,1),1),...
                    data_z(dv(is,1)+1:dv(is+1,1),:),1,numRegressorsZ,length,ds(is,1),...
                    prewhit,robust,x(dv(is,1)+1:dv(is+1,1),:),numRegressorsX,hetdat,hetvar);
            end
            
        else
            ftestv(is,1)=0.0;
        end
        
        is=is+1;
    end
    
    maxf=max(ftestv(1:nseg,1));
    
    if maxf < cv(numRegressorsZ,nseg)
        nbreak=nbreak;
        dv0=dv(2:nbreak+1,1);
    else
        [dum,newseg]=max(ftestv(1:nseg,1));
        disp(['The next break found is at: ' num2str(ds(newseg,1)+dv(newseg,1))]);
        
        dv(nseg+2,1)=ds(newseg,1)+dv(newseg,1);
        nbreak=nbreak+1;
        dv2=sort(dv(2:nseg+2,1));
        dv(1,1)=0;
        dv(2:nseg+2,1)=dv2;
    end
    
    nseg=nseg+1;
end

disp(['The sequential procedure has reached the upper limit'])

dv0=dv(2:nbreak+1,1);

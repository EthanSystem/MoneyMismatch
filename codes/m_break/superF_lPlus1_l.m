function [maxf, newd]=superF_lPlus1_l(bigvec,dt,numSegmentation,data_y,data_z,minSizeOfSegment,numRegressorsZ,prewhit,robust,x,numRegressorsX,hetdat,hetvar)

% procedure that computes the supF(l+1|l) test l=nseg-1. The l breaks under
% the null are taken from the global minimization (in dt).

ssr=zeros(numSegmentation,1);
ftestv=zeros(numSegmentation,1);
numEffectiveSample=size(data_z,1);
ftestv=zeros(numSegmentation,1);
dv=zeros(numSegmentation+1,1);
dv(2:numSegmentation,1)=dt;
dv(numSegmentation+1,1)=numEffectiveSample;
ds=zeros(numSegmentation,1);

in=0;

for is=1:numSegmentation
    length=dv(is+1,1)-dv(is,1);
    
    if length >= 2*minSizeOfSegment
        if numRegressorsX==0
            [ssr(is,1), ds(is,1)]=parti(dv(is,1)+1,...
                dv(is,1)+minSizeOfSegment,dv(is+1,1)-minSizeOfSegment,dv(is+1,1),bigvec,numEffectiveSample);
            ftestv(is,1)=pftest(data_y(dv(is,1)+1:dv(is+1,1),1), data_z(dv(is,1)+1:dv(is+1,1),:),...
                1,numRegressorsZ,length,ds(is,1)-dv(is,1),prewhit,robust,0,numRegressorsX,hetdat,hetvar);
        else
            [ssr(is,1), ds(is,1)]=onebp(data_y,data_z,x,minSizeOfSegment,dv(is,1)+1,dv(is+1,1));
            ftestv(is,1)=pftest(data_y(dv(is,1)+1:dv(is+1,1),1), data_z(dv(is,1)+1:dv(is+1,1),:),...
                1,numRegressorsZ,length,ds(is,1)-dv(is,1),prewhit,robust,x(dv(is,1)+1:dv(is+1,1),:),...
                numRegressorsX,hetdat,hetvar);
        end
        
    else
        in=in+1;
        ftestv(is,1)=0.0;
    end
end

if in == numSegmentation
    disp(['Given the location of the breaks from the global optimization with ' ...
        num2str(1) ' breaks there was no more place to insert' ...
        ' an additional breaks that satisfy the minimal length requirement.']);
end

[maxf,news]=max(ftestv(1:numSegmentation,1));
newd=ds(news,1);



        
            

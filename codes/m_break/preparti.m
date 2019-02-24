function dr=preparti(data_y,data_z,nbreak,dateseq,minSizeOfSegment,x,numRegressorsX)

% procedure that constructs modified sequential estimates using the
% repartition method of Bai (1995), Estimating Breaks one at a time.
numEffectiveSample=size(data_z,1);
numRegressorsZ=size(data_z,2);

dv=zeros(nbreak+2,1);

dv(1,1)=0;
dv(2:nbreak+1,1)=dateseq;

dv(nbreak+2,1)=numEffectiveSample;
ds=zeros(nbreak,1);
dr=zeros(nbreak,1);

for is=1:nbreak
    
    length=dv(is+2,1)-dv(is,1);
    
    if numRegressorsX==0
        vssr=ssr(1,data_y(dv(is,1)+1:dv(is+2,1),1),...
            data_z(dv(is,1)+1:dv(is+2,1),:),minSizeOfSegment,length);
        vssrev=ssr(1,rot90(rot90(data_y(dv(is,1)+1:dv(is+2,1),1) )),...
            rot90(rot90(data_z(dv(is,1)+1:dv(is+2,1),:))),minSizeOfSegment,length);
        [dummy ds(is,1)]=partione(minSizeOfSegment,length-minSizeOfSegment,length,vssr,vssrev);
        dr(is,1)=ds(is,1)+dv(is,1);
    else
        [dummy ds(is,1)]=onebp(data_y,data_z,x,minSizeOfSegment,dv(is,1)+1,dv(is+2,1));
        dr(is,1)=ds(is,1);
    end
    
end

function [glb datevec bigvec]=dating(data_y,data_z,minSizeOfSegment,maxStructuralChanges,numRegressorsZ,numEffectiveSample)

% This is the main procedure which calculates the break points that globally
% minimizes the SSR. It returns optimal dates and associated SSR for all
% numbers of breaks less than or equal to maxStructuralChanges.

datevec=zeros(maxStructuralChanges,maxStructuralChanges);
optimalDate=zeros(numEffectiveSample,maxStructuralChanges);
optimalSSR=zeros(numEffectiveSample,maxStructuralChanges);
dvec=zeros(numEffectiveSample,1);
glb=zeros(maxStructuralChanges,1);
bigvec=zeros(numEffectiveSample*(numEffectiveSample+1)/2,1);

% 计算时间序列在序列头和序列尾两两组合的情况下所有序列集合的SSR值
for i=1:numEffectiveSample-minSizeOfSegment+1
    vectorSSR=ssr(i,data_y,data_z,minSizeOfSegment,numEffectiveSample);
    bigvec((i-1)*numEffectiveSample+i-(i-1)*i/2:i*numEffectiveSample-(i-1)*i/2,1)=vectorSSR(i:numEffectiveSample,1);
end

% 根据最小的SSR值对应的序列，遴选出最优的序列作为间断点分隔的序列，计算出该序列的SSR值，该序列的末端作为第一个间断点
if maxStructuralChanges==1
    [ssrmin,datx]=parti(1,minSizeOfSegment,numEffectiveSample-minSizeOfSegment,numEffectiveSample,bigvec,numEffectiveSample);
    datevec(1,1)=datx;
    glb(1,1)=ssrmin;
else
    
    for j1=2*minSizeOfSegment:numEffectiveSample
        [ssrmin, datx]=parti(1,minSizeOfSegment,j1-minSizeOfSegment,j1,bigvec,numEffectiveSample);
        optimalSSR(j1,1)=ssrmin;
        optimalDate(j1,1)=datx;
    end
    % 记录该间断点的SSR值和点标号
    glb(1,1)=optimalSSR(numEffectiveSample,1);
    datevec(1,1)=optimalDate(numEffectiveSample,1);
    
    % 在接下来的子区间内遴选出其余的间断点
    for ib=2:maxStructuralChanges
        if ib==maxStructuralChanges
            jlast=numEffectiveSample;
            for jb=ib*minSizeOfSegment:jlast-minSizeOfSegment
                dvec(jb,1)=optimalSSR(jb,ib-1)+bigvec((jb+1)*numEffectiveSample-jb*(jb+1)/2,1);
            end
            optimalSSR(jlast,ib)=min(dvec(ib*minSizeOfSegment:jlast-minSizeOfSegment,1))';
            [dummy, minindcdvec]=min(dvec(ib*minSizeOfSegment:jlast-minSizeOfSegment,1));
            optimalDate(jlast,ib)=(ib*minSizeOfSegment-1)+minindcdvec';
            
        else
            
            for jlast=(ib+1)*minSizeOfSegment:numEffectiveSample
                for jb =ib*minSizeOfSegment: jlast-minSizeOfSegment
                    dvec(jb,1)=optimalSSR(jb,ib-1)+bigvec(jb*numEffectiveSample-jb*(jb-1)/2+jlast-jb,1);
                end 
                optimalSSR(jlast,ib)=min(dvec(ib*minSizeOfSegment:jlast-minSizeOfSegment,1));
                [dummy,minindcdvec]=min(dvec(ib*minSizeOfSegment:jlast-minSizeOfSegment,1));
                optimalDate(jlast,ib)=(ib*minSizeOfSegment-1)+minindcdvec';
            end
        end
        
        datevec(ib,ib)=optimalDate(numEffectiveSample,ib);
        
        for i=1:ib-1
            xx=ib-i;
            datevec(xx,ib)=optimalDate(datevec(xx+1,ib),xx);
        end
        glb(ib,1)=optimalSSR(numEffectiveSample,ib);
    end
end



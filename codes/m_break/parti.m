function [ssrmin, dx]=parti(start,firstPossibleBreakPoint,lastPossibleBreakPoint,last,bigvec,numEffectiveSample)

% procedure to obtain an optimal one break partitions for a segment that
% starts at date start and ends at date last. It returns the optimal break
% and the associated SSR.

% start: beginning of the segment considered
% firstPossibleBreakPoint: first possible break date
% lastPossibleBreakPoint: last possible break date
% last: end of segment considered

dvec=zeros(numEffectiveSample,1);
ini=(start-1)*numEffectiveSample-(start-2)*(start-1)/2+1;
j=firstPossibleBreakPoint;
kk=zeros(numEffectiveSample,1);
while j <= lastPossibleBreakPoint
    jj=j-start;
    k=j*numEffectiveSample-(j-1)*j/2+last-j;
    kk(j)=k;
    dvec(j,1)=bigvec(ini+jj,1)+bigvec(k,1);
    j=j+1;
end
kk;
% modified at 2018-10-15 %%%%%
% ssrmin=min(dvec(firstPossibleBreakPoint:lastPossibleBreakPoint,1))';
[ssrmin, minindcdvec]=min(dvec(firstPossibleBreakPoint:lastPossibleBreakPoint,1));
% ssrmin means the minimum value of SSR.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%
dx=(firstPossibleBreakPoint-1)+minindcdvec';
% dx 表示最小的SSR值的序号
    
    

function [ssrmin,dx]=partione(firstPossibleBreakPoint,lastPossibleBreakPoint,last,vssr,vssrev)

% procedure to obtain an optimal one break partitions for a segment that
% starts at date start and ends at date last. It returns the optimal break
% and the associated SSR.
% Procedure used with the sequential method when the T*(T+1)/2 vector of SSR
% is not computed.

% start: beginning of the segment considered
% firstPossibleBreakPoint: first possible break date
% lastPossibleBreakPoint: last possible break date
% last end of segment considered

dvec=zeros(last,1);

j=firstPossibleBreakPoint;

while j <= lastPossibleBreakPoint
    dvec(j,1)=vssr(j,1)+vssrev(last-j,1);
    j=j+1;
end

[ssrmin ssrminind]=min(dvec(firstPossibleBreakPoint:lastPossibleBreakPoint,1));
dx=(firstPossibleBreakPoint-1)+ssrminind;

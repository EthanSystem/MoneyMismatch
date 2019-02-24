function [ssrind,bd]=onebp(data_y,data_z,x,minSizeOfSegment,start,last)

% procedure that computes an optimal one break partition in the case of a
% partial structural by searching over all possible breaks.

ssrind=9999999999;
i=minSizeOfSegment;

while i <= last-start+1-minSizeOfSegment
    zb=pzbar(data_z(start:last,:),1,i);
    bb=olsqr(data_y(start:last,1),[x(start:last,:) zb]);
    rr=(data_y(start:last,1)-[x(start:last,:) zb]*bb)'...
        *(data_y(start:last,:)-[x(start:last,:) zb]*bb);
    
        if rr < ssrind
            ssrind=rr;
            bdat=i;
        end
    i=i+1;
end

bd=bdat+start-1;
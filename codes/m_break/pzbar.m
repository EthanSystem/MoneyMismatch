function zb=pzbar(zz,maxStructuralChanges,bb)

% procedure to construct the diagonal partition of data_z with maxStructuralChanges break at date
% b.

[nt,q1]=size(zz);

zb=zeros(nt,(maxStructuralChanges+1)*q1);
zb(1:bb(1,1),1:q1)=zz(1:bb(1,1),:);
i=2;
while i <= maxStructuralChanges
    zb(bb(i-1,1)+1:bb(i,1),(i-1)*q1+1:i*q1)=zz(bb(i-1,1)+1:bb(i,1),:);
    i=i+1;
end
zb(bb(maxStructuralChanges,1)+1:nt,maxStructuralChanges*q1+1:(maxStructuralChanges+1)*q1)=zz(bb(maxStructuralChanges,1)+1:nt,:);

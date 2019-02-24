function ftest=pftest(data_y,data_z,i,numRegressorsZ,numEffectiveSample,datevec,prewhit,robust,x,numRegressorsX,hetdat,hetvar)

% procedure that computes the supF test for i breaks.

% construct the R matrix.

rsub=zeros(i,i+1);
j=1;
while j <= i
    rsub(j,j)=-1;
    rsub(j,j+1)=1;
    j=j+1;
end
rmat=kron(rsub,eye(numRegressorsZ));
zbar=pzbar(data_z,i,datevec(1:i,i));
if numRegressorsX==0
    delta=olsqr(data_y,zbar);
else
    dbdel=olsqr(data_y,[zbar x]);
    delta=dbdel(1:(i+1)*numRegressorsZ,1);
end

vdel=pvdel(data_y,data_z,i,numRegressorsZ,numEffectiveSample,datevec(1:i,i),prewhit,robust,x,numRegressorsX,0,hetdat,hetvar);

fstar=delta'*rmat'*inv(rmat*vdel*rmat')*rmat*delta;
ftest=(numEffectiveSample-(i+1)*numRegressorsZ-numRegressorsX)*fstar/(numEffectiveSample*i);


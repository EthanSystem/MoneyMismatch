function vectorSSR=ssr(start,data_y,data_z,minSizeOfSegment,last)

% this procedure computes recursive residuals from a data set that starts
% at date "start" and ends at date "last". It returns a vector of sum of
% squared residuals (SSR) of length last-start+1 (stored for convenience in
% a vector of length T).

% start: starting entry of the sample used.
% last: ending date of the last segment considered.
% data_y: dependent variable
% data_z: matrix of regressors of dimension numRegressorsZ
% minSizeOfSegment: minimal length of a segment

% initialize the vectors

vectorSSR=zeros(last,1);

% initialize the recursion with the first minSizeOfSegment data points.

inv1=inv(data_z(start:start+minSizeOfSegment-1,:)'*data_z(start:start+minSizeOfSegment-1,:));
delta1=inv1*(data_z(start:start+minSizeOfSegment-1,:)'*data_y(start:start+minSizeOfSegment-1,1));
res=data_y(start:start+minSizeOfSegment-1,1)-data_z(start:start+minSizeOfSegment-1,:)*delta1;
vectorSSR(start+minSizeOfSegment-1,1)=res'*res;

% loop to construct the recursive residuals and update the SSR

r=start+minSizeOfSegment;
while r <= last
    v=data_y(r,1)-data_z(r,:)*delta1;
    invz=inv1*data_z(r,:)';
    f=1+data_z(r,:)*invz;
    delta2=delta1+(invz*v)/f;
    inv2=inv1-(invz*invz')/f;
    inv1=inv2;
    delta1=delta2;
    vectorSSR(r,1)=vectorSSR(r-1,1)+v*v/f;
    r=r+1;
end

    
function b=olsqr(data_y,x)

b=inv(x'*x)*x'*data_y;

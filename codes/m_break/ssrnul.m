function ssrn=ssrnul(data_y,zz)

% Computes the SSR under the null of no break

delta=olsqr(data_y,zz);
ssrn=(data_y-zz*delta)'*(data_y-zz*delta);


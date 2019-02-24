function [EMP] = calculate_EMP(exchangeRate,RES,MB)
% calculate_EMP Summary of this function goes here
%   Detailed explanation goes here.

log_exchangeRate = log(exchangeRate);
Delta_RES = diff(RES);
Delta_EMP = diff(log_exchangeRate) + Delta_RES./MB(1:end-1);
Delta_exchangeRate = diff(exchangeRate);
beta = (Delta_exchangeRate./exchangeRate(1:end-1))./(Delta_RES./RES(1:end-1));
EMP = beta.*Delta_EMP;

end


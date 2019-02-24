function [EMP] = calculate_EMP_2(exchangeRate,RES,MB)
% calculate_EMP_2 Summary of this function goes here
%   Detailed explanation goes here.

log_exchangeRate = log(exchangeRate);
Delta_RES = diff(RES);
Delta_EMP = diff(log_exchangeRate) + Delta_RES./MB(1:end-1);
Delta_exchangeRate = diff(exchangeRate);
Delta_RES = diff(RES);
EMP = Delta_EMP;

% 【删除
% beta = (Delta_exchangeRate./exchangeRate(1:end-1))./(Delta_RES./RES(1:end-1));
% EMP = beta.*Delta_EMP;
% 】

end


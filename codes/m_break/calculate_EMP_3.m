function [Delta_EMP] = calculate_EMP_3(exchangeRate,RES,MB)
% calculate_EMP_2 Summary of this function goes here
%   Detailed explanation goes here.

log_exchangeRate = log(exchangeRate);
Delta_Log_exchangeRate = diff(log_exchangeRate);
Delta_RESdivMB = diff(RES./MB);
Delta_EMP = Delta_Log_exchangeRate + Delta_RESdivMB;

% ??????
% beta = (Delta_exchangeRate./exchangeRate(1:end-1))./(Delta_RES./RES(1:end-1));
% EMP = beta.*Delta_EMP;
% ??

end


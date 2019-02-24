function [Delta_log_exchangeRate] = calculate_DeltaAndLog_1(exchangeRate)
% calculate_DeltaAndLog Summary of this function goes here
%   Detailed explanation goes here.

log_exchangeRate = log(exchangeRate);
Delta_log_exchangeRate = diff(log_exchangeRate)./log_exchangeRate(1:end-1);

end

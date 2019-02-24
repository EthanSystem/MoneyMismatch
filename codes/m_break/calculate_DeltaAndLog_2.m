function [Delta_log_exchangeRate] = calculate_DeltaAndLog_2(exchangeRate)
% calculate_DeltaAndLog Summary of this function goes here
%   Detailed explanation goes here.

log_exchangeRate = log(exchangeRate);
Delta_log_exchangeRate = diff(log_exchangeRate);

end

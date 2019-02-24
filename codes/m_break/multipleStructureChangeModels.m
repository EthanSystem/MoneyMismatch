function [modelData] = multipleStructureChangeModels(table_inputData,weight_exchangeRate,weight_c,numEffectiveSample,numClassOfCurrency)
% multipleStructureChangeModels Summary of this function goes here
%   Detailed explanation goes here.
inputData = table2array(table_inputData(:,2:end));
[EMP] = calculate_EMP(table_inputData.CNY,table_inputData.ForeignExchangeReserve,table_inputData.ChinaBaseMoney);

for k=3:1:3+numClassOfCurrency+1-1
    log_exchangeRate(:,k) = log(inputData(:,k));
    Delta_log_exchangeRate(:,k) = diff(log_exchangeRate(:,k))./log_exchangeRate(1:end - 1,k);
    Delta_log_exchangeRate(:,1) = [];
end

for i=1:1:numEffectiveSample
    temp01(i) = (Delta_log_exchangeRate(i,2:2+numClassOfCurrency-1) - Delta_log_exchangeRate(i,2+numClassOfCurrency-1)) * weight_exchangeRate' ;
    temp03(i) = weight_c + temp01(i) + EMP(i);
end

modelData = temp03';
end


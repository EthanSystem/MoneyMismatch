
library(readxl)
library(tidyr)
library(dbplyr)
library(urca)
library(lmtest)



root_path <- getwd()

# 导入要计算货币错配程度的表格数据
# data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/ProjectsFile/Research/EconomicAndFinance/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")
data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/ProjectsFile/Research/EconomicAndFinance/MoneyMismatch/data/计算SSM的数据.xlsx", sheet = "need calculating of Chinese")

# Log 化预处理
data_need_log <- log(data_original[,c("AECM","REF","REER")])
data_logged <- cbind(data_need_log,data_original[,c("EX")])

# 单位根检验：ADF
data_AECM.adf <- adf.test(data_logged$AECM)
data_AECM.diff.adf <- adf.test(diff(data_logged$AECM))
data_REF.adf <- adf.test(data_logged$REF)
data_REF.diff.adf <- adf.test(diff(data_logged$REF))
data_REER.adf <- adf.test(data_logged$REER)
data_REER.diff.adf <- adf.test(diff(data_logged$REER))
data_EX.adf <- adf.test(data_logged$EX)
data_EX.diff.adf <- adf.test(diff(data_logged$EX))


# 接下来构建VAR模型，并做 Johnson 协整检验
ts_logged <- as.ts(data_logged)
ts_diff_logged <- diff(ts_logged)

# 确定滞后阶数
ts_diff_logged.var_select_lags <- vars::VARselect(ts_diff_logged,lag.max = 20,type = "const" )  # 结果显示，建议滞后阶数 <- 2

# 构建VAR模型
VAR_ts <- vars::VAR(ts_diff_logged,p=2,type = "const")
summary(VAR_ts)

# Johnson 协整检验
num_lags <- 1
ts_diff_logged.ca.jo <- urca::ca.jo(data_logged,ecdet = "const",type = "trace",K = num_lags )
print("trace ",num_lags)
summary(ts_diff_logged.ca.jo)
ts_diff_logged.ca.jo <- urca::ca.jo(data_logged,ecdet = "const",type = "eigen",K = num_lags )
print("eigen ",num_lags)
summary(ts_diff_logged.ca.jo)


# 格兰杰因果检验(更详细的结果见Eviews的分析)
ts_diff_logged.grangertest <- grangertest(AECM ~ EX ,data = ts_diff_logged)
ts_diff_logged.grangertest

# SSM
TODO 


























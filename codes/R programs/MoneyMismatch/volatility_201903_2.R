



### 根据导师在3月初给的资料做划分区间进行分析 ###
## 封装了重复过程。但是还是有问题，无法使用。


# 分析波动性


library(readxl)
library(tidyr)
library(dplyr)
library(urca)
library(lmtest)15
library(xlsx)

root_path <- getwd()
data_path <- "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data"
setwd(root_path)
source(paste(root_path, "program_function.R", sep = "/"))

output_data_name <- "汇率"




##### 中国的 #######

# 输入的数据的数据表名称

input_sheet_name <- "中国日度数据"

# 提取需要的区间段
data_time_keyword <-
  as.character(
    c(
      '1981-01-02',
      '1994-01-03',
      '2005-07-21',
      '2015-08-11',
      '2017-05-02',
      '2018-12-31'
    )
  )


# 输出的数据的数据表名称
output_sheet_name <- "中国"

# ## 代入程序运算
emptyValue <-
  program_function(
    input_data.sheet_name = input_sheet_name,
    output_data.sheet_name = output_sheet_name,
    output_data.data_name = output_data_name,
    data_used.time.keyword = data_time_keyword
  )










##### 印度的 #######


# 输入的数据的数据表名称
input_sheet_name <- "印度日度数据"


# 提取需要的区间段
data_time_keyword <-
  as.character(c('1973-01-02',
                 '1975-09-24',
                 '1992-03-02',
                 '2008-04-30',
                 '2018-12-31'
  ))

# 输出的数据的数据表名称
output_sheet_name <- "印度"

# ## 代入程序运算
emptyValue <-
  program_function(
    input_data.sheet_name = input_sheet_name,
    output_data.sheet_name = output_sheet_name,
    output_data.data_name = output_data_name,
    data_used.time.keyword = data.time.keyword
  )






##### 俄罗斯的 #######

# 输入的数据的数据表名称
input_sheet_name <- "俄罗斯日度数据"

# 提取需要的区间段
data_time_keyword <-
  as.character(c('1992-07-01',
                 '1995-07-05',
                 '1998-08-17',
                 '2014-11-10',
                 '2018-12-31'
  ))

# 输出的数据的数据表名称
output_sheet_name <- "俄罗斯"

# ## 代入程序运算
emptyValue <-
  program_function(
    input_data.sheet_name = input_sheet_name,
    output_data.sheet_name = output_sheet_name,
    output_data.data_name = output_data_name,
    data_used.time.keyword = data.time.keyword
  )





##### 巴西的 #######

# 输入的数据的数据表名称
input_sheet_name <- "巴西日度数据"

# 提取需要的区间段
data_time_keyword <-
  as.character(c('1984-12-03',
                 '1992-05-05',
                 '1994-07-01',
                 '1999-02-01',
                 '2018-12-31'
  ))

# 输出的数据的数据表名称
output_sheet_name <- "巴西"

# ## 代入程序运算
emptyValue <-
  program_function(
    input_data.sheet_name = input_sheet_name,
    output_data.sheet_name = output_sheet_name,
    output_data.data_name = output_data_name,
    data_used.time.keyword = data.time.keyword
  )













#
#
# ####### delete ##################
# # Log 化预处理
# data_need_log <- log(data_used[, c("data")])
# data_logged <- cbind(data_used[, c("time")], data_need_log)
#
# # 单位根检验：ADF
# data_AECM.adf <- adf.test(data_logged$AECM)
# data_AECM.diff.adf <- adf.test(diff(data_logged$AECM))
# data_REF.adf <- adf.test(data_logged$REF)
# data_REF.diff.adf <- adf.test(diff(data_logged$REF))
# data_REER.adf <- adf.test(data_logged$REER)
# data_REER.diff.adf <- adf.test(diff(data_logged$REER))
# data_EX.adf <- adf.test(data_logged$EX)
# data_EX.diff.adf <- adf.test(diff(data_logged$EX))
#
#
# # 接下来构建VAR模型，并做 Johnson 协整检验
# ts_logged <- as.ts(data_logged)
# ts_diff_logged <- diff(ts_logged)
#
# # 确定滞后阶数
# ts_diff_logged.var_select_lags <-
#   vars::VARselect(ts_diff_logged, lag.max = 20, type = "const") # 结果显示，建议滞后阶数 <- 2
#
# # 构建VAR模型
# VAR_ts <- vars::VAR(ts_diff_logged, p = 2, type = "const")
# summary(VAR_ts)
#
# # Johnson 协整检验
# num_lags <- 1
# ts_diff_logged.ca.jo <-
#   urca::ca.jo(data_logged,
#               ecdet = "const",
#               type = "trace",
#               K = num_lags)
# print("trace ", num_lags)
# summary(ts_diff_logged.ca.jo)
# ts_diff_logged.ca.jo <-
#   urca::ca.jo(data_logged,
#               ecdet = "const",
#               type = "eigen",
#               K = num_lags)
# print("eigen ", num_lags)
# summary(ts_diff_logged.ca.jo)
#
#
# # 格兰杰因果检验(更详细的结果见Eviews的分析)
# ts_diff_logged.grangertest <-
#   grangertest(AECM ~ EX, data = ts_diff_logged)
# ts_diff_logged.grangertest
#
# # SSM
# TODO

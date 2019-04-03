








##### 根据导师在3月初给的资料做划分区间进行分析 ###
##### 分析波动性

##### 预处理 ########
library(readxl)
library(tidyr)
library(dplyr)
library(urca)
library(lmtest)
library(xlsx)
library(forecast)
library(tseries)

root_path <- getwd()
data_path <-
  "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data"
setwd(root_path)
source(paste(root_path, "/program_function.R", sep = ""))

output_data.data_name <- "汇率"
input_data.data_source_name <- "BIS"
output_data.project_name <- "自动生成的"





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
      '2010-06-21',
      '2015-08-11',
      '2018-12-31'
    )
  )


# 输出的数据的数据表名称
output_sheet_name <- "中国"



###### 重复代码段 ######
input_data.sheet_name = input_sheet_name
output_data.sheet_name = output_sheet_name
data_used.time.keyword = data_time_keyword
# 导入要计算货币错配程度的表格数据
data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = input_data.sheet_name,
    col_types = c("date", "numeric")
  )


# 去掉无用数据
data_used <- subset(data_original,
                    data != 'NaN')
data_used <- subset(data_used, data != '')


# 调整日期的格式
data_used$time <- as.character.Date(data_used$time)

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.gradient$data <-
  diff(data_used$data, lag = 1, differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
data_used.gradient.timeRange.index[1] <-
  which(as.character.Date(data_used$time) == data_used.time.keyword[1])
for (i in 2:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i]) -
    1
}
data_used.gradient.time.keyword <-
  data_used.gradient$time[data_used.gradient.timeRange.index]

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.rate_of_change$data <-
  data_used.gradient$data / data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <-
  data_used.gradient.timeRange.index
data_used.rate_of_change.time.keyword <-
  data_used.rate_of_change$time[data_used.rate_of_change.timeRange.index]

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)

# 样本数据的标准差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的标准差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的标准差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# ACF、PACF、自动化ARMA的变量定义
data_used.acf <- list()
data_used.pacf <- list()
data_used.gradient.acf <- list()
data_used.gradient.pacf <- list()
data_used.autoARMA <- list()
data_used.gradient.autoARMA <- list()
data_used.RandomWalk <-
  list(lm = list(),
       summary = list(),
       confint = list())
data_used.RandomWalk.Box_test <- list()


# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     sd(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     sd(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     sd(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_temp <-
    data_used$data[(data_used.timeRange.index[j]):(data_used.timeRange.index[j + 1] - 1)]
  data_gradient_temp <-
    data_used.gradient$data[(data_used.gradient.timeRange.index[j] + 1):(data_used.gradient.timeRange.index[j + 1] - 1)]
  data_rate_of_change_temp <-
    data_used.rate_of_change$data[(data_used.rate_of_change.timeRange.index[j] + 1):(data_used.rate_of_change.timeRange.index[j + 1] - 1)]
  
  data_used.mean[j] <- mean(data_temp, na.rm = FALSE)
  data_used.variance[j] <-
    sd(data_temp, na.rm = FALSE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_gradient_temp, na.rm = FALSE)
  data_used.gradient.variance[j] <-
    sd(data_gradient_temp, na.rm = FALSE)
  data_used.rate_of_change.mean[j] <-
    mean(data_rate_of_change_temp, na.rm = FALSE)
  data_used.rate_of_change.variance[j] <-
    sd(data_rate_of_change_temp, na.rm = FALSE)
  
  # ARMA模型
  # 自相关性和偏相关性检验
  data_used.acf[[j]] <- acf(data_temp)
  data_used.acf[[j]]
  acf(data_temp)
  data_used.pacf[[j]] <- pacf(data_temp)
  data_used.pacf[[j]]
  pacf(data_temp)
  data_used.gradient.acf[[j]] <- acf(data_gradient_temp)
  data_used.gradient.acf[[j]]
  acf(data_gradient_temp)
  data_used.gradient.pacf[[j]] <- pacf(data_gradient_temp)
  data_used.gradient.pacf[[j]]
  pacf(data_gradient_temp)
  
  # 自动化ARMA模型
  data_used.autoARMA[[j]] <-
    forecast::auto.arima(data_temp)
  data_used.autoARMA[[j]]
  
  # 判别是否符合随机游走
  
  # # 因为存在自相关性和异方差性，因此不能直接一元线性回归模型
  # data_used.RandomWalk$lm[[j]] <-
  #   lm(formula = data_temp ~ stats::lag(x = data_temp, 1))
  # data_used.RandomWalk$lm[[j]]
  # data_used.RandomWalk$summary[[j]] <-
  #   summary(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$summary[[j]]
  # data_used.RandomWalk$confint[[j]] <-
  #   confint(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$confint[[j]]
  
  
  data_arima_temp <- arima(data_temp, order = c(1, 0, 0))
  # data_gradient_arima_temp <- arima(data_temp,order = c(1,1,0))
  
  # Ljung-Box 分析
  Box.test(data_gradient_temp-mean(data_gradient_temp), type = 'Ljung-Box')
  data_used.RandomWalk.Box_test[[j]]

  # # 这个是用ARIMA方法做的，有误
  # data_used.RandomWalk.Box_test[[j]] <- Box.test(residuals(data_arima_temp), type = 'Ljung-Box')
  # data_used.RandomWalk.Box_test[[j]] <-

  
  # # ADF 分析
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_gradient_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  
  
  
  j = j + 1
}



# 生成简单统计量的数据框
dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(c(
      data_used$time[data_used.timeRange.index[-c(1, length(data_used.time.keyword))] -
                       1], data_used.time.keyword[length(data_used.time.keyword)]
    )),
    as.numeric(data_used.mean),
    as.numeric(data_used.variance),
    as.numeric(data_used.gradient.mean),
    as.numeric(data_used.gradient.variance),
    as.numeric(data_used.rate_of_change.mean),
    as.numeric(data_used.rate_of_change.variance)
  )
names(dataframe_used) <-
  c('时点开始',
    '时点结束',
    '样本均值',
    '样本标准差',
    '差分均值',
    '差分标准差',
    '日变化率均值',
    '日变化率标准差')

# 写出数据到指定表格的指定位置
output_data.data_type <- "数据指标结果"
output_data.file_name <-
  paste(
    "金砖四国",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  dataframe_used,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)

## 暂时不用到
# output_data.data_type <- "日度数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(data_used,
#   file = paste(data_path,output_data.file_name,sep='/'),
#                   sheetName = output_data.sheet_name,
#                   append = TRUE)
#
# output_data.data_type <- "差分数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.gradient,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )
#
# output_data.data_type <- "日变化率数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.rate_of_change,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )


# 生成判断是否随机游走的数据框

# # 数据框1:过时了
# data_used.RandomWalk_dataframe <-
#   lapply(lapply(
#     X = (data_used.RandomWalk$summary %>% transpose)[['coefficients']],
#     FUN = t
#   ), FUN = as.vector) %>% unlist %>% matrix(ncol = 8, byrow = TRUE) %>% data.frame()
#
# names(data_used.RandomWalk.summary.coefficients_dataframe) <- c(
#   'beta 0',
#   'Std. Error of beta 0',
#   't-value of beta 0',
#   'Pr(>|t|) of beta 0',
#   'beta 1',
#   'Std. Error of beta 1',
#   't-value of beta 1',
#   'Pr(>|t|) of beta 1'
# )

# 数据框2:
data_used.RandomWalk_dataframe <- (data_used.RandomWalk.Box_test %>% transpose())[['p.value']]  %>% matrix(ncol = 1, byrow = TRUE) %>% data.frame()

names(data_used.RandomWalk_dataframe) <- c(
  'Ljung-Box p值'
)

# 写出数据到指定表格的指定位置
output_data.data_type <- "判断随机游走"
output_data.file_name <-
  paste(
    "金砖四国",
    "_",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  data_used.RandomWalk_dataframe,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)









##### 印度的 #######


# 输入的数据的数据表名称
input_sheet_name <- "印度日度数据"


# 提取需要的区间段
data_time_keyword <-
  as.character(c(
    '1973-01-02',
    '1975-09-24',
    '1992-03-02',
    '2009-02-02',
    '2018-12-31'
  ))

# 输出的数据的数据表名称
output_sheet_name <- "印度"



###### 重复代码段 ######
input_data.sheet_name = input_sheet_name
output_data.sheet_name = output_sheet_name
data_used.time.keyword = data_time_keyword
# 导入要计算货币错配程度的表格数据
data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = input_data.sheet_name,
    col_types = c("date", "numeric")
  )


# 去掉无用数据
data_used <- subset(data_original,
                    data != 'NaN')
data_used <- subset(data_used, data != '')


# 调整日期的格式
data_used$time <- as.character.Date(data_used$time)

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.gradient$data <-
  diff(data_used$data, lag = 1, differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
data_used.gradient.timeRange.index[1] <-
  which(as.character.Date(data_used$time) == data_used.time.keyword[1])
for (i in 2:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i]) -
    1
}
data_used.gradient.time.keyword <-
  data_used.gradient$time[data_used.gradient.timeRange.index]

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.rate_of_change$data <-
  data_used.gradient$data / data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <-
  data_used.gradient.timeRange.index
data_used.rate_of_change.time.keyword <-
  data_used.rate_of_change$time[data_used.rate_of_change.timeRange.index]

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)

# 样本数据的标准差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的标准差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的标准差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# ACF、PACF、自动化ARMA的变量定义
data_used.acf <- list()
data_used.pacf <- list()
data_used.gradient.acf <- list()
data_used.gradient.pacf <- list()
data_used.autoARMA <- list()
data_used.gradient.autoARMA <- list()
data_used.RandomWalk <-
  list(lm = list(),
       summary = list(),
       confint = list())
data_used.RandomWalk.Box_test <- list()


# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     sd(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     sd(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     sd(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_temp <-
    data_used$data[(data_used.timeRange.index[j]):(data_used.timeRange.index[j + 1] - 1)]
  data_gradient_temp <-
    data_used.gradient$data[(data_used.gradient.timeRange.index[j] + 1):(data_used.gradient.timeRange.index[j + 1] - 1)]
  data_rate_of_change_temp <-
    data_used.rate_of_change$data[(data_used.rate_of_change.timeRange.index[j] + 1):(data_used.rate_of_change.timeRange.index[j + 1] - 1)]
  
  data_used.mean[j] <- mean(data_temp, na.rm = FALSE)
  data_used.variance[j] <-
    sd(data_temp, na.rm = FALSE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_gradient_temp, na.rm = FALSE)
  data_used.gradient.variance[j] <-
    sd(data_gradient_temp, na.rm = FALSE)
  data_used.rate_of_change.mean[j] <-
    mean(data_rate_of_change_temp, na.rm = FALSE)
  data_used.rate_of_change.variance[j] <-
    sd(data_rate_of_change_temp, na.rm = FALSE)
  
  # ARMA模型
  # 自相关性和偏相关性检验
  data_used.acf[[j]] <- acf(data_temp)
  data_used.acf[[j]]
  acf(data_temp)
  data_used.pacf[[j]] <- pacf(data_temp)
  data_used.pacf[[j]]
  pacf(data_temp)
  data_used.gradient.acf[[j]] <- acf(data_gradient_temp)
  data_used.gradient.acf[[j]]
  acf(data_gradient_temp)
  data_used.gradient.pacf[[j]] <- pacf(data_gradient_temp)
  data_used.gradient.pacf[[j]]
  pacf(data_gradient_temp)
  
  # 自动化ARMA模型
  data_used.autoARMA[[j]] <-
    forecast::auto.arima(data_temp)
  data_used.autoARMA[[j]]
  
  # 判别是否符合随机游走
  
  # # 因为存在自相关性和异方差性，因此不能直接一元线性回归模型
  # data_used.RandomWalk$lm[[j]] <-
  #   lm(formula = data_temp ~ stats::lag(x = data_temp, 1))
  # data_used.RandomWalk$lm[[j]]
  # data_used.RandomWalk$summary[[j]] <-
  #   summary(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$summary[[j]]
  # data_used.RandomWalk$confint[[j]] <-
  #   confint(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$confint[[j]]
  
  
  data_arima_temp <- arima(data_temp, order = c(1, 0, 0))
  # data_gradient_arima_temp <- arima(data_temp,order = c(1,1,0))
  
  # Ljung-Box 分析
  Box.test(data_gradient_temp-mean(data_gradient_temp), type = 'Ljung-Box')
  data_used.RandomWalk.Box_test[[j]]

  # # 这个是用ARIMA方法做的，有误
  # data_used.RandomWalk.Box_test[[j]] <- Box.test(residuals(data_arima_temp), type = 'Ljung-Box')
  # data_used.RandomWalk.Box_test[[j]] <-

  
  # # ADF 分析
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_gradient_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  
  
  
  j = j + 1
}



# 生成简单统计量的数据框
dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(c(
      data_used$time[data_used.timeRange.index[-c(1, length(data_used.time.keyword))] -
                       1], data_used.time.keyword[length(data_used.time.keyword)]
    )),
    as.numeric(data_used.mean),
    as.numeric(data_used.variance),
    as.numeric(data_used.gradient.mean),
    as.numeric(data_used.gradient.variance),
    as.numeric(data_used.rate_of_change.mean),
    as.numeric(data_used.rate_of_change.variance)
  )
names(dataframe_used) <-
  c('时点开始',
    '时点结束',
    '样本均值',
    '样本标准差',
    '差分均值',
    '差分标准差',
    '日变化率均值',
    '日变化率标准差')

# 写出数据到指定表格的指定位置
output_data.data_type <- "数据指标结果"
output_data.file_name <-
  paste(
    "金砖四国",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  dataframe_used,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)

## 暂时不用到
# output_data.data_type <- "日度数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(data_used,
#   file = paste(data_path,output_data.file_name,sep='/'),
#                   sheetName = output_data.sheet_name,
#                   append = TRUE)
#
# output_data.data_type <- "差分数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.gradient,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )
#
# output_data.data_type <- "日变化率数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.rate_of_change,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )


# 生成判断是否随机游走的数据框

# # 数据框1:过时了
# data_used.RandomWalk_dataframe <-
#   lapply(lapply(
#     X = (data_used.RandomWalk$summary %>% transpose)[['coefficients']],
#     FUN = t
#   ), FUN = as.vector) %>% unlist %>% matrix(ncol = 8, byrow = TRUE) %>% data.frame()
#
# names(data_used.RandomWalk.summary.coefficients_dataframe) <- c(
#   'beta 0',
#   'Std. Error of beta 0',
#   't-value of beta 0',
#   'Pr(>|t|) of beta 0',
#   'beta 1',
#   'Std. Error of beta 1',
#   't-value of beta 1',
#   'Pr(>|t|) of beta 1'
# )

# 数据框2:
data_used.RandomWalk_dataframe <- (data_used.RandomWalk.Box_test %>% transpose())[['p.value']]  %>% matrix(ncol = 1, byrow = TRUE) %>% data.frame()

names(data_used.RandomWalk_dataframe) <- c(
  'Ljung-Box p值'
)

# 写出数据到指定表格的指定位置
output_data.data_type <- "判断随机游走"
output_data.file_name <-
  paste(
    "金砖四国",
    "_",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  data_used.RandomWalk_dataframe,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)











##### 俄罗斯的 #######

# 输入的数据的数据表名称
input_sheet_name <- "俄罗斯日度数据"

# 提取需要的区间段
data_time_keyword <-
  as.character(c(
    '1992-07-01',
    '1995-07-06',
    '1998-08-17',
    '2014-11-10',
    '2018-12-31'
  ))

# 输出的数据的数据表名称
output_sheet_name <- "俄罗斯"



###### 重复代码段 ######
input_data.sheet_name = input_sheet_name
output_data.sheet_name = output_sheet_name
data_used.time.keyword = data_time_keyword
# 导入要计算货币错配程度的表格数据
data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = input_data.sheet_name,
    col_types = c("date", "numeric")
  )


# 去掉无用数据
data_used <- subset(data_original,
                    data != 'NaN')
data_used <- subset(data_used, data != '')


# 调整日期的格式
data_used$time <- as.character.Date(data_used$time)

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.gradient$data <-
  diff(data_used$data, lag = 1, differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
data_used.gradient.timeRange.index[1] <-
  which(as.character.Date(data_used$time) == data_used.time.keyword[1])
for (i in 2:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i]) -
    1
}
data_used.gradient.time.keyword <-
  data_used.gradient$time[data_used.gradient.timeRange.index]

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.rate_of_change$data <-
  data_used.gradient$data / data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <-
  data_used.gradient.timeRange.index
data_used.rate_of_change.time.keyword <-
  data_used.rate_of_change$time[data_used.rate_of_change.timeRange.index]

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)

# 样本数据的标准差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的标准差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的标准差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# ACF、PACF、自动化ARMA的变量定义
data_used.acf <- list()
data_used.pacf <- list()
data_used.gradient.acf <- list()
data_used.gradient.pacf <- list()
data_used.autoARMA <- list()
data_used.gradient.autoARMA <- list()
data_used.RandomWalk <-
  list(lm = list(),
       summary = list(),
       confint = list())
data_used.RandomWalk.Box_test <- list()


# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     sd(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     sd(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     sd(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_temp <-
    data_used$data[(data_used.timeRange.index[j]):(data_used.timeRange.index[j + 1] - 1)]
  data_gradient_temp <-
    data_used.gradient$data[(data_used.gradient.timeRange.index[j] + 1):(data_used.gradient.timeRange.index[j + 1] - 1)]
  data_rate_of_change_temp <-
    data_used.rate_of_change$data[(data_used.rate_of_change.timeRange.index[j] + 1):(data_used.rate_of_change.timeRange.index[j + 1] - 1)]
  
  data_used.mean[j] <- mean(data_temp, na.rm = FALSE)
  data_used.variance[j] <-
    sd(data_temp, na.rm = FALSE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_gradient_temp, na.rm = FALSE)
  data_used.gradient.variance[j] <-
    sd(data_gradient_temp, na.rm = FALSE)
  data_used.rate_of_change.mean[j] <-
    mean(data_rate_of_change_temp, na.rm = FALSE)
  data_used.rate_of_change.variance[j] <-
    sd(data_rate_of_change_temp, na.rm = FALSE)
  
  # ARMA模型
  # 自相关性和偏相关性检验
  data_used.acf[[j]] <- acf(data_temp)
  data_used.acf[[j]]
  acf(data_temp)
  data_used.pacf[[j]] <- pacf(data_temp)
  data_used.pacf[[j]]
  pacf(data_temp)
  data_used.gradient.acf[[j]] <- acf(data_gradient_temp)
  data_used.gradient.acf[[j]]
  acf(data_gradient_temp)
  data_used.gradient.pacf[[j]] <- pacf(data_gradient_temp)
  data_used.gradient.pacf[[j]]
  pacf(data_gradient_temp)
  
  # 自动化ARMA模型
  data_used.autoARMA[[j]] <-
    forecast::auto.arima(data_temp)
  data_used.autoARMA[[j]]
  
  # 判别是否符合随机游走
  
  # # 因为存在自相关性和异方差性，因此不能直接一元线性回归模型
  # data_used.RandomWalk$lm[[j]] <-
  #   lm(formula = data_temp ~ stats::lag(x = data_temp, 1))
  # data_used.RandomWalk$lm[[j]]
  # data_used.RandomWalk$summary[[j]] <-
  #   summary(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$summary[[j]]
  # data_used.RandomWalk$confint[[j]] <-
  #   confint(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$confint[[j]]
  
  
  data_arima_temp <- arima(data_temp, order = c(1, 0, 0))
  # data_gradient_arima_temp <- arima(data_temp,order = c(1,1,0))
  
  # Ljung-Box 分析
  Box.test(data_gradient_temp-mean(data_gradient_temp), type = 'Ljung-Box')
  data_used.RandomWalk.Box_test[[j]]

  # # 这个是用ARIMA方法做的，有误
  # data_used.RandomWalk.Box_test[[j]] <- Box.test(residuals(data_arima_temp), type = 'Ljung-Box')
  # data_used.RandomWalk.Box_test[[j]] <-

  
  # # ADF 分析
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_gradient_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  
  
  
  j = j + 1
}



# 生成简单统计量的数据框
dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(c(
      data_used$time[data_used.timeRange.index[-c(1, length(data_used.time.keyword))] -
                       1], data_used.time.keyword[length(data_used.time.keyword)]
    )),
    as.numeric(data_used.mean),
    as.numeric(data_used.variance),
    as.numeric(data_used.gradient.mean),
    as.numeric(data_used.gradient.variance),
    as.numeric(data_used.rate_of_change.mean),
    as.numeric(data_used.rate_of_change.variance)
  )
names(dataframe_used) <-
  c('时点开始',
    '时点结束',
    '样本均值',
    '样本标准差',
    '差分均值',
    '差分标准差',
    '日变化率均值',
    '日变化率标准差')

# 写出数据到指定表格的指定位置
output_data.data_type <- "数据指标结果"
output_data.file_name <-
  paste(
    "金砖四国",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  dataframe_used,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)

## 暂时不用到
# output_data.data_type <- "日度数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(data_used,
#   file = paste(data_path,output_data.file_name,sep='/'),
#                   sheetName = output_data.sheet_name,
#                   append = TRUE)
#
# output_data.data_type <- "差分数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.gradient,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )
#
# output_data.data_type <- "日变化率数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.rate_of_change,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )


# 生成判断是否随机游走的数据框

# # 数据框1:过时了
# data_used.RandomWalk_dataframe <-
#   lapply(lapply(
#     X = (data_used.RandomWalk$summary %>% transpose)[['coefficients']],
#     FUN = t
#   ), FUN = as.vector) %>% unlist %>% matrix(ncol = 8, byrow = TRUE) %>% data.frame()
#
# names(data_used.RandomWalk.summary.coefficients_dataframe) <- c(
#   'beta 0',
#   'Std. Error of beta 0',
#   't-value of beta 0',
#   'Pr(>|t|) of beta 0',
#   'beta 1',
#   'Std. Error of beta 1',
#   't-value of beta 1',
#   'Pr(>|t|) of beta 1'
# )

# 数据框2:
data_used.RandomWalk_dataframe <- (data_used.RandomWalk.Box_test %>% transpose())[['p.value']]  %>% matrix(ncol = 1, byrow = TRUE) %>% data.frame()

names(data_used.RandomWalk_dataframe) <- c(
  'Ljung-Box p值'
)

# 写出数据到指定表格的指定位置
output_data.data_type <- "判断随机游走"
output_data.file_name <-
  paste(
    "金砖四国",
    "_",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  data_used.RandomWalk_dataframe,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)







##### 巴西的 #######

# 输入的数据的数据表名称
input_sheet_name <- "巴西日度数据"

# 提取需要的区间段
data_time_keyword <-
  as.character(
    c(
      '1984-12-03',
      '1991-10-01',
      '1994-07-01',
      '1999-02-01',
      '2002-10-25',
      '2008-08-05',
      '2009-10-19',
      '2015-09-25',
      '2018-12-31'
    )
  )

# 输出的数据的数据表名称
output_sheet_name <- "巴西"



###### 重复代码段 ######
input_data.sheet_name = input_sheet_name
output_data.sheet_name = output_sheet_name
data_used.time.keyword = data_time_keyword
# 导入要计算货币错配程度的表格数据
data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = input_data.sheet_name,
    col_types = c("date", "numeric")
  )


# 去掉无用数据
data_used <- subset(data_original,
                    data != 'NaN')
data_used <- subset(data_used, data != '')


# 调整日期的格式
data_used$time <- as.character.Date(data_used$time)

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.gradient$data <-
  diff(data_used$data, lag = 1, differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
data_used.gradient.timeRange.index[1] <-
  which(as.character.Date(data_used$time) == data_used.time.keyword[1])
for (i in 2:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i]) -
    1
}
data_used.gradient.time.keyword <-
  data_used.gradient$time[data_used.gradient.timeRange.index]

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <-
  data.frame(time = as.character.Date(data_used$time[-1]), data = 1)
data_used.rate_of_change$data <-
  data_used.gradient$data / data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <-
  data_used.gradient.timeRange.index
data_used.rate_of_change.time.keyword <-
  data_used.rate_of_change$time[data_used.rate_of_change.timeRange.index]

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)

# 样本数据的标准差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的标准差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的标准差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# ACF、PACF、自动化ARMA的变量定义
data_used.acf <- list()
data_used.pacf <- list()
data_used.gradient.acf <- list()
data_used.gradient.pacf <- list()
data_used.autoARMA <- list()
data_used.gradient.autoARMA <- list()
data_used.RandomWalk <-
  list(lm = list(),
       summary = list(),
       confint = list())
data_used.RandomWalk.Box_test <- list()


# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     sd(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     sd(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     sd(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_temp <-
    data_used$data[(data_used.timeRange.index[j]):(data_used.timeRange.index[j + 1] - 1)]
  data_gradient_temp <-
    data_used.gradient$data[(data_used.gradient.timeRange.index[j] + 1):(data_used.gradient.timeRange.index[j + 1] - 1)]
  data_rate_of_change_temp <-
    data_used.rate_of_change$data[(data_used.rate_of_change.timeRange.index[j] + 1):(data_used.rate_of_change.timeRange.index[j + 1] - 1)]
  
  data_used.mean[j] <- mean(data_temp, na.rm = FALSE)
  data_used.variance[j] <-
    sd(data_temp, na.rm = FALSE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_gradient_temp, na.rm = FALSE)
  data_used.gradient.variance[j] <-
    sd(data_gradient_temp, na.rm = FALSE)
  data_used.rate_of_change.mean[j] <-
    mean(data_rate_of_change_temp, na.rm = FALSE)
  data_used.rate_of_change.variance[j] <-
    sd(data_rate_of_change_temp, na.rm = FALSE)
  
  # ARMA模型
  # 自相关性和偏相关性检验
  data_used.acf[[j]] <- acf(data_temp)
  data_used.acf[[j]]
  acf(data_temp)
  data_used.pacf[[j]] <- pacf(data_temp)
  data_used.pacf[[j]]
  pacf(data_temp)
  data_used.gradient.acf[[j]] <- acf(data_gradient_temp)
  data_used.gradient.acf[[j]]
  acf(data_gradient_temp)
  data_used.gradient.pacf[[j]] <- pacf(data_gradient_temp)
  data_used.gradient.pacf[[j]]
  pacf(data_gradient_temp)
  
  # 自动化ARMA模型
  data_used.autoARMA[[j]] <-
    forecast::auto.arima(data_temp)
  data_used.autoARMA[[j]]
  
  # 判别是否符合随机游走
  
  # # 因为存在自相关性和异方差性，因此不能直接一元线性回归模型
  # data_used.RandomWalk$lm[[j]] <-
  #   lm(formula = data_temp ~ stats::lag(x = data_temp, 1))
  # data_used.RandomWalk$lm[[j]]
  # data_used.RandomWalk$summary[[j]] <-
  #   summary(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$summary[[j]]
  # data_used.RandomWalk$confint[[j]] <-
  #   confint(data_used.RandomWalk$lm[[j]])
  # data_used.RandomWalk$confint[[j]]
  
  
  data_arima_temp <- arima(data_temp, order = c(1, 0, 0))
  # data_gradient_arima_temp <- arima(data_temp,order = c(1,1,0))
  
  # Ljung-Box 分析
  Box.test(data_gradient_temp-mean(data_gradient_temp), type = 'Ljung-Box')
  data_used.RandomWalk.Box_test[[j]]

  # # 这个是用ARIMA方法做的，有误
  # data_used.RandomWalk.Box_test[[j]] <- Box.test(residuals(data_arima_temp), type = 'Ljung-Box')
  # data_used.RandomWalk.Box_test[[j]] <-

  
  # # ADF 分析
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  # data_used.RandomWalk.ADF_test[[j]] <- adf.test(residuals(data_gradient_arima_temp))
  # data_used.RandomWalk.ADF_test[[j]]
  
  
  
  j = j + 1
}



# 生成简单统计量的数据框
dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(c(
      data_used$time[data_used.timeRange.index[-c(1, length(data_used.time.keyword))] -
                       1], data_used.time.keyword[length(data_used.time.keyword)]
    )),
    as.numeric(data_used.mean),
    as.numeric(data_used.variance),
    as.numeric(data_used.gradient.mean),
    as.numeric(data_used.gradient.variance),
    as.numeric(data_used.rate_of_change.mean),
    as.numeric(data_used.rate_of_change.variance)
  )
names(dataframe_used) <-
  c('时点开始',
    '时点结束',
    '样本均值',
    '样本标准差',
    '差分均值',
    '差分标准差',
    '日变化率均值',
    '日变化率标准差')

# 写出数据到指定表格的指定位置
output_data.data_type <- "数据指标结果"
output_data.file_name <-
  paste(
    "金砖四国",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  dataframe_used,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)

## 暂时不用到
# output_data.data_type <- "日度数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(data_used,
#   file = paste(data_path,output_data.file_name,sep='/'),
#                   sheetName = output_data.sheet_name,
#                   append = TRUE)
#
# output_data.data_type <- "差分数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.gradient,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )
#
# output_data.data_type <- "日变化率数据"
# output_data.file_name <- paste("金砖四国",output_data.data_name,output_data.data_type,"_",input_data.data_source_name,"_",output_data.project_name,".xlsx",sep = '')
# xlsx::write.xlsx2(
#   data_used.rate_of_change,
#   file = paste(data_path,output_data.file_name,sep='/'),
#   sheetName = output_data.sheet_name,
#   append = TRUE
# )


# 生成判断是否随机游走的数据框

# # 数据框1:过时了
# data_used.RandomWalk_dataframe <-
#   lapply(lapply(
#     X = (data_used.RandomWalk$summary %>% transpose)[['coefficients']],
#     FUN = t
#   ), FUN = as.vector) %>% unlist %>% matrix(ncol = 8, byrow = TRUE) %>% data.frame()
#
# names(data_used.RandomWalk.summary.coefficients_dataframe) <- c(
#   'beta 0',
#   'Std. Error of beta 0',
#   't-value of beta 0',
#   'Pr(>|t|) of beta 0',
#   'beta 1',
#   'Std. Error of beta 1',
#   't-value of beta 1',
#   'Pr(>|t|) of beta 1'
# )

# 数据框2:
data_used.RandomWalk_dataframe <- (data_used.RandomWalk.Box_test %>% transpose())[['p.value']]  %>% matrix(ncol = 1, byrow = TRUE) %>% data.frame()

names(data_used.RandomWalk_dataframe) <- c(
  'Ljung-Box p值'
)

# 写出数据到指定表格的指定位置
output_data.data_type <- "判断随机游走"
output_data.file_name <-
  paste(
    "金砖四国",
    output_data.data_name,
    output_data.data_type,
    "_",
    input_data.data_source_name,
    "_",
    output_data.project_name,
    ".xlsx",
    sep = ''
  )
xlsx::write.xlsx2(
  data_used.RandomWalk_dataframe,
  file = paste(data_path, output_data.file_name, sep = '/'),
  sheetName = output_data.sheet_name,
  append = TRUE
)






### 根据导师在3月初给的资料做划分区间进行分析 ###

# 分析波动性


library(readxl)
library(tidyr)
library(dplyr)
library(urca)
library(lmtest)
library(xlsx)

root_path <- getwd()
setwd(root_path)
source(paste(root_path, "/program_function.R", sep = ''))



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
# emptyValue <-
#   program_function(
#     input_data.sheet_name = input_sheet_name,
#     output_data.sheet_name = output_sheet_name,
#     data_used.time.keyword = data.time.keyword
#   )

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

# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的方差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     var(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     var(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                 1] - 1],na.rm = TRUE)
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                1] - 1],na.rm = TRUE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_used.gradient$data[data_used.gradient.timeRange.index[j] + 1:data_used.gradient.timeRange.index[j +
                                                                                                                1] - 1],na.rm = TRUE)
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j] +
                                  1:data_used.gradient.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.mean[j] <-
    mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                         1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                        1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  j = j + 1
}

dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(as.Date(data_used.time.keyword[-1]) - 1),
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
    '样本方差',
    '样本差分均值',
    '样本差分方差',
    '样本日变化率均值',
    '样本日变化率方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(data_used,
                  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率日度数据_BIS_自动生成的.xlsx",
                  sheetName = output_data.sheet_name,
                  append = TRUE)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
# data_used.var[j] <- var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j+1]-1])
















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
# emptyValue <-
#   program_function(
#     input_data.sheet_name = input_sheet_name,
#     output_data.sheet_name = output_sheet_name,
#     data_used.time.keyword = data.time.keyword
#   )

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

# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的方差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     var(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     var(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                 1] - 1],na.rm = TRUE)
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                1] - 1],na.rm = TRUE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_used.gradient$data[data_used.gradient.timeRange.index[j] + 1:data_used.gradient.timeRange.index[j +
                                                                                                                1] - 1],na.rm = TRUE)
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j] +
                                  1:data_used.gradient.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.mean[j] <-
    mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                         1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                        1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  j = j + 1
}

dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(as.Date(data_used.time.keyword[-1]) - 1),
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
    '样本方差',
    '样本差分均值',
    '样本差分方差',
    '样本日变化率均值',
    '样本日变化率方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(data_used,
                  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率日度数据_BIS_自动生成的.xlsx",
                  sheetName = output_data.sheet_name,
                  append = TRUE)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
# data_used.var[j] <- var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j+1]-1])









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
# emptyValue <-
#   program_function(
#     input_data.sheet_name = input_sheet_name,
#     output_data.sheet_name = output_sheet_name,
#     data_used.time.keyword = data.time.keyword
#   )

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

# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的方差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     var(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     var(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                 1] - 1],na.rm = TRUE)
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                1] - 1],na.rm = TRUE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_used.gradient$data[data_used.gradient.timeRange.index[j] + 1:data_used.gradient.timeRange.index[j +
                                                                                                                1] - 1],na.rm = TRUE)
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j] +
                                  1:data_used.gradient.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.mean[j] <-
    mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                         1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                        1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  j = j + 1
}

dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(as.Date(data_used.time.keyword[-1]) - 1),
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
    '样本方差',
    '样本差分均值',
    '样本差分方差',
    '样本日变化率均值',
    '样本日变化率方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(data_used,
                  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率日度数据_BIS_自动生成的.xlsx",
                  sheetName = output_data.sheet_name,
                  append = TRUE)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
# data_used.var[j] <- var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j+1]-1])



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
# emptyValue <-
#   program_function(
#     input_data.sheet_name = input_sheet_name,
#     output_data.sheet_name = output_sheet_name,
#     data_used.time.keyword = data.time.keyword
#   )

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

# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 差分样本数据的方差
data_used.gradient.variance <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <-
  array(dim = length(data_used.time.keyword) - 1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# for (i in 1:length(data_used.time.keyword) - 1) {
#   i
#   data_used.mean[i] <-
#     mean(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   data_used.variance[i] <-
#     var(data_used$data[data_used.timeRange.index[i]:data_used.timeRange.index[i+1]-1])
#   # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
#   data_used.gradient.mean[i] <-
#     mean(data_used.gradient$data[data_used.gradient.timeRange.index[i]+1:data_used.gradient.timeRange.index[i+1]-1])
#   data_used.gradient.variance[i] <-
#     var(data_used.gradient$data[data_used.gradient.timeRange.index[i] +
#                                   1:data_used.gradient.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.mean[i] <-
#     mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                          1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
#   data_used.rate_of_change.variance[i] <-
#     var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[i] +
#                                         1:data_used.rate_of_change.timeRange.index[i + 1] - 1])
# }

j = 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                 1] - 1],na.rm = TRUE)
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j +
                                                                                1] - 1],na.rm = TRUE)
  # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
  data_used.gradient.mean[j] <-
    mean(data_used.gradient$data[data_used.gradient.timeRange.index[j] + 1:data_used.gradient.timeRange.index[j +
                                                                                                                1] - 1],na.rm = TRUE)
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j] +
                                  1:data_used.gradient.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.mean[j] <-
    mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                         1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                        1:data_used.rate_of_change.timeRange.index[j + 1] - 1],na.rm = TRUE)
  j = j + 1
}

dataframe_used <-
  data.frame(
    data_used.time.keyword[-length(data_used.time.keyword)],
    as.character.Date(as.Date(data_used.time.keyword[-1]) - 1),
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
    '样本方差',
    '样本差分均值',
    '样本差分方差',
    '样本日变化率均值',
    '样本日变化率方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(data_used,
                  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率日度数据_BIS_自动生成的.xlsx",
                  sheetName = output_data.sheet_name,
                  append = TRUE)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = output_data.sheet_name,
  append = TRUE
)
# data_used.var[j] <- var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j+1]-1])











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

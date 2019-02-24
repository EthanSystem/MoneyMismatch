



# 分析波动性


library(readxl)
library(tidyr)
library(dplyr)
library(urca)
library(lmtest)
library(xlsx)

root_path <- getwd()





##### 中国的 #######

# 导入要计算货币错配程度的表格数据
# data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")

data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = "中国日度数据",
    col_types = c("date", "numeric")
  )

# 去掉无用数据
data_used <- subset(data_original, data != 'NaN')
data_used <- subset(data_used, data != '')

# 提取需要的区间段
data_used.time.keyword <-
  as.character(
    c(
      '1981-01-02',
      '1986-07-07',
      '1991-04-09',
      '1994-01-03',
      '1997-07-01',
      '1999-01-04',
      '2005-07-21',
      '2006-01-03',
      '2008-07-01',
      '2010-07-01',
      '2015-08-11',
      '2017-05-02',
      '2018-12-31'
    )
  )

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.gradient$data <- diff(data_used$data,lag = 1,differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])+1
}

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.rate_of_change$data <- data_used.gradient$data/data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <- data_used.gradient.timeRange.index

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)
  
# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <- array(dim=length(data_used.time.keyword)-1)
# 差分样本数据的方差
data_used.gradient.variance <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <- array(dim=length(data_used.time.keyword)-1)
j <- 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.gradient.mean[j] <- mean(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.mean[j] <- mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  
  # summary(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  j <- j + 1
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
names(dataframe_used) <- c('时点开始', '时点结束', '样本均值', '样本方差','差分样本均值','差分样本方差','变化率样本均值','变化率样本方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = '中国',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = '中国',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = '中国',
  append = TRUE
)
# data_used.var[j] <- var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j+1]-1])







##### 印度的 #######

# 导入要计算货币错配程度的表格数据
# data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")

data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = "印度日度数据",
    col_types = c("date", "numeric")
  )

# 去掉无用数据
data_used <- subset(data_original, data != 'NaN')
data_used <- subset(data_used, data != '')

# 提取需要的区间段
data_used.time.keyword <-
  as.character(
    c(
      '1973-01-02',
      '1975-09-24',
      '1980-01-02',
      '1991-06-28',
      '1992-03-02',
      '1993-03-01',
      '1994-08-01',
      '1997-07-01',
      '1999-01-04',
      '2000-06-01',
      '2004-01-02',
      '2008-01-02',
      '2010-01-04',
      '2018-12-31'
    )
  )

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.gradient$data <- diff(data_used$data,lag = 1,differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])+1
}

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.rate_of_change$data <- data_used.gradient$data/data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <- data_used.gradient.timeRange.index

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)
  
# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <- array(dim=length(data_used.time.keyword)-1)
# 差分样本数据的方差
data_used.gradient.variance <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <- array(dim=length(data_used.time.keyword)-1)
j <- 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.gradient.mean[j] <- mean(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.mean[j] <- mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  
  # summary(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  j <- j + 1
}
dataframe_statistic <-
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
names(dataframe_statistic) <- c('时点开始', '时点结束', '样本均值', '样本方差','差分样本均值','差分样本方差','变化率样本均值','变化率样本方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_statistic,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = '印度',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = '印度',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = '印度',
  append = TRUE
)




##### 巴西的 #######

# 导入要计算货币错配程度的表格数据
# data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")

data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = "巴西日度数据",
    col_types = c("date", "numeric")
  )

# 去掉无用数据
data_used <- subset(data_original, data != 'NaN')
data_used <- subset(data_used, data != '')

# 提取需要的区间段
data_used.time.keyword <-
  as.character(
    c(
      '1984-12-03',
      '1992-05-05',
      '1994-07-01',
      '1999-02-01',
      '2008-04-30',
      '2008-09-01',
      '2009-05-27',
      '2018-12-31'
    )
  )

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.gradient$data <- diff(data_used$data,lag = 1,differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])+1
}

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.rate_of_change$data <- data_used.gradient$data/data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <- data_used.gradient.timeRange.index

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)
  
# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <- array(dim=length(data_used.time.keyword)-1)
# 差分样本数据的方差
data_used.gradient.variance <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <- array(dim=length(data_used.time.keyword)-1)
j <- 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.gradient.mean[j] <- mean(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.mean[j] <- mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  
  # summary(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  j <- j + 1
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
names(dataframe_used) <- c('时点开始', '时点结束', '样本均值', '样本方差','差分样本均值','差分样本方差','变化率样本均值','变化率样本方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = '巴西',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = '巴西',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = '巴西',
  append = TRUE
)






##### 俄罗斯的 #######

# 导入要计算货币错配程度的表格数据
# data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")

data_original <-
  readxl::read_xlsx(
    path = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/要处理的金砖四国汇率日度数据_BIS.xlsx",
    sheet = "俄罗斯日度数据",
    col_types = c("date", "numeric")
  )

# 去掉无用数据
data_used <- subset(data_original, data != 'NaN')
data_used <- subset(data_used, data != '')

# 提取需要的区间段
data_used.time.keyword <-
  as.character(
    c(
      '1992-07-01',
      '1995-07-05',
      '1998-08-17',
      '1998-09-09',
      '2001-01-02',
      '2006-03-01',
      '2008-06-02',
      '2010-01-04',
      '2014-11-10',
      '2018-12-31'
    )
  )

# 对样本生成新的数据集
data_used.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])
}

# 计算样本数据的一阶差分，生成新的数据集。
data_used.gradient <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.gradient$data <- diff(data_used$data,lag = 1,differences = 1)
data_used.gradient.timeRange.index <-
  array(dim = length(data_used.time.keyword))
for (i in 1:length(data_used.time.keyword)) {
  data_used.gradient.timeRange.index[i] <-
    which(as.character.Date(data_used$time) == data_used.time.keyword[i])+1
}

# 计算样本数据的变化率，生成新的数据集。
data_used.rate_of_change <- data.frame(time=as.character.Date(data_used$time[-1]),data=1)
data_used.rate_of_change$data <- data_used.gradient$data/data_used$data[-length(data_used$time)]
data_used.rate_of_change.timeRange.index <- data_used.gradient.timeRange.index

# 计算每一区间段的指标：
# 样本数据的均值
data_used.mean <-
  array(dim = length(data_used.time.keyword) - 1)
  
# 样本数据的方差
data_used.variance <-
  array(dim = length(data_used.time.keyword) - 1)

# data_used.summary <- array(dim = length(data_used.time.keyword) - 1)

# 差分样本数据的均值
data_used.gradient.mean <- array(dim=length(data_used.time.keyword)-1)
# 差分样本数据的方差
data_used.gradient.variance <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的均值
data_used.rate_of_change.mean <- array(dim=length(data_used.time.keyword)-1)
# 变化率样本数据的方差
data_used.rate_of_change.variance <- array(dim=length(data_used.time.keyword)-1)
j <- 1
for (i in 1:length(data_used.time.keyword) - 1) {
  data_used.mean[j] <-
    mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.variance[j] <-
    var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  data_used.gradient.mean[j] <- mean(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.gradient.variance[j] <-
    var(data_used.gradient$data[data_used.gradient.timeRange.index[j]:data_used.gradient.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.mean[j] <- mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  data_used.rate_of_change.variance[j] <-
    var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j]:data_used.rate_of_change.timeRange.index[j + 1] - 1])
  
  # summary(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
  j <- j + 1
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
names(dataframe_used) <- c('时点开始', '时点结束', '样本均值', '样本方差','差分样本均值','差分样本方差','变化率样本均值','变化率样本方差')

# 写出数据到指定表格的指定位置
xlsx::write.xlsx2(
  dataframe_used,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率数据指标结果_BIS_自动生成的.xlsx",
  sheetName = '俄罗斯',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.gradient,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率差分数据_BIS_自动生成的.xlsx",
  sheetName = '俄罗斯',
  append = TRUE
)
xlsx::write.xlsx2(
  data_used.rate_of_change,
  file = "/Users/ethan/Documents/Ethan/CoreFiles/CodesFile/MoneyMismatch/data/金砖四国汇率变化率数据_BIS_自动生成的.xlsx",
  sheetName = '俄罗斯',
  append = TRUE
)













####### delete ##################
# Log 化预处理
data_need_log <- log(data_used[, c("data")])
data_logged <- cbind(data_used[, c("time")], data_need_log)

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
ts_diff_logged.var_select_lags <-
  vars::VARselect(ts_diff_logged, lag.max = 20, type = "const") # 结果显示，建议滞后阶数 <- 2

# 构建VAR模型
VAR_ts <- vars::VAR(ts_diff_logged, p = 2, type = "const")
summary(VAR_ts)

# Johnson 协整检验
num_lags <- 1
ts_diff_logged.ca.jo <-
  urca::ca.jo(data_logged,
              ecdet = "const",
              type = "trace",
              K = num_lags)
print("trace ", num_lags)
summary(ts_diff_logged.ca.jo)
ts_diff_logged.ca.jo <-
  urca::ca.jo(data_logged,
              ecdet = "const",
              type = "eigen",
              K = num_lags)
print("eigen ", num_lags)
summary(ts_diff_logged.ca.jo)


# 格兰杰因果检验(更详细的结果见Eviews的分析)
ts_diff_logged.grangertest <-
  grangertest(AECM ~ EX, data = ts_diff_logged)
ts_diff_logged.grangertest

# SSM
TODO

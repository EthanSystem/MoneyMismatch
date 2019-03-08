## OLS_20181231.R
## 用于计算2000-01——2014-12的数据。

library(xlsx)
library(readxl)
library(tidyr)
library(dbplyr)

root_path <- getwd()

# 导入要判断汇率制度的表格数据
data_original <- readxl::read_xlsx(path = "/Users/ethan/Documents/Ethan/CoreFiles/ProjectsFile/Research/EconomicAndFinance/MoneyMismatch/data/判断人民币汇率制度的数据_20181126.xlsx", sheet = "need calculating")

# 生成分段的数据区域
data_region_all <- data_original[which(as.character.Date(data_original$year)=='2000-01-01'):which(as.character.Date(data_original$year)=='2014-11-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]
data_region_1 <- data_original[which(as.character.Date(data_original$year)=='2000-01-01'):which(as.character.Date(data_original$year)=='2005-03-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]
data_region_2 <- data_original[which(as.character.Date(data_original$year)=='2005-04-01'):which(as.character.Date(data_original$year)=='2008-05-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]
data_region_3 <- data_original[which(as.character.Date(data_original$year)=='2008-06-01'):which(as.character.Date(data_original$year)=='2010-06-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]
data_region_4 <- data_original[which(as.character.Date(data_original$year)=='2010-07-01'):which(as.character.Date(data_original$year)=='2011-11-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]
data_region_5 <- data_original[which(as.character.Date(data_original$year)=='2011-12-01'):which(as.character.Date(data_original$year)=='2014-11-01'), c("CNY", "USD", "JPY", "KRW", "EUR", "EMP")]

# 对各时间段做多元线性回归
lm_region_all <-  lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_all)
lm_region_1 <- lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_1)
lm_region_2 <- lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_2)
lm_region_3 <- lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_3)
lm_region_4 <- lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_4)
lm_region_5 <- lm(CNY ~ USD + JPY + KRW + EUR + EMP, data = data_region_5)

# 输出各时间段的线性回归结果的总结报告
summary(lm_region_all)
summary(lm_region_1)
summary(lm_region_2)
summary(lm_region_3)
summary(lm_region_4)
summary(lm_region_5)



# 生成和导出各影响因素的系数数据表
lm_region.coefficeients = cbind(lm_region_all$coefficients,lm_region_1$coefficients,lm_region_2$coefficients,lm_region_3$coefficients,lm_region_4$coefficients,lm_region_5$coefficients)


# 生成和导出各影响因素的显著性表
# TODO



# 测算货币错配程度

# TODO













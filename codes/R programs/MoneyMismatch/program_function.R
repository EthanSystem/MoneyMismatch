## 用于单次处理一个表格里的数据的代码

##### functions ###################
program_function <-
  function(input_data.sheet_name,
           output_data.sheet_name,
           data_used.time.keyword) {
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
    

    j = 1
    for (i in 1:length(data_used.time.keyword) - 1) {
      try(data_used.mean[j] <-
            mean(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1]), silent = TRUE)
      data_used.variance[j] <-
        var(data_used$data[data_used.timeRange.index[j]:data_used.timeRange.index[j + 1] - 1])
      # 对于各段差分数据等类型的数据的描述性统计的计算，每段的第一个数据值不计入计算。
      data_used.gradient.mean[j] <-
        mean(data_used.gradient$data[data_used.gradient.timeRange.index[j] + 1:data_used.gradient.timeRange.index[j + 1] - 1])
      data_used.gradient.variance[j] <-
        var(data_used.gradient$data[data_used.gradient.timeRange.index[j] +
                                      1:data_used.gradient.timeRange.index[j + 1] - 1])
      data_used.rate_of_change.mean[j] <-
        mean(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                             1:data_used.rate_of_change.timeRange.index[j + 1] - 1])
      data_used.rate_of_change.variance[j] <-
        var(data_used.rate_of_change$data[data_used.rate_of_change.timeRange.index[j] +
                                            1:data_used.rate_of_change.timeRange.index[j + 1] - 1])
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
    
    return(NULL)
    
  }
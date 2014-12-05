complete <- function(directory, id = 1:332) {
    ## 'directory' 是长度为1的字符向量，指明
    ##  CSV 文件的位置
    directory <- as.character(directory)
    
    ## 'id' 是正整数向量，指明监测点的ID号，
    ## 将要被使用的
    id <- as.integer(id)
    
    ## calculate
    filenameArray <- paste("./", directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
    data <- data.frame(id = id, nobs = numeric(length(id)))
    for (i in seq_along(filenameArray)) {
        buf <- read.csv(filenameArray[i])
        data[i,"nobs"] <- sum(!is.na(buf[["sulfate"]]) & !is.na(buf[["nitrate"]]))
    }
    
    ## 返回以下格式的数据帧：
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## 其中'id' 是监测点ID编号，而'nobs'是
    ## 完整案例的数量
    data
}
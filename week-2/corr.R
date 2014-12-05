corr <- function(directory, threshold = 0) {
    ## 'directory' 是长度为1的字符向量，指明
    ##  CSV 文件的位置
    directory <- as.character(directory)
    
    ## 'threshold' 是长度为1的数值向量，指明
    ## 完整观测的案例的数量 (针对所有
    ## 变量) 是必须的，为了计算这两个的相关性：
    ## 硝酸盐(nitrate)和硫酸盐(sulfate); 默认值为 0
    
    
    ## calculate
    buf <- complete(directory)
    subsetOfItem <- buf[buf[,"nobs"] > threshold, ]
    id <- subsetOfItem[, "id"]
    if (length(id) == 0)
    {
        return(numeric())
    }
    filenameArray <- paste("./", directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
    result <- numeric()
    for (i in seq_along(filenameArray)) {
        buf <- read.csv(filenameArray[i])
        validSubset <- !is.na(buf[["sulfate"]]) & !is.na(buf[["nitrate"]])
        value <- cor(buf[validSubset,"sulfate"],buf[validSubset,"nitrate"])
        result <- c(result, value)
    }
    
    ## 返回相关性的数值向量
    result
}
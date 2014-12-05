pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' 是长度为1的字符向量，指明
    ## CSV文件的位置
    directory <- as.character(directory)
    
    ## 'pollutant' 是长度为1的字符向量，指明
    ## 污染物的名称，我们将会计算其
    ## 平均值; 要么是“硫酸盐(sulfate)”要么是“硝酸盐(nitrate)”
    pollutant <- as.character(pollutant)
    
    ## 'id'是正整数向量，指明监测点的ID，
    ## 将要被要使用的
    id <- as.integer(id)
    
    ## calculate
    filenameArray <- paste("./", directory, "/", formatC(id, width=3, flag="0"), ".csv", sep="")
    data <- numeric()
    for (i in seq_along(filenameArray)) {
        buf <- read.csv(filenameArray[i])
        data <- c(data,buf[,pollutant])
    }
    
    ## 返回列表内的所有监测点的污染物平均值，
    ## “id”向量中的 (忽略 NA值)
    mean(data, na.rm = TRUE)
}
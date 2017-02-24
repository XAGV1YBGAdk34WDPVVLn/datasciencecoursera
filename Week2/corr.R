corr <- function(directory, threshold = 0){
    data <- NULL
    result <- NULL
    # get a list of all files in the specified directory that end in .csv
    files <- list.files(directory, pattern="*.csv")

    # loop through the monitors specified
    for(index in files)
    {
        # build a path to the file
        path <- file.path(directory, index)
        # print(path)

        # read in the data
        data <- read.csv(path)#, nrows=5)

        # only include complete cases
        data <- data[complete.cases(data),]

        # only include complete cases
        if(nrow(data) > threshold)
        {
            result <- c(result, cor(data$sulfate, data$nitrate))
        }
    }

    # print(class(result))
    # return numeric vector of correlations
    return(result)

}

answer <- corr("/home/user/Downloads/specdata", 150)
print(head(answer))
print(summary(answer))
print("=====")

answer <- corr("/home/user/Downloads/specdata", 400)
print(head(answer))
print(summary(answer))
print("=====")

# the length returned here should be 0
answer <- corr("/home/user/Downloads/specdata", 5000)
print(summary(answer))
print(length(answer))
print("=====")

answer <- corr("/home/user/Downloads/specdata")
print(summary(answer))
print(length(answer))

complete <- function(directory, id = 1:332){
    data <- NULL
    result <- NULL
    # get a list of all files in the specified directory that end in .csv
    files <- list.files(directory, pattern="*.csv")

    # loop through the monitors specified
    for(index in id)
    {
        # build a path to the file
        path <- file.path(directory, files[index])
        # print(path)

        # read in the data
        data <- read.csv(path)#, nrows=5)

        # only include complete cases
        data <- data[complete.cases(data),]

        # combine, by rows, the monitor number and a sum of the completed cases
        result <- rbind(result, c(id=index, nobs=nrow(data)))
    }

    # print(result)
    # print(class(result))
    # requirements state to return this result as a dataframe (convert our matrix to a dataframe)
    return(data.frame(result))
}

answer <- complete("/home/user/Downloads/specdata", 1)
print(answer)
print("=====")

answer <- complete("/home/user/Downloads/specdata", c(2, 4, 8, 10, 12))
print(answer)
print("=====")

answer <- complete("/home/user/Downloads/specdata", 30:25)
print(answer)
print("=====")

answer <- complete("/home/user/Downloads/specdata", 3)
print(answer)

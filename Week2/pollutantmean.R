pollutantmean <- function(directory, pollutant, id = 1:332){
    data <- NA
    result <- 0

    # the id vector is a reference to the monitor data we need to read.
    # read in the monitors specified by the callee
    files = file.path(directory, paste0(formatC(id, width=3, format="d", flag="0"), ".csv"))
    # print(files)

    for(item in files)
    {
        # print(paste0("Processing File:", item))
        data <- c(data, read.csv(item)[[pollutant]])
    }

    # handle excluding NA rows here
    result <- mean(data, na.rm=TRUE)

    return(result)
}

# check against provided example output
answer <- pollutantmean("/home/user/Downloads/specdata", "sulfate", 1:10)
print(answer)

answer <- pollutantmean("/home/user/Downloads/specdata", "nitrate", 70:72)
print(answer)

answer <- pollutantmean("/home/user/Downloads/specdata", "nitrate", 23)
print(answer)

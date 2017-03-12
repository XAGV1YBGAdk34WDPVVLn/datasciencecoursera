# supress warning messages
options(warn=-1)

# this function takes a 2 character state abbrevation and an outcome name ("heart attack", "heart failure", "pneumonia")
# returns the hospital name with the best mortality rate as a character vector
best <- function(state, outcome){
    working <- data.frame()

    # read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

    # check that state and outcome are valid
    if(!state %in% data$State)
    {
        stop("invalid state")
    }

    # grab the columns we need
    if(outcome == "heart attack"){
        working <- data[,c(2,7,11)]
    } else if(outcome == "heart failure"){
        working <- data[,c(2,7,17)]
    } else if(outcome == "pneumonia"){
        working <- data[,c(2,7,23)]
    } else {
        stop("invalid outcome")
    }

    # get data for the requested state
    working <- working[working[,2] == state,]

    # print(head(working))

    # convert the column we need to min to numeric
    working[,3] <- as.numeric(working[,3])

    # print(working)

    # sort alphabetically. if there is a tie, the first hospital, sorted alphabetically is returned
    working <- working[order(working[,3], working[,1], na.last=NA),]

    # print(head(working,20))

    # min to get best rates
    working <- working[which.min(working[,3]),]

    # return
    return(working[[1]])
}

runtests <- function(){
    print(best("TX", "heart attack"))
    print(best("TX", "heart failure"))
    print(best("MD", "heart attack"))
    print(best("MD", "pneumonia"))
}
runtestbad1 <- function(){
    best("BB", "heart attack")
}
runtestbad2 <- function(){
    best("NY", "heat attack")
}

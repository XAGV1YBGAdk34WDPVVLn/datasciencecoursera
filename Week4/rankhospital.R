# supress warning messages
options(warn=-1)

# 2 character state abbrev
# outcome name ("heart attack", "heart failure", "pneumonia")
# num "best", "worst", index rank
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    result = NULL
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
    # remove NAs
    working <- working[order(working[,3], working[,1], na.last=NA),]

    # print(head(working,20))
    # print(tail(working,2))

    #
    if(num == "best"){
        result <- working[1,]
    }else if(num == "worst"){
        result <- working[nrow(working),]
    }else if(is.numeric(num)){
        result <- working[num,]
    }else{
        # unknown condition
        stop("invalid num")
    }

    # return
    return(result[[1]])
}

runtests_rank <- function(){
    print(rankhospital("TX", "heart failure", 4))
    print(rankhospital("MD", "heart attack", "worst"))
    print(rankhospital("MN", "heart attack", 5000))
    print(rankhospital("OH", "heart attack", "best"))
    print(rankhospital("WI", "pneumonia", "worst"))
    print(rankhospital("WV", "pneumonia", "worst"))
    print(rankhospital("WY", "pneumonia", "worst"))
}

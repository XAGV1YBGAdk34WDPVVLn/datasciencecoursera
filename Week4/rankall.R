# supress warning messages
options(warn=-1)

# 2 character state abbrev
# outcome name ("heart attack", "heart failure", "pneumonia")
# num "best", "worst", index rank
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    result = NULL
    working <- data.frame()

    # read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

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

    # convert the column we need to min to numeric
    working[,3] <- as.numeric(working[,3])

    result <- lapply(
        split(working, working[2]),
        function(s) {
            x <- NULL
            # order by min deaths and then by hospital name
            s <- s[order(s[3], s[1], na.last=NA),]

            if(num == "best"){
                x <- s[1,1]
            }else if(num == "worst"){
                x <- s[nrow(s),1]
            }else if(is.numeric(num)){
                x <- s[num,1]
            }else{
                # unknown condition
                x <- NA
            }

            return(x)
        }
    )

    return(data.frame(hospital=unlist(result), state=names(result)))
}

runtests_rankall <- function(){
    print(head(rankall("heart attack", 20), 10))
    print("==========")
    print(tail(rankall("pneumonia", "worst"), 3))
    print("==========")
    print(tail(rankall("heart failure"), 10))
    print("==========")
    print(head(rankall("heart failure", 50), 10))
}


rankhospital <- function(state="WA",outcome="heart attack",rnk=1)
{
    #start
    library(dplyr)
    library(magrittr)
    #vector to store the best hospital result
    besthospital <- character()
    
    # Read the outcome of care measures file and subset the required columns
    care <- read.csv("outcome-of-care-measures.csv")
    care <- subset(care[,c(2,7,11,17,23)])
    
    #validate the state and outcome values and raise a error upon incorrect values supplied'
    
    Uniqueoutcomevalues <- c("heart attack","heart failure","pneumonia")
    if (!(outcome %in% Uniqueoutcomevalues)) stop('Invalid Outcome')
    if (!(state %in% unique(care$State))) stop('Invalid State')
    
    # Subset based on the outcome specified and remove the hospitals which are not available
    if (outcome =="heart attack")
    {
        care <- na.omit(subset(care[,c(1,3)],care$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available" & care$State==state ))
    }
    
    else if (outcome =="heart failure")
    {
        care <- na.omit(subset(care[,c(1,4)],care$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure !="Not Available" & care$State==state))
    }
    
    else if (outcome =="pneumonia")
    {
        care <-na.omit(subset(care[,c(1,5)],care$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia !="Not Available" & care$State==state))
        
    }
    
    if (rnk=="best") rnk=1
    else if (rnk=="worst") { 
        rnk=nrow(care) 
        }
    else rnk
    
    if (nrow(care) >= rnk)
    {
    care[,2] <- as.numeric(care[,2])
    care <- arrange(care,care[,2],Hospital.Name)
    care$Rank <- row_number(care[,2])
    besthospital <- subset(care$Hospital.Name,care$Rank==rnk)
    }
    else besthospital <- c("NA")
    besthospital 
}
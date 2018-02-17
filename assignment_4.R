rm(list=ls(all=TRUE))
getwd()
setwd("/Users/Xiuyuan/Documents/Coursera/Data Science/R Programming")
getwd()
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

source("best.R")
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

source("rankhospital.R")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)

source("rankall.R")
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)



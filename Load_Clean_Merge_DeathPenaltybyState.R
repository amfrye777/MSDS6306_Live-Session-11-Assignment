#install.packages("sas7bdat")

library(sas7bdat)
library(formattable)
library(XML)

Dataset1<-read.sas7bdat("D:/Documents/School/SMU/2016 Summer/MSDS 6306 - Into to Data Science/Assignments/Week 11/dataset1.sas7bdat")

getKeyStats_xpath <- function() {
  URL <- "http://www.infoplease.com/ipa/A0004916.html"
  html_text <- htmlParse(URL, encoding="UTF-8")
  html_text
  #search for <td> nodes anywhere that have class 'yfnc_tablehead1'
  nodes <- getNodeSet(html_text, "/*//div[@class='center']//td")
  
  if(length(nodes) > 0 ) {
    measures <- sapply(nodes, xmlValue)
    
    #Clean up the loaded values name
    measures <- gsub(" $","",gsub("  "," ",gsub("  "," ",gsub("  "," ",gsub("  "," ",gsub("\n","",gsub("[0-9]","",gsub("[:,:]","",gsub("[:*:]","",gsub("\\s*$","",gsub("^\\s*","",measures)))))))))))
    measures <-as.data.frame(cbind(measures,c("Odd","Even")))
    names(measures)<-c("Measure","positionType")
    
    #define header/values from measures

    States<- as.character(measures$Measure[which(measures$positionType == "Odd")])
    DeathPenaltyDesc <- as.character(measures$Measure[which(measures$positionType == "Even")])
    
    #Create Data.Frame
    df <- as.data.frame(cbind(States,DeathPenaltyDesc))
    df <- df[which(df$States != '' & df$DeathPenaltyDesc != ''),] 
    df$DeathPenaltyCode[which(df$DeathPenaltyDesc == 'No death penalty')] <- 0
    df$DeathPenaltyCode[which(df$DeathPenaltyDesc != 'No death penalty')] <- 1
    
    return(df)
  } else {
    break
  }
}

DeathPenalty <- getKeyStats_xpath()

formattable(DeathPenalty)


MergedData <- merge(Dataset1,DeathPenalty,by.x = "State", by.y = "States", all.x = TRUE)

formattable(MergedData)

write.csv(DeathPenalty, "DeathPenaltyStatsNotMerged.csv",row.names=TRUE)  
write.csv(MergedData, "DeathPenaltyStatsMerged.csv",row.names=TRUE)  

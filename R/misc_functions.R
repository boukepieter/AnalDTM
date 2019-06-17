get.newcols <- function(date){
  if (!class(date)=="Date"){stop("give a 'Date' as input")}
  c(paste("families_abs",date,sep="_"),paste("families_diff_abs",date,sep="_"),
    paste("families_diff_perc",date,sep="_"))
}

compareReturnees <- function(blineVec,targetVec){
  if (ncol(blineVec)!=2 | ncol(targetVec) != 2){stop("Input must have 2 columns exactly")}
  result <- targetVec
  result[which(targetVec[,1] %in% blineVec[,1] == TRUE),"diff"] <- 
    targetVec[which(targetVec[,1] %in% blineVec[,1] == TRUE),2] - 
    blineVec[which(blineVec[,1] %in% targetVec[,1] == TRUE),2]
  
  result[which(targetVec[,1] %in% blineVec[,1] == FALSE),"diff"] <-
    targetVec[which(targetVec[,1] %in% blineVec[,1] == FALSE),2]
  
  result[which(result[,1] %in% blineVec[,1] == TRUE),"diff_perc"] <- 
    round(result[which(result[,1] %in% blineVec[,1] == TRUE),"diff"] / 
            blineVec[which(blineVec[,1] %in% result[,1] == TRUE),2] * 100, digits=1)
  result
}

summarize.returnees <- function(absValues, group){
  group <- enquo(group)
  summarised <- absValues %>%
    group_by(!!group) %>%
    summarise_if(is.numeric, sum, na.rm=T)
  summarised[,seq(4, ncol(summarised),3)] <- 
    round(summarised[,seq(3, ncol(summarised),3)] / summarised[,seq(2, ncol(summarised),3)] * 100,1)
  summarised
}


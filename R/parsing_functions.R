read.DTM <- function(file, sheet, columns){
  if (!requireNamespace("xlsx", quietly = TRUE)) {
    stop("Package \"xlsx\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  data <- xlsx::read.xlsx(paste("input/",file,sep=""),sheetName=sheet, startRow=1, colIndex=1:columns)
  data <- data[-nrow(data),]
  i <- sapply(data, is.factor)
  data[i] <- lapply(data[i], as.character)
  data <- data[order(data$Location.ID),]
  #target[(nrow(target)-5):nrow(target),1:13]
  data
}

extract.bline.from.dbase <- function(dbase, target) {
  bline <- target
  bline[which(bline$Location.ID %in% dbase$Location.ID == TRUE),12] <-
    dbase[which(dbase$Location.ID %in% target$Location.ID == TRUE),length(names(dbase))-2]
  bline[which(bline$Location.ID %in% dbase$Location.ID == FALSE),12] <- 0
  bline
}
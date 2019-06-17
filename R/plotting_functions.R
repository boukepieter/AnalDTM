plot.returnee.graph <- function(summarised,loc){
  test <- t(summarised[,seq(1,(ncol(summarised)-3),3)+1])
  df <- data.frame(date=substr(rownames(test),14,24), returnees=test[,1], stringsAsFactors=F)
  df$date[nrow(df)] <- gsub("-",".",df$date[nrow(df)])
  df$date <- as.Date(df$date, format="%Y.%m.%d")
  
  ggplot(df, aes(date, returnees)) + geom_line() + geom_point() +
    scale_x_date(date_labels= "%b-%Y") + xlab("") + ylab("Returnees") + 
    scale_color_grey() + 
    ggtitle(summarised[,1])#district_shapes@data[match(pull(summarised[n,],OCHA_admin_2), district_shapes$A2Code),2])
  #+ theme_classic()
  ggsave(loc, scale=2,width=3, height=2, dpi=100)
  link <- sprintf('%s.png',summarised[1,1])
  link
}

plot.returnee.graph.shiny <- function(summarised){
  test <- t(summarised[,seq(1,(ncol(summarised)-3),3)+1])
  df <- data.frame(date=substr(rownames(test),14,24), returnees=test[,1], stringsAsFactors=F)
  df$date[nrow(df)] <- gsub("-",".",df$date[nrow(df)])
  df$date <- as.Date(df$date, format="%Y.%m.%d")
  
  g <- ggplot(df, aes(date, returnees)) + geom_line() + geom_point() +
    scale_x_date(date_labels= "%b-%Y") + xlab("") + ylab("Returnees") + 
    scale_color_grey() + ggtitle(as.character(summarised[,1]))
  #district_shapes@data[match(pull(summarised[n,],OCHA_admin_2), district_shapes$A2Code),2])
  #+ theme_classic()
  g
}


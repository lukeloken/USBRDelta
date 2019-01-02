# Clean outliers using sensorQC
# Need to update table of rules in Flame/SensorQC
# Rules can be any True/False expression but need to be included in quotes (e.g.,  'x < 0', 'MAD(x)>3')
# datatable must be a data.frame with "times" as the first column. All columns with a rule specified will be cleaned

library(devtools)
library(dplyr)
# install.packages("sensorQC",
#                  repos = c("http://owi.usgs.gov/R","http://cran.rstudio.com/"),
#                  dependencies = TRUE)
library(sensorQC)

# ==========================================================
# Rolling MAD function. 
# Windows subsequent observations and computes local MAD statistic
# ==========================================================

rollingMAD<-function (x, Median_Window, MAD_Window){
  
  # Create empty vector for computed MAD values
  WindowedMAD<-as.numeric(rep(NA, length(x)))
  
  for (obs in 1:length(x)){
    # Set interval of observations for MAD
    # truncate interval for observations near start and end
    MADseq<-c((obs-MAD_Window):(obs+MAD_Window))
    MADseq<-MADseq[which(MADseq<=length(x) & MADseq>=1)]
    MADinterval<-x[MADseq]
    
    # Set interval of observations for median
    # truncate interval for observations near start and end
    Medseq<-c((obs-Median_Window):(obs+Median_Window))
    Medseq<-Medseq[which(Medseq<=length(x) & Medseq>=1)]
    Medinterval<-x[Medseq]
    
    # Compute MAD
    localmedian<-median(Medinterval, na.rm=TRUE)
    localmad<-mad(MADinterval, na.rm=TRUE)
    WindowedMAD[obs]<-abs(x[obs]-localmedian)/localmad
  }
  
  # Replace infinite values (mad==0) with NA
  WindowedMAD[which(is.finite(WindowedMAD)==FALSE)]<-NA
  
  # Return vector of windowed MAD values.
  # Equal length to x
  return (WindowedMAD)
}

# ==========================================================
# Function to clean multiple parameters of a dataframe
# col1 = times; col2 and up == variables
# Requires a data.frame of rules that match datatable column names
# window = number of observations to include in both directions for rollingMAD
# ==========================================================

sensorclean<-function(datatable, ruletable){

  library(devtools)
  library(dplyr)
  library(sensorQC)
  
# Create emtpy datatable to fill with cleaned values. 
# Keep only column 1 (times)
clean_df<- datatable
data_df<- datatable
time_col<-which(names(clean_df)=='DateTime')

clean_df[,-time_col]<-NA

#Clean all variables in datatable except datetime column
col=2
par(mar=c(3,3,1,1), mgp=c(2,1,0))
for (col in which(names(clean_df)!='DateTime')){
  
  # Create sensor object and fill 'w' slot with rollingMAD
  data_col<-data_df[,c(time_col,col)]
  rules<- unlist(ruletable[which(ruletable[,1]==names(data_col)[2]), -1:-4])
  
  if (length(rules)>0 & class(data_col[,2]) %in% c('numeric', 'integer')){

    sensored<-sensor(data_col)
    MADwindow<- unlist(ruletable[which(ruletable[,1]==names(data_col)[2]), 2])
    Medianwindow<- unlist(ruletable[which(ruletable[,1]==names(data_col)[2]), 3])
    sensored$sensor$w<-rollingMAD(sensored$sensor$x, Medianwindow, MADwindow)
    
    PersistMax<-unlist(ruletable[which(ruletable[,1]==names(data)[2]), 4])
    p<-persist(sensored$sensor$x)
    p_plus<-c(NA,p[-length(p)])
    
    sensored$sensor$x[which(p_plus>PersistMax)]<-NA
    
    # Set rules manually or from ruletable and clean sensor object
    # rules<-as.character(c('w>3', 'is.na(x)'))
    
    clean_df[,col]<-clean(sensored, rules, replace=NA)$sensor[,2] 
    
    # Plot flagged (red) and retained (black) observations 
    flags_plot<-data_col[,2]
    flags_plot[which(flags_plot==clean_df[,col])]<-NA
    plot(clean_df[,col], col="black", ylab=names(data_col)[2], xlab="time", ylim=range(data_col[,2], na.rm=TRUE))
    points(flags_plot, col="red", pch=16)
    
    rm(data_col)
    rm(sensored)
    rm(flags_plot)
    rm(p)
    rm(p_plus)
  } else { 
    clean_df[,col]<-data_col[,2]
    
  }

}

return(clean_df)
rm(clean_df)
rm(data_df)
}

# =====================================================
# End sensorclean script
# =====================================================
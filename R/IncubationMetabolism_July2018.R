
#Load libraies

library(readxl)
library(openxlsx)

library(plyr)
library(dplyr)
library(tidyr)

library(ggplot2)
library(gridExtra)

#Function to read in all sheets from an excel file
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#Read in and format data
setwd("E:/Dropbox/USBR Delta Project")

list.files('Data/Incubations/071218')

mysheets <- read_excel_allsheets('Data/Incubations/071218/Delta071218_incubation.xlsx')

str(mysheets)

DOdata<-mysheets[[2]]
JarData<-mysheets[[1]]

times<-names(DOdata[-1])

times2<-convertToDateTime(times, origin = "1900-01-01", tz='America/Los_Angeles')
attr(times2, "tzone") <- 'America/Los_Angeles'
times_diff<-diff(times2)
hours_diff<-as.numeric(times_diff)

names(DOdata) <- c('counter', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', 'T7', 'T8', 'T9')

DOdata$Jar<-rep(JarData$Jar__1, each=5)


#Average multiple observations of the same jar/time

DOmean <- DOdata[,-1] %>%
  group_by(Jar) %>%
  summarize_all(mean)

DOmedian <- DOdata[,-1] %>%
  group_by(Jar) %>%
  summarize_all(median)

DOsd <- DOdata[,-1] %>%
  group_by(Jar) %>%
  summarize_all(sd)

png('Figures/Incubations/2018JulyMetabolism_WithinSampleSD.png', width=5, height=4, units='in', res=200)
par(mar=c(3,3,0.5,0.5))
par(mgp=c(3,0.5,0))

boxplot(DOsd[2:10], col=c(rep(c('grey50', 'darkgreen'),4), 'grey50'), boxwex=0.5, cex=0.8)
axis(1, at=1:9, line=1, labels=c(rep(c('AM', 'PM'),4), 'AM'), tick=F, lty=0)
mtext('Within measurement SD (mg O2/L)', 2, 2)
mtext('Timepoint', 1, 2.5)

dev.off()

#Calculate metabolism for each jar/time
#Note that ER is negative
DOchange<-as.data.frame(matrix(nrow=nrow(DOmean), ncol=(length(times))))
names(DOchange)<-c('Jar', 'NEP1', 'ER1', 'NEP2', 'ER2', 'NEP3', 'ER3', 'NEP4', 'ER4')
DOchange[,1]<-DOmean$Jar

row=1
for (row in 1:nrow(DOmean)){
  DO <- as.numeric(DOmean[row,2:10])
  DOdiff <- diff(DO)
  DOdiff_per_h <- DOdiff/hours_diff
  DOchange[row,2:length(times)]<- round(DOdiff_per_h,3)
  
}

DOchange$GPP1<-DOchange$NEP1-DOchange$ER1
DOchange$GPP2<-DOchange$NEP2-DOchange$ER2
DOchange$GPP3<-DOchange$NEP3-DOchange$ER3
DOchange$GPP4<-DOchange$NEP4-DOchange$ER4


DOchange$Treatment<-JarData$Treatment__1[match(DOchange$Jar, JarData$Jar__1)]
DOchange$Site<-JarData$Site__1[match(DOchange$Jar, JarData$Jar__1)]

DOchange_long_table<- DOchange %>%
  gather("Metric", "Value", 2:13)

DOchange_long_table$Day<-as.numeric(gsub("[^0-9]", "", DOchange_long_table$Metric) )
DOchange_long_table$Metric<-gsub('[0-9]+', '', DOchange_long_table$Metric)

#Create tables of each metabolism metric
GPPtable<- DOchange_long_table %>%
  filter(Metric=='GPP')
GPPrange<-range(GPPtable$Value, na.rm=T)

ERtable<- DOchange_long_table %>%
  filter(Metric=='ER')
ERrange<-range(ERtable$Value, na.rm=T)

NEPtable<- DOchange_long_table %>%
  filter(Metric=='NEP')
NEPrange<-range(NEPtable$Value, na.rm=T)


#Plotting parameters
jitterwidth=0.15

GPP_site34 <-ggplot(GPPtable[GPPtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  # ylim(GPPrange) +
  ggtitle('Site 34') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position = c(0.25, 0.7))

GPP_site64 <-ggplot(GPPtable[GPPtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 64') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

GPP_site70 <-ggplot(GPPtable[GPPtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 70') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

GPP_site74 <-ggplot(GPPtable[GPPtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='GPP') +
  ggtitle('Site 74') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

# grid.arrange(GPP_site34, GPP_site64, GPP_site70, GPP_site74 , ncol=1)



ER_site34 <-ggplot(ERtable[ERtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 34') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

ER_site64 <-ggplot(ERtable[ERtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 64') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

ER_site70 <-ggplot(ERtable[ERtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 70') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

ER_site74 <-ggplot(ERtable[ERtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='ER') +
  ggtitle('Site 74') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

# grid.arrange(ER_site34, ER_site64, ER_site70, ER_site74 , ncol=1)



NEP_site34 <-ggplot(NEPtable[NEPtable$Site=='NL34',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 34') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

NEP_site64 <-ggplot(NEPtable[NEPtable$Site=='NL64',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 64') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

NEP_site70 <-ggplot(NEPtable[NEPtable$Site=='NL70',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 70') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

NEP_site74 <-ggplot(NEPtable[NEPtable$Site=='NL74',], aes(Day, Value, colour=Treatment)) + 
  labs(x='Day', y='NEP') +
  ggtitle('Site 74') +
  geom_jitter(size=2, width=jitterwidth, height=0) + 
  geom_smooth(method='lm', se=F) +
  theme_bw() +
  theme(plot.title = element_text(hjust=0.5), legend.position="none")

# grid.arrange(NEP_site34, NEP_site64, NEP_site70, NEP_site74 , ncol=1)

png('Figures/Incubations/2018JulyMetabolism_Timeseries.png', width=8, height=12, units='in', res=200)

grid.arrange(GPP_site34, GPP_site64, GPP_site70, GPP_site74, ER_site34, ER_site64, ER_site70, ER_site74, NEP_site34, NEP_site64, NEP_site70, NEP_site74 , ncol=3, as.table=F)

dev.off()

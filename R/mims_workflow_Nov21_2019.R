


############################################################################################################################################################################
# Processing raw MIMS data to calculate concentrations & ratios
# Original code from Hilary Madinger and Bob Hall
# 16 July 2016

# Updated by Luke Loken
# Aug 2018

# Execute the functions in MIMS_gas_functions.R before this code. 
# MIMS_gas_functions.R calculates gas concentrations at saturation, while correcting for water density, temperature, and barometric presssure, which are used in the functions below. 
# Using gas saturations, two water bath calibrations (at different temperatures), and the data copied and pasted from the MIMS, this code converts mass signals into concentrations/ratios. 
############################################################################################################################################################################


###########################################
# DATA IMPORT AND MANAGEMENT
###########################################	

# I have had problems with .csv files removing decimal places for values from the MIMS. One way to fix this is to use the readxl package to read in an excel file instead of a .csv file.
library(readxl)
# library(RcppRoll)

#load gas functions
source('R/mims_gas_functions.R')
#load processing function
source('R/MIMScalc.R')

data_dir<-'C:/Dropbox/USBR Delta Project'

# Call and name the MIMS data. 
# MIMSdata_Run1<-read_excel(paste0(data_dir, "/Data/MIMS/MIMS_Run1_2018_08_18.xlsx"))
# MIMSdata_Run2<-read_excel(paste0(data_dir, "/Data/MIMS/MIMS_Run2_2018_08_18.xlsx"))
MIMSdata_Run1<-read_excel(paste0(data_dir, "/Data/MIMS/Loken_MIMS_SSC_2019-11-21.xlsx"))
# MIMSdata_Run1<-read_excel(paste0(data_dir, "/Data/MIMS/Loken_MIMS_SSC_2019-11-20_v2.xlsx"))



#Rename columns
names(MIMSdata_Run1)[match(c('18', '28', '32', '40', 'N2/Ar', 'O2/Ar'), names(MIMSdata_Run1))]<-c('X18', 'X28', 'X32', 'X40', 'N2.Ar', 'O2.Ar')
# names(MIMSdata_Run2)[match(c('18', '28', '32', '40', 'N2/Ar', 'O2/Ar'), names(MIMSdata_Run2))]<-c('X18', 'X28', 'X32', 'X40', 'N2.Ar', 'O2.Ar')

if ('34' %in% names(MIMSdata_Run1)){
  names(MIMSdata_Run1)[match(c('34', 'O2-18/Ar'), names(MIMSdata_Run1))]<-c('X34', 'O2-18.Ar')
}
# if ('34' %in% names(MIMSdata_Run2)){
#   names(MIMSdata_Run2)[match(c('34', 'O2-18/Ar'), names(MIMSdata_Run2))]<-c('X34', 'O2-18.Ar')
# }

#Default pressure for sea level
MIMSdata_Run1$Pressure[which(is.na(MIMSdata_Run1$Pressure))] <- 760

# The needed columns for this code include: 
# X28, X32, X40, N2.Ar, O2.Ar = columns from the MIMS output. These can come from MIMS_datasheet_mean_example.R too.  
# Temp = waterbath or water sample temperature in Celcius
# Pressure = air pressure when sample was collected or when running samples in mmHg (barometric pressure conversions also in MIMS_gas_functions.R)
# Sampletype = datasheet column distinguishing data from calibrating the MIMS and samples (Calibrate/Samp)
# Calibnum = datasheet column where each calibration period has a seperate, sequential number (1, 2, 3, ect. )
# Calibtype = datasheet column distinguishing colder and warmer calibration temperatures (Low/High)
# Useful metadata may include sample ID, treatment type, sample location, time and date of sample collection, ect. 
# There is also a comments column for including any irregularities or observations while running the MIMS



###########################################
# Process data
###########################################

MIMS_outdf_Run1 <-MIMScalc(MIMSdata_Run1)   #Name the file made in the MIMScalc function anything you would like.
# MIMS_outdf_Run2 <-MIMScalc(MIMSdata_Run2) 

# MIMS_outdf_Full<-full_join(MIMS_outdf_Run1, MIMS_outdf_Run2)
MIMS_outdf_Full<-MIMS_outdf_Run1

# write.csv(MIMS_outdf_Full, paste0(data_dir, "/Data/MIMS/Outputs/MIMScalculations_2019-11-20.csv"))  #Save the file to your computer or dropbox.

## The new datafile is only data from samples
## You can delete the new first column without a header (it's just a count of the samples). 

## The resulting narcalc and O2arcalc columns are the ratios you then use for calculations. Nar and O2Ar are unprocessed numbers. 
## The '_calc' are the now calibrated concentration and ratio values. 
## The ratios are more accurate than concentrations calculated from the MIMS samples because the machine is better at measureing ratios of masses than individual masses. 


# ##################################
# Plotting
# Should probably put this in a different script
# ##################################

MIMS_outdf_Full$SampleID <- gsub('Prosp', 'Pro', MIMS_outdf_Full$SampleID )

sitesIDs<-factor(MIMS_outdf_Full$SampleID)

# png(paste0(data_dir, '/Figures/MIMS/2019_ArbTest_MIMS_Boxplots.png'), width=5, height=8, units='in', res=200)
png(paste0(data_dir, '/Figures/MIMS/2018_SSC_EC6_Deep.png'), width=5, height=8, units='in', res=200)

par(mar=c(2,3.75,0.5,0.5))
par(oma=c(1,0,0,0))
par(mgp=c(3,0.5,0))
par(mfrow=c(4,1))
par(cex.axis=.8)
colors<-c('#1f78b4', '#b2df8a')

boxplot(MIMS_outdf_Full$O2calc~ MIMS_outdf_Full$Date, col=colors[1])
# boxplot(MIMS_outdf_Full$O2satv ~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O2satv), lty=3)
mtext(expression(paste(O[2], ' (mg L'^'-1', ')')), 2, 1.75)

# legend('topleft', 'Saturation', lty=3, bty='n')

boxplot(MIMS_outdf_Full$O2arcalc~ MIMS_outdf_Full$Date, col=colors[1])
# boxplot(MIMS_outdf_Full$O2arsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O2arsatv), lty=3)
mtext(expression(paste(O[2], ':Ar', ' (molar ratio)')), 2,  1.75)


boxplot(MIMS_outdf_Full$ncalc~ MIMS_outdf_Full$Date, col=colors[2])
# boxplot(MIMS_outdf_Full$nsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$nsatv), lty=3)
mtext(expression(paste(N[2], ' (mg L'^'-1', ')')), 2,  1.75)


boxplot(MIMS_outdf_Full$narcalc~  MIMS_outdf_Full$Date , col=colors[2])
# boxplot(MIMS_outdf_Full$narsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$narsatv), lty=3)
mtext(expression(paste(N[2], ':Ar', ' (molar ratio)')), 2,  1.75)

mtext('Station ID', 1, 0, outer=T)

dev.off()



# png(paste0(data_dir, '/Figures/MIMS/2018August_MIMS_Boxplots_Conc.png'), width=6, height=4, units='in', res=200)
par(mar=c(2,3.75,0.5,0.5))
par(oma=c(1,0,0,0))
par(mgp=c(3,0.5,0))
par(mfrow=c(2,1))
par(cex.axis=.7)
colors<-c('#1f78b4', '#b2df8a')

boxplot(MIMS_outdf_Full$O2calc~ MIMS_outdf_Full$SampleID, col=colors[1])
# boxplot(MIMS_outdf_Full$O2satv ~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O2satv ), lty=3)
mtext(expression(paste(O[2], ' (mg L'^'-1', ')')), 2, 1.75)

# legend('topleft', 'Saturation', lty=3, bty='n')

boxplot(MIMS_outdf_Full$ncalc~ MIMS_outdf_Full$SampleID, col=colors[2])
# boxplot(MIMS_outdf_Full$nsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$nsatv), lty=3)
mtext(expression(paste(N[2], ' (mg L'^'-1', ')')), 2,  1.75)

mtext('Station ID', 1, 0, outer=T)

dev.off()



if ('X34' %in% names(MIMS_outdf_Full)){

png(paste0(data_dir, '/Figures/MIMS/2018August_MIMS_Boxplots_O18_withRatios.png'), width=5, height=10, units='in', res=200)
par(mar=c(2,3.75,0.5,0.5))
par(oma=c(1,0,0,0))
par(mgp=c(3,0.5,0))
par(mfrow=c(5,1))
par(cex.axis=.7)
colors<-c('#1f78b4', '#b2df8a', '#a6cee3', '#fb9a99')

boxplot(MIMS_outdf_Full$O2calc~ MIMS_outdf_Full$SampleID, col=colors[1])
# boxplot(MIMS_outdf_Full$O2satv ~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O2satv ), lty=3)
mtext(expression(paste(O[2], ' (mg L'^'-1', ')')), 2, 1.75)


boxplot(MIMS_outdf_Full$O2arcalc~ MIMS_outdf_Full$SampleID, col=colors[1])
# boxplot(MIMS_outdf_Full$O2arsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O2arsatv), lty=3)
mtext(expression(paste(O[2], ':Ar', ' (molar ratio)')), 2,  1.75)


boxplot(MIMS_outdf_Full$O18calc~ MIMS_outdf_Full$SampleID, col=colors[3])
# boxplot(MIMS_outdf_Full$O18satv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O18satv, na.rm=T), lty=3)
mtext(expression(paste(''^'18','O-',  O[2], ' (mg L'^'-1', ')')), 2, 1.75)

boxplot(MIMS_outdf_Full$O18arcalc~ MIMS_outdf_Full$SampleID, col=colors[3])
# boxplot(MIMS_outdf_Full$O18arsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
abline(h=mean(MIMS_outdf_Full$O18arsatv, na.rm=T), lty=3)
mtext(expression(paste(''^'18','O-',O[2], ':Ar', ' (molar ratio)')), 2,  1.75)

boxplot(MIMS_outdf_Full$O18arcalc/MIMS_outdf_Full$O2arcalc*1000~ MIMS_outdf_Full$SampleID, col=colors[4])
# boxplot(MIMS_outdf_Full$O18arsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
# abline(h=mean(MIMS_outdf_Full$O18arsatv, na.rm=T), lty=3)
mtext(expression(paste(''^'18', 'O:', ''^'16','O-',O[2], ' (per mil)')), 2,  1.75)


dev.off()

boxplot((MIMS_outdf_Full$O18calc/MIMS_outdf_Full$O2calc/0.002005-1)*1000~ MIMS_outdf_Full$SampleID, col=colors[4])
# boxplot(MIMS_outdf_Full$O18arsatv~ MIMS_outdf_Full$SampleID, add=T, border='blue', boxwex=0.3, lwd=3)
# abline(h=mean(MIMS_outdf_Full$O18arsatv, na.rm=T), lty=3)
mtext(expression(paste(delta,''^'18', 'O (per mil)')), 2,  1.75)


}
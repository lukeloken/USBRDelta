

#Function to estimate PAR as a function of location and time

lightModel <- function (Altitude, Aspect, Slope,  Lat, localHour,
                        Long,  DayOfYear, TimeZone, DST, Transmissivity) {
  
  SolConst <- 1367  #Solar constant in Watts m-2
  DayOfYear <- as.double(DayOfYear)
  localHour <- as.double(localHour)
  
  SolarHour <- localHour + 4 / 60 * Long - TimeZone - DST
  HourAngle <- 15 * (SolarHour - 12)
  SolDec <- -23.45 * cos(360 * (DayOfYear + 10) / 365 * pi/180)
  SolAlt <- asin(sin(Lat * pi/180) * sin(SolDec * pi/180) + cos(Lat * pi/180) * cos(SolDec * pi/180) * cos(HourAngle * pi/180))
  SolAzi <- (sin(SolDec * pi/180) * cos(Lat * pi/180) * sin(Lat * pi/180) * cos(HourAngle * pi/180)) / cos(SolAlt)
  
  SolAzi[which(SolAzi>0.999)] <- 0.999
  SolAzi <- SolAzi / pi/180
  
  # if (SolAzi > 0.999) SolAzi <- 0.999 / pi/180  else SolAzi <- SolAzi / pi/180
  
  SolAziCorr<-rep(NA, length(SolAzi))
  SolAziCorr[which(SolAzi<12)]<-SolAzi
  SolAziCorr[which(SolAzi>=12)]<- 360 - SolAzi
  
  # if (HourAngle < 12) SolAziCorr <- SolAzi   else  SolAziCorr <- 360 - SolAzi
  
  CosInci <- sin(SolAlt) * cos(Slope * pi/180) + cos(SolAlt) * sin(Slope * pi/180) * cos((SolAziCorr - Aspect) * pi/180)
  AtmPressCorr <- ((288.15 - 0.0065 * Altitude) / 288.15) ^ 5.256
  OptCorr <- AtmPressCorr * Transmissivity ^ ((1229 + (614 * sin(SolAlt)) ^ 2) ^ 0.5 - 614 * sin(SolAlt))
  SOuter <- SolConst * (1 + 0.034 * cos(2 * pi * DayOfYear / 365))
  SNormal <- SOuter * OptCorr
  
  SDirect <- SNormal * CosInci
  SDirect[which(SDirect < 0)] <- 0
  
  SDiffuse <- SOuter * (0.271 - 0.294 * OptCorr) * sin(SolAlt)
  SDiffuse[which(SDiffuse < 0)] <- 0
  
  STotalWatts <- SDirect + SDiffuse     #in Watts m-2
  STotal <- STotalWatts * 0.45 * 4.57   #Convert Watts m-2 full spectrun irradiance (FSI) to photosynthetically
  #active radiation (PAR=0.45 X FSI; Gates 1966, Jellison and Melack 1993). Convert to microEinsteins s-1 m-2. _
  #Rough conversion is 1 W m-2 (PAR) <- 4.57 uE m-2 s-1 (PAR) (McCree 1972).
  return(STotal)
  
}



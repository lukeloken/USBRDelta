
GasSolubility <- function(t, S, gas){
  if (gas =='N2'){
    #Nitrogen Constants
    A0 <- 6.42931
    A1 <- 2.92704
    A2 <- 4.32531
    A3 <- 4.469149
    B0 <- -7.44129*10^-3
    B1 <- -8.02566*10^-3
    B2 <- -1.46775*10^-2
  } else if  (gas == 'Ar'){
    #Argon Constants
    A0 <- 2.79150
    A1 <- 3.17609
    A2 <- 4.13116
    A3 <- 4.90379
    B0 <- -6.96233*10^-3
    B1 <- -7.66670*10^-3
    B2 <- -1.16888*10^-2
  } else {
    Warning('incorrect gas name')
  }
  Ts <- log((298.15-t)/(273.15+t))
  
  lnC <- A0 + A1*Ts + A2*Ts^2 + A3*Ts^3 + S*(B0 + B1*Ts + B2*Ts^2)
  return(exp(lnC))
}





t=20
S=seq(0,1.2,.1)

N2vec<-GasSolubility(t, S, 'N2')
Arvec<-GasSolubility(t, S, 'Ar')

plot(N2vec/Arvec)

# gas concentration at 1atm umol N2 per kg water)
exp(lnC)

plot(S, exp(lnC))





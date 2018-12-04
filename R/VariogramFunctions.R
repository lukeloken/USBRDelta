

# Load Functions for variogram fitting

# function to assess fit of variogram model
# uses weighted residual sum of squares
variogramfit<-function(model, var){
  tmp<-variogramLine(model,dist_vector=var$dist)
  fit<-.5*sum(var$np/tmp$gamma^2*(var$gamma-tmp$gamma)^2)
  return(fit)
}

# Calculate effective range for variogram models
EffectiveRange<-function(model, width){
  
  psill<-model$psill[2]
  nug<-model$psill[1]
  sill95<-nug+(psill-nug)*.95
  
  distances<-seq(0,width*20000, by=width)
  table<-variogramLine(model, dist_vector=distances)
  effective_range<-min(table[which(table$gamma>sill95),1])
  
  return(effective_range)
}

# plot variogram and model as plots rather than images
# Allows user to treat plot similar to 'base' plot
vplot<-function(variogram, model, ...){
  distances<-seq(variogram$dist[1], max(variogram$dist), max(variogram$dist)/20000 )
  table<-variogramLine(model, dist_vector=distances)
  plot(variogram$dist, variogram$gamma, ...)
  lines(table, ...)
}



# Make an example figure for ecosystem response

# Project folder where outputs are stored
dropbox_dir<-'C:/Dropbox/USBR Delta Project'

#Dummy data

x<-(-2:13)*2

y1<-rnorm(length(x),4,0.5)
y2<-rnorm(length(x),4,0.5)
y3<-rnorm(length(x),4,0.5)+c(0,0,0,0,1,2,4,7,9,9,5,4,4,3,3,2)


png(paste0(dropbox_dir, '/Figures/Example_EcosystemResponse.png'), units='in', width=7, height=7, res=400, bg='white')

par(mfrow=c(2,1))
par(mar=c(1.5,2,0.5,.5), oma=c(1.5,1.5, 0,0))
par(mgp=c(2,0.5,0), tck=-0.02, ps=14, cex=1)

plot(x,y1, type='o', pch=21, bg='black', ylim=range(c(y1, y2, y3), na.rm=T), xlab='', ylab='', cex=1.5)
points(x,y2, bg='red', col='red', pch=22, type='o', cex=1.5)

abline(v=0.1, lty=2, col='grey')
legend('topright', inset=0.01, bty='n', c('Maniuplated', 'Reference'), col=c('red', 'black'), pt.bg=c('red', 'black'), pch=c(22,21), y.intersp=2, pt.cex=1.5)

plot(x,y1, type='o', pch=21, bg='black', ylim=range(c(y1, y2, y3), na.rm=T), xlab='', ylab='', cex=1.5)
points(x,y3, bg='red', col='red', pch=22, type='o', cex=1.5)

abline(v=0.1, lty=2, col='grey')

mtext('Days after maniuplation', 1, 0, outer=T)
mtext('Ecosystem response', 2, 0, outer=T)

dev.off()

# -- Add Compass to a Map-----------------------------------------------------------------------------
# -- https://stat.ethz.ch/pipermail/r-sig-geo/2010-February/007564.html

compassRose<-function(x,y,rot=0,cex=1) {
  oldcex<-par(cex=cex)
  mheight<-strheight("M")
  xylim<-par("usr")
  plotdim<-par("pin")
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180
  crspans<-rep(c(mheight*3,mheight/2),4)
  xpoints<-cos(point.angles)*crspans*xmult+x
  ypoints<-sin(point.angles)*crspans+y
  polygon(xpoints,ypoints,col="gold")
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y
  text(txtxpoints,txtypoints,c("E","N","W","S"))
  par(oldcex)
}

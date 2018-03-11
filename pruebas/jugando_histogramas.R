#===============================
#      histograms of status by strategy
#==============================

dat <- aux

StatusViv<-function(strat)
{
  sub<-dat$STATUS[dat$ESTRATEGIA==strat]
  return(sub)
}


status_Con<-StatusViv(strat="Control")
status_Lot<-StatusViv(strat="GroupLotto")
status_Adv<-StatusViv(strat="Adv.Planning")
status_Lid<-StatusViv(strat="Comm.Leaders")

#plot the histograms
par(mfrow=c(2,2))
XL<-c(0,10)
YL<-c(0,1200)
BR<-0:30-.01
XA="Status"
hist(status_Con, xlim=XL, ylim=YL, breaks=BR, main="Control", col="blue", xlab=XA)
hist(status_Lot, xlim=XL, ylim=YL, breaks=BR, main="Lotto", col="red", xlab=XA)
hist(status_Adv, xlim=XL, ylim=YL, breaks=BR, main="Adv_Planning", col="green", xlab=XA)
hist(status_Lid, xlim=XL, ylim=YL, breaks=BR, main="Lideres", col="orange", xlab=XA)

#***********************************************************************************************

# Not counting visits to Public place and uninhabited
dat$sen_vis <- rowSums(dat[c("P_Sensis","RP_Sensis","C_Sensis","R_Sensis","V_Sensis")])
aggregate(sen_vis ~ ESTRATEGIA, data = dat, mean)

#===============================
#      histograms of visit number by strategy
#==============================
  StatusViv<-function(hierarchy,strat)
  {
    sub<-dat$sen_vis[dat$ESTRATEGIA==strat & dat$STATUS==hierarchy]
    return(sub)
  }
#------------------------------------------------------------------------
# Histograma de cuantas visitas se hizo para que una vivienda sea TRATADA
#------------------------------------------------------------------------
  trat_Con<-StatusViv(hierarchy=1, strat="Control" )
  trat_Lot<-StatusViv(hierarchy=1, strat="GroupLotto")
  trat_Adv<-StatusViv(hierarchy=1, strat="Adv.Planning")
  trat_Lid<-StatusViv(hierarchy=1, strat="Comm.Leaders")
  
  #plot the histograms
  par(mfrow=c(2,2))
  XL<-c(0,21)
  YL<-c(0,350)
  BR<-0:30-.01
  XA="Visits"
  hist(trat_Con, xlim=XL, ylim=YL, breaks=BR, main="Control - Tratadas", col="blue", xlab=XA)
  hist(trat_Lot, xlim=XL, ylim=YL, breaks=BR, main="Lotto - Tratadas", col="red", xlab=XA)
  hist(trat_Adv, xlim=XL, ylim=YL, breaks=BR, main="Adv_Planning - Tratadas", col="green", xlab=XA)
  hist(trat_Lid, xlim=XL, ylim=YL, breaks=BR, main="Lideres - Tratadas", col="orange", xlab=XA)
#------------------------------------------------------------------------
  
#------------------------------------------------------------------------
# Histograma de cuantas visitas se hizo para que una vivienda sea RENUENTE
#------------------------------------------------------------------------
  renu_Con<-StatusViv(hierarchy=2, strat="Control" )
  renu_Lot<-StatusViv(hierarchy=2, strat="GroupLotto")
  renu_Adv<-StatusViv(hierarchy=2, strat="Adv.Planning")
  renu_Lid<-StatusViv(hierarchy=2, strat="Comm.Leaders")
  
  #plot the histograms
  par(mfrow=c(2,2))
  XL<-c(0,21)
  YL<-c(0,25)
  BR<-0:30-.01
  XA="Visits"
  hist(renu_Con, xlim=XL, ylim=YL, breaks=BR, main="Control - Renuente", col="blue", xlab=XA)
  hist(renu_Lot, xlim=XL, ylim=YL, breaks=BR, main="Lotto - Renuente", col="red", xlab=XA)
  hist(renu_Adv, xlim=XL, ylim=YL, breaks=BR, main="Adv_Planning - Renuente", col="green", xlab=XA)
  hist(renu_Lid, xlim=XL, ylim=YL, breaks=BR, main="Lideres - Renuente", col="orange", xlab=XA)
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Histograma de cuantas visitas se hizo para que una vivienda sea CERRADA
#------------------------------------------------------------------------
  cerr_Con<-StatusViv(hierarchy=3, strat="Control" )
  cerr_Lot<-StatusViv(hierarchy=3, strat="GroupLotto")
  cerr_Adv<-StatusViv(hierarchy=3, strat="Adv.Planning")
  cerr_Lid<-StatusViv(hierarchy=3, strat="Comm.Leaders")
  
  #plot the histograms
  par(mfrow=c(2,2))
  XL<-c(0,21)
  YL<-c(0,30)
  BR<-0:30-.01
  XA="Visits"
  hist(cerr_Con, xlim=XL, ylim=YL, breaks=BR, main="Control - Cerrada", col="blue", xlab=XA)
  hist(cerr_Lot, xlim=XL, ylim=YL, breaks=BR, main="Lotto - Cerrada", col="red", xlab=XA)
  hist(cerr_Adv, xlim=XL, ylim=YL, breaks=BR, main="Adv_Planning - Cerrada", col="green", xlab=XA)
  hist(cerr_Lid, xlim=XL, ylim=YL, breaks=BR, main="Lideres - Cerrada", col="orange", xlab=XA)
#------------------------------------------------------------------------
  
#***********************************************************************************************
  
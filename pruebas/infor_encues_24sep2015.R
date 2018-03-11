#Direccion de la carpeta
setwd('/home/gianfranco/Descargas')
bic_consul.csv=read.csv("BD_COMMCARE_BICHOS 01_09_15.csv", stringsAsFactors=FALSE)
esf_consul.csv=read.csv("BD_COMMCARE_ESFUERZOS 01_09_15.csv", stringsAsFactors=FALSE) 
exp_consul.csv=read.csv("BD_COMMCARE_EXPERIENCIAS 01_09_15.csv", stringsAsFactors=FALSE)
bic_sep.csv=read.csv("bichos_set.csv", stringsAsFactors=FALSE)
esf_sep.csv=read.csv("esfuerzos_set.csv", stringsAsFactors=FALSE)
exp_sep.csv=read.csv("experiencias_set.csv", stringsAsFactors=FALSE)

#Obtener solo columnas que me sirven
bic_consul.csv<- bic_consul.csv[c("unicode.P.D.L.V","nombre","info.completed_time","info.username")]
esf_consul.csv<- esf_consul.csv[c("unicode.P.D.L.V","nombre","info.completed_time","info.username")]
exp_consul.csv<- exp_consul.csv[c("unicode.P.D.L.V","Nombre","info.completed_time","info.username")]
bic_sep.csv<- bic_sep.csv[c("unicode.P.D.L.V","nombre","info.completed_time","info.username")]
esf_sep.csv<- esf_sep.csv[c("unicode.P.D.L.V","nombre","info.completed_time","info.username")]
exp_sep.csv<- exp_sep.csv[c("unicode.P.D.L.V","Nombre","info.completed_time","info.username")]

#unir
bic_consul.csv<- rbind(bic_consul.csv,bic_sep.csv)
esf_consul.csv<- rbind(esf_consul.csv,esf_sep.csv)
exp_consul.csv<- rbind(exp_consul.csv,exp_sep.csv)

#eliminar data.frame que no sirven
rm(bic_sep.csv)
rm(esf_sep.csv)
rm(exp_sep.csv)
#ordernar
bic_consul.csv <- bic_consul.csv[order(bic_consul.csv$unicode.P.D.L.V),]
esf_consul.csv <- esf_consul.csv[order(esf_consul.csv$unicode.P.D.L.V),]
exp_consul.csv <- exp_consul.csv[order(exp_consul.csv$unicode.P.D.L.V),]
########################
#SOLUCIONANDO DUPLICADOS
#Se equivocaron al digitar en la tablet
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.22.63" & bic_consul.csv$info.completed_time=="6/2/2015 20:20:00"),1]<-"1.1.22.63A"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.26.227" & bic_consul.csv$info.completed_time=="7/3/2015 9:50:00"),1]<-"1.1.27.22"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.27.85" & bic_consul.csv$info.completed_time=="7/3/2015 11:06:00"),1]<-"1.1.27.89"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.27.85" & bic_consul.csv$info.completed_time=="7/7/2015 10:42:00"),1]<-"1.1.27.82"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.29.81" & bic_consul.csv$info.completed_time=="6/3/2015 21:00:00"),1]<-"1.1.23.81"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.130" & bic_consul.csv$info.completed_time=="5/21/2015 23:03:00"),1]<-"1.1.34.271"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.130" & bic_consul.csv$info.completed_time=="5/31/2015 13:11:00"),1]<-"1.1.34.427"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.427" & bic_consul.csv$info.completed_time=="5/31/2015 22:22:00"),1]<-"1.1.34.30"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.500" & bic_consul.csv$info.completed_time=="6/28/2015 7:33:00"),1]<-"1.1.34.501"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.160" & bic_consul.csv$info.completed_time=="5/26/2015 11:24:00"),1]<-"1.1.39.208"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.160" & bic_consul.csv$info.completed_time=="8/19/2015 13:01:00"),1]<-"1.1.39.260"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.78.105" & bic_consul.csv$info.completed_time=="8/6/2015 10:17:00"),1]<-"1.1.78.115"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.78.219" & bic_consul.csv$info.completed_time=="8/10/2015 10:44:00"),1]<-"1.1.78.195"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.8.96" & bic_consul.csv$info.completed_time=="6/17/2015 10:03:00"),1]<-"1.1.8.96A"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.466" & bic_consul.csv$info.completed_time=="6/8/2015 11:56:00"),1]<-"1.1.34.645A"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.13.76" & esf_consul.csv$info.completed_time=="7/8/2015 10:44:00"),1]<-"1.1.13.77"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.13.9" & esf_consul.csv$info.completed_time=="7/3/2015 15:06:00"),1]<-"1.1.26.5"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.26.9" & esf_consul.csv$info.completed_time=="7/14/2015 11:43:00"),1]<-"1.1.26.7"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.466" & esf_consul.csv$info.completed_time=="6/15/2015 11:25:00"),1]<-"1.1.34.465"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.500" & esf_consul.csv$info.completed_time=="6/28/2015 12:06:00"),1]<-"1.1.34.501"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.70" & esf_consul.csv$info.completed_time=="5/28/2015 20:03:00"),1]<-"1.1.34.75"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.78.105" & esf_consul.csv$info.completed_time=="8/7/2015 8:06:00"),1]<-"1.1.78.115"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.82.52" & esf_consul.csv$info.completed_time=="9/1/2015 9:07:00"),1]<-"1.1.82.51"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.71.93" & esf_consul.csv$info.completed_time=="11/09/2015 14:06"),1]<-"1.1.71.80"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.78.566" & esf_consul.csv$info.completed_time=="11/09/2015 14:58"),1]<-"1.1.78.556"
esf_consul.csv[which(esf_consul.csv$unicode.P.D.L.V == "1.1.22.51" & esf_consul.csv$info.completed_time=="5/27/2015 22:50:00"),1]<-"1.1.22.151"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.27.95" & exp_consul.csv$info.completed_time=="02/09/2015 13:21"),1]<-"1.1.27.85"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.29.111A" & exp_consul.csv$info.completed_time=="6/25/2015 12:43:00"),1]<-"1.1.29.111"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.123" & exp_consul.csv$info.completed_time=="8/25/2015 13:50:00"),1]<-"1.1.34.153"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.479" & exp_consul.csv$info.completed_time=="8/21/2015 14:15:00"),1]<-"1.1.34.479A"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.501" & exp_consul.csv$info.completed_time=="8/2/2015 12:56:00"),1]<-"1.1.34.501A"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.62" & exp_consul.csv$info.completed_time=="13/09/2015 11:38"),1]<-"1.1.34.621"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.160" & exp_consul.csv$info.completed_time=="8/19/2015 13:02:00"),1]<-"1.1.39.260"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.27.114" & exp_consul.csv$info.completed_time=="15/09/2015 14:44"),1]<-"1.1.27.214"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.19" & exp_consul.csv$info.completed_time=="10/09/2015 14:39"),1]<-"1.1.39.18"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.71.14" & exp_consul.csv$info.completed_time=="29/09/2015 14:41"),1]<-"1.1.71.72"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.77.176" & exp_consul.csv$info.completed_time=="29/09/2015 11:27"),1]<-"1.1.77.161"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.78.544" & exp_consul.csv$info.completed_time=="24/09/2015 13:51"),1]<-"1.1.78.542"
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.78.575" & exp_consul.csv$Nombre=="Marisol  Estofanero"),1]<-"1.1.78.571"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.271" & bic_consul.csv$info.completed_time=="5/23/2015 0:14:00"),1]<-"1.1.34.16"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.645A" & bic_consul.csv$info.completed_time=="6/23/2015 13:54:00"),1]<-"1.1.34.491"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.208" & bic_consul.csv$info.completed_time=="5/28/2015 10:21:00"),1]<-"1.1.39.133"
#No marcaron en su registro, casa no tenia codigo
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.22.64" & bic_consul.csv$info.username=="user08"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.153" & bic_consul.csv$info.username=="user01"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.36.34" & bic_consul.csv$info.username=="user07"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.7" & bic_consul.csv$info.username=="user07"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.44.20" & bic_consul.csv$info.username=="user02"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.164" & esf_consul.csv$info.username=="user07"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.294" & esf_consul.csv$info.username=="user09"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.71" & esf_consul.csv$info.username=="user03"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.36.34" & esf_consul.csv$info.username=="user07"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.39.7" & esf_consul.csv$info.username=="user07"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.44.20" & esf_consul.csv$info.username=="user02"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.77.93" & esf_consul.csv$info.username=="user08"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.78.293" & esf_consul.csv$info.username=="user09"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.8.93" & esf_consul.csv$info.username=="user08"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.27.87" & exp_consul.csv$info.username=="user01"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.164" & exp_consul.csv$info.username=="user07"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.294" & exp_consul.csv$info.username=="user09"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.71" & exp_consul.csv$info.username=="user03"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.36.34" & exp_consul.csv$info.username=="user07"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.8.96" & exp_consul.csv$info.username=="user08"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.22.63A" & bic_consul.csv$info.username=="user06"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.153" & exp_consul.csv$info.username=="user05"),]
#Otros
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.8.50" & bic_consul.csv$info.completed_time=="6/17/2015 8:41:00"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.177" & exp_consul.csv$info.completed_time=="6/11/2015 9:39:00"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.177" & exp_consul.csv$info.completed_time=="6/19/2015 11:43:00"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.7" & exp_consul.csv$info.completed_time=="8/26/2015 13:38:00"),]
exp_consul.csv[which(exp_consul.csv$unicode.P.D.L.V == "1.1.39.7" & exp_consul.csv$info.completed_time=="8/26/2015 13:38:00"),1]<-"1.1.39.217"
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.7.19" & exp_consul.csv$info.completed_time=="16/09/2015 15:01"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.7.83" & exp_consul.csv$info.completed_time=="10/09/2015 13:11"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.78.443" & exp_consul.csv$info.completed_time=="27/09/2015 14:59"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.81.88" & exp_consul.csv$info.completed_time=="27/09/2015 15:05"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.427" & bic_consul.csv$info.completed_time=="5/31/2015 13:11:00"),]

######
## Eliminando casas que no estaban en la muestra y tienen consentimiento
######
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.26.136"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.26.136"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.26.136"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.504"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.504"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.504"),]
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.504A"),]
esf_consul.csv<-esf_consul.csv[-which(esf_consul.csv$unicode.P.D.L.V == "1.1.34.504A"),]
exp_consul.csv<-exp_consul.csv[-which(exp_consul.csv$unicode.P.D.L.V == "1.1.34.504A"),]

###
#Cambiando fallas al ingresar el codigo
###
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.78.63"),1]<-"1.1.81.63"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.16" & bic_consul.csv$info.completed_time=="5/21/2015 9:51:00"),1]<-"1.1.34.341"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.341" & bic_consul.csv$info.completed_time=="5/26/2015 1:33:00"),1]<-"1.1.34.70"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.70" & bic_consul.csv$info.completed_time=="5/21/2015 11:20:00"),1]<-"1.1.34.396"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.396" & bic_consul.csv$info.completed_time=="5/27/2015 22:21:00"),1]<-"1.1.34.1"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.1" & bic_consul.csv$info.completed_time=="5/21/2015 9:06:00"),1]<-"1.1.34.602"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.602" & bic_consul.csv$info.completed_time=="5/29/2015 11:41:00"),1]<-"1.1.34.517"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.517" & bic_consul.csv$info.completed_time=="5/28/2015 21:51:00"),1]<-"1.1.34.282"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.282" & bic_consul.csv$info.completed_time=="5/23/2015 1:59:00"),1]<-"1.1.34.75"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.75" & bic_consul.csv$info.completed_time=="5/21/2015 12:31:00"),1]<-"1.1.34.400"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.400" & bic_consul.csv$info.completed_time=="5/27/2015 23:06:00"),1]<-"1.1.34.144"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.144" & bic_consul.csv$info.completed_time=="5/22/2015 0:22:00"),1]<-"1.1.34.294"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.294" & bic_consul.csv$info.completed_time=="5/25/2015 12:25:00"),1]<-"1.1.34.731"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.731" & bic_consul.csv$info.completed_time=="6/2/2015 22:28:00"),1]<-"1.1.34.329"
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.329" & bic_consul.csv$info.completed_time=="5/26/2015 0:21:00"),]
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.731" & bic_consul.csv$info.completed_time=="5/31/2015 9:01:00"),1]<-"1.1.34.716"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.491" & bic_consul.csv$info.completed_time=="6/9/2015 10:08:00"),1]<-"1.1.34.695"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.695" & bic_consul.csv$info.completed_time=="6/18/2015 13:02:00"),1]<-"1.1.34.543"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.543" & bic_consul.csv$info.completed_time=="6/15/2015 9:04:00"),1]<-"1.1.34.687"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.687" & bic_consul.csv$info.completed_time=="6/16/2015 8:17:00"),1]<-"1.1.34.514"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.514" & bic_consul.csv$info.completed_time=="6/10/2015 12:38:00"),1]<-"1.1.34.269"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.269" & bic_consul.csv$info.completed_time=="5/31/2015 13:26:00"),1]<-"1.1.34.600"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.600" & bic_consul.csv$info.completed_time=="6/15/2015 12:03:00"),1]<-"1.1.34.466"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.466" & bic_consul.csv$info.completed_time=="5/28/2015 14:21:00"),1]<-"1.1.34.474"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.474" & bic_consul.csv$info.completed_time=="5/28/2015 19:54:00"),1]<-"1.1.34.95"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.95" & bic_consul.csv$info.completed_time=="5/21/2015 21:08:00"),1]<-"1.1.34.151"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.151" & bic_consul.csv$info.completed_time=="5/22/2015 9:18:00"),1]<-"1.1.34.465"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.133" & bic_consul.csv$info.completed_time=="5/26/2015 10:06:00"),1]<-"1.1.39.183"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.183" & bic_consul.csv$info.completed_time=="5/27/2015 9:43:00"),1]<-"1.1.39.171"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.171" & bic_consul.csv$info.completed_time=="5/26/2015 20:23:00"),1]<-"1.1.39.181"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.181" & bic_consul.csv$info.completed_time=="5/27/2015 9:33:00"),1]<-"1.1.39.270"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.270" & bic_consul.csv$info.completed_time=="5/31/2015 20:58:00"),1]<-"1.1.39.40"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.40" & bic_consul.csv$info.completed_time=="5/25/2015 21:17:00"),1]<-"1.1.39.1"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.1" & bic_consul.csv$info.completed_time=="5/25/2015 10:31:00"),1]<-"1.1.39.93"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.93" & bic_consul.csv$info.completed_time=="5/26/2015 9:20:00"),1]<-"1.1.39.189"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.189" & bic_consul.csv$info.completed_time=="5/27/2015 20:09:00"),1]<-"1.1.39.26"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.26" & bic_consul.csv$info.completed_time=="5/25/2015 20:38:00"),1]<-"1.1.39.60"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.60" & bic_consul.csv$info.completed_time=="5/25/2015 22:17:00"),1]<-"1.1.39.2"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.2" & bic_consul.csv$info.completed_time=="5/25/2015 11:29:00"),1]<-"1.1.39.177"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.177" & bic_consul.csv$info.completed_time=="5/27/2015 8:54:00"),1]<-"1.1.39.245"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.245" & bic_consul.csv$info.completed_time=="5/29/2015 8:43:00"),1]<-"1.1.39.207"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.207" & bic_consul.csv$info.completed_time=="5/27/2015 21:06:00"),1]<-"1.1.39.90"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.90" & bic_consul.csv$info.completed_time=="5/26/2015 9:14:00"),1]<-"1.1.39.174"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.174" & bic_consul.csv$info.completed_time=="5/27/2015 8:06:00"),1]<-"1.1.39.230"
bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.230" & bic_consul.csv$info.completed_time=="5/28/2015 21:04:00"),1]<-"1.1.39.173"
bic_consul.csv<-bic_consul.csv[-which(bic_consul.csv$unicode.P.D.L.V == "1.1.39.173" & bic_consul.csv$info.completed_time=="5/26/2015 20:52:00"),]
###bic_consul.csv[which(bic_consul.csv$unicode.P.D.L.V == "1.1.34.30" & bic_consul.csv$info.completed_time=="5/31/2015 9:01:00"),1]<-"1.1.34.716"

#Eliminar duplicados
#bic_sin_dupli<- bic_consul.csv[which(!duplicated(bic_consul.csv$unicode.P.D.L.V)),]
#esf_sin_dupli<- esf_consul.csv[which(!duplicated(esf_consul.csv$unicode.P.D.L.V)),]
#exp_sin_dupli<- exp_consul.csv[which(!duplicated(exp_consul.csv$unicode.P.D.L.V)),]

#Duplicados en bichos, esfuerzos y experiencias
dup_bic <- bic_consul.csv[which(duplicated(bic_consul.csv$unicode.P.D.L.V)),1]
aux1<-bic_consul.csv[bic_consul.csv$unicode.P.D.L.V %in% dup_bic,]
dup_esf <- esf_consul.csv[which(duplicated(esf_consul.csv$unicode.P.D.L.V)),1]
aux2<-esf_consul.csv[esf_consul.csv$unicode.P.D.L.V %in% dup_esf,]
dup_exp <- exp_consul.csv[which(duplicated(exp_consul.csv$unicode.P.D.L.V)),1]
aux3<- exp_consul.csv[exp_consul.csv$unicode.P.D.L.V %in% dup_exp,]
#Aumentar una columna de bichos, esfuerzo y experiencias segund corresponda
#Eliminando las columnas q no me sirven
bic_sin_dupli$encuesta_bichos<- unlist(1)
inf_bic<- bic_sin_dupli[,-c(2,3)]
esf_sin_dupli$encuesta_esfuerzos<- unlist(1)
inf_esf<- esf_sin_dupli[,-c(2,3)]
exp_sin_dupli$encuesta_experiencias<- unlist(1)
inf_exp<- exp_sin_dupli[,-c(2,3)]

#Juntando las tablas
total_consul<-merge(inf_bic, inf_esf, by=c("unicode.P.D.L.V"), all= TRUE)
total_consul<-merge(total_consul, inf_exp, by=c("unicode.P.D.L.V"), all= TRUE)

#ordenando
total_consul<- total_consul[order(total_consul$unicode.P.D.L.V),]

#Imprimiendo en CSV
write.csv(total_consul,'Informe_Encuestas_02oct2015.csv')
write.csv(aux1,'Duplicados_bichos_07oct2015_II.csv')
write.csv(aux2,'Duplicados_esfuerzos_07oct2015.csv')
write.csv(aux3,'Duplicados_experiencias_07oct2015_II.csv')

#APUNTES
#dummyData = rep(c(1,2, 2, 2), 25)
#as.data.frame(table(dummyData))
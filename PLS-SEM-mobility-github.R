# As alternatives:
x<-read.csv("dataset_clean.csv", sep=";")[1:1003,1:128]
x<-read.csv("dataset_clean.csv", sep=";")[1:729,1:128] # random sample

library(seminr)

####### Data #######################

for (i in 1:dim(x)[1])
{if (x$BVTECH[i] == "Private"){
  x$BVTECH[i] <- "MPTU"}}
for (i in 1:dim(x)[1])
{if (x$BVTECH[i] == "Public"){
  x$BVTECH[i] <- "PTU"}}
for (i in 1:dim(x)[1])
{if (x$BVTECH[i] == "N/A"){
     x$BVTECH[i] <- "Others"}}
### bike data matrix ###
x.bike<-x[,c(12,14,18,22,26,30,
         34,37,40,22,
         57,
         63,
         66,
         96,98,100,102,104)]
colnames(x.bike)<- c("BVTECH",
                "risk1","risk2","risk3","risk4","risk5", 
                "usef1","usef2","usef3","usef4",
                "ease", "int","use",
                "bel1","bel2","bel3","bel4","bel5")

### bike groups ###
x.bike.priv<-x.bike[x.bike$BVTECH=="MPTU",]
x.bike.pub<-x.bike[x.bike$BVTECH=="PTU",]
x.bike.vru<-x.bike[x.bike$BVTECH=="VRU",]
x.bike.oth<-x.bike[x.bike$BVTECH=="Others",]


### e-scooter/e-bike data matrix ###
x.eScooter<-x[,c(12,15,19,23,27,31,
             35,38,41,23,
             58,
             64,
             67,
             96,98,100,102,104)]
colnames(x.eScooter)<- c("BVTECH",
                "risk1","risk2","risk3","risk4","risk5", 
                "usef1","usef2","usef3","usef4",
                "ease", "int","use",
                "bel1","bel2","bel3","bel4","bel5")
### eScooter groups ###
x.eScooter.priv<-x.eScooter[x.eScooter$BVTECH=="MPTU",]
x.eScooter.pub<-x.eScooter[x.eScooter$BVTECH=="PTU",]
x.eScooter.vru<-x.eScooter[x.eScooter$BVTECH=="VRU",]
x.eScooter.oth<-x.eScooter[x.eScooter$BVTECH=="Others",]

# Measurement model - constructs and weights
MM <- constructs(
  composite("Pers. beliefs",    multi_items("bel", 1:5), weights = mode_A),
  composite("Perc. usefulness", multi_items("usef", 1:4), weights = mode_A),
  composite("Ease-of-use",      single_item("ease"), weights = mode_A),
  composite("Perc. risk",       multi_items("risk", 1:5), weights = mode_A),
  composite("Intension to use", single_item("int"), weights = mode_A),
  composite("Actual use",       single_item("use"), weights = mode_A),
interaction_term(iv="Ease-of-use", "Perc. risk", method = product_indicator),
interaction_term(iv="Perc. usefulness", "Perc. risk", method = product_indicator)
)

# Structural model
SM <- relationships(
  paths(from = "Pers. beliefs",     to = c("Perc. usefulness", "Ease-of-use")),
  paths(from = "Ease-of-use",       to = c("Perc. usefulness", "Intension to use")),
  paths(from = "Perc. usefulness",  to = c("Intension to use")),
  paths(from = "Perc. risk",        to = c("Intension to use")),
  paths(from = "Intension to use",  to = c("Actual use")),
  paths(from = "Perc. usefulness*Perc. risk",  to = c("Intension to use")),
  paths(from = "Ease-of-use*Perc. risk",  to = c("Intension to use"))
)
# Model plot settings
tema<-seminr_theme_create(
  plot.title.fontsize = 30,
  mm.node.label.fontsize = 16,
  sm.node.label.fontsize = 20,
  mm.edge.label.fontsize = 15,
  sm.edge.label.fontsize = 18)

######## bike model estimation ########
seminr_bike <- estimate_pls(
  data = x.bike[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.bike <- bootstrap_model(seminr_bike, nboot = 100, cores = 1)
res.bike<-summary(boot.bike)
plot(boot.bike, theme = tema, title="Bike") 

### bike.priv ###
seminr_bike.priv <- estimate_pls(
  data = x.bike.priv[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.bike.priv <- bootstrap_model(seminr_bike.priv, nboot = 100, cores = 1)
res.bike.priv<-summary(boot.bike.priv)
#write.csv(res.bike.priv[2], file="boot_bike_priv.csv")
plot(boot.bike.priv, theme = tema, title="Bike - Private") 

### bike.pub ###
seminr_bike.pub <- estimate_pls(
  data = x.bike.pub[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.bike.pub <- bootstrap_model(seminr_bike.pub, nboot = 100, cores = 1)
res.bike.pub<-summary(boot.bike.pub)
#write.csv(res.bike.pub[2], file="boot_bike_pub.csv")
plot(boot.bike.pub, theme = tema, title="Bike - Public")

### bike.vru ###
seminr_bike.vru <- estimate_pls(
  data = x.bike.vru[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.bike.vru <- bootstrap_model(seminr_bike.vru, nboot = 100, cores = 1)
res.bike.vru <- summary(boot.bike.vru)
#write.csv(res.bike.vru[2], file="boot_bike_vru.csv")
plot(boot.bike.vru, theme = tema, title="Bike - VRU")

### bike.oth ###
seminr_bike.oth <- estimate_pls(
  data = x.bike.oth[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.bike.oth <- bootstrap_model(seminr_bike.oth, nboot = 100, cores = 1)
res.bike.oth <- summary(boot.bike.oth)
#write.csv(res.bike.oth[2], file="boot_bike_oth.csv")
plot(boot.bike.oth, theme = tema, title="Bike - Others")

######## eScooter model estimation ########
seminr_eScooter <- estimate_pls(
  data = x.eScooter[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.eScooter <- bootstrap_model(seminr_eScooter, nboot = 100, cores = 1)
res.eScooter <-summary(boot.eScooter)
plot(boot.eScooter, theme = tema, title="eScooter") 

### eScooter.priv ###
seminr_eScooter.priv <- estimate_pls(
  data = x.eScooter.priv[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.eScooter.priv <- bootstrap_model(seminr_eScooter.priv, nboot = 100, cores = 1)
res.eScooter.priv<-summary(boot.eScooter.priv)
#write.csv(res.eScooter.priv[2], file="boot_eScooter_priv.csv")
plot(boot.eScooter.priv, theme = tema, title="eScooter - MPTU") 

### eScooter.pub ###
seminr_eScooter.pub <- estimate_pls(
  data = x.eScooter.pub[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.eScooter.pub <- bootstrap_model(seminr_eScooter.pub, nboot = 100, cores = 1)
res.eScooter.pub<-summary(boot.eScooter.pub)
#write.csv(res.eScooter.pub[2], file="boot_eScooter_pub.csv")
plot(boot.eScooter.pub, theme = tema, title="eScooter - PTU")

### eScooter.vru ###
seminr_eScooter.vru <- estimate_pls(
  data = x.eScooter.vru[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.eScooter.vru <- bootstrap_model(seminr_eScooter.vru, nboot = 100, cores = 1)
res.eScooter.vru <- summary(boot.eScooter.vru)
#write.csv(res.eScooter.vru[2], file="boot_eScooter_vru.csv")
plot(boot.eScooter.vru, theme = tema, title="eScooter - VRU")

### eScooter.oth ###
seminr_eScooter.oth <- estimate_pls(
  data = x.eScooter.oth[,-1],
  measurement_model = MM,
  structural_model = SM)

boot.eScooter.oth <- bootstrap_model(seminr_eScooter.oth, nboot = 100, cores = 1)
res.eScooter.oth <- summary(boot.eScooter.oth)
#write.csv(res.eScooter.oth[2], file="boot_eScooter_oth.csv")
plot(boot.eScooter.oth, theme = tema, title="eScooter - Others")

# path coefficients table
path_coeff<-round(cbind(
      as.matrix(res.bike[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.eScooter[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.bike.priv[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.bike.pub[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.bike.vru[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.bike.oth[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.eScooter.priv[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.eScooter.pub[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.eScooter.vru[2])[,1][["bootstrapped_paths"]][1:9],
      as.matrix(res.eScooter.oth[2])[,1][["bootstrapped_paths"]][1:9]),3)
colnames(path_coeff)<-c("Bike","eScooter","Bike MPTU","Bike PTU","Bike VRU","Bike Others",
                        "eScooter MPTU","eScooter PTU","eScooter VRU","eScooter Others")
rownames(path_coeff)<-rownames(res.bike$bootstrapped_paths)
write.csv(path_coeff, file="coefficients.csv")

# Rsquare table
rsq<-round(rbind(seminr_bike.priv$rSquared[2,], 
                 seminr_bike.pub$rSquared[2,], 
                 seminr_bike.vru$rSquared[2,], 
                 seminr_bike.oth$rSquared[2,],
                 seminr_eScooter.priv$rSquared[2,], 
                 seminr_eScooter.pub$rSquared[2,], 
                 seminr_eScooter.vru$rSquared[2,], 
                 seminr_eScooter.oth$rSquared[2,]), 3)
rownames(rsq)<-c("Bike.priv", "Bike.pub", "Bike.vru", "Bike.oth", 
                 "eScooter.priv", "eScooter.pub", "eScooter.vru", "eScooter.oth")
write.csv(rsq, file = "rsq.csv")

# Effect size

effsize.bike<-summary(seminr_bike)$fSquare
effsize.bike.priv<-summary(seminr_bike.priv)$fSquare
effsize.bike.pub<-summary(seminr_bike.pub)$fSquare
effsize.bike.vru<-summary(seminr_bike.vru)$fSquare
effsize.bike.oth<-summary(seminr_bike.oth)$fSquare

effsize.eScooter<-summary(seminr_eScooter)$fSquare
effsize.eScooter.priv<-summary(seminr_eScooter.priv)$fSquare
effsize.eScooter.pub<-summary(seminr_eScooter.pub)$fSquare
effsize.eScooter.vru<-summary(seminr_eScooter.vru)$fSquare
effsize.eScooter.oth<-summary(seminr_eScooter.oth)$fSquare

## Eff size tables
# expoused values Effect Size
expvalues.bike<-rbind(effsize.bike[1,c(2,3)], 
                      effsize.bike.priv[1,c(2,3)],
                      effsize.bike.pub[1,c(2,3)],
                      effsize.bike.vru[1,c(2,3)],
                      effsize.bike.oth[1,c(2,3)])

expvalues.eScooter<-rbind(effsize.eScooter[1,c(2,3)], 
                          effsize.eScooter.priv[1,c(2,3)],
                          effsize.eScooter.pub[1,c(2,3)],
                          effsize.eScooter.vru[1,c(2,3)],
                          effsize.eScooter.oth[1,c(2,3)])

expvalues<-round(cbind(expvalues.bike,expvalues.eScooter),2)
rownames(expvalues)<- c("All sample", "MPTU", "PTU", "VRU", "Others")


# ease of use Effect Size
ease.bike<-rbind(effsize.bike[2,c(3,5)], 
                 effsize.bike.priv[2,c(3,5)],
                 effsize.bike.pub[2,c(3,5)],
                 effsize.bike.vru[2,c(3,5)],
                 effsize.bike.oth[2,c(3,5)])

ease.eScooter<-rbind(effsize.eScooter[2,c(3,5)], 
                     effsize.eScooter.priv[2,c(3,5)],
                     effsize.eScooter.pub[2,c(3,5)],
                     effsize.eScooter.vru[2,c(3,5)],
                     effsize.eScooter.oth[2,c(3,5)])

ease<-round(cbind(ease.bike,ease.eScooter),2)
rownames(ease)<- c("All sample", "MPTU", "PTU", "VRU", "Others")

# perceived usefulness*risk Effect Size
usef.bike<-rbind(effsize.bike[6,5], 
                 effsize.bike.priv[6,5],
                 effsize.bike.pub[6,5],
                 effsize.bike.vru[6,5],
                 effsize.bike.oth[6,5])

usef.eScooter<-rbind(effsize.eScooter[6,5], 
                     effsize.eScooter.priv[6,5],
                     effsize.eScooter.pub[6,5],
                     effsize.eScooter.vru[6,5],
                     effsize.eScooter.oth[6,5])

usef<-round(cbind(usef.bike,usef.eScooter),2)
rownames(usef)<- c("All sample", "MPTU", "PTU", "VRU", "Others")

# ease*risk Effect Size
easeR.bike<-rbind(effsize.bike[7,5], 
                 effsize.bike.priv[7,5],
                 effsize.bike.pub[7,5],
                 effsize.bike.vru[7,5],
                 effsize.bike.oth[7,5])

easeR.eScooter<-rbind(effsize.eScooter[7,5], 
                     effsize.eScooter.priv[7,5],
                     effsize.eScooter.pub[7,5],
                     effsize.eScooter.vru[7,5],
                     effsize.eScooter.oth[7,5])

easeR<-round(cbind(easeR.bike,easeR.eScooter),2)
rownames(easeR)<- c("All sample", "MPTU", "PTU", "VRU", "Others")


# perceived risk Effect Size
risk.bike<-rbind(effsize.bike[4,5], 
                 effsize.bike.priv[4,5],
                 effsize.bike.pub[4,5],
                 effsize.bike.vru[4,5],
                 effsize.bike.oth[4,5])

risk.eScooter<-rbind(effsize.eScooter[4,5], 
                     effsize.eScooter.priv[4,5],
                     effsize.eScooter.pub[4,5],
                     effsize.eScooter.vru[4,5],
                     effsize.eScooter.oth[4,5])

risk<-round(cbind(risk.bike,risk.eScooter),2)
rownames(risk)<- c("All sample", "MPTU", "PTU", "VRU", "Others")

# intension to use Effect Size
int.bike<-rbind(effsize.bike[5,8], 
                effsize.bike.priv[5,8],
                effsize.bike.pub[5,8],
                effsize.bike.vru[5,8],
                effsize.bike.oth[5,8])

int.eScooter<-rbind(effsize.eScooter[5,8], 
                    effsize.eScooter.priv[5,8],
                    effsize.eScooter.pub[5,8],
                    effsize.eScooter.vru[5,8],
                    effsize.eScooter.oth[5,8])

int<-round(cbind(int.bike,int.eScooter),2)
rownames(int)<- c("All sample", "MPTU", "PTU", "VRU", "Others")

## Appendix 4 results
# outer weights
bikeOutW<-c(seminr_bike$outer_weights[1:5,1],  seminr_bike$outer_weights[6:9,3],
            seminr_bike$outer_weights[10,2],   seminr_bike$outer_weights[11:15,4],
            seminr_bike$outer_weights[16,5],   seminr_bike$outer_weights[17,8],
            seminr_bike$outer_weights[18:22,7],seminr_bike$outer_weights[23:27,6],
            seminr_bike$outer_weights[28:32,6],seminr_bike$outer_weights[33:37,6],
            seminr_bike$outer_weights[38:42,6])

bike.priv.OutW<-c(seminr_bike.priv$outer_weights[1:5,1],  seminr_bike.priv$outer_weights[6:9,3],
                  seminr_bike.priv$outer_weights[10,2],   seminr_bike.priv$outer_weights[11:15,4],
                  seminr_bike.priv$outer_weights[16,5],   seminr_bike.priv$outer_weights[17,8],
                  seminr_bike.priv$outer_weights[18:22,7],seminr_bike.priv$outer_weights[23:27,6],
                  seminr_bike.priv$outer_weights[28:32,6],seminr_bike.priv$outer_weights[33:37,6],
                  seminr_bike.priv$outer_weights[38:42,6])

bike.pub.OutW<-c(seminr_bike.pub$outer_weights[1:5,1],  seminr_bike.pub$outer_weights[6:9,3],
                 seminr_bike.pub$outer_weights[10,2],   seminr_bike.pub$outer_weights[11:15,4],
                 seminr_bike.pub$outer_weights[16,5],   seminr_bike.pub$outer_weights[17,8],
                 seminr_bike.pub$outer_weights[18:22,7],seminr_bike.pub$outer_weights[23:27,6],
                 seminr_bike.pub$outer_weights[28:32,6],seminr_bike.pub$outer_weights[33:37,6],
                 seminr_bike.pub$outer_weights[38:42,6])

bike.vru.OutW<-c(seminr_bike.vru$outer_weights[1:5,1],  seminr_bike.vru$outer_weights[6:9,3],
                 seminr_bike.vru$outer_weights[10,2],   seminr_bike.vru$outer_weights[11:15,4],
                 seminr_bike.vru$outer_weights[16,5],   seminr_bike.vru$outer_weights[17,8],
                 seminr_bike.vru$outer_weights[18:22,7],seminr_bike.vru$outer_weights[23:27,6],
                 seminr_bike.vru$outer_weights[28:32,6],seminr_bike.vru$outer_weights[33:37,6],
                 seminr_bike.vru$outer_weights[38:42,6])

bike.oth.OutW<-c(seminr_bike.oth$outer_weights[1:5,1],  seminr_bike.oth$outer_weights[6:9,3],
                 seminr_bike.oth$outer_weights[10,2],   seminr_bike.oth$outer_weights[11:15,4],
                 seminr_bike.oth$outer_weights[16,5],   seminr_bike.oth$outer_weights[17,8],
                 seminr_bike.oth$outer_weights[18:22,7],seminr_bike.oth$outer_weights[23:27,6],
                 seminr_bike.oth$outer_weights[28:32,6],seminr_bike.oth$outer_weights[33:37,6],
                 seminr_bike.oth$outer_weights[38:42,6])

eScooterOutW<-c(seminr_eScooter$outer_weights[1:5,1],  seminr_eScooter$outer_weights[6:9,3],
                seminr_eScooter$outer_weights[10,2],   seminr_eScooter$outer_weights[11:15,4],
                seminr_eScooter$outer_weights[16,5],   seminr_eScooter$outer_weights[17,8],
                seminr_eScooter$outer_weights[18:22,7],seminr_eScooter$outer_weights[23:27,6],
                seminr_eScooter$outer_weights[28:32,6],seminr_eScooter$outer_weights[33:37,6],
                seminr_eScooter$outer_weights[38:42,6])

eScooter.priv.OutW<-c(seminr_eScooter.priv$outer_weights[1:5,1],  seminr_eScooter.priv$outer_weights[6:9,3],
                      seminr_eScooter.priv$outer_weights[10,2],   seminr_eScooter.priv$outer_weights[11:15,4],
                      seminr_eScooter.priv$outer_weights[16,5],   seminr_eScooter.priv$outer_weights[17,8],
                      seminr_eScooter.priv$outer_weights[18:22,7],seminr_eScooter.priv$outer_weights[23:27,6],
                      seminr_eScooter.priv$outer_weights[28:32,6],seminr_eScooter.priv$outer_weights[33:37,6],
                      seminr_eScooter.priv$outer_weights[38:42,6])

eScooter.pub.OutW<-c(seminr_eScooter.pub$outer_weights[1:5,1],  seminr_eScooter.pub$outer_weights[6:9,3],
                 seminr_eScooter.pub$outer_weights[10,2],   seminr_eScooter.pub$outer_weights[11:15,4],
                 seminr_eScooter.pub$outer_weights[16,5],   seminr_eScooter.pub$outer_weights[17,8],
                 seminr_eScooter.pub$outer_weights[18:22,7],seminr_eScooter.pub$outer_weights[23:27,6],
                 seminr_eScooter.pub$outer_weights[28:32,6],seminr_eScooter.pub$outer_weights[33:37,6],
                 seminr_eScooter.pub$outer_weights[38:42,6])

eScooter.vru.OutW<-c(seminr_eScooter.vru$outer_weights[1:5,1],  seminr_eScooter.vru$outer_weights[6:9,3],
                 seminr_eScooter.vru$outer_weights[10,2],   seminr_eScooter.vru$outer_weights[11:15,4],
                 seminr_eScooter.vru$outer_weights[16,5],   seminr_eScooter.vru$outer_weights[17,8],
                 seminr_eScooter.vru$outer_weights[18:22,7],seminr_eScooter.vru$outer_weights[23:27,6],
                 seminr_eScooter.vru$outer_weights[28:32,6],seminr_eScooter.vru$outer_weights[33:37,6],
                 seminr_eScooter.vru$outer_weights[38:42,6])

eScooter.oth.OutW<-c(seminr_eScooter.oth$outer_weights[1:5,1],  seminr_eScooter.oth$outer_weights[6:9,3],
                 seminr_eScooter.oth$outer_weights[10,2],   seminr_eScooter.oth$outer_weights[11:15,4],
                 seminr_eScooter.oth$outer_weights[16,5],   seminr_eScooter.oth$outer_weights[17,8],
                 seminr_eScooter.oth$outer_weights[18:22,7],seminr_eScooter.oth$outer_weights[23:27,6],
                 seminr_eScooter.oth$outer_weights[28:32,6],seminr_eScooter.oth$outer_weights[33:37,6],
                 seminr_eScooter.oth$outer_weights[38:42,6])

OutWeights<-cbind(bikeOutW,bike.priv.OutW,bike.pub.OutW,bike.vru.OutW,bike.oth.OutW,
                  eScooterOutW,eScooter.priv.OutW,eScooter.pub.OutW,eScooter.vru.OutW,eScooter.oth.OutW)
rownames(OutWeights)<- c("Exp.values1","Exp.values2","Exp.values3","Exp.values4","Exp.values5",
                         "Perc.usef1","Perc.usef2","Perc.usef3","Perc.usef4", "Ease of use",
                         "Perc.risk1","Perc.risk2","Perc.risk3","Perc.risk4","Perc.risk5",
                         "Int. to use","Actuale use","Ease*Risk1","Ease*Risk2","Ease*Risk3",
                         "Ease*Risk4","Ease*Risk5","Usef1*Risk1","Usef1*Risk2","usef1*Risk3",
                         "Usef1*Risk4","Usef1*Risk5","Usef2*Risk1","Usef2*Risk2","Usef2*Risk3",
                         "Usef2*Risk4","Usef2*Risk5","Rsef3*Risk1","Usef3*Risk2","Usef3*Risk3",
                         "Usef3*Risk4", "Usef3*Risk5","Usef4*Risk1","Usef4*Risk2","Usef4*Risk3",
                         "Usef4*Risk4","Usef4*Risk5")

# cross loadings bike
summary_model_bike <- summary(seminr_bike)
write.csv(summary_model_bike$validity$cross_loadings, file="crossLbike_randomS.csv")

summary_model_eScooter <- summary(seminr_eScooter)
write.csv(summary_model_eScooter$validity$cross_loadings, file="crossLeScooter_randomS.csv")

# MM assessment
summary_model_bike$validity$vif_items
write.csv(summary_model_bike$validity$vif_items, file="vif_bike.csv") 
summary_model_bike$validity$htmt # reports the HTMT for the constructs
write.csv(summary_model_bike$validity$htmt, file="htmt_bike.csv")
summary_model_bike$validity$fl_criteria # reports the fornell larcker criteria for the constructs
write.csv(summary_model_bike$validity$fl_criteria, file="FLcrit_bike.csv")

summary_model_eScooter$validity$vif_items 
write.csv(summary_model_eScooter$validity$vif_items, file="vif_eScooter.csv")
summary_model_eScooter$validity$htmt 
write.csv(summary_model_eScooter$validity$htmt, file="htmt_eScooter.csv")
summary_model_eScooter$validity$fl_criteria 
write.csv(summary_model_eScooter$validity$fl_criteria, file="FLcrit_eScooter.csv")

# outer loadings
bikeOutL<-c(seminr_bike$outer_loadings[1:5,1],  seminr_bike$outer_loadings[6:9,3],
            seminr_bike$outer_loadings[10,2],   seminr_bike$outer_loadings[11:15,4],
            seminr_bike$outer_loadings[16,5],   seminr_bike$outer_loadings[17,8],
            seminr_bike$outer_loadings[18:22,7],seminr_bike$outer_loadings[23:27,6],
            seminr_bike$outer_loadings[28:32,6],seminr_bike$outer_loadings[33:37,6],
            seminr_bike$outer_loadings[38:42,6])

bike.priv.OutL<-c(seminr_bike.priv$outer_loadings[1:5,1],  seminr_bike.priv$outer_loadings[6:9,3],
                  seminr_bike.priv$outer_loadings[10,2],   seminr_bike.priv$outer_loadings[11:15,4],
                  seminr_bike.priv$outer_loadings[16,5],   seminr_bike.priv$outer_loadings[17,8],
                  seminr_bike.priv$outer_loadings[18:22,7],seminr_bike.priv$outer_loadings[23:27,6],
                  seminr_bike.priv$outer_loadings[28:32,6],seminr_bike.priv$outer_loadings[33:37,6],
                  seminr_bike.priv$outer_loadings[38:42,6])

bike.pub.OutL<-c(seminr_bike.pub$outer_loadings[1:5,1],  seminr_bike.pub$outer_loadings[6:9,3],
                 seminr_bike.pub$outer_loadings[10,2],   seminr_bike.pub$outer_loadings[11:15,4],
                 seminr_bike.pub$outer_loadings[16,5],   seminr_bike.pub$outer_loadings[17,8],
                 seminr_bike.pub$outer_loadings[18:22,7],seminr_bike.pub$outer_loadings[23:27,6],
                 seminr_bike.pub$outer_loadings[28:32,6],seminr_bike.pub$outer_loadings[33:37,6],
                 seminr_bike.pub$outer_loadings[38:42,6])

bike.vru.OutL<-c(seminr_bike.vru$outer_loadings[1:5,1],  seminr_bike.vru$outer_loadings[6:9,3],
                 seminr_bike.vru$outer_loadings[10,2],   seminr_bike.vru$outer_loadings[11:15,4],
                 seminr_bike.vru$outer_loadings[16,5],   seminr_bike.vru$outer_loadings[17,8],
                 seminr_bike.vru$outer_loadings[18:22,7],seminr_bike.vru$outer_loadings[23:27,6],
                 seminr_bike.vru$outer_loadings[28:32,6],seminr_bike.vru$outer_loadings[33:37,6],
                 seminr_bike.vru$outer_loadings[38:42,6])

bike.oth.OutL<-c(seminr_bike.oth$outer_loadings[1:5,1],  seminr_bike.oth$outer_loadings[6:9,3],
                 seminr_bike.oth$outer_loadings[10,2],   seminr_bike.oth$outer_loadings[11:15,4],
                 seminr_bike.oth$outer_loadings[16,5],   seminr_bike.oth$outer_loadings[17,8],
                 seminr_bike.oth$outer_loadings[18:22,7],seminr_bike.oth$outer_loadings[23:27,6],
                 seminr_bike.oth$outer_loadings[28:32,6],seminr_bike.oth$outer_loadings[33:37,6],
                 seminr_bike.oth$outer_loadings[38:42,6])

eScooterOutL<-c(seminr_eScooter$outer_loadings[1:5,1],  seminr_eScooter$outer_loadings[6:9,3],
                seminr_eScooter$outer_loadings[10,2],   seminr_eScooter$outer_loadings[11:15,4],
                seminr_eScooter$outer_loadings[16,5],   seminr_eScooter$outer_loadings[17,8],
                seminr_eScooter$outer_loadings[18:22,7],seminr_eScooter$outer_loadings[23:27,6],
                seminr_eScooter$outer_loadings[28:32,6],seminr_eScooter$outer_loadings[33:37,6],
                seminr_eScooter$outer_loadings[38:42,6])

eScooter.priv.OutL<-c(seminr_eScooter.priv$outer_loadings[1:5,1],  seminr_eScooter.priv$outer_loadings[6:9,3],
                      seminr_eScooter.priv$outer_loadings[10,2],   seminr_eScooter.priv$outer_loadings[11:15,4],
                      seminr_eScooter.priv$outer_loadings[16,5],   seminr_eScooter.priv$outer_loadings[17,8],
                      seminr_eScooter.priv$outer_loadings[18:22,7],seminr_eScooter.priv$outer_loadings[23:27,6],
                      seminr_eScooter.priv$outer_loadings[28:32,6],seminr_eScooter.priv$outer_loadings[33:37,6],
                      seminr_eScooter.priv$outer_loadings[38:42,6])

eScooter.pub.OutL<-c(seminr_eScooter.pub$outer_loadings[1:5,1],  seminr_eScooter.pub$outer_loadings[6:9,3],
                     seminr_eScooter.pub$outer_loadings[10,2],   seminr_eScooter.pub$outer_loadings[11:15,4],
                     seminr_eScooter.pub$outer_loadings[16,5],   seminr_eScooter.pub$outer_loadings[17,8],
                     seminr_eScooter.pub$outer_loadings[18:22,7],seminr_eScooter.pub$outer_loadings[23:27,6],
                     seminr_eScooter.pub$outer_loadings[28:32,6],seminr_eScooter.pub$outer_loadings[33:37,6],
                     seminr_eScooter.pub$outer_loadings[38:42,6])

eScooter.vru.OutL<-c(seminr_eScooter.vru$outer_loadings[1:5,1],  seminr_eScooter.vru$outer_loadings[6:9,3],
                     seminr_eScooter.vru$outer_loadings[10,2],   seminr_eScooter.vru$outer_loadings[11:15,4],
                     seminr_eScooter.vru$outer_loadings[16,5],   seminr_eScooter.vru$outer_loadings[17,8],
                     seminr_eScooter.vru$outer_loadings[18:22,7],seminr_eScooter.vru$outer_loadings[23:27,6],
                     seminr_eScooter.vru$outer_loadings[28:32,6],seminr_eScooter.vru$outer_loadings[33:37,6],
                     seminr_eScooter.vru$outer_loadings[38:42,6])

eScooter.oth.OutL<-c(seminr_eScooter.oth$outer_loadings[1:5,1],  seminr_eScooter.oth$outer_loadings[6:9,3],
                     seminr_eScooter.oth$outer_loadings[10,2],   seminr_eScooter.oth$outer_loadings[11:15,4],
                     seminr_eScooter.oth$outer_loadings[16,5],   seminr_eScooter.oth$outer_loadings[17,8],
                     seminr_eScooter.oth$outer_loadings[18:22,7],seminr_eScooter.oth$outer_loadings[23:27,6],
                     seminr_eScooter.oth$outer_loadings[28:32,6],seminr_eScooter.oth$outer_loadings[33:37,6],
                     seminr_eScooter.oth$outer_loadings[38:42,6])

OutLoadings<-cbind(bikeOutL,bike.priv.OutL,bike.pub.OutL,bike.vru.OutL,bike.oth.OutL,
                   eScooterOutL,eScooter.priv.OutL,eScooter.pub.OutL,eScooter.vru.OutL,eScooter.oth.OutL)
rownames(OutLoadings)<- c("Exp.values1","Exp.values2","Exp.values3","Exp.values4","Exp.values5",
                          "Perc.usef1","Perc.usef2","Perc.usef3","Perc.usef4", "Ease of use",
                          "Perc.risk1","Perc.risk2","Perc.risk3","Perc.risk4","Perc.risk5",
                          "Int. to use","Actuale use","Ease*Risk1","Ease*Risk2","Ease*Risk3",
                          "Ease*Risk4","Ease*Risk5","Usef1*Risk1","Usef1*Risk2","usef1*Risk3",
                          "Usef1*Risk4","Usef1*Risk5","Usef2*Risk1","Usef2*Risk2","Usef2*Risk3",
                          "Usef2*Risk4","Usef2*Risk5","Rsef3*Risk1","Usef3*Risk2","Usef3*Risk3",
                          "Usef3*Risk4", "Usef3*Risk5","Usef4*Risk1","Usef4*Risk2","Usef4*Risk3",
                          "Usef4*Risk4","Usef4*Risk5")


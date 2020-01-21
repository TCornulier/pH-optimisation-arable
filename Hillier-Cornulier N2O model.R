###################
# below is a leaned-out version of Thomas Cornulier's code to load and test the final (M15) model for N2O
# initial leaning-out performed by TC — recorded in script [RcommandsN2O_predict.r]
# final leaning-out performed by AJS in this script
###################

# load packages
library(mgcv)
library(rjags)
load.module("glm") ## improved samplers for GLMs often worth loading
library(coda)
library(R2jags)
library(abind)

# set repos
data_repo <- "DEFRA Clean Growth Project/pH Optimisation/Extension for publication"

# read in base data
load(find_onedrive(dir = data_repo, path = "Jon H N2O paper/N2O model data files/Data_full_20180817.RData"))

jags2sam<- function(x){
  lapply(x$BUGSoutput$sims.list, FUN= function(z){array(t(z), dim= c(dim(z)[2:1], 1))})
}

##################################################################
########################### Model M15c #############################
##################################################################

####### Generate data sets with and without NAs (dummy imputed values for use with jagam to generate the right model matrix size)

M15c.vars<- c("logN2O.cum.pos", "pH.cent", "Grasslands", "logNrate.scaled", "logclay.imp.ct", "Fert01", "NO3.avail.v3", "studyID.f", "logDegDays.exp.compact", "NO3.avail.f3", "logSumPrecip.exp.compact", "logWetDays.exp.compact", "logCarbon", "logNrate.scaled.lowNO3", "logNrate.scaled.highNO3", "highNO3") #, "PosDDays.air.exp", "FrostDDays.soil.exp", "PosDDays.soil.exp", "Nuptake.group.num", "logFTCy.exp", "logAvPrecip.exp.ct", "AvDegDays.exp.ct"
sub.M15c<- apply(dat[, c("logN2O.cum.pos", "logNrate.cent.v2", "Fert01", "NO3.avail.v3", "studyID.f", "logWetDays.exp", "logclay.imp")], 1, FUN= function(x) !any(is.na(x))) # leave only variables for which NAs cannot be imputed, so that obs with missing data are removed # , "Bulk.density.imputed"
table(sub.M15c) 	# FALSE  TRUE 
#   486  2244 
dat.M15c<- dat[sub.M15c, M15c.vars]
dat.M15c.dummy<- dat.M15c # for use with jagam to generate the right model matrix size
# hard-code "Grasslands:Fert01" interaction
dat.M15c.dummy$GrassFert<- as.numeric(dat.M15c.dummy$Grasslands) * dat.M15c.dummy$Fert01


# replace NAs by any value (here median of column)
for(i in 1:ncol(dat.M15c)){
  if(is.numeric(dat.M15c.dummy[, i])){ dat.M15c.dummy[is.na(dat.M15c.dummy[, i]), i]<- median(dat.M15c.dummy[, i], na.rm= T) }
  if(is.character(dat.M15c.dummy[, i])){ dat.M15c.dummy[is.na(dat.M15c.dummy[, i]), i]<- median(dat.M15c.dummy[, i], na.rm= T) }
  if(is.factor(dat.M15c.dummy[, i])){ dat.M15c.dummy[is.na(dat.M15c.dummy[, i]), i]<- median(as.character(dat.M15c.dummy[, i]), na.rm= T) }
  if(is.logical(dat.M15c.dummy[, i])){ dat.M15c.dummy[is.na(dat.M15c.dummy[, i]), i]<- F }
}

#################### run JAGAM - Model M15c #####################
pregam.M15c<- jagam(formula= logN2O.cum.pos ~ Grasslands
                    # + Fert01 #Fert01 unnecessary due to 'by= Fert01' below?
                    + GrassFert
                    # + highNO3
                    + logCarbon
                    + s(pH.cent, bs= "cr", k= 9)
                    + te(logclay.imp.ct, logWetDays.exp.compact, k= c(5, 5))
                    + te(logclay.imp.ct, logWetDays.exp.compact, k= c(5, 5), by= logNrate.scaled.lowNO3)
                    + te(logclay.imp.ct, logWetDays.exp.compact, k= c(5, 5), by= logNrate.scaled.highNO3)
                    + s(logDegDays.exp.compact, k= 8) 
                    + s(logNrate.scaled, by= Fert01)
                    + s(studyID.f, bs= "re"),
                    family= gaussian, data= dat.M15c.dummy,
                    file= find_onedrive(dir = data_repo, path = "Jon H N2O paper/N2O model data files/JAGS/modelM15c.txt"),
                    centred=TRUE, sp.prior = "gamma", diagonalize= FALSE)


#################### correct JAGS data #####################
# identify number of fixed effects
nfixed.M15c<- rle(colnames(pregam.M15c$jags.data$X)=="")$lengths[1] # run length encoding -> take first run of non-empty names (last is random effects)
fixed.names.M15c<- pregam.M15c$pregam$term.names[1:nfixed.M15c]

# replace dummy values in model matrix by NA where they should be (according to dat.M15c)
for(i in M15c.vars){
  pregam.M15c$jags.data$X[is.na(dat.M15c[, i]), grep(i, pregam.M15c$pregam$term.names)]<- NA
}

# vars2impute<- apply(pregam.M15c$jags.data$X[, 1:nfixed], 2, FUN= function(x) any(is.na(x)))
vars2impute<- apply(pregam.M15c$jags.data$X, 2, FUN= function(x) any(is.na(x)))
vars2impute.colIndex<- which(vars2impute)
vars2impute.NMissing<- as.list(apply(pregam.M15c$jags.data$X[, vars2impute.colIndex], 2, FUN= function(x) sum(is.na(x))))
vars2impute.whichMissing<- list(apply(pregam.M15c$jags.data$X[, vars2impute.colIndex], 2, FUN= function(x) as.numeric(which(is.na(x)))))[[1]]
names(vars2impute.whichMissing)<- paste(gsub(":", ".", gsub("(", ".", gsub(")", "", pregam.M15c$pregam$term.names[vars2impute.colIndex], fixed= T), fixed= T)), "whichMissing", sep= ".")
names(vars2impute.NMissing)<- paste(gsub(":", ".", gsub("(", ".", gsub(")", "", pregam.M15c$pregam$term.names[vars2impute.colIndex], fixed= T), fixed= T)), "NMissing", sep= ".")

# create lookup table for pH.cent spline bases coords imputation
pH.cent.cat<- cut(dat.M15c$pH.cent, breaks= c(-4, -2, -1.5, -1, -0.5, 0, 0.5, 1, 2)) # create 8 classes of pH.cent
pH.cent.cat.prop<- round(table(pH.cent.cat)/sum(table(pH.cent.cat)), 2) # proportion of data per class
pH.cent.ref.index<- unlist(lapply(as.list(round(tapply(dat.M15c$pH.cent, pH.cent.cat, mean, na.rm= T), 1)), FUN= function(x) which.min(abs(dat.M15c$pH.cent - x)))) # index of pH.cent value closest to class mean
pH.cent.lookup<- pregam.M15c$jags.data$X[pH.cent.ref.index, grep("pH.cent", pregam.M15c$pregam$term.names)]


pregam.M15c$jags.data<- c(pregam.M15c$jags.data, 
                          vars2impute.NMissing, 
                          vars2impute.whichMissing, 
                          Grasslands.prop= list(as.vector(table(dat$Grasslands)/sum(!is.na(dat$Grasslands)))), 
                          list(pH.cent.cat.prop= as.vector(pH.cent.cat.prop)), 
                          list(pH.cent.lookup= as.matrix(pH.cent.lookup)))#,
str(pregam.M15c$jags.data)

# let JAGS auto-initialize the parametric coefficients so that each chain starts from a different point
# (in order to investigate possible identifiability issues with the intercept for fertilizer treatment)
pregam.M15c$jags.ini$b[1:nfixed.M15c]<- NA


iter <- 3000
nb<- 2000

# load in M15 models
root <- find_onedrive(dir = data_repo, path = "Jon H N2O paper/N2O model data files/jagsM15c_")
for(i in 1:6){
  filepath = paste0(root, i, ".RData")
  load(filepath)
}

jagsM15c.6c<- jagsM15c.1
jagsM15c.6c$BUGSoutput$sims.list<- mapply(rbind, jagsM15c.1$BUGSoutput$sims.list, jagsM15c.2$BUGSoutput$sims.list, jagsM15c.3$BUGSoutput$sims.list, jagsM15c.4$BUGSoutput$sims.list, jagsM15c.5$BUGSoutput$sims.list, jagsM15c.6$BUGSoutput$sims.list)
jagsM15c.6c$BUGSoutput$sims.array<- abind(	jagsM15c.1$BUGSoutput$sims.array, 
                                           jagsM15c.2$BUGSoutput$sims.array,
                                           jagsM15c.3$BUGSoutput$sims.array,
                                           jagsM15c.4$BUGSoutput$sims.array,
                                           jagsM15c.5$BUGSoutput$sims.array,
                                           jagsM15c.6$BUGSoutput$sims.array, along= 1)

jagsM15c.6c$n.iter<- iter*6
jagsM15c.6c$BUGSoutput$n.iter<- iter*6
jagsM15c.6c$BUGSoutput$n.burnin<- nb*6
jagsM15c.6c$BUGSoutput$n.keep<- (iter-nb)*6
jagsM15c.6c$BUGSoutput$n.sims<- (iter-nb)*6

# create model for predictions
samM15c<- jags2sam(jagsM15c.6c)
jamM15c<- sim2jam(samM15c,pregam.M15c$pregam)

####### Predictions from M15c

df.M15c.complete<- function(x){ # function to compute transformed variables for prediction
  x$logCarbon<- log(x$SOC + 1)
  x$pH.cent<- x$pH - 7
  x$GrassFert<- as.numeric(x$Grasslands) * x$Fert01
  x$logclay.imp.ct<- log(x$Clay / 100 + 1) - 0.24
  x$logWetDays.exp.compact<- log(x$WetDays.exp+10) - 4
  x$logDegDays.exp.compact<- log(x$DegDays.exp+1500) - 8
  x$logNrate.scaled<- log(x$N.rate + 1) / 8
  x$logNrate.scaled.lowNO3<- x$logNrate.scaled * x$Fert01 * x$lowNO3
  x$logNrate.scaled.highNO3<- x$logNrate.scaled * x$Fert01 * x$highNO3
  x
}

# below is for verification against shiny app
#test1.base <- data.frame(Fert01 = 0, lowNO3 = 0, highNO3 = 0, Grasslands = FALSE, 
#                          pH = 5, Clay = 25, SOC = 3, # pH @ median, Clay @ median, SOC @ 2%
#                          WetDays.exp = 100, DegDays.exp= 3500, # @ Germany over 1 year
#                          N.rate = 200, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
#test1.base <- df.M15c.complete(test1.base)
#test1.base.pred <- exp(predict(jamM15c, newdata= test1.base))

#test1.fert <- data.frame(Fert01 = 1, lowNO3 = 0, highNO3 = 0, Grasslands = FALSE, 
#                         pH = 5, Clay = 25, SOC = 3, # pH @ median, Clay @ median, SOC @ 2%
#                         WetDays.exp = 100, DegDays.exp= 3500, # @ Germany over 1 year
#                         N.rate = 200, studyID.f= " 32.58 119.702007 8  8120082007-08-152007-11-04") # some random studyID.f (the first one)
#test1.fert <- df.M15c.complete(test1.fert)
#test1.fert.pred <- exp(predict(jamM15c, newdata= test1.fert))
#test1.ef <- (test1.fert.pred - test1.base.pred) / 200
#test1.base.pred
#test1.fert.pred
#test1.ef

# added to write out easy prediction-friendly version of model for later use
save(jamM15c, df.M15c.complete, file = find_onedrive(dir = data_repo, path = "Jon H N2O paper/H-C N2O model lite.RData"))


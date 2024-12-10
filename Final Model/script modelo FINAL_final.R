library(INLA)
library(sf)
library(spdep)
library(Matrix)
library(INLAOutputs)
library(ggplot2)
library(dplyr)
inla.setOption(num.threads = "auto")

setwd("c:\\users\\gerson.laurindo\\onedrive - prodesp\\pos-Doc\\modelo final")
data_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\mapa\\"
source("HighstatLibV10.R") 
source('reading data.R')

str(data)

###############
#the Data objects Data must be created for each MODEL
#############
## Define 'V.area' and 'V.year' variables ##
S <- length(unique(data$codigo7))
T <- length(unique(data$year))*length(unique(data$seasons))
##################################################
## Define spatial and temporal structure matrix ##
##################################################
# ξ - is the vector of the spatial random efect 
# Qs is the spatial neighboarhood matrix S x S with  (i,j) = -1 if i is the neighbor of j and 0 otherwise
#and the element of the i-esima diagonal is the number of the neighboar in the i-esima area
g <- inla.read.graph("graph")  #neighboarhood matrix
Qs <- matrix(0, g$n, g$n) 
for (i in 1:g$n){
  Qs[i,i]=g$nnbs[[i]]
  Qs[i,g$nbs[[i]]]=-1
}
#length(Qs)

#head(Qs)
#Qs[1,1]
#Qs[2,2]
#Qs[3,3]
#Qs[4,4]
# log r = 1Sβ0 + Xβ + ξ = 1Sβ0 + Xβ + 1Sβξ + Uξrαξ,
Dm <- diff(diag(T),differences=1)
Qt <- t(Dm)%*%Dm
#Qt <- as(Qt, "Matrix")  #(Ue - pg 12.)
ones.S <- matrix(1,S,1)
ones.T <- matrix(1,T,1)
ones.ST <- matrix(1,S*T,1)
#length(unique(Data$ID.year))
###############################
## Read the cartography file ##
###############################
carto_up <- st_read("mapa/sao paulo completo.shp")
plot(carto_up$geometry, axes=T)
## Add the neighbourhood graph ##
W <- -Qs
diag(W) <- 0
#W[1,184]
#length(W) - See (pg 13 paper) 
carto.nb <- mat2listw(W, style="B")$neighbours
plot(carto.nb, st_coordinates(st_centroid(carto_up)), pch=19, cex=0.8, col="blue", add=TRUE)
#############################################################
## Prepare the data for INLA models (SPATIO-TEMPORAL CASE) ##
#############################################################
str(data)
#Naming the models
######################################## Testing for nonlinear Temperature
#MODELO 5 NL - equal model1 with tmax nonlinear
data$ID.TEMP1 <- inla.group(data$tmax, n = 25, method = "cut", idx.only = TRUE)
data$ID.TEMP2 <- inla.group(data$tmax, n = 75, method = "cut", idx.only = TRUE)
#MODEL 5 - scale (atract)
#     M5X1=scale(data$atract_ind_10mil)
#     M5X2=data$pending_cat
#     M5X3=data$clas_ip
#     M5X4=scale(data$siops_ind_32)
#     M5X5=scale(data$coverage_work)
#     M5X6=scale(data$coverage_neb)
#     M5X7=scale(data$gdp_per_capta_10mil)
#MODEL 6 - scale (katz)
#     M6X1=scale(data$katz)
#     M6X2=data$pending_cat
#     M6X3=data$clas_ip
#     M6X4=scale(data$siops_ind_32)
#     M6X5=scale(data$coverage_work)
#     M6X6=scale(data$coverage_neb)
#     M6X7=scale(data$imdsp) 
######################################### THE TEST SHOW THAT MODEL IS LINEAR  -= don't use the model 5 and 6
#MODEL 1 - scale (atract)
     M1X1=scale(data$atract_ind_10mil)
     M1X2=data$pending_cat
     M1X3=data$clas_ip
     M1X4=scale(data$siops_ind_32)
     M1X5=scale(data$coverage_work)
     M1X6=scale(data$coverage_neb)
     M1X7=scale(data$gdp_per_capta_10mil)
     M1X8=scale(data$tmax)
#MODEL 2 - scale (katz)
     M2X1=scale(data$katz)
     M2X2=data$pending_cat
     M2X3=data$clas_ip
     M2X4=scale(data$siops_ind_32)
     M2X5=scale(data$coverage_work)
     M2X6=scale(data$coverage_neb)
     M2X7=scale(data$imdsp) 
     M2X8=scale(data$tmax)
#########################################
## Define the hyperprior distributions ##
#########################################
sdunif="expression:
  logdens=-log_precision/2;
  return(logdens)"
#############################################
## Compute posterior pattern distributions ##
#############################################
# compute.patterns <- T  ## Set compute.patterns=FALSE if posterior patterns are not required
# if(compute.patterns){
#  source("R/posterior_lincombs.R")
#  all.lc <- all.lc
# 
#  source("R/posterior_lincombs_APredictor.R")
#  all.lc.Apredictor <- all.lc
# }else{
#  all.lc <- NULL
#  all.lc.Apredictor <- NULL
# }
#save.image("lincomb_rodado.RData")
#########################################################################################################
#########################################################################################################
## IMPORTANT NOTE: Uncomment and replace formula lines (f.M1-f.M4) for automatic definition when using ##
##                 your own data. In that case, the covariates should be named as X1, X2, ..., Xp.     ##               
#########################################################################################################
#########################################################################################################
#strategy <- "simplified.laplace" - 
strategy<-'ep'
#########################################################################
## Model ST2: Intercept + random-effects (Eq. 2.4 -> with constraints) ##
#########################################################################
## Sum-to-zero constraints for the interaction ##
#A inflação da variância é grande se a correlação for grande entre a 
#covariável Xj e o autovetor da matriz espacial Qξ 
#tendo o menor autovalor não nulo, ou seja, há um problema de colinearidade
R <- kronecker(Qt,Qs) #(ST x ST)
#length(R)
r.def <- S+T-1
Bst1 <- kronecker(t(ones.T),diag(S)) 
#length(Bst1)
Bst2 <- kronecker(diag(T),t(ones.S)) # (ST x S)
#length(Bst2)
Bst <- rbind(Bst1[-1,],Bst2[-1,])
#length(Bst)
#ones.T
#length(diag(S))
#any(is.na(R))
#any(is.na(Bst1))
#any(is.na(Bst2))
#any(is.na(Bst))
############################################
#Create one 'Data' for each model
############################################
############################################
Data_m1 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M1X1,M1X2,M1X3,M1X4,M1X5,M1X6,M1X7, M1X8,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time)
Data_m2 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M2X1,M2X2,M2X3,M2X4,M2X5,M2X6,M2X7, M2X8,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time)
############################## TEST NON LINEAR
Data_m5 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M5X1,M5X2,M5X3,M5X4,M5X5,M5X6,M5X7,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time, ID.TEMP1=data$ID.TEMP1)
Data_m6 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M6X1,M6X2,M6X3,M6X4,M6X5,M6X6,M6X7,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time, ID.TEMP1=data$ID.TEMP1)
###############################
save.image('inicial data.RData')
#load('inicial data.RData')
############################################
################################################################  MODEL 5 (EQUAL 1 - NONLINEAR)
#for each model verify the 
#teste de cut 25 e cut 75 
Data_m5 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M5X1,M5X2,M5X3,M5X4,M5X5,M5X6,M5X7,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time, ID.TEMP1=data$ID.TEMP1)

Data_m5_75 <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,M5X1,M5X2,M5X3,M5X4,M5X5,M5X6,M5X7,
                   ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time, ID.TEMP2=data$ID.TEMP2)
#Create one variable with beta (covariables) 
hyper.iid <- list(theta=list(prior = "pc.prec",param=c(1,0.01)))
f.M5_75 <- O ~ 1 + M5X1 + as.factor(M5X2) + as.factor(M5X3) + M5X4 + M5X5 + M5X6 + M5X7+ 
  f(ID.area, model="besag", graph=Qs, constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.year, model="rw1", constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.area.year, model="generic0", Cmatrix=R, rankdef=r.def, hyper=list(prec=list(prior=sdunif)),
    constr=TRUE, extraconstr=list(A=Bst, e=rep(0,nrow(Bst))))+
#  f(ID.TEMP1, model="rw1", constr = TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif)))
   f(ID.TEMP2, model="rw1", hyper=hyper.iid, scale.model=TRUE)

ini<-Sys.time()
Model5_scale_75 <- inla(f.M5_75, family="poisson", data=Data_m5_75, E=E,
               control.predictor=list(compute=TRUE, link=1),
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               control.inla=list(int.strategy="eb"))
end<-Sys.time()
print(end-ini)

summary(Model5_scale_75)
FixedEffects(Model5_scale_75)
save(f.M5,Model5_scale_75,file="model5_scale_75.RData")
print(paste('WAIC-Model5 75- ',Model5_scale_75$waic$waic ))
print(paste('DIC-Model5 75- ',Model5_scale_75$dic$dic))
print(paste('LS-Model5- 75',-mean(log(Model5_scale_75$cpo$cpo))))

par(mar=c(5,5,1,0.5))
myplot = c("mean", "0.025quant", "0.975quant")
matplot(Model5_scale_75$summary.random$ID.TEMP2[, myplot],
        lty =c(1,3,3), type="l", col=1, ylab="Posterior estimate",
        main="Temperatura Máxima - 75")
abline(h=0, col="blue", lty=2)

#Create one variable with beta (covariables) 
hyper.iid <- list(theta=list(prior = "pc.prec",param=c(1,0.01)))
f.M5 <- O ~ 1 + M5X1 + as.factor(M5X2) + as.factor(M5X3) + M5X4 + M5X5 + M5X6 + M5X7+ 
  f(ID.area, model="besag", graph=Qs, constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.year, model="rw1", constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.area.year, model="generic0", Cmatrix=R, rankdef=r.def, hyper=list(prec=list(prior=sdunif)),
    constr=TRUE, extraconstr=list(A=Bst, e=rep(0,nrow(Bst))))+
#  f(ID.TEMP1, model="rw1", constr = TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif)))
   f(ID.TEMP1, model="rw1", hyper=hyper.iid, scale.model=TRUE)

ini<-Sys.time()
Model5_scale <- inla(f.M5, family="poisson", data=Data_m5, E=E,
               control.predictor=list(compute=TRUE, link=1),
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               control.inla=list(int.strategy="eb"))
end<-Sys.time()
print(end-ini)

summary(Model5_scale)
FixedEffects(Model5_scale)
save(f.M5,Model5_scale,file="model5_scale_25.RData")
print(paste('WAIC-Model5 - ',Model5_scale$waic$waic ))
print(paste('DIC-Model5 - ',Model5_scale$dic$dic))
print(paste('LS-Model5- ',-mean(log(Model5_scale$cpo$cpo))))

par(mar=c(5,5,1,0.5))
myplot = c("mean", "0.025quant", "0.975quant")
matplot(Model5_scale$summary.random$ID.TEMP1[, myplot],
        lty =c(1,3,3), type="l", col=1, ylab="Posterior estimate",
        main="Temperatura Máxima - 25")
abline(h=0, col="blue", lty=2)
######################################################
# TEMPERATURE GRAPHIC
par(mar=c(5,5,1,0.5))
myplot = c("mean", "0.025quant", "0.975quant")
matplot(Model5_scale$summary.random$ID.TEMP1[, myplot],
        lty =c(1,3,3), type="l", col=1, ylab="Posterior estimate",
        main="Temperatura Máxima - 50")
abline(h=0, col="blue", lty=2)

################################################################  MODEL 1 - atractive index
#for each model verify the p
p <- 8 # Number of covariates
#Create one variable with beta (covariables) 
Beta.df <- as.matrix(Data_m1[,paste("M1X",1:p,sep="")])
any(is.na(Beta.df))
## INLA model ##
f.M1 <- O ~ 1 + M1X1 + as.factor(M1X2) + as.factor(M1X3) + M1X4 + M1X5 + M1X6 + M1X7 +  M1X8 + 
  f(ID.area, model="besag", graph=Qs, constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.year, model="rw1", constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.area.year, model="generic0", Cmatrix=R, rankdef=r.def, hyper=list(prec=list(prior=sdunif)),
    constr=TRUE, extraconstr=list(A=Bst, e=rep(0,nrow(Bst)))) #+
ini<-Sys.time()
Model1_scale <- inla(f.M1, family="poisson", data=Data_m1, E=E,
               control.predictor=list(compute=TRUE, link=1),
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               control.inla=list(int.strategy="eb"))
end<-Sys.time()
print(end-ini)

summary(Model1_scale)
FixedEffects(Model1_scale)
save(f.M1,Model1_scale,file="model1_scale.RData")
print(paste('WAIC-Model1 - ',Model1_scale$waic$waic ))
print(paste('DIC-Model1 - ',Model1_scale$dic$dic))
print(paste('LS-Model1- ',-mean(log(Model1_scale$cpo$cpo))))
################################################################  MODEL 2 - katz index
#tem que atualizar para cada modelo O TAMANHO DO p
p <- 8 # Number of covariates
#Cria uma variável com os betas (covariáveis) 
Beta.df <- as.matrix(Data_m2[,paste("M2X",1:p,sep="")])
any(is.na(Beta.df))
## INLA model ##
f.M2 <- O ~ 1 + M2X1 + as.factor(M2X2) + as.factor(M2X3) + M2X4 + M2X5 + M2X6 + M2X7 +  M2X8 + 
  f(ID.area, model="besag", graph=Qs, constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.year, model="rw1", constr=TRUE, rankdef=1, hyper=list(prec=list(prior=sdunif))) + 
  f(ID.area.year, model="generic0", Cmatrix=R, rankdef=r.def, hyper=list(prec=list(prior=sdunif)),
    constr=TRUE, extraconstr=list(A=Bst, e=rep(0,nrow(Bst)))) #+
ini<-Sys.time()
Model2_scale <- inla(f.M2, family="poisson", data=Data_m2, E=E,
               control.predictor=list(compute=TRUE, link=1),
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               control.inla=list(int.strategy="eb"))
end<-Sys.time()
print(end-ini)

summary(Model2_scale)
FixedEffects(Model2_scale)
save(f.M2,Model2_scale,file="model2_scale.RData")
print(paste('WAIC-Model2 - ',Model2_scale$waic$waic ))
print(paste('DIC-Model2 - ',Model2_scale$dic$dic))
print(paste('LS-Model2- ',-mean(log(Model2_scale$cpo$cpo))))

####################################################################
## Model ST3: Restricted regression (Eq. 3.2 -> with constraints) ##
####################################################################
# 2 models - 1 and 2 with scale
####################  MODEL 1
load("model1_scale.RData")
any(is.na(Model1_scale$summary.fitted.values$mode))
W <- Diagonal(S*T, Model1_scale$summary.fitted.values$mode*Data_m1$E)

W0 <- sqrt(diag(W))
W0[is.na(W0)] <- 1e-10
any(is.na(W0))

W.sqrt <-  Diagonal(S*T, W0)
p<-8
Beta.df <- as.matrix(Data_m1[,paste("M1X",1:p,sep="")])

dim(W.sqrt)
X <- cbind(ones.ST, as.matrix(Beta.df))
any(is.na(X))

inicio <- Sys.time()
P <- W.sqrt%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W.sqrt 
print(Sys.time()-inicio)

inicio <- Sys.time()
Pc <- diag(S*T)-P 
print(Sys.time()-inicio)

inicio <- Sys.time()
eigen.P <- eigen(P)
print(Sys.time()-inicio)

inicio <- Sys.time()
eigen.Pc <- eigen(Pc)
print(Sys.time()-inicio)

inicio <- Sys.time()
K <- as.matrix(eigen.P$vectors[,eigen.P$values>1e-12])
print(Sys.time()-inicio)

inicio <- Sys.time()
L <- eigen.Pc$vectors[,eigen.Pc$values>1e-12]
print(Sys.time()-inicio)

inicio <- Sys.time()
M <- solve(W.sqrt)%*%L%*%t(L)%*%W.sqrt
print(Sys.time()-inicio)

inicio <- Sys.time()
Z.area <- M%*%kronecker(ones.T,diag(S))
Z.year <- M%*%kronecker(diag(T),ones.S)
Z.area.year <- M%*%diag(S*T)
print(Sys.time()-inicio)
inicio <- Sys.time()
M0 <- solve(t(X)%*%X)%*%t(X)
print(Sys.time()-inicio)

inicio <- Sys.time()
beta.lc = inla.make.lincombs(Predictor=M0, ID.area=-M0%*%Z.area, ID.year=-M0%*%Z.year, ID.area.year=-M0%*%Z.area.year)
names(beta.lc) <- paste("beta",as.character(0:p),sep="")
print(Sys.time()-inicio)

R <- kronecker(Qt,Qs)
r.def <- S+T-1
Bst1 <- kronecker(t(ones.T),diag(S))
Bst2 <- kronecker(diag(T),t(ones.S))
Bst <- rbind(Bst1[-1,],Bst2[-1,])

## INLA model ##
f.M1_st3 <- f.M1

inicio<-Sys.time()
Model1_st3_scale <- inla(f.M1_st3, family="poisson", data=Data_m1, E=E,
               control.predictor=list(compute=TRUE, link=1), #troquei aqui
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               lincomb=beta.lc,
               control.inla=list(int.strategy="eb"))

fim<-Sys.time()
print(Sys.time()-inicio)
inicio <- Sys.time()
save(Model1_st3_scale, file = "model1_st3_scale.RData")
print(Sys.time()-inicio)
summary(Model1_st3_scale)
FixedEffects(Model1_st3_scale)
print(paste('WAIC-Model1_st3_scale - ',Model1_st3_scale$waic$waic ))
print(paste('DIC-Model1_st3_scale - ',Model1_st3_scale$dic$dic))
print(paste('LS-Model1_st3_scale- ',-mean(log(Model1_st3_scale$cpo$cpo))))

####################  MODEL 2
load("model2_scale.RData")
any(is.na(Model2_scale$summary.fitted.values$mode))
W <- Diagonal(S*T, Model2_scale$summary.fitted.values$mode*Data_m2$E)

W0 <- sqrt(diag(W))
W0[is.na(W0)] <- 1e-10
any(is.na(W0))

W.sqrt <-  Diagonal(S*T, W0)
p<-8
Beta.df <- as.matrix(Data_m2[,paste("M2X",1:p,sep="")])

dim(W.sqrt)
X <- cbind(ones.ST, as.matrix(Beta.df))
any(is.na(X))

inicio <- Sys.time()
P <- W.sqrt%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%W.sqrt 
print(Sys.time()-inicio)

inicio <- Sys.time()
Pc <- diag(S*T)-P 
print(Sys.time()-inicio)

inicio <- Sys.time()
eigen.P <- eigen(P)
print(Sys.time()-inicio)

inicio <- Sys.time()
eigen.Pc <- eigen(Pc)
print(Sys.time()-inicio)

inicio <- Sys.time()
K <- as.matrix(eigen.P$vectors[,eigen.P$values>1e-12])
print(Sys.time()-inicio)

inicio <- Sys.time()
L <- eigen.Pc$vectors[,eigen.Pc$values>1e-12]
print(Sys.time()-inicio)

inicio <- Sys.time()
M <- solve(W.sqrt)%*%L%*%t(L)%*%W.sqrt
print(Sys.time()-inicio)

inicio <- Sys.time()
Z.area <- M%*%kronecker(ones.T,diag(S))
Z.year <- M%*%kronecker(diag(T),ones.S)
Z.area.year <- M%*%diag(S*T)
print(Sys.time()-inicio)
inicio <- Sys.time()
M0 <- solve(t(X)%*%X)%*%t(X)
print(Sys.time()-inicio)

inicio <- Sys.time()
beta.lc = inla.make.lincombs(Predictor=M0, ID.area=-M0%*%Z.area, ID.year=-M0%*%Z.year, ID.area.year=-M0%*%Z.area.year)
names(beta.lc) <- paste("beta",as.character(0:p),sep="")
print(Sys.time()-inicio)

R <- kronecker(Qt,Qs)
r.def <- S+T-1
Bst1 <- kronecker(t(ones.T),diag(S))
Bst2 <- kronecker(diag(T),t(ones.S))
Bst <- rbind(Bst1[-1,],Bst2[-1,])

## INLA model ##
f.M2_st3 <- f.M2

inicio<-Sys.time()
Model2_st3_scale <- inla(f.M2_st3, family="poisson", data=Data_m2, E=E,
               control.predictor=list(compute=TRUE, link=1), #troquei aqui
               control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE),
               lincomb=beta.lc,
               control.inla=list(int.strategy="eb"))

fim<-Sys.time()
print(Sys.time()-inicio)
inicio <- Sys.time()
save(Model2_st3_scale, file = "model2_st3_scale.RData")
print(Sys.time()-inicio)
summary(Model2_st3_scale)
FixedEffects(Model2_st3_scale)
print(paste('WAIC-Model2_st3_scale - ',Model2_st3_scale$waic$waic ))
print(paste('DIC-Model2_st3_scale - ',Model2_st3_scale$dic$dic))
print(paste('LS-Model2_st3_scale- ',-mean(log(Model2_st3_scale$cpo$cpo))))


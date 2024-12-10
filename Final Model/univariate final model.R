library(INLA)
library(tidyverse)
library(dplyr)
library(sf)
library(lattice)
library(spdep)
library(ggplot2)
library(viridis)
library(INLAOutputs)

setwd("c:\\users\\gerson.laurindo\\onedrive - prodesp\\pos-Doc\\modelo final")
data_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\mapa\\"
source("HighstatLibV10.R") 
source('reading data.R')

####MODEL to run one variable

formula.rw1<- cases ~ 1 +  as.factor(pending_cat) +
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_pending_cat <- inla(formula.rw1,
            family = "poisson",data = data,
            offset =  log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 + as.factor(clas_ip) +
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_clas_ip <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  siops_ind_32+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_siops_32 <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  coverage_work_01+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_cov_work <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  coverage_neb_01+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_cov_neb <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  gdp_per_capta_10mil+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_gdp <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  tmax+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_tmax <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  katz+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_katz <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  imdsp+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_imdsp <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)

formula.rw1<- cases ~ 1 +  atract_ind_10mil+
        f(id_space1, model = "bym2", hyper = hyper.bym2, graph = adj, scale.model = TRUE, constr = TRUE) +
        f(id_time1, model = "iid", hyper = hyper.iid, constr = TRUE) +
        f(id_time2, model='rw1', hyper=hyper.iid,constr = TRUE, scale.model = TRUE) +
        f(id_space_time, model = "iid", hyper = hyper.iid,constr = TRUE)
inicio<-Sys.time()
mod_atract <- inla(formula.rw1,
            family = "poisson",data = data,
            offset = log(E),
            lincomb=lcs.tp,
            control.predictor = list(link = 1, compute = TRUE),
            inla.mode = "experimental",
            control.compute = list(dic = TRUE, waic = TRUE,cpo=TRUE,
            control.gcpo=list(enable=TRUE)))
fim<-Sys.time()
print(fim-inicio)
###########SAVING THE MODELS
save(mod_pending_cat,mod_clas_ip,mod_siops_32,mod_cov_work,
mod_cov_neb,mod_gdp,mod_tmax,mod_katz,mod_imdsp,mod_atract,file="model_one_variable.RData")
###########
###############################RODAR OS MODELOS
FixedEffects(mod_pending_cat)
FixedEffects(mod_clas_ip)
FixedEffects(mod_siops_32)
FixedEffects(mod_cov_work)
FixedEffects(mod_cov_neb)
FixedEffects(mod_gdp)
FixedEffects(mod_tmax)
FixedEffects(mod_katz)
FixedEffects(mod_imdsp)
FixedEffects(mod_atract)

print(paste('WAIC-',mod_pending_cat$waic$waic,' :: DIC-',mod_pending_cat$dic$dic,'  :: GCPO-',-sum(log(mod_pending_cat$gcpo$gcpo)),'  :: LS-',-mean(log(mod_pending_cat$cpo$cpo))))
print(paste('WAIC-',mod_clas_ip$waic$waic,' :: DIC-',mod_clas_ip$dic$dic,'  :: GCPO-',-sum(log(mod_clas_ip$gcpo$gcpo)),'  :: LS-',-mean(log(mod_clas_ip$cpo$cpo))))
print(paste('WAIC-',mod_siops_32$waic$waic,' :: DIC-',mod_siops_32$dic$dic,'  :: GCPO-',-sum(log(mod_siops_32$gcpo$gcpo)),'  :: LS-',-mean(log(mod_siops_32$cpo$cpo))))
print(paste('WAIC-',mod_cov_work$waic$waic,' :: DIC-',mod_cov_work$dic$dic,'  :: GCPO-',-sum(log(mod_cov_work$gcpo$gcpo)),'  :: LS-',-mean(log(mod_cov_work$cpo$cpo))))
print(paste('WAIC-',mod_cov_neb$waic$waic,' :: DIC-',mod_cov_neb$dic$dic,'  :: GCPO-',-sum(log(mod_cov_neb$gcpo$gcpo)),'  :: LS-',-mean(log(mod_cov_neb$cpo$cpo))))
print(paste('WAIC-',mod_gdp$waic$waic,' :: DIC-',mod_gdp$dic$dic,'  :: GCPO-',-sum(log(mod_gdp$gcpo$gcpo)),'  :: LS-',-mean(log(mod_gdp$cpo$cpo))))
print(paste('WAIC-',mod_tmax$waic$waic,' :: DIC-',mod_tmax$dic$dic,'  :: GCPO-',-sum(log(mod_tmax$gcpo$gcpo)),'  :: LS-',-mean(log(mod_tmax$cpo$cpo))))
print(paste('WAIC-',mod_katz$waic$waic,' :: DIC-',mod_katz$dic$dic,'  :: GCPO-',-sum(log(mod_katz$gcpo$gcpo)),'  :: LS-',-mean(log(mod_katz$cpo$cpo))))
print(paste('WAIC-',mod_imdsp$waic$waic,' :: DIC-',mod_imdsp$dic$dic,'  :: GCPO-',-sum(log(mod_imdsp$gcpo$gcpo)),'  :: LS-',-mean(log(mod_imdsp$cpo$cpo))))
print(paste('WAIC-',mod_atract$waic$waic,' :: DIC-',mod_atract$dic$dic,'  :: GCPO-',-sum(log(mod_atract$gcpo$gcpo)),'  :: LS-',-mean(log(mod_atract$cpo$cpo))))


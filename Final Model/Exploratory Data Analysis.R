rm(list=ls())

library(INLA)
library(lattice)
library(spdep)
library(car) # VIF
library(ggplot2)
library(caret)
library(gstat)
library(sf)
library(nlme)
library(corrplot)
library(dplyr)
library(INLAOutputs)

setwd("c:\\users\\gerson.laurindo\\onedrive - prodesp\\pos-Doc\\modelo final")
data_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\mapa\\"
source("HighstatLibV10.R") 
source('reading data.R')

MyVar <- c('pending_cat','clas_ip','precipitacao_tot','precipitation_log','siops_ind_32','coverage_work','coverage_work_01','coverage_neb','coverage_neb_01','gdp_per_capta_10mil','gdp_per_capta_log','tmax','imdsp','atract_ind_10mil','katz','atract_ind_log','katz_log')

#MYVAR
cor <- cor(na.omit(data[,MyVar])) # criterio de corte - coef de correl >= 0.5 a 0.8
corrplot(cor, method="number", col = NULL,tl.cex = 0.6, tl.col = "blue", number.cex = 0.6, number.font = 2)

par(mfrow=c(4,5), mar=c(1,1,1,1))
for (k in 1: 54) 
{
   if (names(data[k]) %in% MyVar) 
   {
     var<-names(data[k])
#     print(var)
     boxplot(na.omit=TRUE,(data[,k]),main=var)
   }
}
#Loop
#to print a figure with all boxplot
#png(filename="figura1.png", height=20, width=20, unit="cm", res=300)

# comparing two graphics
## Outliers in independent variables
Mydotplot(data[,MyVar]) # usando a funcao 'HighstatLibV9.R'

# Normality

par(mfrow=c(5,4), mar=c(2,2,2,2))
for (m in 1: 54) 
{
   if (names(data[m]) %in% MyVar) 
   {
     var<-names(data[m])
     hist(data[,m],main=var)
   }
}


par(mfrow=c(5,4), mar=c(2,2,2,2))
for (i in 1:54) 
{
   if (names(data[i]) %in% MyVar) 
   {
     var<-names(data[i])
     qqnorm((data[,i]),main=var)
   }
}  


#if printing in file. Close 
#dev.off()

# Excess of zeros
sum(data$cases == 0) / nrow(data) * 100 # 10%?de valores iguais a zero

################################
#to list the correlations bigest than 0.5
#17 variables
for (m in 1:17) {
     for (n in m:17) {
          if (n!=m) {   
              if (na.omit(cor[n,m])>=0.5 || na.omit(cor[n,m])<=-0.5) {
                  print(paste('correlation - ',colnames(cor)[m],'-',colnames(cor)[n],':',na.omit(cor[n,m])))
              }
          }  
     }
}
###################################
# VIF - variance inflation factor 
# criterio VIF < 3
# Remoing colinearity - using VIF
#bxpt1 só as sem colinearidade
#bxpt2 só as sem colinearidade (changing)
correl1<-c("atract_ind_10mil","pending_cat","clas_ip","siops_ind_32","coverage_work","coverage_neb","gdp_per_capta_10mil","tmax")
correl2<-c("katz","pending_cat","clas_ip","siops_ind_32","coverage_work","coverage_neb","imdsp","tmax")
corvif(data[,correl2])

# Ploting dependent variable x each one of the independent variable
# A ausencia de padroes claros entre X e cd uma da var indep nao significa q nao haja relacao entre elas.
# Signfica apenas q nao um ha claro relacionamento bivariado entre X e Y
# Modelagem com multiplas var explan pode proprocionar ainda um bom ajuste

### pode ajudar na decisao na escolha entre covariaveis colineares - entre duas ou mais, escolher a que tem "melhor" relacao com  variael dependente

# plotando Y em relacao a cd um dos Xs - usando o script 'HighstatLib.V9.R'

#png(filename="figura6.png", height=20, width=20, unit="cm", res=300)
MyX <- correl1
Myxyplot(data, MyX, "SMR", MyYlab = "Tx Padron")
#dev.off()

# Store the graph
bx_plt_imdsp <- ggplot(data, aes(x = as.factor(group_pop), y = imdsp))
bx_plt_imdsp + geom_boxplot()

bx_plt_cvg_work <-ggplot(data, aes(x = as.factor(group_pop), y = coverage_work))
bx_plt_cvg_work + geom_boxplot()


#CHECKING POSSIBLE CONFUNDING
############################################################################################
## Figure:  Boxplots of correlations between the covariates and the spatial eigenvector ##
##            U_xi69 for each year (left) and correlations between the covariates and the ##
##            temporal eigenvector U_gamma13 for each year (right).                       ##
############################################################################################
##################################################
## Define spatial and temporal structure matrix ##
##################################################
g <- inla.read.graph("graph")
Qs <- matrix(0, g$n, g$n)
for (i in 1:g$n){
  Qs[i,i]=g$nnbs[[i]]
  Qs[i,g$nbs[[i]]]=-1
}

Dm <- diff(diag(T),differences=1)
Qt <- t(Dm)%*%Dm

#st <- kronecker(Qt,Qs)

S <- length(unique(data$codigo7))
T <- length(unique(data$id_time1))
eigen.spatial <- eigen(Qs)$vectors[,S-1]
eigen.temporal <- eigen(Qt)$vectors[,T-1]
spatial.eigencor.X <- vector("list",10)
temporal.eigencor.X <- vector("list",10)
Data <- data.frame(O=data$cases, E=data$E, SMR=data$SMR,
	X1=data$atract_ind_10mil,
	X2=data$katz,
	X3=data$pending_cat,
	X4=data$clas_ip,
	X5=data$siops_ind_32 ,
	X6=data$coverage_work,
	X7=data$coverage_neb,
	X8=data$gdp_per_capta_10mil,
	X9=data$imdsp,
	X10=data$tmax,
      ID.area=data$id_space1, ID.year=rep(1:T,each=S), ID.area.year=data$id_space_time)

for(k in 1:10) {
  
  ## Compute (spatial) correlations by year ##
  spatial.cor <- numeric(T)
  
  for(i in 1:T){
    X.year <- Data[Data$ID.year==i, which(names(Data)==paste("X",k,sep=""))]
    spatial.cor[i] <- cor(X.year,eigen.spatial)
  }

  ## Compute (temporal) correlations by region ##
  temporal.cor <- numeric(S)
  
  for(i in 1:S){
    X.area <- Data[Data$ID.area==i, which(names(Data)==paste("X",k,sep=""))]
    temporal.cor[i] <- cor(X.area,eigen.temporal)
  }

  spatial.eigencor.X[[k]] <- spatial.cor
  temporal.eigencor.X[[k]] <- temporal.cor
}

var.name<-c("AtractIndex","Pending","PredIndex",
	"SiopsInd","DomVisitCov","NebulCov",
      "GDPperCapta","KatzIndex","SPIMD","MaxTemp")

graphics.off()
#pdf("figures/Figure_confound.pdf", width=12, height=8)
#png(filename = "figures/Figure_confound.png", width = 4000, height = 3000, res = 300)
png(filename = "figures/Figure_confound.png", width = 8000, height = 4500, res = 600)


par(mfrow=c(1,2), pty="s")

# Parâmetros gráficos para rotacionar o texto do eixo x
#par(xpd=TRUE, mar=c(5, 4, 4, 2) + 0.1)  # Ajusta as margens para melhor visualização dos rótulos
# Plot do boxplot
boxplot(spatial.eigencor.X, main=expression(paste("Correlation between X and ",U[xi[645]])), cex.main=1.5, ylim=c(-1,1), xaxt="n")
# Adicionando o eixo x com rotação de 45 graus
axis(1, at=1:k, labels=FALSE)  # Adiciona o eixo sem rótulos
text(x=1:k, y=par("usr")[3] - 0.1, labels=var.name, srt=45, adj=1, xpd=TRUE, cex=0.8)  # Rotaciona os rótulos

boxplot(temporal.eigencor.X, main=expression(paste("Correlation between X and ",U[gamma[44]])), cex.main=1.5, ylim=c(-1,1), xaxt="n")
axis(1, at=1:k, labels=FALSE)  # Adiciona o eixo sem rótulos
text(x=1:k, y=par("usr")[3] - 0.1, labels=var.name, srt=45, adj=1, xpd=TRUE, cex=0.8)  # Rotaciona os rótulos

dev.off()

#First open Exploratroy Data Analysis.R
source('HighstatLibV10.R')
#### 1 - reading the database
data_csv <- read.csv(paste(data_folder,"dados_tri.csv",sep=""),sep=';')
#### 2 - reading the map of the municipalities of the state of Sao Paulo and obtaining the neighborhood matrix
contour <- read_sf(paste(map_folder,"Contorno_SP.shp",sep=""))
shp <- read_sf(paste(map_folder,"Sao Paulo Completo.shp",sep=""))
ggplot() + geom_sf(data=shp) + theme_bw()
# neighborhood
nb <- poly2nb(as(shp, "Spatial"))
list(nb)
#nb2INLA("graph", nb)
adj <- paste(getwd(), "/graph", sep ="")
list(nb)
### transf the map in dataframe
sp.df <- shp %>% st_drop_geometry()
shp.df <- as.data.frame(sp.df)
################################
### Union of tables
data <- left_join(data_csv,shp.df,by=c("codigo7" = "CODM_7_NUM"))
data <- arrange(data, id_time1, codigo7) # order by time and MUN, keeping order shapefile
#CATEGORIZE THE PENDING VARIABLE.
#<10 - 1 EXCELENTE
#10 A 20 GOOD
#20 A 40 REGULAR
#>40 BAD
#DON'T HAVE PENDING BECAUSE DON'T WORK - terrible
# without information - 5
data$pending_cat <-5
data$pending_cat[data$pending < 10] <- 1
data$pending_cat[data$pending >= 10 & data$pending < 20] <- 2
data$pending_cat[data$pending >= 20 & data$pending < 40] <- 3
data$pending_cat[data$pending >= 40] <- 4

#transforming coverage work anda coverage neb in 0-1 / dividing by 100
data$coverage_work_01<-data$coverage_work/100
data$coverage_neb_01<-data$coverage_neb/100
data$coverage_work_01[is.na(data$coverage_work_01)]<-0
data$coverage_neb_01[is.na(data$coverage_neb_01)]<-0
data$coverage_work[is.na(data$coverage_work)]<-0.0
data$coverage_neb[is.na(data$coverage_neb)]<-0.0
data$E <- data$pop_muni*(sum(data$cases)/sum(data$pop_muni))
data$SMR <- data$cases/data$E
################################
################################
## Priors
hyper.bym2 <-list(prec = list(prior = "pc.prec", param = c(1, 0.01)), 
                  phi = list(prior = "pc", param = c(0.5, 0.5)))
hyper.iid <- list(theta=list(prior="pc.prec", param=c(1, 0.01))) # Pr(sd<1) = 0.01
##### Linear combination for the temporal random effects
lcs.tp <- inla.make.lincombs(id_time1 = diag(length(unique(data$id_time1))),
                             id_time2 = diag(length(unique(data$id_time2))))
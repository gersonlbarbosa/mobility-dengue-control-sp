library(INLA)
library(sf)
library(spdep)
library(Matrix)
library(INLAOutputs)
library(ggplot2)
library(dplyr)
library(ggspatial)
inla.setOption(num.threads = "auto")

setwd("c:\\users\\gerson.laurindo\\onedrive - prodesp\\pos-Doc\\modelo final")
data_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\mapa\\"
source("HighstatLibV10.R") 
source('reading data.R')


##########MAPS
load('model2_st3.RData')  #OPEN THE MODEL
##############################################################
mod_ajust<-Model2_st3$summary.fitted.values[,'mean']

ajuste<-data.frame(codigo = c(data$codigo7),ano=c(data$year),mes=c(data$seasons),
id=c(data$id_space_time),SMR=c(data$SMR),mod_st3=c(mod_ajust))
head(ajuste)
#################################################### MAPAS
#Merge the data - FILTRANDO POR PERÍODO  - fazer loop para o período todo
rm(list='mapas')
mapas<-data.frame()
for (i in 12:22)
{ 
   for (k in 1:4)
   {
   z<-'' 
   if (i==4 & k==1) 
      {
         x<-paste('mapa_ano_mes',i,0,k,sep='')
         mapas<-data.frame(x)
       } else 
       {
         if (k<10) {z<-0}
         x<-paste('mapa_ano_mes',i,z,k,sep='')
         mapas<-rbind(mapas,c(x))
       }
    }
}
mapas
#open the map
contorno <- read_sf("mapa/Contorno_SP.shp")
shp <- read_sf("mapa/Sao Paulo Completo.shp")
ggplot() + geom_sf(data=shp) + theme_bw()

for (i in 1:44) {
    inicio<-(i-1)*645+1
    fim<-i*645
    subset_ajuste<-ajuste[inicio:fim,]
    assign(mapas[i,1],inner_join(shp,subset_ajuste, by = c("CODM_7_NUM" = "codigo")))
}
lista_modelos<-list(mapa_ano_mes1201,mapa_ano_mes1202,mapa_ano_mes1203,mapa_ano_mes1204,
mapa_ano_mes1301,mapa_ano_mes1302,mapa_ano_mes1303,mapa_ano_mes1304,
mapa_ano_mes1401,mapa_ano_mes1402,mapa_ano_mes1403,mapa_ano_mes1404,
mapa_ano_mes1501,mapa_ano_mes1502,mapa_ano_mes1503,mapa_ano_mes1504,
mapa_ano_mes1601,mapa_ano_mes1602,mapa_ano_mes1603,mapa_ano_mes1604,
mapa_ano_mes1701,mapa_ano_mes1702,mapa_ano_mes1703,mapa_ano_mes1704,
mapa_ano_mes1801,mapa_ano_mes1802,mapa_ano_mes1803,mapa_ano_mes1804,
mapa_ano_mes1901,mapa_ano_mes1902,mapa_ano_mes1903,mapa_ano_mes1904,
mapa_ano_mes2001,mapa_ano_mes2002,mapa_ano_mes2003,mapa_ano_mes2004,
mapa_ano_mes2101,mapa_ano_mes2102,mapa_ano_mes2103,mapa_ano_mes2104,
mapa_ano_mes2201,mapa_ano_mes2202,mapa_ano_mes2203,mapa_ano_mes2204)
#glimpse(dados_map)
###################### MODEL
intervalo <- quantile(mod_ajust,probs=seq(0,1, by = (1/10)))
length(intervalo)
interv <- rep(NA,10)
for (n in 1:10)
{
     interv[n] <- paste( format(intervalo[n], digits = 1, scientific = TRUE),'to',format(intervalo[n+1], digits = 1, scientific = TRUE))
}
interv
cores_vermelhas <- colorRampPalette(c("white","red","darkred"))(10)
ver_mapas<-vector("list",44)
for (m in 1:11) {
    for (k in 1:4) {
         decis<-cut(lista_modelos[[(m-1)*4+k]]$mod_st3,breaks=intervalo,include.lowest=T,labels=(interv))
         mp<-ggplot() + 
             geom_sf(data=contorno)+
             geom_sf(data=lista_modelos[[(m-1)*4+k]],aes(fill=decis),color="transparent")+
           #  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.5, "cm"), style = "bar") +  #escala
           #  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(0, "cm"),pad_y = unit(7, "cm"), style = north_arrow_nautical) + #norte
           #  labs(title =  paste('Ano: 20', substr(mapas[(m-1)*4+k,1],13,14),' Periodo: ',substr(mapas[(m-1)*4+k,1],15,17),sep=''), size=9) + #titulo
             scale_fill_manual(values=cores_vermelhas,drop=F)+
             theme_classic()+
             theme(
             axis.line=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             plot.title = element_text(hjust = 0.2, vjust = 0, size = 14),legend.position='none')+   #para apresentar legenda embaixo colocar ='bottom'
          coord_sf(expand = FALSE, clip = "off") 
          arquivo <- paste("graphic2\\",mapas[(m-1)*4+k,1],".png",sep="")
          png(filename=arquivo, height=14, width=14, unit="cm", res=600)
          ver_mapas[[(m-1)*4+k]]<-mp
          print(mp)
          dev.off()
    }
}

####################  MAKE ONLY ONE MAP
m=1
k=1
         decis<-cut(lista_modelos[[(m-1)*4+k]]$mod_st3,breaks=intervalo,include.lowest=T,labels=(interv))
         mp<-ggplot() + 
             geom_sf(data=contorno)+
             geom_sf(data=lista_modelos[[(m-1)*4+k]],aes(fill=decis),color="transparent")+
           #  annotation_scale(location = "br", width_hint = 0.2, height = unit(0.5, "cm"), style = "bar") +  #escala
           #  annotation_north_arrow(location = "br", which_north = "true", pad_x = unit(2, "cm"),pad_y = unit(7.5, "cm"), style = north_arrow_nautical) + #norte
           #  labs(title =  paste('Ano: 20', substr(mapas[(m-1)*4+k,1],13,14),' Periodo: ',substr(mapas[(m-1)*4+k,1],15,17),sep=''), size=9) + #titulo
             scale_fill_manual(values=cores_vermelhas,drop=F)+
             theme_classic()+
             theme(
             axis.line=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
             plot.margin = unit(c(0, 0, 0, 0), "cm"),
             plot.title = element_text(hjust = 0.2, vjust = 0, size = 8),legend.position='bottom',legend.title = element_text(size = 7), legend.text = element_text(size = 7)) +   #para apresentar legenda embaixo colocar ='bottom'
             guides(fill = guide_legend(override.aes = list(color = "black"))) +  # Adiciona contorno preto aos quadrados da legenda
	    coord_sf(expand = FALSE, clip = "off") 
          arquivo <- paste(mapas[(m-1)*4+k,1],".png",sep="")
          png(filename=arquivo, height=14, width=14, unit="cm", res=600)
          ver_mapas[[(m-1)*4+k]]<-mp
          print(mp)
          dev.off()
####################


######################	ANIMATION
library(magick)
image_dir <- "C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\Modelo Final\\graphic2"
# Obtenha uma lista de todos os arquivos PNG no diretório
image_files <- list.files(image_dir, pattern = "*.png", full.names = TRUE)
image_files12 <- c(grep("120", image_files_mod1nl, value = TRUE),grep("121", image_files_mod1nl, value = TRUE))
image_files13 <- c(grep("130", image_files_mod1nl, value = TRUE),grep("131", image_files_mod1nl, value = TRUE))
image_files14 <- c(grep("140", image_files_mod1nl, value = TRUE),grep("141", image_files_mod1nl, value = TRUE))
image_files15 <- c(grep("150", image_files_mod1nl, value = TRUE),grep("151", image_files_mod1nl, value = TRUE))
image_files16 <- c(grep("160", image_files_mod1nl, value = TRUE),grep("161", image_files_mod1nl, value = TRUE))
image_files17 <- c(grep("170", image_files_mod1nl, value = TRUE),grep("171", image_files_mod1nl, value = TRUE))
image_files18 <- c(grep("180", image_files_mod1nl, value = TRUE),grep("181", image_files_mod1nl, value = TRUE))
image_files19 <- c(grep("190", image_files_mod1nl, value = TRUE),grep("191", image_files_mod1nl, value = TRUE))
image_files20 <- c(grep("200", image_files_mod1nl, value = TRUE),grep("201", image_files_mod1nl, value = TRUE))
image_files21 <- c(grep("210", image_files_mod1nl, value = TRUE),grep("211", image_files_mod1nl, value = TRUE))
image_files22 <- c(grep("220", image_files_mod1nl, value = TRUE),grep("221", image_files_mod1nl, value = TRUE))

# Carregue todas as imagens
images12 <- image_read(image_files12)
images13 <- image_read(image_files13)
images14 <- image_read(image_files14)
images15 <- image_read(image_files15)
images16 <- image_read(image_files16)
images17 <- image_read(image_files17)
images18 <- image_read(image_files18)
images19 <- image_read(image_files19)
images20 <- image_read(image_files20)
images21 <- image_read(image_files21)
images22 <- image_read(image_files22)
# Crie a animação a partir das imagens carregadas
animation12 <- image_animate(images12, fps = 1) # fps define os frames por segundo
animation13 <- image_animate(images13, fps = 2) # fps define os frames por segundo
animation14 <- image_animate(images14, fps = 1) # fps define os frames por segundo
animation15 <- image_animate(images15, fps = 1) # fps define os frames por segundo
animation16 <- image_animate(images16, fps = 1) # fps define os frames por segundo
animation17 <- image_animate(images17, fps = 1) # fps define os frames por segundo
animation18 <- image_animate(images18, fps = 1) # fps define os frames por segundo
animation19 <- image_animate(images19, fps = 1) # fps define os frames por segundo
animation20 <- image_animate(images20, fps = 1) # fps define os frames por segundo
animation21 <- image_animate(images21, fps = 1) # fps define os frames por segundo
animation22 <- image_animate(images22, fps = 1) # fps define os frames por segundo
# Salve a animação em um arquivo GIF
image_write(animation12, "map_mod1nl12.gif")
image_write(animation13, "map_mod1nl13.gif")
image_write(animation14, "map_mod1nl14.gif")
image_write(animation15, "map_mod1nl15.gif")
image_write(animation16, "map_mod1nl16.gif")
image_write(animation17, "map_mod1nl17.gif")
image_write(animation18, "map_mod1nl18.gif")
image_write(animation19, "map_mod1nl19.gif")
image_write(animation20, "map_mod1nl20.gif")
image_write(animation21, "map_mod1nl21.gif")
image_write(animation22, "map_mod1nl22.gif")
browseURL('map_mod1nl12.gif')
browseURL('map_mod1nl13.gif')




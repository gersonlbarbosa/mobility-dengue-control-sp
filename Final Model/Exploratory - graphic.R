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
library(cowplot)

setwd("c:\\users\\gerson.laurindo\\onedrive - prodesp\\pos-Doc\\modelo final")
data_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\dados\\"
map_folder<-"C:\\Users\\gerson.laurindo\\OneDrive - PRODESP\\Pos-Doc\\modelo final\\mapa\\"
source("HighstatLibV10.R") 
source('reading data.R')

################################ EXPLORAÇÃO DE VARIÁVEIS
#CASOS DE DENGUE E TRABALHO VISITAS
data$periodo<-paste(data$year,data$seasons,sep='-')
casos_periodo <- data %>%  group_by(periodo) %>% summarise(Casos = sum(as.integer(cases)),Trab=sum(na.omit(as.integer(at_vis_visita))),Count = n())
#par(mfrow=c(1,2), mar=c(1,1,1,1))
#edit(casos_periodo)

#casos de dengue
dengue<-ggplot(casos_periodo, aes(y = Casos, x = periodo))+ 
  geom_bar(stat = "identity") +
  geom_col(fill="red") +
  scale_y_continuous(
    limits = c(0, max(casos_periodo$Casos, na.rm = TRUE)),  # Definir o limite superior automaticamente
    breaks = seq(0, max(casos_periodo$Casos, na.rm = TRUE), by = round(max(casos_periodo$Casos, na.rm = TRUE)/5, 0)),  # Definir intervalos
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")  # Formatação com separador de milhar
    ) + 
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
  axis.text.y = element_text(face = "bold"))+
  xlab("Period") + 
  ylab("Dengue cases")+
  ggtitle("B") 
 

# Ajustando o eixo Y para mostrar múltiplos valores
imoveis <- ggplot(casos_periodo, aes(y = Trab, x = periodo), fill = periodo) + 
  geom_bar(stat = "identity") +
  geom_col(fill = "darkblue") +
  # Personalizando o eixo Y para mostrar intervalos regulares e o valor máximo
  scale_y_continuous(
    limits = c(0, max(casos_periodo$Trab, na.rm = TRUE)),  # Definir o limite superior automaticamente
    breaks = seq(0, max(casos_periodo$Trab, na.rm = TRUE), by = round(max(casos_periodo$Trab, na.rm = TRUE)/5, 0)),  # Definir intervalos
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")  # Formatação com separador de milhar
  ) +
  theme_classic(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face="bold"),
  axis.text.y = element_text(face = "bold")  # Adiciona o negrito ao eixo y também, se desejado
) +
  xlab("Period") + 
  ylab("Domiciliary Visit") +
  ggtitle("A") 

# Combinando os gráficos
grafico_combinado <- plot_grid(imoveis, dengue, ncol = 1, align = 'v')
print(grafico_combinado)

graphics.off()
png(filename = "figures/FigureX1.png", width = 8000, height = 8000, res = 600)
print(grafico_combinado)
dev.off()

###########################
#CASOS DE DENGUE E TRABALHO VISITAS POR GRUPO POPULACIONAL
casos_periodo_grp <- data %>%  group_by(periodo, group_pop) %>% summarise(Casos = sum(as.integer(cases)),Trab=sum(na.omit(as.integer(at_vis_visita))),Count = n())
par(mfrow=c(1,2), mar=c(1,1,1,1))
#edit(casos_periodo)
#CASOS
dengue_grp<-ggplot(casos_periodo_grp, aes(x = periodo, y = Casos, color = factor(group_pop), group = group_pop)) +
  geom_line(size = 2)+  # Adiciona as linhas com espessura
  geom_point(size = 3) + 
 scale_y_continuous(
    limits = c(0, max(casos_periodo_grp$Casos, na.rm = TRUE)),  # Definir o limite superior automaticamente
    breaks = seq(0, max(casos_periodo_grp$Casos, na.rm = TRUE), by = round(max(casos_periodo_grp$Casos, na.rm = TRUE)/5, 0)),  # Definir intervalos
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")  # Formatação com separador de milhar
)+ # Adiciona pontos nas linhas para destacar os dados
  scale_color_brewer(palette = "Set1") +  # Escolhe uma paleta de cores para as linhas
  labs(x = "Period", y = "Dengue Cases", color = "Population Group", size=20) +
  ggtitle("A") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    axis.line = element_line(color = "black"),  # Adiciona linhas no eixo x e y
    axis.ticks = element_line(color = "black"),  # Adiciona as marcas nos eixos
    legend.position = "bottom"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size=16),  # Rotaciona e coloca em negrito
    axis.text.y = element_text(face = "bold", size=16),  # Adiciona o negrito ao eixo y também, se desejado
    legend.text = element_text(size = 14),  # Aumenta o tamanho do texto na legenda
    legend.title = element_text(size = 16, face = "bold"),  # Aumenta o tamanho do título da legenda
    axis.title.x = element_text(size = 16, face = "bold"),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 16, face = "bold")   # Aumenta o tamanho do título do eixo y (Dengue Cases)
)  # Rotaciona os labels do eixo x


#VISITAS
imoveis_grp<-ggplot(casos_periodo_grp, aes(x = periodo, y = Trab, color = factor(group_pop), group = group_pop)) +
  geom_line(size = 2)+  # Adiciona as linhas com espessura
  geom_point(size = 3) + 
 scale_y_continuous(
    limits = c(0, max(casos_periodo_grp$Trab, na.rm = TRUE)*1.1),  # Definir o limite superior automaticamente
    breaks = seq(0, max(casos_periodo_grp$Trab, na.rm = TRUE), by = round(max(casos_periodo_grp$Trab, na.rm = TRUE)/5, 0)),  # Definir intervalos
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")  # Formatação com separador de milhar
)+ # Adiciona pontos nas linhas para destacar os dados
  scale_color_brewer(palette = "Set1") +  # Escolhe uma paleta de cores para as linhas
  labs(x = "Period", y = "Visited househoulds", color = "Population Group") +
  ggtitle("B") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    axis.line = element_line(color = "black"),  # Adiciona linhas no eixo x e y
    axis.ticks = element_line(color = "black"),  # Adiciona as marcas nos eixos
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size=16),  # Rotaciona e coloca em negrito
    axis.text.y = element_text(face = "bold", size=16),  # Adiciona o negrito ao eixo y também, se desejado
    legend.text = element_text(size = 16),  # Aumenta o tamanho do texto na legenda
    legend.title = element_text(size = 16, face = "bold"),  # Aumenta o tamanho do título da legenda
    axis.title.x = element_text(size = 16, face = "bold"),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 16, face = "bold")   # Aumenta o tamanho do título do eixo y (Dengue Cases)
)  # Rotaciona os labels do eixo x

grafico_combinado_grp <- plot_grid(dengue_grp, imoveis_grp, ncol = 1, align = 'v')
print(grafico_combinado_grp)

graphics.off()
png(filename = "figures/FigureX2.png", width = 8000, height = 8000, res = 600)
print(grafico_combinado_grp)
dev.off()

#cor(as.integer(data$cases), as.integer(data$at_vis_visita))

##################################################
#	FEITO PELA INCIDENCIA  E OS IMOVEIS TRABALHADOS RELATIVISADOS PELA POPULAÇÃO
####################################################
incidencia <- data %>%  group_by(periodo, group_pop) %>% summarise(Incidencia = sum(as.integer(cases))/sum(pop_muni),Trab=sum(na.omit(as.integer(at_vis_visita)))/sum(pop_muni),Count = n())

inc_dengue<-ggplot(incidencia, aes(x = periodo, y = Incidencia, color = factor(group_pop), group = group_pop)) +
  geom_line(size = 1) +  # Adiciona as linhas com espessura
  geom_point(size = 2) + 
 scale_y_continuous(labels = scales::comma)+ # Adiciona pontos nas linhas para destacar os dados
  scale_color_brewer(palette = "Set1") +  # Escolhe uma paleta de cores para as linhas
  labs(x = "Period", y = "Dengue Incidence", color = "Population Group") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    axis.line = element_line(color = "black"),  # Adiciona linhas no eixo x e y
    axis.ticks = element_line(color = "black"),  # Adiciona as marcas nos eixos
    legend.position = "bottom"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotaciona e coloca em negrito
    axis.text.y = element_text(face = "bold")  # Adiciona o negrito ao eixo y também, se desejado
)  # Rotaciona os labels do eixo x

imoveis_pop<-ggplot(incidencia, aes(x = periodo, y = Trab, color = factor(group_pop), group = group_pop)) +
  geom_line(size = 1) +  # Adiciona as linhas com espessura
  geom_point(size = 2) + 
 scale_y_continuous(labels = scales::comma)+ # Adiciona pontos nas linhas para destacar os dados
  scale_color_brewer(palette = "Set1") +  # Escolhe uma paleta de cores para as linhas
  labs(x = "Period", y = "Visited househoulds", color = "Population Group") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    axis.line = element_line(color = "black"),  # Adiciona linhas no eixo x e y
    axis.ticks = element_line(color = "black"),  # Adiciona as marcas nos eixos
    legend.position = "bottom"
  )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotaciona e coloca em negrito
    axis.text.y = element_text(face = "bold")  # Adiciona o negrito ao eixo y também, se desejado
)  # Rotaciona os labels do eixo x

grafico_combinado <- plot_grid(inc_dengue, imoveis_pop, ncol = 1, align = 'v')
print(grafico_combinado)


###############################  FEITO NA FSP
#   CONSTRUINDO GRÁFICO DE CASOS DE DENGUE POR GRUPO POPULACIONAL
dados_agrupados <- data %>%
  group_by(group_pop, year, seasons) %>%
  summarise(casos = sum(cases, na.rm = TRUE),
  temp = mean(tmax, na.rm = TRUE)
) %>%
  mutate(periodo = paste(year, seasons, sep = "-")) 

cores <- c("1" = "#33ff47",  # Cor para o Grupo 1
           "2" = "#33fffc",  # Cor para o Grupo 2
           "3" = "#4433ff",  # Cor para o Grupo 3
           "4" = "#f50519",  # Cor para o Grupo 4
           "5" = "#0f0f0f",
           "Temperatura" = "#ebca0e") 

ggplot(dados_agrupados, aes(x = periodo, y = casos/1000, color = factor(group_pop), group = group_pop)) +
  geom_line(size = 1) +  # Adiciona as linhas com espessura
  geom_point(size = 2) +  # Adiciona pontos nas linhas para destacar os dados
#  geom_line(data = dados_agrupados, aes(x = periodo, y = temp, color = "Temperatura"), size = 1, linetype = "dashed") +  # Linha para temperatura
  scale_color_manual(values = cores) +  
  #scale_color_brewer(palette = "Set1") +  # Escolhe uma paleta de cores para as linhas
  labs(x = "Period", y = "Dengue cases x 1000", color = "Population Group") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove linhas de grade principais
    panel.grid.minor = element_blank(),  # Remove linhas de grade menores
    plot.title = element_text(size = 16, face = "bold"),  # Tamanho e estilo do título
    axis.title.x = element_text(size = 14),               # Tamanho do título do eixo x
    axis.title.y = element_text(size = 14),
    axis.line = element_line(color = "black"),  # Adiciona linhas no eixo x e y
    axis.ticks = element_line(color = "black"),  # Adiciona as marcas nos eixos
    legend.position = "bottom",  # Modifica a posição da legenda (exemplo: "bottom")
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(face='bold')
  )

###############


bx_plt_cvg_work <-ggplot(data, aes(x = as.factor(group_pop), y = coverage_work))
bx_plt_cvg_work + geom_boxplot()


bxplt_cvg<-ggplot(data, aes(x = as.factor(group_pop), y = coverage_work)) +
  geom_boxplot() +  # Adiciona o boxplot
  labs(title = "(A)   Domiciliary Visit Coverage by Population Group",  # Título do gráfico
       x = "Population Group",  # Rótulo do eixo X
       y = "Percentage of Domiciliary Visit") +  # Rótulo do eixo Y
  theme_minimal() +  # Remove o fundo padrão e usa um tema limpo
  theme(
    panel.background = element_blank(),  # Remove o fundo do gráfico
    axis.line = element_line(color = "black"),  # Adiciona linhas dos eixos X e Y
    axis.title = element_text(face = "bold", size=16),  # Deixa os títulos dos eixos em negrito
    axis.text = element_text(face = "bold", size=16),  # Deixa os rótulos dos eixos em negrito
    plot.title = element_text(hjust = 0.5, face = "bold",size=16),  # Centraliza e coloca o título em negrito
    panel.grid.major = element_blank(),  # Remove as linhas de grade maiores
    panel.grid.minor = element_blank()   # Remove as linhas de grade menores
  )
bxplt_cvg

bxplt_pend<-ggplot(data, aes(x = as.factor(group_pop), y = pending)) +
  geom_boxplot() +  # Adiciona o boxplot
  labs(title = "(B)   Pending (Unaddressed properties) by Population Group",  # Título do gráfico
       x = "Population Group",  # Rótulo do eixo X
       y = "unaddressed properties") +  # Rótulo do eixo Y
  theme_minimal() +  # Remove o fundo padrão e usa um tema limpo
  theme(
    panel.background = element_blank(),  # Remove o fundo do gráfico
    axis.line = element_line(color = "black"),  # Adiciona linhas dos eixos X e Y
    axis.title = element_text(face = "bold",size=16),  # Deixa os títulos dos eixos em negrito
    axis.text = element_text(face = "bold",size=16),  # Deixa os rótulos dos eixos em negrito
    plot.title = element_text(hjust = 0.5, face = "bold",size=16),  # Centraliza e coloca o título em negrito
    panel.grid.major = element_blank(),  # Remove as linhas de grade maiores
    panel.grid.minor = element_blank()   # Remove as linhas de grade menores
  )
bxplt_pend

grafico_combinado_bxplt <- plot_grid(bxplt_cvg, bxplt_pend, ncol = 1, align = 'v')
print(grafico_combinado_bxplt)

graphics.off()
png(filename = "figures/FigureX3.png", width = 8000, height = 8000, res = 600)
print(grafico_combinado_bxplt)
dev.off()


library(ggplot2)

Covariates = c("Intercept","Katz","Pending Between 10% and 20% (good)",
"Pending Between 20% and 40% (regular)","Pending > 40% (Bad)",
"Pending - No activities executed","PredIndex Between 1 and 3.9 (alert)",
"PredIndex > 4.0 (risk)","PredIndex - No activities executed",
"SiopsInd","DomVisitCov","NebuliCov","SPIMD","MaxTemp")

IRR =   c(0.048,1.132,1.067,1.262,1.365,1.035,1.003,0.915,0.980,1.094,1.083,1.180,1.219,1.208)
lower = c(0.044,1.084,0.975,1.146,1.219,0.884,0.960,0.836,0.935,1.041,1.044,1.160,1.121,1.064)
upper = c(0.052,1.181,1.165,1.386,1.524,1.205,1.049,0.999,1.026,1.150,1.122,1.200,1.323,1.366)

data <- data.frame(Covariates,IRR,lower, upper)
data$Covariates <- factor(data$Covariates, levels = rev(data$Covariates)) #(para deixar na mesma ordem)

data$color <- ifelse(data$upper <1 & data$lower  < 1, "blue",
              ifelse(data$lower <1 & data$upper >1, "red","blue")) # Vermelho se intervalo contiver 1, caso contrário, azul

# Criar o gráfico
ggplot(data, aes(x = IRR, y = Covariates)) +
  geom_point(aes(color = color), size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = color), height = 0.2) + # Barras de erro coloridas
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") + # Linha de referência
  labs(
    x = "Incidence Rate Ratio (IRR)", 
    y = "Covariates"
  ) +
  scale_color_identity() + # Usa as cores definidas em 'color' diretamente
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"), # Linhas de fundo pontilhadas
    axis.line.x = element_line(color = "black"), # Adiciona linha no eixo X
    axis.line.y = element_line(color = "black"), # Adiciona linha no eixo Y
    axis.text.y = element_text(size = 10)
  )


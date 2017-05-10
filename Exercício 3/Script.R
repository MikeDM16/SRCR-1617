library(neuralnet)
library(hydroGOF)
dados <- read.csv("C:/Users/Diogo/OneDrive/Universidade/3? Ano/2? Semestre/SRCR/TP1/SRCR-1617/Exerc?cio 3/exaustao_aleatorio.csv", header= TRUE, sep = ';', dec = ',' )
treino <- dados[1:559,]
teste <- dados[559:844,]
formula01 <- FatigueLevel ~ Performance.KDTMean + Performance.MAMean + Performance.DDCMean

i <- 3
while(i >= 1) {
  j <- i
  while(j >= 1) {
    rna <- neuralnet(formula01, treino, hidden = c(i,j), threshold = 0.01, stepmax = 1e+06)
    teste.01 <- subset(teste,select = c("Performance.KDTMean","Performance.MAMean","Performance.DDCMean"))
    rna.resultados <- compute(rna, teste.01)
    resultados <- data.frame(atual = teste$FatigueLevel, previsao = rna.resultados$net.result)
    resultados$previsao <- round(resultados$previsao, digits = 0)
    out <- paste("Número de nodos:", i, j, "| RMSE:", rmse(c(teste$FatigueLevel),c(resultados$previsao)), sep = " ")
    print(out)
    plot(rna)
    j <- j - 1
  }
  i <- i - 1
}

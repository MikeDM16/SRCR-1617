library(neuralnet)
library(hydroGOF)

------------------------------------------ Fatigue Level 7 niveis -------------------------------------------
dados <- read.csv("~/OneDrive/Documentos/MiEI/3.º ano/2.º semestre/Sistemas de Representação de Conhecimento e Raciocínio/Trabalhos práticos/SRCR-1617/Exercício 3/Dados/exaustao_FatigueLevel_7.csv", header= TRUE, sep = ';', dec = ',' )
treino <- dados[1:559,]
teste <- dados[559:844,]

formula01 <- FatigueLevel ~ Performance.MAMean + Performance.MVMean + Performance.DDCMean + Performance.DMSMean + Performance.ADMSLMean + Performance.KDTMean + Performance.TBCMean
rna <- neuralnet(formula01, treino, hidden = c(8,4), threshold = 0.01, stepmax = 1e+08, lifesign = "full")
teste.01 <- subset(teste,select = c("Performance.MAMean","Performance.MVMean","Performance.DDCMean","Performance.DMSMean","Performance.ADMSLMean","Performance.KDTMean","Performance.TBCMean"))
rna.resultados <- compute(rna, teste.01)
resultados <- data.frame(atual = teste$FatigueLevel, previsao = rna.resultados$net.result)
rmse(c(teste$FatigueLevel),c(resultados$previsao))



------------------------------------------ Fatigue Level 2 niveis -------------------------------------------
dados <- read.csv("~/OneDrive/Documentos/MiEI/3.º ano/2.º semestre/Sistemas de Representação de Conhecimento e Raciocínio/Trabalhos práticos/SRCR-1617/Exercício 3/Dados/exaustao_FatigueLevel_2.csv", header= TRUE, sep = ';', dec = ',' )
treino <- dados[1:559,]
teste <- dados[559:844,]

formula01 <- Performance.Task ~ Performance.KDTMean
rna <- neuralnet(formula01, treino, hidden = c(3,2), threshold = 0.01, stepmax = 1e+08, lifesign = "full")
teste.01 <- subset(teste,select = c("Performance.KDTMean"))
rna.resultados <- compute(rna, teste.01)
resultados <- data.frame(atual = teste$Performance.Task, previsao = rna.resultados$net.result)
rmse(c(teste$Performance.Task),c(resultados$previsao))


# Qui-quadrado de aderência

levels(Data2018$Q12P2.B_Atitude_Turno_2)

quiqua <- chisq.test(x = table(Data2018$Q12P2.B_Atitude_Turno_2),
                     p = c(0.711, 0.212, 0.058, 0.019))

#2018, conforme site TSE.
#nominais: 0,7117
#abstenção 0,2129
#Voto nulo 0,0584
#voto em branco 0,0193

quiqua

options(scipen = 999)#se o valor de p sair em notação cirntífica,
#usar este recurso converte para valor mais compreensível.

quiqua$observed#valor observado
quiqua$expected#valor esperado

quiqua$stdres#resídos são significativos fora da faixa -1,96 + 1,96
# [-1.96, 1.96]
# Voto nominal ñ significatico.

#Visualizalização gráfica, mais fácil de entender e mais bonita
#Diferença significatica entre valores observados e esperados.
#Abstenção e voto nulo estão subpresentados, ao passo que voto
#nominal está superrepresentado.

library(fstat)

fstat::graf_obs_esp_ader(Data2018, v1 = Q12P2.B_Atitude_Turno_2,
                         props = c(0.711, 0.212, 0.058, 0.019), cor = "cadetblue")

# Qui-quadrado de aderência

levels(Data2018$Q12P2.B_Atitude_Turno_2)

options(scipen = 999)

quiqua <- chisq.test(x = table(Data2018$Q12P2.B_Atitude_Turno_2),
                     p = c(0.78, 0.21, 0.01))

#2018, conforme site TSE.
#nominais: 0,6793
#abstenção 0,2199
#Voto nulo 0,0743
#voto em branco 0,0193
#Voto branco e nulo 0.1008

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

QuiQua <- fstat::graf_obs_esp_ader(Data2018, v1 = Q12P2.B_Atitude_Turno_2,
                         props = c(0.78, 0.21, 0.01), cor = paleta)


ggsave("EligQUIQUAD.png", width = 6, height = 4.5, dpi = 400)

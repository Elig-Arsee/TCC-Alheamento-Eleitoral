###################################################_____ANÁLISE SATISFAÇÃO_____

# Passo 1: Checagem de pressupostos para análise Satisfação

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
correlação <- psych::pairs.panels(Data2018[1:18])

glimpse(Data2018)


#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)
library(mlogit)

VIF_todos <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~
                Regiao +
                D2_Sexo +
                D1A_Faixa_Idade +
                D3_Escolaridade +
                D10_Religiao +
                D12a_Cor_Raca_IBGE +
                D9B_FAIXA_RENDA +
                Q11_Satisfacao_Democracia +
                P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + 
                P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados +
                P5_Confianca_Eleicoes +
                P4.4_Confianca_Governo_Federal +
                P4.7_Confianca_Partidos_Politicos +
                P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados +
                P12_Percepcao_Corrupcao_Problema_Serio +
                Q7_Percepcao_Corrupcao_Generalizada +
                P24_Voto_Obrigatorio,
              data = Data2018)

car::vif(VIF_todos)


#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção)
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.

install.packages("mlogit")#se não tiver pacote instalado precisa instalar.
library(mlogit)

# Modelo com todas as alternativas
modi_todos0 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                                Regiao +
                                D2_Sexo +
                                D1A_Faixa_Idade +
                                D3_Escolaridade +
                                D10_Religiao +
                                D12a_Cor_Raca_IBGE +
                                D9B_FAIXA_RENDA +
                                Q11_Satisfacao_Democracia +
                                P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + 
                                P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados +
                                P5_Confianca_Eleicoes +
                                P4.4_Confianca_Governo_Federal +
                                P4.7_Confianca_Partidos_Politicos +
                                P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados +
                                P12_Percepcao_Corrupcao_Problema_Serio +
                                Q7_Percepcao_Corrupcao_Generalizada +
                                P24_Voto_Obrigatorio,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal")#Categoria de referência

# Modelo excluindo voto branco ou nulo
modi_todos2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                             Regiao +
                             D2_Sexo +
                             D1A_Faixa_Idade +
                             D3_Escolaridade +
                             D10_Religiao +
                             D12a_Cor_Raca_IBGE +
                             D9B_FAIXA_RENDA +
                             Q11_Satisfacao_Democracia +
                             P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + 
                             P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados +
                             P5_Confianca_Eleicoes +
                             P4.4_Confianca_Governo_Federal +
                             P4.7_Confianca_Partidos_Politicos +
                             P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados +
                             P12_Percepcao_Corrupcao_Problema_Serio +
                             Q7_Percepcao_Corrupcao_Generalizada +
                             P24_Voto_Obrigatorio,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Abstenção", "Voto nominal"))

# Modelo excluindo voto abstenção
modi_todos3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                                Regiao +
                                D2_Sexo +
                                D1A_Faixa_Idade +
                                D3_Escolaridade +
                                D10_Religiao +
                                D12a_Cor_Raca_IBGE +
                                D9B_FAIXA_RENDA +
                                Q11_Satisfacao_Democracia +
                                P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + 
                                P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados +
                                P5_Confianca_Eleicoes +
                                P4.4_Confianca_Governo_Federal +
                                P4.7_Confianca_Partidos_Politicos +
                                P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados +
                                P12_Percepcao_Corrupcao_Problema_Serio +
                                Q7_Percepcao_Corrupcao_Generalizada +
                                P24_Voto_Obrigatorio,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Voto nominal", "Voto branco ou nulo"))


# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_todos0, modi_todos2)
mlogit::hmftest(modi_todos0, modi_todos3)



# 5. Construção de modelos de regressão multinomial
library(nnet)
?multinom

# Modelo todos
modelo_todos <- multinom(Q12P2.B_Atitude_Turno_2 ~ Regiao +
                           D2_Sexo +
                           D1A_Faixa_Idade +
                           D3_Escolaridade +
                           D10_Religiao +
                           D12a_Cor_Raca_IBGE +
                           D9B_FAIXA_RENDA +
                           Q11_Satisfacao_Democracia +
                           P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + 
                           P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados +
                           P5_Confianca_Eleicoes +
                           P4.4_Confianca_Governo_Federal +
                           P4.7_Confianca_Partidos_Politicos +
                           P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados +
                           P12_Percepcao_Corrupcao_Problema_Serio +
                           Q7_Percepcao_Corrupcao_Generalizada +
                           P24_Voto_Obrigatorio,
                    data = Data2018, model = TRUE)

# Modelo todos sem nenhum previsor
modelo_todos0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)

# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova <- anova(modelo_todos0, modelo_todos)

# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
DescTools::PseudoR2(modelo_todos, which = "Nagelkerke")


#Para verificar efeitos globais
car::Anova(modelo_todos, type = "II", test = "Nald")
gtsummary::tbl_summary(EG)

#Para verificar coeficientes do modelo
summary(modelo_todos)

#Obtenção dos valores de p - por Wald (mesmo tipo do spss)
lmtest::coeftest(modelo_todos)

#Calcula razão de chance
exp(coef(modelo_todos))

#Calcula intervalo de confiança para coeficientes
exp(confint(modelo_todos))

#Tabela completa

gtsummary::tbl_regression(modelo_todos, exponentiate = TRUE)

options(scipen = 999)

install.packages("pacman", repos="http://cran.rstudio.com/", dependencies=TRUE)

#Gráfico
sjPlot::plot_model(modelo_todos)

#Verificar modelo: ficou bem ruim.
summary(predict(mod_satisfação))




#Carrego Bibliotecas
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools,
               ggrastr, sjPlot)

#Carrego Banco de dados completo
dados2018 <- read.csv2("Data2018.csv", stringsAsFactors = TRUE,
                       fileEncoding = "latin1")


#Visualizo Banco de Dados
View(dados2018)
glimpse(dados2018)

#Gravo novo dataframe onde a coluna de atitude turno 2 não tem as linhas de pessoas que não sabem ou não
#responderam em quem votaram
Data2018 <- droplevels(subset(dados2018,
                              Q12P2.B_Atitude_Turno_2 == "Abstenção" |
                                Q12P2.B_Atitude_Turno_2 == "Voto nulo" |
                                Q12P2.B_Atitude_Turno_2 == "Voto em branco" |
                                Q12P2.B_Atitude_Turno_2 == "Voto nominal"))

#Visualizo somente a variável Atitude turno 2 para ver se realmente linhas com NR/NS foram excluídas
summary(Data2018$Q12P2.B_Atitude_Turno_2)


#Estabeleço a ordem das categorias
Data2018$Q12P2.B_Atitude_Turno_2 <- factor(Data2018$Q12P2.B_Atitude_Turno_2,
                                           levels=c("Voto nominal","Abstenção","Voto nulo","Voto em branco"))

#Visualizo somente a variável Atitude turno 2 para ver se varáveis foram reordenadas
summary(Data2018$Q12P2.B_Atitude_Turno_2)


#Verifico níveis das categorias, mudo ordem e aplico relevel para estipular categorias de referência

#Idade
levels(Data2018$D1A_Faixa_Idade) #Verifico níveis
table(Data2018$D1A_Faixa_Idade) # Verifico ocorrências de cada nível
Data2018$D1A_Faixa_Idade <- relevel(Data2018$D1A_Faixa_Idade,
                                    ref = "25 A 34")#Seleciono categoria mais recorrente como referência

#Sexo
levels(Data2018$D2_Sexo)#Não precisa estipular categoria de referência porque já é a feminina.

#Escolaridade
levels(Data2018$D3_Escolaridade)
Data2018$D3_Escolaridade <- factor(Data2018$D3_Escolaridade,
                                           levels=c("Analfabeto", "Fundamental incompleto",
                                                    "Fundamental completo", "Médio incompleto",
                                                    "Médio completo", "Universitário incompleto",
                                                    "Universitário completo", "Pós-graduação ou mais"))

table(Data2018$D3_Escolaridade)
Data2018$D3_Escolaridade <- relevel(Data2018$D3_Escolaridade,
                                    ref = "Médio completo")

#Renda
levels(Data2018$D9B_FAIXA_RENDA)
Data2018$D9B_FAIXA_RENDA <- factor(Data2018$D9B_FAIXA_RENDA,
                                   levels=c("Até R$ 954,00 (até 1 salário mínimo)",
                                            "Mais de R$ 954,00 até R$ 1.908,00 (mais de 1 até 2 salários mínimos)",
                                            "Mais de R$ 1.908,00 até R$ 4.770,00 (mais de 2 até 5 salários mínimos)",
                                            "Mais de R$ 4.770,00 até R$ 9.540,00 (mais de 5 até 10 salários mínimos)",
                                            "Mais de R$ 9.540,00 até R$ 14.310,00 (mais de 10 até 15 salários mínimos)",
                                            "Mais de R$ 14.310,00 até R$ 19.080,00 (mais de 15 até 20 salários mínimos)",
                                            "Mais de R$ 19.080,00 (mais de 20 salários mínimos)",
                                            "NS/NR"))

table(Data2018$D9B_FAIXA_RENDA)
Data2018$D9B_FAIXA_RENDA <- relevel(Data2018$D9B_FAIXA_RENDA,
                                    ref = "Mais de R$ 1.908,00 até R$ 4.770,00 (mais de 2 até 5 salários mínimos)")

#Cor/Raça
levels(Data2018$D12a_Cor_Raca_IBGE)
Data2018$D12a_Cor_Raca_IBGE <- factor(Data2018$D12a_Cor_Raca_IBGE,
                                   levels=c("Pardo", "Preto", "Branco", "Amarelo", "Indígena", "NS/NR"))

table(Data2018$D12a_Cor_Raca_IBGE)
Data2018$D12a_Cor_Raca_IBGE <- relevel(Data2018$D12a_Cor_Raca_IBGE,
                                       ref = "Pardo")

#Localidade
levels(Data2018$Localidade)
table(Data2018$Localidade)
Data2018$Localidade <- relevel(Data2018$Localidade,
                               ref = "URBANO")
#Região
levels(Data2018$Regiao)
table(Data2018$Regiao)
Data2018$Regiao <- relevel(Data2018$Regiao,
                           ref = "Sudeste")
#Religião
levels(Data2018$D10_Religiao)
table(Data2018$D10_Religiao)

Data2018$D10_Religiao <- factor(Data2018$D10_Religiao,
                                      levels=c("Católica", "Evangélica", "Outras",
                                               "Ateu/Agnóstico/Não tem religião", "NS/NR"))

Data2018$D10_Religiao <- relevel(Data2018$D10_Religiao,
                                 ref = "Católica")

#Situação profissional
levels(Data2018$D07_situacao.profissional)
Data2018$D07_situacao.profissional <- factor(Data2018$D07_situacao.profissional,
                                             levels=c("Trabalho formal", "Trabalho informal",
                                                      "Aposentado ou pensionista", "Estudante, estagiário ou aprendiz",
                                                      "Atividade sem remuneração",
                                                      "Desempregado procurando ou não por emprego",
                                                      "NS/NR"))

table(Data2018$D07_situacao.profissional)
Data2018$D07_situacao.profissional <- relevel(Data2018$D07_situacao.profissional,
                                              ref = "Trabalho informal")

###################################################_____ANÁLISE SATISFAÇÃO_____

# Passo 1: Checagem de pressupostos para análise Satisfação

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
psych::pairs.panels(Data2018[10:13])


#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)

VIF_sat <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~ Q21_Satisfacao_Democracia+P3.4_Avaliacao_Governo_Federal+P3.7_Avaliacao_Partidos_Politicos+P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
              data = Data2018)

car::vif(VIF_sat)


#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção ou NR/NS),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.

install.packages("mlogit")#se não tiver pacote instalado precisa instalar.
library(mlogit)

# Modelo com todas as alternativas
modi_ai <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                         data = Data2018,
                         shape = "wide",
                         reflevel = "Voto nominal")#Categoria de referência

# Modelo excluindo voto em branco
modi_ai2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal",
                          alt.subset = c("Abstenção", "Voto nominal", "Voto anulado"))

# Modelo excluindo voto nulo
modi_ai3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal",
                          alt.subset = c("Abstenção", "Voto nominal", "Voto em branco"))

# Modelo excluindo abstenção
modi_ai4 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal",
                          alt.subset = c("Voto nominal", "Voto anulado", "Voto em branco"))

# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai, modi_ai2)
mlogit::hmftest(modi_ai, modi_ai3)
mlogit::hmftest(modi_ai, modi_ai4)


# 5. Construção de modelos de regressão multinomial
library(nnet)
?multinom

# Modelo Satisfação
mod_sat <- multinom(Q12P2.B_Atitude_Turno_2 ~ Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                           data = Data2018, model = TRUE)

# Modelo nulo sem nenhum previsor
mod_sat0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)

# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova <- anova(mod_sat0, mod_sat)

gtsummary::tbl_summary(Anova)


# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
R2_Neig_sat <- DescTools::PseudoR2(mod_sat, which = "Nagelkerke")


#Obtenção dos valores de p - por Wald (mesmo tipo do spss)
T_Wald_sat <- lmtest::coeftest(mod_sat)


#Para verificar efeitos globais
car::Anova(mod_sat, type = "II", test = "Nald")


#Para verificar coeficientes do modelo
summary(mod_sat)


#Calcula razão de chance
exp(coef(mod_sat))

#Calcula intervalo de confiança para coeficientes
exp(confint(mod_sat))

#Tabela completa

Tab_Multi_Sat <- gtsummary::tbl_regression(mod_sat, exponentiate = TRUE)

as_gt(Tab_Multi_Sat)



install.packages("pacman", repos="http://cran.rstudio.com/", dependencies=TRUE)

#Gráfico
sjPlot::plot_model(mod_sat)

#Verificar modelo: ficou bem ruim.
summary(predict(mod_sat))

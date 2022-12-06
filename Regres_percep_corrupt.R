#Carrego Bibliotecas
if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools,
               ggrastr, sjPlot)


#Carrego Banco de dados completo
dados2018 <- read.csv("Data_2018.csv", stringsAsFactors = TRUE,
                      fileEncoding = "latin1", header = TRUE, sep = "\t")


#Visualizo Banco de Dados
View(dados2018)
glimpse(dados2018)

#Gravo novo dataframe onde a coluna de atitude turno 2 não tem as linhas de pessoas que não sabem ou não
#responderam em quem votaram
Data2018 <- droplevels(subset(dados2018,
                              Q12P2.B_Atitude_Turno_2 == "Abstenção" |
                                Q12P2.B_Atitude_Turno_2 == "Voto branco ou nulo" |
                                Q12P2.B_Atitude_Turno_2 == "Voto nominal"))

#Visualizo somente a variável Atitude turno 2 para ver se realmente linhas com NR/NS foram excluídas
summary(Data2018$Q12P2.B_Atitude_Turno_2)


#Estabeleço a ordem das categorias
Data2018$Q12P2.B_Atitude_Turno_2 <- factor(Data2018$Q12P2.B_Atitude_Turno_2,
                                           levels=c("Voto nominal","Abstenção","Voto branco ou nulo"))

#Visualizo somente a variável Atitude turno 2 para ver se varáveis foram reordenadas
summary(Data2018$Q12P2.B_Atitude_Turno_2)


###################################################_____ANÁLISE PERCEPÇÃO CORRUPÇÃO E VOTO OBRIGATÓRIO_____

# Passo 1: Checagem de pressupostos para análise

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
psych::pairs.panels(Data2018[16:18])#somente variáveis confiança

#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)

VIF_perc <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~ P12_Percepcao_Corrupcao_Problema_Serio +
                 Q7_Percepcao_Corrupcao_Generalizada + P24_Voto_Obrigatorio,
               data = Data2018)

car::vif(VIF_perc)

#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.

# Modelo com todas as alternativas
modi_ai_perc <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P12_Percepcao_Corrupcao_Problema_Serio +
                                 Q7_Percepcao_Corrupcao_Generalizada + P24_Voto_Obrigatorio,
                               data = Data2018,
                               shape = "wide",
                               reflevel = "Voto nominal")

# Modelo excluindo voto branco ou nulo
modi_ai_perc2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P12_Percepcao_Corrupcao_Problema_Serio +
                                  Q7_Percepcao_Corrupcao_Generalizada + P24_Voto_Obrigatorio,
                                data = Data2018,
                                shape = "wide",
                                reflevel = "Voto nominal",
                                alt.subset = c("Abstenção", "Voto nominal"))

# Modelo excluindo abstenção
modi_ai_perc3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P12_Percepcao_Corrupcao_Problema_Serio +
                                  Q7_Percepcao_Corrupcao_Generalizada + P24_Voto_Obrigatorio,
                                data = Data2018,
                                shape = "wide",
                                reflevel = "Voto nominal",
                                alt.subset = c("Voto nominal", "Voto branco ou nulo"))


# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai_perc, modi_ai_perc2)
mlogit::hmftest(modi_ai_perc, modi_ai_perc3)


# 5. Construção de modelos de regressão multinomial para confiança
modelo_perc <- multinom(Q12P2.B_Atitude_Turno_2 ~ P12_Percepcao_Corrupcao_Problema_Serio +
                         Q7_Percepcao_Corrupcao_Generalizada + P24_Voto_Obrigatorio,
                     data = Data2018, model = TRUE)

# Modelo nulo sem nenhum previsor, para identificar se o modelo aplocado é melhor que o modelo nulo
modelo_perc0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)


# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova.perc <- anova(modelo_perc0, modelo_perc)

gtsummary::tbl_summary(Anova.conf)


# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
DescTools::PseudoR2(modelo_perc, which = "Nagelkerke")

# Para verificar efeitos globais
car::Anova(modelo_perc, type = "II", test = "Nald")

#Obtenção dos valores de p - por Wald (mesmo tipo do spss)
lmtest::coeftest(modelo_perc)

#Para verificar coeficientes do modelo
summary(modelo_perc)

#Calcula razão de chance
exp(coef(modelo_perc))

#Calcula intervalo de confiança para coeficientes
exp(confint(modelo_perc))

#Tabela completa
Tab_Mult_perc <- gtsummary::tbl_regression(modelo_perc, exponentiate = TRUE)

#gráfico:
sjPlot::plot_model(modelo_perc)
sjPlot::plot_model(modelo_perc,
                   title = "Atitude no segundo turno",
                   show.legend = F,
                   axis.labels = list
                   ("Voto obrigatório)",
                     "Percepção corrupção generalizada",
                     "Percepção corrupção problema sério"))

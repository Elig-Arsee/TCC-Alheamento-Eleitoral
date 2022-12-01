###################################################_____ANÁLISE CONFIANÇA_____

# Passo 1: Checagem de pressupostos para análise Confiança

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
psych::pairs.panels(Data2018[14:17])#somente variáveis confiança

#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)

VIF_conf <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~ P5_Confianca_Eleicoes +
                 P4.4_Confianca_Governo_Federal + P4.7_Confianca_Partidos_Politicos +
                 P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
               data = Data2018)

car::vif(VIF_conf)

#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.

# Modelo com todas as alternativas
modi_ai_conf <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P5_Confianca_Eleicoes +
                                 P4.4_Confianca_Governo_Federal +
                                 P4.7_Confianca_Partidos_Politicos +
                                 P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                               data = Data2018,
                               shape = "wide",
                               reflevel = "Voto nominal")

# Modelo excluindo voto em branco
modi_ai_conf2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P5_Confianca_Eleicoes +
                                  P4.4_Confianca_Governo_Federal +
                                  P4.7_Confianca_Partidos_Politicos +
                                  P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                                data = Data2018,
                                shape = "wide",
                                reflevel = "Voto nominal",
                                alt.subset = c("Abstenção", "Voto nominal", "Voto nulo"))

# Modelo excluindo voto nulo
modi_ai_conf3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P5_Confianca_Eleicoes +
                                  P4.4_Confianca_Governo_Federal +
                                  P4.7_Confianca_Partidos_Politicos +
                                  P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                                data = Data2018,
                                shape = "wide",
                                reflevel = "Voto nominal",
                                alt.subset = c("Abstenção", "Voto nominal", "Voto em branco"))

# Modelo excluindo abtenção
modi_ai_conf4 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | P5_Confianca_Eleicoes +
                                  P4.4_Confianca_Governo_Federal +
                                  P4.7_Confianca_Partidos_Politicos +
                                  P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                                data = Data2018,
                                shape = "wide",
                                reflevel = "Voto nominal",
                                alt.subset = c("Voto nominal", "Voto anulado", "Voto em branco"))

# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai_conf, modi_ai_conf2)
mlogit::hmftest(modi_ai_conf, modi_ai_conf3)
mlogit::hmftest(modi_ai_conf, modi_ai_conf4)

# 5. Construção de modelos de regressão multinomial para confiança
mod_conf <- multinom(Q12P2.B_Atitude_Turno_2 ~ P5_Confianca_Eleicoes +
                       P4.4_Confianca_Governo_Federal +
                       P4.7_Confianca_Partidos_Politicos +
                       P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                     data = Data2018, model = TRUE)

# Modelo nulo sem nenhum previsor, para identificar se o modelo aplocado é melhor que o modelo nulo
mod_conf0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)


# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova.conf <- anova(mod_conf0, mod_conf)

gtsummary::tbl_summary(Anova.conf)


# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
DescTools::PseudoR2(mod_conf, which = "Nagelkerke")

# Para verificar efeitos globais
car::Anova(mod_conf, type = "II", test = "Nald")

#Obtenção dos valores de p - por Wald (mesmo tipo do spss)
lmtest::coeftest(mod_conf)

#Para verificar coeficientes do modelo
summary(mod_conf)

#Calcula razão de chance
exp(coef(mod_conf))

#Calcula intervalo de confiança para coeficientes
exp(confint(mod_conf))

#Tabela completa
Tab_Mult_Conf <- gtsummary::tbl_regression(mod_conf, exponentiate = TRUE)

#gráfico:

sjPlot::plot_model(mod_conf)

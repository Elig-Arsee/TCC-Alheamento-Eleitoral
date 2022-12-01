

###################################################_____ANÁLISE SOCIODEMOGRÁFICA_____
# Passo 1: Checagem de pressupostos para análise Satisfação

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
psych::pairs.panels(Data2018[1:9])

#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)

VIF_socio <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~ D2_Sexo + D1A_Faixa_Idade + D3_Escolaridade +
                  Localidade + Regiao + D12a_Cor_Raca_IBGE + D10_Religiao + D07_situacao.profissional + 
                  D9B_FAIXA_RENDA,
              data = Data2018)

car::vif(VIF_socio)

#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção ou NR/NS),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.
library(mlogit)

# Modelo com todas as alternativas
modi_ai_socio <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | D2_Sexo + D1A_Faixa_Idade + D3_Escolaridade +
                            Localidade + Regiao + D12a_Cor_Raca_IBGE + D10_Religiao +
                            D07_situacao.profissional + D9B_FAIXA_RENDA,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal")#Categoria de referência

# Modelo excluindo voto em branco
modi_ai_socio2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | D2_Sexo + D1A_Faixa_Idade + D3_Escolaridade +
                                   Localidade + Regiao + D12a_Cor_Raca_IBGE + D10_Religiao +
                                   D07_situacao.profissional + D9B_FAIXA_RENDA,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Abstenção", "Voto nominal", "Voto anulado"))

# Modelo excluindo voto nulo
modi_ai_socio3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | D2_Sexo + D1A_Faixa_Idade + D3_Escolaridade +
                                   Localidade + Regiao + D12a_Cor_Raca_IBGE + D10_Religiao +
                                   D07_situacao.profissional + D9B_FAIXA_RENDA,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Abstenção", "Voto nominal", "Voto em branco"))

# Modelo excluindo abstenção
modi_ai_socio4 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 | D2_Sexo + D1A_Faixa_Idade + D3_Escolaridade +
                                   Localidade + Regiao + D12a_Cor_Raca_IBGE + D10_Religiao +
                                   D07_situacao.profissional + D9B_FAIXA_RENDA,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Voto nominal", "Voto anulado", "Voto em branco"))

# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai_socio, modi_ai_socio2)
mlogit::hmftest(modi_ai_socio, modi_ai_socio3)
mlogit::hmftest(modi_ai_socio, modi_ai_socio4)


# 5. Construção de modelos de regressão multinomial para variáveis sociodemográficas

#mod_socio <- multinom(Q12P2.B_Atitude_Turno_2 ~ D2_Sexo + as.numeric(D3_Escolaridade) +
                        #as.numeric(D9B_FAIXA_RENDA) + D12a_Cor_Raca_IBGE +
                        #as.numeric(D1A_Faixa_Idade) + D10_Religiao,#
                      
mod_socio <- multinom(Q12P2.B_Atitude_Turno_2 ~ D2_Sexo + as.numeric(D1A_Faixa_Idade) +
                        as.numeric(D3_Escolaridade) + Localidade + Regiao +
                        D12a_Cor_Raca_IBGE + D10_Religiao + 
                        D07_situacao.profissional + as.numeric(D9B_FAIXA_RENDA),
                      data = Data2018, model = TRUE)

# Modelo nulo sem nenhum previsor
mod_socio0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)


# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova_socio <- anova(mod_socio0, mod_socio)

gtsummary::tbl_summary(Anova_socio)


# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
DescTools::PseudoR2(mod_socio, which = "Nagelkerke")


# Obtenção dos valores de p - por Wald (mesmo tipo do spss)
 lmtest::coeftest(mod_socio)


# Para verificar efeitos globais
car::Anova(mod_socio, type = "II", test = "Nald")

# Para verificar coeficientes do modelo
summary(mod_socio)

# Tabela completa
gtsummary::tbl_regression(mod_socio, exponentiate = TRUE)

#Gráfico
sjPlot::plot_model(mod_socio)

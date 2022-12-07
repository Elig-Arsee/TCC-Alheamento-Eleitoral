
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



###################################################_____ANÁLISE SOCIODEMOGRÁFICA_____
# Passo 1: Checagem de pressupostos para análise Satisfação

#1.Variavel dependente nominal com categorias mutamente exclusivas - ok

#2. Independência das observações (sem medidas repetidas): ok

#3. Ausência de multicolinearidade (alta correlação entre variáveis independentes do modelo)

#3.1 Checagem de correlação entre variáveis independentes
psych::pairs.panels(Data2018[1:7])

#3.2 Checagem de Vif (Independence of Irrelevant Alternatives).
# Fator de Inflação de Variância verifica correlação entre variáveis independentes.
# Quando vif está acima de 10 tem problema nos pressupostos.
# Construção de modelo futuro: voto foi transformado em variável numérica,
# mas isso não influencia pois o teste é realizado somente para avaliar correlação entre variáveis independentes)

VIF_socio <- lm(as.numeric(Q12P2.B_Atitude_Turno_2)~
                  Regiao +
                  D2_Sexo +
                  D1A_Faixa_Idade +
                  D3_Escolaridade +
                  D10_Religiao +
                  + D12a_Cor_Raca_IBGE + 
                  D9B_FAIXA_RENDA,
              data = Data2018)

car::vif(VIF_socio)

#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, abstenção ou NR/NS),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.
library(mlogit)

# Modelo com todas as alternativas
modi_ai_socio <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                                  Regiao +
                                  D2_Sexo +
                                  D1A_Faixa_Idade +
                                  D3_Escolaridade +
                                  D10_Religiao +
                                  D12a_Cor_Raca_IBGE + 
                                  D9B_FAIXA_RENDA,
                          data = Data2018,
                          shape = "wide",
                          reflevel = "Voto nominal")#Categoria de referência

# Modelo excluindo voto branco ou nulo
modi_ai_socio2 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                                   Regiao +
                                   D2_Sexo +
                                   D1A_Faixa_Idade +
                                   D3_Escolaridade +
                                   D10_Religiao +
                                   D12a_Cor_Raca_IBGE + 
                                   D9B_FAIXA_RENDA,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Abstenção", "Voto nominal"))


# Modelo excluindo abstenção
modi_ai_socio3 <- mlogit::mlogit(Q12P2.B_Atitude_Turno_2 ~ 1 |
                                   Regiao +
                                   D2_Sexo +
                                   D1A_Faixa_Idade +
                                   D3_Escolaridade +
                                   D10_Religiao +
                                   D12a_Cor_Raca_IBGE + 
                                   D9B_FAIXA_RENDA,
                           data = Data2018,
                           shape = "wide",
                           reflevel = "Voto nominal",
                           alt.subset = c("Voto nominal", "Voto branco ou nulo"))

# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai_socio, modi_ai_socio2)
mlogit::hmftest(modi_ai_socio, modi_ai_socio3)


# 5. Construção de modelos de regressão multinomial para variáveis sociodemográficas

#mod_socio <- multinom(Q12P2.B_Atitude_Turno_2 ~ D2_Sexo + as.numeric(D3_Escolaridade) +
                        #as.numeric(D9B_FAIXA_RENDA) + D12a_Cor_Raca_IBGE +
                        #as.numeric(D1A_Faixa_Idade) + D10_Religiao,#
                      
mod_socio <- multinom(Q12P2.B_Atitude_Turno_2 ~
                        Regiao +
                        as.numeric(D1A_Faixa_Idade) +
                        as.numeric(D3_Escolaridade) + 
                        D10_Religiao +
                        D12a_Cor_Raca_IBGE +  
                        as.numeric(D9B_FAIXA_RENDA),
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
sjPlot::plot_model(mod_socio,
                   title = "Atitude no segundo turno",
                   show.legend = F,
                   axis.labels = list
                   ("Faixa de Renda",
                     "Cor/Raça [NS/NR]",
                     "Cor/Raça [Não Branco]",
                     "Religião [Ns/NR]",
                     "Religião [Não tem Religião]",
                     "Religião [Outras]",
                     "Religião [Evangélica]",
                     "Escolaridade",
                     "Faixa de Idade",
                     "Região [Sul]",
                     "Região [Norte]",
                     "Região [Nordeste]",
                     "Região [Centro-Oeste]"))

ggsave("graf13.png", width = 6, height = 4.5, dpi = 400)
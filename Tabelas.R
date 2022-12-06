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
                                            "Fundamental completo",
                                            "Médio completo",
                                            "Universitário completo",))

table(Data2018$D3_Escolaridade)
Data2018$D3_Escolaridade <- relevel(Data2018$D3_Escolaridade,
                                    ref = "Médio completo")

#Renda
levels(Data2018$D9B_FAIXA_RENDA)
Data2018$D9B_FAIXA_RENDA <- factor(Data2018$D9B_FAIXA_RENDA,
                                   levels=c("1 salário mínimo",
                                            "1 até 2 salários mínimos",
                                            "2 até 5 salários mínimos",
                                            "5 até 10 salários mínimos",
                                            "10 até 15 salários mínimos",
                                            "15 até 20 salários mínimos",
                                            "Mais de 20 salários mínimos",
                                            "NS/NR"))

table(Data2018$D9B_FAIXA_RENDA)
Data2018$D9B_FAIXA_RENDA <- relevel(Data2018$D9B_FAIXA_RENDA,
                                    ref = "2 até 5 salários mínimos")

#Cor/Raça
levels(Data2018$D12a_Cor_Raca_IBGE)
Data2018$D12a_Cor_Raca_IBGE <- factor(Data2018$D12a_Cor_Raca_IBGE,
                                      levels=c("Branco", "Não branco", "NS/NR"))

table(Data2018$D12a_Cor_Raca_IBGE)
Data2018$D12a_Cor_Raca_IBGE <- relevel(Data2018$D12a_Cor_Raca_IBGE,
                                       ref = "Não branco")


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
                                         "Não tem religião", "NS/NR"))

Data2018$D10_Religiao <- relevel(Data2018$D10_Religiao,
                                 ref = "Católica")

#Crio tabela de resumo com distribuição percentual dos entrevistados segundo variáveis socioeconômicas
Tabela_socio <- Data2018%>%
  tbl_summary(
    by = Q12P2.B_Atitude_Turno_2,
    statistic = list(all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    label = list(Regiao ~ "Região",
                 D2_Sexo ~ "Sexo",
                 D1A_Faixa_Idade ~ "Idade",
                 D3_Escolaridade ~ "Escolaridade",
                 D10_Religiao ~ "Religião",
                 D12a_Cor_Raca_IBGE ~ "IBGE:Cor/Raça",
                 D9B_FAIXA_RENDA ~ "Faixa de Renda"),
    percent = "row")%>%
  bold_labels()%>%
  add_overall(statistic = list(all_categorical() ~ "{p}% ({n})"),
              last = T)%>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~"**Atitude no segundo turno da eleição**") %>%
  modify_caption("**Tabela 1: Distribuição percentual dos entrevistados,
  por atitude no segundo turno da eleição de 2018, segundo variáveis socioeconômicas**")

#Verifico nomes das colunas da tabela
show_header_names(Tabela_socio)

#Altero nomes das colunas da tabela
Tabela_socio%>%
  modify_header(update = list(label ~ "**Variáveis**",
                              stat_1 ~ "**Voto nominal**, N = 1734",
                              stat_0 ~ "**Total**, N = 2413"))

#Converto tabela para formato word
install.packages("flextable") #instalo pacote
library(flextable) #chamo biblioteca

Tabela_socio <- as_flex_table(Tabela_socio)#converto em word

save_as_docx(Tabela_socio, path = "C:\Users\arsee\Desktop\TCC2022\Tabela_Socio.docx")
write.table(Tabela_socio, file = “Tabela_Socio.docx”, sep = “,”)#salvo na pasta

Tabela_Socio <- flextable()

#Produção de tabelas com % e nº de casos de cada categoria das variáveis
Tabela_2 <- Data2018%>%
  tbl_summary(statistic = list(all_categorical() ~ "{p}% ({n})"),
              digits = all_continuous() ~ 3,
              label = list(Regiao ~ "Região",
                           D2_Sexo ~ "Sexo",
                           D1A_Faixa_Idade ~ "Idade",
                           D3_Escolaridade ~ "Escolaridade",
                           D10_Religiao ~ "Religião",
                           D12a_Cor_Raca_IBGE ~ "IBGE:Cor/Raça",
                           D9B_FAIXA_RENDA ~ "Faixa de Renda"),
              percent = "col")%>%
  bold_labels()%>%
  modify_caption("**Tabela 2: % das categorias relativas a amostra**")


summary((Data2018$P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
         Data2018$Q12P2.B_Atitude_Turno_2))
prop.table(table(Data2018$P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados,
                 Data2018$Q12P2.B_Atitude_Turno_2))*100

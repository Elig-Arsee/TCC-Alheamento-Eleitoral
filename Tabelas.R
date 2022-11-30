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

#Crio tabela de resumo com distribuição percentual dos entrevistados segundo variáveis socioeconômicas
Tabela_socio <- Data2018%>%
  tbl_summary(
    by = Q12P2.B_Atitude_Turno_2,
    statistic = list(all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    label = list(D2_Sexo ~ "Sexo",
                 D1A_Faixa_Idade ~ "Idade",
                 D3_Escolaridade ~ "Escolaridade",
                 Localidade ~ "Localidade",
                 Regiao ~ "Região",
                 D12a_Cor_Raca_IBGE ~ "IBGE:Cor/Raça",
                 D07_situacao.profissional ~ "Situação Profissional",
                 D9B_FAIXA_RENDA ~ "Faixa de Renda", 
                 D10_Religiao ~ "Religião"),
    percent = "row")%>%
  bold_labels()%>%
  add_overall(statistic = list(all_categorical() ~ "{p}% ({n})"),
              last = T)%>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~"**Atitude no segundo turno da eleição**") %>%
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
              label = list(D2_Sexo ~ "Sexo",
                           D1A_Faixa_Idade ~ "Idade",
                           D3_Escolaridade ~ "Escolaridade",
                           Localidade ~ "Localidade",
                           Regiao ~ "Região",
                           D12a_Cor_Raca_IBGE ~ "IBGE:Cor/Raça",
                           D07_situacao.profissional ~ "Situação Profissional",
                           D9B_FAIXA_RENDA ~ "Faixa de Renda", 
                           D10_Religiao ~ "Religião"),
              percent = "col")%>%
  bold_labels()%>%
  modify_caption("**Tabela 2: % das categorias relativas a amostra**")


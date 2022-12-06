
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



#Crio esquema de cores
paleta <- c("#00798c", "#d1495b", "#edae49", "#66a182", "#2e4057")

#Gráfico de barras
G0 <- ggplot(Data2018) +
  aes(x = Q12P2.B_Atitude_Turno_2,
      fill = Q12P2.B_Atitude_Turno_2) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  labs(x = "Atitude segundo turno",y = "Ocorrências",
    title = "Distribuição da amostra segundo atitude no segundo turno",
    fill = "Atitude no segundo turno") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

G0 <- G0 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto branco ou nulo"))

G0 <- G0 + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

G0



#Gráfico violino para satisfação

G1 <- ggplot(Data2018, aes(x = Q12P2.B_Atitude_Turno_2,
                     y = Q11_Satisfacao_Democracia)) +
  geom_violin(aes(fill = Q12P2.B_Atitude_Turno_2)) +
  geom_boxplot_jitter(outlier.alpha = 0, fill = "white", width = 0.3) +
  labs(title = "Satisfação com a democracia",
       x = "Atitude no segundo turno",
       y = "Satisfação com a democracia",
       fill = "Atitude no segundo turno",
       caption = "1 - Satisfeito;
       2 - Nem satisfeiro, nem insatisfeito;
       3 - Insatisfeito;
       4 - NS/NR") +
  theme_classic() +
  theme(legend.position = "bottom", plot.caption = element_text(size = 11L))


G1 <- G1 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto branco ou nulo"))
G1


#Gráfico violino plot com três fatores comparando notas de Satisfação
#Primeiro gera novo df chamado G_sat.

G_sat <- Data2018 %>%
  select(9:11, 20) %>% 
  tidyr::pivot_longer(cols = 1:3, names_to = "Instituição avaliada",
                      values_to = "Nota") %>% 
  mutate(`Segmento avaliado` = factor(`Instituição avaliada`,
                                      levels = c("P3.4_Avaliacao_Governo_Federal",
                                                 "P3.7_Avaliacao_Partidos_Politicos",
                                                 "P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados"),
                                      labels = c("Governo Federal",
                                                 "Partidos Políticos",
                                                 "Congresso Nacional"))) %>% 
  rename(`Atitude no segundo turno` = Q12P2.B_Atitude_Turno_2)

pacman::p_load(ggrastr)


G2 <- ggplot(G_sat, aes(x = `Segmento avaliado`,
                        y = Nota,
                        fill = `Atitude no segundo turno`)) +
  geom_violin(aes(fill = `Atitude no segundo turno`)) +
  #geom_boxplot_jitter(outlier.alpha = 2, fill = "white", width = 0.03) +
  labs(title = "Avaliação das instituições",
       fill = "Atitude no segundo turno",
       caption = "1 - Positiva;
       2 - Regular;
       3 - Negativa;
       4 - NS/NR") +
  theme_classic() +
  theme(legend.position = "bottom", plot.caption = element_text(size = 11L))

install.packages("vioplot")
library("vioplot")

V1 <- vioplot(Nota ~ `Segmento avaliado`, data = G_sat,
        col = c(paleta),
        border = NA)

V2 <- vioplot(Q11_Satisfacao_Democracia ~ Q12P2.B_Atitude_Turno_2, data = Data2018,
        col = c(paleta),
        border = NA)

?geom_boxplot_jitter

G2 <- G2 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto branco ou nulo"))
G1



#Gráfico violino para confiança
library(esquisse)

G3 <- ggplot(Data2018) + aes(x = Q12P2.B_Atitude_Turno_2,
                       y = P5_Confianca_Eleicoes,
                       fill = Q12P2.B_Atitude_Turno_2) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_manual(values = c(paleta)) +
  labs(x = "Nota",
       y = "Atitude no segundo turno",
       title = "Confiança nas Eleições",
       caption = "1 - As eleições são confiáveis; 2 - São objeto de fraude; 3 - NS/NR",
       fill = "Atitude segundo turno") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14L),
        plot.caption = element_text(size = 11L),
        axis.title.y = element_text(size = 11L,),
        axis.title.x = element_text(size = 11L))

vioplot(P5_Confianca_Eleicoes ~ Q12P2.B_Atitude_Turno_2, data = Data2018,
        col = c(paleta),
        border = NA)

#Gráfico violino com três fatores comparando notas de Confiança
G_conf <- Data2018 %>%
  select(13:15, 20) %>% 
  tidyr::pivot_longer(cols = 1:3, names_to = "Instituição avaliada",
                      values_to = "Nota") %>% 
  mutate(`Segmento avaliado` = factor(`Instituição avaliada`,
                                      levels = c("P4.4_Confianca_Governo_Federal",
                                                 "P4.7_Confianca_Partidos_Politicos",
                                                 "P4.8_Confianca_Congresso_Nacional_Senado_CamaraDeputados"),
                                      labels = c("Governo Federal",
                                                 "Partidos Políticos",
                                                 "Congresso Nacional"))) %>% 
  rename(`Atitude no segundo turno` = Q12P2.B_Atitude_Turno_2)

pacman::p_load(ggrastr)


G4 <- ggplot(G_conf, aes(x = `Segmento avaliado`,
                        y = Nota,
                        fill = `Atitude no segundo turno`)) +
  geom_violin(aes(fill = `Atitude no segundo turno`)) +
  #geom_boxplot_jitter(outlier.alpha = 0, fill = "white", width = 0.2) +
  labs(title = "Confiança nas instituições",
       fill = "Atitude no segundo turno",
       caption = "1 - Muita confiança;
       2 - Alguma confiança;
       3 - Pouca confiança;
       4 - Nenhuma confiança,
       5 - NS/NR",) +
  theme_classic() +
  theme(legend.position = "bottom", plot.caption = element_text(size = 11L))

G4 <- G4 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto branco ou nulo"))
G4

#Gráfico para percepção corrupção problema sério
library(esquisse)

G5 <- ggplot(Data2018) + aes(x = Q12P2.B_Atitude_Turno_2,
                             y = P12_Percepcao_Corrupcao_Problema_Serio,
                             fill = Q12P2.B_Atitude_Turno_2) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_manual(values = c(paleta)) +
  labs(x = "Nota",
       y = "Atitude no segundo turno",
       title = "Percepção da Corrupção",
       subtitle = "Você diria que a corrupção no Brasil é um problema muito sério, sério, pouco sério ou não 
é um problema sério?",
       caption = "1 - Muito sério;
       2 - Sério;
       3 - Pouco sério;
       4 - Não é um problema sério;
       5 - NS/NR",
       fill = "Atitude segundo turno") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14L),
        plot.caption = element_text(size = 11L),
        axis.title.y = element_text(size = 11L,),
        axis.title.x = element_text(size = 11L))

glimpse(Data2018)

#Gráfico para percepção corrupção generalizada
library(esquisse)

G6 <- ggplot(Data2018) + aes(x = Q12P2.B_Atitude_Turno_2,
                             y = Q7_Percepcao_Corrupcao_Generalizada,
                             fill = Q12P2.B_Atitude_Turno_2) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_manual(values = c(paleta)) +
  labs(x = "Nota",
       y = "Atitude no segundo turno",
       title = "Percepção da Corrupção",
       subtitle = "O quanto você acha que a corrupção está generalizada no Brasil, como por exemplo, as 
propinas entre políticos: muito generalizada, bem generalizada, pouco generalizada ou você acha que 
isso dificilmente acontece?",
       caption = "1 - Muito generalizada;
       2 - Bem genealizada;
       3 - Pouco generalizada;
       4 - Dificilmente acontece;
       5 - NS/NR",
       fill = "Atitude segundo turno") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14L),
        plot.caption = element_text(size = 11L),
        axis.title.y = element_text(size = 11L,),
        axis.title.x = element_text(size = 11L))


#Gráfico para voto obrigatório

G7 <- ggplot(Data2018) + aes(x = P24_Voto_Obrigatorio,
                       fill = Q12P2.B_Atitude_Turno_2) +
  geom_bar(bins = 30L) +
  scale_fill_hue(direction = 1) +
  labs(x = "Nota", y = "proporção",
       title = "Voto obrigatório",
       subtitle = "Nas eleições deste ano, se o voto NÃO fosse obrigatório o(a) sr(a) teria ido votar?",
    caption = "1 - Sim;
    2 - Não;
    3 - Talvez/Depende;
    4 - NS/NR",
    fill = "Atitude segundo turno") +
  theme_classic() +
  theme(plot.title = element_text(size = 14L, face = "bold"),
        plot.subtitle = element_text(size = 12L),
        plot.caption = element_text(size = 11L))

G7 <- G7 + scale_fill_manual(values = paleta,
                               breaks = c("Voto nominal", "Abstenção", "Voto branco ou nulo"))

G7 <- G7+scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


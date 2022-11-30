#Crio esquema de cores
paleta <- c("#00798c", "#d1495b", "#edae49", "#66a182", "#2e4057")

#Gráfico boxplot para satisfação
G1 <- ggplot(Data2018, aes(x = Q12P2.B_Atitude_Turno_2, y = Q21_Satisfacao_Democracia)) +
  geom_errorbar(stat = "boxplot", width = 0.3) +
  geom_boxplot_jitter(aes(fill = Q12P2.B_Atitude_Turno_2), outlier.shape = 1,
                      outlier.alpha = 0.3) +
  labs(title = "Satisfação com a democracia",
       subtitle = "2413 respondestes",
       x = "Atitude no segundo turno",
       y = "Satisfação com a democracia",
       fill = "Atitude no segundo turno") +
  theme_classic()+
  theme(legend.position = "bottom")

G1 <- G1 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto nulo", "Voto em branco"))
G1


#Gráfico box plot com três fatores comparando notas de Satisfação
G_sat <- Data2018 %>%
  select(11:13, 22) %>% 
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

G2 <- ggplot(G_sat, aes(x = `Segmento avaliado`, y = Nota,
                        fill = `Atitude no segundo turno`)) +
  geom_errorbar(stat = "boxplot",
                width = 0.3,
                position = position_dodge(width = 0.8)) +
  ggrastr::geom_boxplot_jitter(outlier.shape = 1,
                               outlier.alpha = 0.3,
                               width = 0.5,
                               position = position_dodge(width = 0.8)) +
  labs(title = "Avaliação das instituições",
       subtitle = "2413 entrevistados") +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",")) +
  scale_fill_manual(values = paleta,
                    breaks = c("Voto nominal", "Abstenção", "Voto nulo", "Voto em branco")) +
  theme_classic() +
  theme(legend.position = "bottom")

G2


#Gráfico boxplot para confiança
G3 <- ggplot(Data2018, aes(x = Q12P2.B_Atitude_Turno_2, y = P5_Confianca_Eleicoes)) +
  geom_errorbar(stat = "boxplot", width = 0.3) +
  geom_boxplot_jitter(aes(fill = Q12P2.B_Atitude_Turno_2), outlier.shape = 1,
                      outlier.alpha = 0.3) +
  labs(title = "Confiança nas eleições",
       subtitle = "2413 respondestes",
       x = "Atitude no segundo turno",
       y = "Confiança nas eleições",
       fill = "Atitude no segundo turno") +
  theme_classic()+
  theme(legend.position = "bottom")

G3 <- G3 + scale_fill_manual(values = paleta,#Aplico paleta no gráfico
                             breaks = c("Voto nominal", "Abstenção", "Voto nulo", "Voto em branco"))
G3


#Gráfico box plot com três fatores comparando notas de Confiança
G_conf <- Data2018 %>%
  select(15:17, 22) %>% 
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

G4 <- ggplot(G_conf, aes(x = `Segmento avaliado`, y = Nota,
                         fill = `Atitude no segundo turno`)) +
  geom_errorbar(stat = "boxplot",
                width = 0.3,
                position = position_dodge(width = 0.8)) +
  ggrastr::geom_boxplot_jitter(outlier.shape = 1,
                               outlier.alpha = 0.3,
                               width = 0.5,
                               position = position_dodge(width = 0.8)) +
  labs(title = "Confiança nas instituições",
       subtitle = "2413 respondestes") +
  scale_y_continuous(labels = scales::number_format(decimal.mark = ",")) +
  scale_fill_manual(values = paleta,
                    breaks = c("Voto nominal", "Abstenção", "Voto nulo", "Voto em branco")) +
  theme_classic() +
  theme(legend.position = "bottom")

G4


# Title: import and format data
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,arrow,sf,sp)

# Clean memory
rm(list=ls())
gc()

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# wd <- paste0("G:/Meu Drive/00 data/TSE/eleicoes/")
# setwd(wd)

# 1. Import databases ----------------------------------------------------------

# serie temporal

eleicoes_sp <- import("../0_data/SERIE_ELEICOES_PREF_SP.xlsx")

serie_sp <- eleicoes_sp %>% group_by(ANO) %>% summarise(
  PT = sum(votos_PT)/sum(votos_TT),
  PSDB = sum(votos_PSDB)/sum(votos_TT)
)

serie_sp <- serie_sp %>% gather(key = "sigla",value = "votos",-ANO)

serie_sp %>% 
  ggplot(aes(x=ANO,y=votos,col=sigla))+
  geom_point(aes(shape=sigla),size=2.5)+
  geom_line(size=1)+
  scale_color_manual(values=c("blue","red"))+
  scale_x_continuous(breaks=seq(2000,2020,4))+
  scale_y_continuous(breaks=seq(0,.6,.1))+
  theme_minimal()+
  labs(x="Ano",y="Proporção de votos no 1ºT",col = "Partido",shape="Partido")

# Save as pdf
pdf(file = "../2_outp/graphs/serie_sp.pdf",width = 10,height = 6)
plot(serie_sp)
dev.off()

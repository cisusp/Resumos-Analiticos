# Title: import and format data
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,survey,eph,patchwork,sf)

# Clean memory
rm(list=ls())
gc()

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

wd_data_cis <- "G:/Drives compartilhados/CIS/Acervo CIS/Bancos/Não postados/"

# 1. Import  ----------------------------------------------------------

CIS0713 <- import(paste0(wd_data_cis,
              "CIS0713 - Intenção de voto para o prefeito de São Paulo - 2024/PO4151_BD.sav"))

CIS0713 %>% head %>% view 

# Malha cartográfica SP - 5 zonas
# http://dados.prefeitura.sp.gov.br/dataset/regiao-5-divisao-do-municipio-em-cinco-regioes

sp_shp <- st_read("G:/Meu Drive/01 - data/SP/LAYER_REGIAO5_2013/DEINFO_REGIAO5_2013.shp")

# 2. Management -------------------------------------------------------

mydta <- CIS0713 %>% mutate(.,
                 
                 idad = factor(idade,
                               levels=c(1:5),
                               labels=c('16-24','25-34','35-44','45-59','60+'),
                               ordered = T
                 ),
                 
                 sex = case_when(
                   sexo == 1 ~ "M",
                   sexo == 2 ~ "F"
                 ),
                 
                 raca = case_when(
                   cor == 1 ~ "Branca",
                   cor == 2 ~ "Preta",
                   cor == 3 ~ "Parda",
                   cor == 4 ~ "Amarela",
                   cor == 5 ~ "Indígena"
                 ),
                 
                 esc = factor(escola, 
                              levels = c(1:8),
                              labels = c(
                   'EF', 'EF', 'EF', 'EF', 'EM', 'ES', 'ES', 'ES'
                 )),
                 
                 rendf = factor(rendaf,
                   levels=c(9,1:7),
                   labels=c('- R$ 1.320,00',
                            'R$ 1.321,00 - R$ 2.640,00',
                            'R$ 2.641,00 - R$ 3.960,00',
                            'R$ 3.961,00 - R$ 6.600,00',
                            'R$ 6.601,00 +',
                            'R$ 6.601,00 +',
                            'R$ 6.601,00 +',
                            'R$ 6.601,00 +')
                 ),
                 
                 rendf2 = factor(rendaf,
                                levels=c(9,1:7),
                                labels=c('até 1 SM',
                                         '1 SM - 2 SM',
                                         '2 SM - 3 SM',
                                         '3 SM - 4 SM',
                                         '4 SM +',
                                         '4 SM +',
                                         '4 SM +',
                                         '4 SM +')
                 ),
                 
                 zona = factor(zona,
                               levels=c(1:5),
                               labels=c('CO','ZN','ZS','ZL','ZO')
                 ),
                 
                 candidato = factor(estimu,
                                    levels = c(1,3,4,2,5,96,99),
                                    labels = c('Guilherme Boulos','Ricardo Nunes',
                                               'Tabata Amaral','Kim Kataguiri',
                                               'Vinicius Poit','BN','NS'),
                                    ordered=T),
                 
                 rejeicao = factor(rejeia,
                                   levels = c(1,3,4,2,5,96,99),
                                   labels = c('Guilherme Boulos','Ricardo Nunes',
                                              'Tabata Amaral','Kim Kataguiri',
                                              'Vinicius Poit','BN','NS'),
                                   ordered=T),
                 
                 prefpartido = case_when(
                   partido %in% c(1,4) ~ "PSDB/MDB",
                   partido %in% c(10,2) ~ "PT/PSOL")
                 )
                 
mydta <- mydta %>% select(idad,sex,raca,esc,rendf2,zona,candidato,rejeicao,prefpartido,PESOE)                 

# Analysis -----------------------------------------------------------

calculate_tabulates(base = mydta,x='candidato',weights = 'PESOE',add.percentage = 'col')

# Graphs ----

theme_text <- theme(text=element_text(size=16))

# Renda ----

tab_renda <- calculate_tabulates(base = mydta,x='candidato',y='rendf2',weights = 'PESOE',add.percentage = 'col')

t_renda <- tab_renda %>% gather(key = faixa_renda,value=p,-`candidato/rendf2`)

t_renda <- t_renda %>% mutate(.,
                              faixa_renda = factor(faixa_renda,
                                             levels=c('até 1 SM',
                                                      '1 SM - 2 SM',
                                                      '2 SM - 3 SM',
                                                      '3 SM - 4 SM',
                                                      '4 SM +')
                              )
                              )

t_renda$p <- ifelse(t_renda$`candidato/rendf2` == "Guilherme Boulos",-t_renda$p,t_renda$p)

plt_renda <- t_renda %>% filter(`candidato/rendf2` %in% c("Guilherme Boulos","Ricardo Nunes")) %>% 
  ggplot(aes(x=faixa_renda,y=p,fill=`candidato/rendf2`))+
  geom_bar(stat='identity',position = "identity")+
  scale_y_continuous(limits = c(-50,50))+
  scale_fill_manual(values=c("#A03232","steelblue"))+
  #facet_wrap(~`candidato/rendf`)+
  theme_minimal()+
  labs(y="(%)",x="",fill="Candidato")+
  theme(legend.position = "bottom")+
  coord_flip()+
  theme_text

# Escolaridade ----

tab_esc <- calculate_tabulates(base = mydta,x='candidato',y='esc',weights = 'PESOE',add.percentage = 'col')

t_esc <- tab_esc %>% gather(key = faixa_esc,value=p,-`candidato/esc`)

t_esc <- t_esc %>% mutate(.,
                          esc = factor(faixa_esc, levels = c(
                            'EF','EM','ES'
                          ))
                              )

t_esc$p <- ifelse(t_esc$`candidato/esc` == "Guilherme Boulos",-t_esc$p,t_esc$p)

plt_esc <- t_esc %>% filter(`candidato/esc` %in% c("Guilherme Boulos","Ricardo Nunes")) %>% 
  ggplot(aes(x=faixa_esc,y=p,fill=`candidato/esc`))+
  geom_bar(stat='identity',position = "identity")+
  scale_y_continuous(limits = c(-50,50))+
  scale_fill_manual(values=c("#A03232","steelblue"))+
  #facet_wrap(~`candidato/esc`)+
  theme_minimal()+
  labs(y="(%)",x="",fill="Candidato")+
  theme(legend.position = "bottom")+
  coord_flip()+
  theme_text+
  theme(legend.position = "none")

plt_renda + plt_esc + plot_annotation(tag_levels = "A")

# Zona ----

tab_zona <- calculate_tabulates(base = mydta,x='candidato',y='zona',weights = 'PESOE',add.percentage = 'col')

t_zona <- tab_zona %>% gather(key = zona,value=p,-`candidato/zona`)

t_zona <- t_zona %>% mutate(.,
                          zona = factor(zona, 
                                        levels=c('CO','ZN','ZS','ZL','ZO'),
                                        labels=c('Centro',"Norte","Sul","Leste",'Oeste')
                          ))

t_zona <- t_zona %>% 
  filter(`candidato/zona` %in% c("Guilherme Boulos","Ricardo Nunes")) %>% 
  rename(NOME=zona)

map_zona <- merge(sp_shp,t_zona)

# We have to remove axis from the ggplot layers
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 axis.line = element_blank(),
                 panel.background = element_rect(fill = "white"),
                 text=element_text(size=16)
)

centroids <- st_centroid(map_zona)
map_zona$centroid <- centroids$geometry

map_zona[,c('X','Y')] <- st_coordinates(map_zona$centroid)

map_zona %>% 
  ggplot(aes(fill=p))+
  geom_sf()+
  scale_fill_gradient2(low = "white", high = "#A03232", midpoint = 15) +
  geom_text(x=map_zona$X,y=map_zona$Y,aes(label = round(p)), size = 3, color = "black") +
  facet_wrap(~`candidato/zona`)+
  theme_minimal()+
  labs(fill="Votos")+
  no_axis

# 2. Export --
export(mydt,"../0_data/SERIE_ELEICOES_PREF_SP.xlsx")

st_write(mydt,"../0_data/SERIE_ELEICOES_PREF_SP.shp")

# Title: import and format data
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,arrow,sf,sp,patchwork)

# Clean memory
rm(list=ls())
gc()

# Working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# wd <- paste0("G:/Meu Drive/00 data/TSE/eleicoes/")
# setwd(wd)

# Import -----------------------------------------------------------------------

mydt <- load("../0_data/SERIE_ELEICOES_PREF_SP.RDS")

# We have to remove axis from the ggplot layers
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 axis.line = element_blank(),
                 panel.background = element_rect(fill = "white") 
)

mydt <- st_as_sf(mydt)

ano = seq(2000,2020,4)

mapas_PT <- list()
mapas_PSDB <- list()

for( i in 1:length(ano)){
  
  mapas_PT[[i]] <- mydt %>% filter(ANO == ano[i]) %>% mutate(votos = vt_PT/vt_TT) %>% 
    ggplot(aes())+
    geom_sf(aes(fill=votos))+
    scale_fill_gradient(low = "white", high = "red", limits = c(0, .7))+
    labs(title=ano[i],fill="Prop. votos \n PT")+
    no_axis
  
  if(i < length(ano)){
    mapas_PT[[i]] <- mapas_PT[[i]]+
      theme(legend.position = "none")
  }
  
  mapas_PSDB[[i]] <- mydt %>% filter(ANO == ano[i]) %>% mutate(votos = vt_PSDB/vt_TT) %>% 
    ggplot(aes())+
    geom_sf(aes(fill=votos))+
    scale_fill_gradient(low = "white", high = "red", limits = c(0, .7))+
    labs(title=ano[i],fill="Prop. votos \n PT")+
    no_axis
  
  if(i < length(ano)){
    mapas_PSDB[[i]] <- mapas_PSDB[[i]]+
      theme(legend.position = "none")
  }
}

map1 <- mapas_PT[[1]] + mapas_PT[[4]] + mapas_PT[6]

# Save as pdf
pdf(file = "../2_outp/graphs/mapa_PT.pdf",width = 10,height = 6)
plot(map1)
dev.off()

map2 <- mapas_PSDB[[1]] + mapas_PSDB[[4]] + mapas_PSDB[6]

# Save as pdf
pdf(file = "../2_outp/graphs/mapa_PSDB.pdf",width = 10,height = 6)
plot(map2)
dev.off()






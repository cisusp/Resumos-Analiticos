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

# 1. Import  ----------------------------------------------------------

ano = seq(2000,2020,4)
md = list()
i=1

for(i in 1:length(ano)){ 
  print(ano[i])
  
  # unzip files  
  # unzip(zipfile = paste0("G:/Meu Drive/00 data/TSE/eleicoes/",ano[i],"/EL",ano[i],"_ZE_RMSP_CEM.zip"),
  #       exdir = paste0(ano[i],"/EL",ano[i],"_ZE_RMSP_CEM/"))
  
  sp_shp <- st_read(paste0("../../",ano[i],"/EL",ano[i],"_ZE_RMSP_CEM/EL",ano[i],"_ZE_RMSP_CEM.shp"))
  sp_shp <- sp_shp %>% filter(CD_MUN == 3550308)
  
  # votos_total_1t <- sp_shp %>% as.data.frame() %>% select(11:37)
  # sp_shp$total_votos_1t <- rowSums(votos_total_1t,na.rm=T)
  
  sp_shp <- sp_shp %>% rename(NR_ZONA = ZE_NUM) %>% select(NR_ZONA,geometry)
  
  # dados votos
  el_pref_sp <- read_parquet(paste0("../../",ano[i],"/ELEICOES_PREF_SP_CAPITAL.parquet"))

  
votos_TT <- el_pref_sp %>% filter(NR_TURNO == 1) %>% group_by(NR_ZONA) %>% 
  summarise(
  votos_TT = sum(QT_VOTOS,na.rm=T)
)

# para votos validos, excluir brancos e nulos com filtro por "!= c(95,96)"
votos_TTV <- el_pref_sp %>% filter(NR_TURNO == 1, NR_VOTAVEL < 95) %>% group_by(NR_ZONA) %>% 
  summarise(
    votos_TTV = sum(QT_VOTOS,na.rm=T)
  )

votos_PT <- el_pref_sp %>% filter(NR_TURNO == 1, NR_VOTAVEL == 13) %>% group_by(NR_ZONA) %>% summarise(
  NM_VOTAVEL_PT = unique(NM_VOTAVEL),
  votos_PT = sum(QT_VOTOS,na.rm=T)
)

votos_PSDB <- el_pref_sp %>% filter(NR_TURNO == 1, NR_VOTAVEL == 45) %>% group_by(NR_ZONA) %>% summarise(
  NM_VOTAVEL_PSDB = unique(NM_VOTAVEL),
  votos_PSDB = sum(QT_VOTOS,na.rm=T)
)

votos_el_pref_sp <- merge(votos_TT,votos_TTV)
votos_el_pref_sp <- merge(votos_el_pref_sp,votos_PT)
votos_el_pref_sp <- merge(votos_el_pref_sp,votos_PSDB)

md[[i]] = merge(sp_shp,votos_el_pref_sp,by="NR_ZONA")
md[[i]][,"ANO"] <- ano[i]

md[[i]] <- relocate(.data = md[[i]],ANO,.after = NR_ZONA)
}

for ( i in 1:5 ){

if(i == 1){
mydt <- bind_rows(md[[i]],md[[i+1]])}
  
  if(i > 1){
    mydt <- bind_rows(mydt,md[[i+1]])}
}

# 2. Export ----
export(mydt,"../0_data/SERIE_ELEICOES_PREF_SP.xlsx")

st_write(mydt,"../0_data/SERIE_ELEICOES_PREF_SP.shp")

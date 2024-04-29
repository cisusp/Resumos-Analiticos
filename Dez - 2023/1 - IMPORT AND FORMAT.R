# Title: import and format data
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,geobr,sf,censobr,patchwork,car)

# Clean memory
rm(list=ls())
gc()

# Minhas funções
processQuantiles <- function(data, col,q, output_col) {
  quantiles <- quantile(data[[col]], probs = seq(0, 1, q), na.rm = TRUE)
  
  for (i in 1:length(quantiles)) {
    if(i == 1){
      condition <- data[[col]] >= quantiles[i] & data[[col]] <= quantiles[i + 1]
    }else{
    condition <- data[[col]] > quantiles[i] & data[[col]] <= quantiles[i + 1]
    }
    
    condition[is.na(condition)] <- FALSE
    data[condition, output_col] <- paste0("Q",i)
  }
  return(data)
}

# 1. Import databases ----------------------------------------------------------

## NSE Soares e Alves 2023 ----
nse2023 <- import("../../Acervo CIS/Bancos/Não postados/CIS705   UMA MEDIDA DO NÍVEL SOCIOECONÔMICO DAS ESCOLAS BRASILEIRAS UTILIZANDO  INDICADORES PRIMÁRIOS E SECUNDÁRIOS1/NSE_ESCOLAS.sav")

## Censo Escolar ----
ceb2021 <- import("G:/Meu Drive/00 data/EDUC/CENSO EB/2021/dados/microdados_ed_basica_2021.csv")

## Indicadores educacionais ----
iee2021 <- import('G:/Meu Drive/00 trab/00 artigo/pesquisa_estrutura_escola/0_data/IEE_2021.csv')

# Indicador Adequação Docente
iad2021 <- import("G:/Meu Drive/00 data/EDUC/IND EB/IAD/2013/AFD_ESCOLAS_2013.xlsx")
colnames(iad2021) <- iad2021[10,]
iad2021 <- iad2021[-c(1:10),]
iad2021 <- iad2021 %>% na.omit(PK_COD_ENTIDADE)

# Indicador Complex Gestão
icg2021 <- import("G:/Meu Drive/00 data/EDUC/IND EB/ICG/2021/ICG_ESCOLAS_2021.xlsx")
colnames(icg2021) <- icg2021[10,]
icg2021 <- icg2021[-c(1:10),]
icg2021 <- icg2021 %>% na.omit(PK_COD_ENTIDADE)

# Indicador Desenvolvimento Educação
ideb2021 <- import("G:/Meu Drive/00 data/EDUC/IND EB/IDEB/EFII/divulgacao_anos_finais_escolas_2021.xlsx")
colnames(ideb2021) <- ideb2021[9,]
ideb2021 <- ideb2021[-c(1:9),]
ideb2021 <- ideb2021 %>% na.omit(ID_ESCOLA)

# Geobr schools
schools <- read_schools(year = 2020)

# 2. Data management -----------------------------------------------------------

# Organizando indicadores

icg <- icg2021 %>% select(CO_ENTIDADE,COMPLEX) %>% 
  rename('ID_ESCOLA'=CO_ENTIDADE,'ICG'=COMPLEX)

iad <- iad2021 %>% select(PK_COD_ENTIDADE,AFD_F581) %>% 
  rename('ID_ESCOLA'=PK_COD_ENTIDADE,'IAD'=AFD_F581)

iee <- iee2021 %>% select(ID_ESCOLA,IEE)

idb <- ideb2021 %>% select(ID_ESCOLA,VL_OBSERVADO_2021) %>% 
  rename('IDEB'=VL_OBSERVADO_2021)

# Limpando CEB

ceb <- ceb2021 %>% filter(TP_SITUACAO_FUNCIONAMENTO == 1,IN_BAS == 1) %>%
  rename("ID_ESCOLA" = CO_ENTIDADE) %>% select(ID_ESCOLA,TP_LOCALIZACAO,
                                               IN_PROF,IN_PROF_TEC,
                                               TP_DEPENDENCIA,
                                               IN_MANT_ESCOLA_PRIVADA_S_FINS,
                                               QT_MAT_BAS)

# Selecionando variavel NSE padronizado
nse <- nse2023 %>% rename("ID_ESCOLA"=CO_ENTIDADE) %>% select(ID_ESCOLA,NSE10)

# Selecionando localização e ID escolas geobr
geo_esc <- schools %>% rename("ID_ESCOLA"=code_school) %>% 
  select(ID_ESCOLA,geom)

# Unindo bases
mydata <- merge(nse,ceb,by = "ID_ESCOLA",all.x = T)
mydata <- merge(mydata,geo_esc,by = "ID_ESCOLA",all.x = T)

mydata <- merge(mydata,icg,by = "ID_ESCOLA",all.x = T)
mydata <- merge(mydata,iad,by = "ID_ESCOLA",all.x = T)
mydata <- merge(mydata,iee,by = "ID_ESCOLA",all.x = T)
mydata <- merge(mydata,idb,by = "ID_ESCOLA",all.x = T)

# Corrigindo classe das variáveis

str(mydata)

mydata[,c('IAD','IDEB')] <- lapply(mydata[,c('IAD','IDEB')],FUN = function(x){as.numeric(x)})

mydata$ICG <- parse_number(mydata$ICG)

# Extrai coordenadas dos pontos (latitude e longitude)
mydata[,c("longitude","latitude")] <- st_coordinates(x = mydata$geom)

mydata <- mydata %>% mutate(.,
                            tipo = case_when(
                              IN_PROF == 1 | IN_PROF_TEC == 1 ~ "Profissional/Técnica",
                              IN_PROF == 0 & IN_PROF_TEC == 0 ~ "Regular")
)

mydata <- mydata %>% mutate(.,
                            depAdmin = case_when(
                              TP_DEPENDENCIA == 1 ~ "Federal",
                              TP_DEPENDENCIA == 2 ~ "Estadual",
                              TP_DEPENDENCIA == 3 ~ "Municipal",
                              TP_DEPENDENCIA == 4 ~ "Privada",
                            ),
                            
                            area = case_when(
                              TP_LOCALIZACAO == 1 ~ "Urbana",
                              TP_LOCALIZACAO == 2 ~ "Rural"
                            )
)

# Quintis do NSE
mydata <- processQuantiles(data = mydata,col = "NSE10",q = 0.20,output_col = "NSE_Q5")

export(mydata,file = "0_data/escolas.xlsx")

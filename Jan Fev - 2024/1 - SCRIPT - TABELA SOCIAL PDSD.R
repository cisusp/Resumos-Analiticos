# Title: tabela social PDSD
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,geobr,sf,censobr,patchwork,car,sjPlot)

# Clean memory
rm(list=ls())
gc()

# Definição do diretório
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Import databases ----------------------------------------------------------

## Pessoas ----
pdsd_pes <- import("../0_data/BD_CIS0549_Pessoas.sav")

# Construindo livro de codigos
cod = names(pdsd_pes)

descricoes = rep(NA,ncol(pdsd_pes))
for (i in seq_along(pdsd_pes)) {
  print(i)
  
  tryCatch({
  # Extrair a descrição da variável e armazená-la no vetor de descrições
  descricoes[i] <- attr(pdsd_pes[[i]], "label")}, error = function(e) {
    # Se ocorrer um erro, imprima uma mensagem de aviso e continue para a próxima variável
    cat("Erro ao extrair descrição da variável ", names(pdsd_pes)[i], ": ", conditionMessage(e), "\n")
  })
  
}

lc <- data.frame(cod,descricoes)
export(lc,"../0_data/LC_PDSD_PES.xlsx")

# 2. Management ----------------------------------------------------------------

# Grau de parentesco com o chefe

mydata <- pdsd_pes %>% mutate(.,
                   
                  ocup_pai = p209, 
                  ocup_mae = p221, 
                  tipo_prim_ocup = p306,
                  prim_ocup = p307
                   )

mydata <- mydata %>% select(sexo,anod,idade,
                            ocup_pai,ocup_mae,prim_ocup,tipo_prim_ocup,
                            rentrali,
                            anosed,manosed,panosed)

mydata <- mydata %>% mutate(.,
                            ocup_pai = substr(mydata$ocup_pai,1,1) %>% as.numeric(),
                            ocup_mae = substr(mydata$ocup_mae,1,1) %>% as.numeric(),
                            prim_ocup = substr(mydata$prim_ocup,1,1) %>% as.numeric()
                            )

str(mydata$ocup_pai)

mydata <- mydata %>% mutate(.,
                            ocup_pai = factor(mydata$ocup_pai,
                                              levels = c(0:9),
                                              labels = c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES",                                                                       
                                                         "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES",
                                                         "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",                    
                                                         "TÉCNICOS DE NIVEL MÉDIO",                                                                                            
                                                         "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS",
                                                         "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
                                                         "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA",                                                          
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                          
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                        
                                                         "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO"                                                                        
                                              )),
                            
                            ocup_mae = factor(mydata$ocup_mae,
                                              levels = c(0:9),
                                              labels = c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES",                                                                       
                                                         "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES",
                                                         "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",                    
                                                         "TÉCNICOS DE NIVEL MÉDIO",                                                                                            
                                                         "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS",
                                                         "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
                                                         "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA",                                                          
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                          
                                                         "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO"                                                                        
                                              )),
                            
                            prim_ocup = factor(mydata$prim_ocup,
                                              levels = c(0:9),
                                              labels = c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES",                                                                       
                                                         "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES",
                                                         "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES",                    
                                                         "TÉCNICOS DE NIVEL MÉDIO",                                                                                            
                                                         "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS",
                                                         "TRABALHADORES DOS SERVIÇOS, VENDEDORES DO COMÉRCIO EM LOJAS E MERCADOS",
                                                         "TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA",                                                          
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",
                                                         "TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                          
                                                         "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO"                                                                        
                                              ))
                            
                            )

mydata <- mydata %>% mutate(.,
                            ocup_pai_2 = case_when(
                              ocup_pai %in% c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES") ~ "Membros das forças armadas",
                              
                              ocup_pai %in% c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES",                                                                       
                                      "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES",
                                      "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES") ~ "Servidores públicos, dirigentes e gerentes",
                              ocup_pai %in% c("TÉCNICOS DE NIVEL MÉDIO",                                                                                            
                                      "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS") ~ "Trabalhadores de serviços técnicos e não manuais",
                              ocup_pai %in% c("TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                          
                                      "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO") ~ "Trabalhadores da produção e de serviços manuais",
                              ocup_pai %in% c("TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA") ~ "Trabalhadores agropecuários, florestais e da pesca"
                              
                            ),
                            
                            prim_ocup_2 = case_when(
                              prim_ocup %in% c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES") ~ "Membros das forças armadas",
                              
                              prim_ocup %in% c("MEMBROS DAS FORÇAS ARMADAS, POLICIAIS E BOMBEIROS MILITARES",                                                                       
                                              "MEMBROS SUPERIORES DO PODER PÚBLICO, DIRIGENTES DE ORGANIZAÇÕES DE INTERESSE PÚBLICO E DE EMPRESAS, GERENTES",
                                              "PROFISSIONAIS DAS CIÊNCIAS E DAS ARTES") ~ "Servidores públicos, dirigentes e gerentes",
                              prim_ocup %in% c("TÉCNICOS DE NIVEL MÉDIO",                                                                                            
                                              "TRABALHADORES DE SERVIÇOS ADMINISTRATIVOS") ~ "Trabalhadores de serviços técnicos e não manuais",
                              prim_ocup %in% c("TRABALHADORES DA PRODUÇÃO DE BENS E SERVIÇOS INDUSTRIAIS",                                                                          
                                              "TRABALHADORES EM SERVIÇOS DE REPARAÇÃO E MANUTENÇÃO") ~ "Trabalhadores da produção e de serviços manuais",
                              prim_ocup %in% c("TRABALHADORES AGROPECUÁRIOS, FLORESTAIS E DA PESCA") ~ "Trabalhadores agropecuários, florestais e da pesca"
                              
                            )
                            
                            )

mydata <- mydata %>% mutate(.,
                            ocup_pai_2 = factor(ocup_pai_2,
                                                levels = c("Membros das forças armadas",
                                                           "Servidores públicos, dirigentes e gerentes",
                                                           "Trabalhadores de serviços técnicos e não manuais",
                                                           "Trabalhadores da produção e de serviços manuais",
                                                           "Trabalhadores agropecuários, florestais e da pesca"),
                                                ordered = T),
                            
                            prim_ocup_2 = factor(prim_ocup_2,
                                                levels = c("Membros das forças armadas",
                                                           "Servidores públicos, dirigentes e gerentes",
                                                           "Trabalhadores de serviços técnicos e não manuais",
                                                           "Trabalhadores da produção e de serviços manuais",
                                                           "Trabalhadores agropecuários, florestais e da pesca"),
                                                ordered = T)
)

table(mydata$ocup_pai_2)

table(mydata$prim_ocup)

mydata <- mydata %>% filter(anod > 1955 & anod < 1970)

mydata %>% group_by(prim_ocup_2) %>% summarise(
  me_rend = mean(rentrali,na.rm=T),
  me_educ = mean(anosed,na.rm=T)
  )

# Tabela social ----

sjt.xtab(var.row = mydata$ocup_pai_2,var.col = mydata$prim_ocup_2,
         show.col.prc = T,
         encoding = "UTF-8",
         var.labels = c("Origem (Ocup. Pai)","Destino (Ocup. Atual)"),
         file = "../2_outp/tab_social_raw.xls")

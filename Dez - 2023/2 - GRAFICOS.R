# Title: gráficos
# Author: Victor Gabriel Alcantara

# 0. Packages and setup --------------------------------------------------------

library(pacman)
p_load(tidyverse,rio,geobr,sf,censobr,patchwork,car)

# Clean memory
rm(list=ls())
gc()

# 1. Import data ---------------------------------------------------------------

escolas <- import("0_data/escolas.xlsx")

# Mapas ------------------------------------------------------------------------

escolas <- escolas %>% na.exclude(.$longitude,.$latitude)

# We have to remove axis from the ggplot layers
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot mapa
mapa1 <- brasil %>%
  ggplot()+
  geom_sf(aes(),fill="white")+
  geom_point(data = escolas,aes(x = X,y=Y,col=NSE_Q5),size=.4)+
  scale_color_manual(values=heat.colors(n = 5,alpha = 1,rev = T))+
  theme_bw()+
  labs(x="",y="",col="NSE",fill="NSE")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=12))+
  guides(color = guide_legend(override.aes = list(size = 7)))

# Plot mapa
mapa2 <- my_brmuni %>% 
  ggplot()+
  geom_sf(aes(fill=IDHM_R_Q5,col=IDHM_R_Q5))+
  scale_fill_manual(values=heat.colors(n = 5,alpha = 1,rev = T))+
  scale_color_manual(values=heat.colors(n = 5,alpha = 1,rev = T))+
  theme_bw()+
  labs(x="",y="",fill="IDHM-R",color="IDHM-R")+
  theme(legend.position = "bottom",
        legend.text = element_text(size=12))

# Save as pdf
pdf(file = "2_outp/mapa2.pdf",width = 10,height = 12)
plot(mapa2)
dev.off()

mapas <-   mapa1+mapa2 + plot_annotation(tag_levels = "A")

# Save as pdf
pdf(file = "2_outp/mapas.pdf",width = 12,height = 10)
plot(mapas)
dev.off()

# Boxplot ----------------------------------------------------------------------

## Dep Admin ----

me_nse <- mean(escolas$NSE10)

escolas %>% .[!is.na(escolas$tipo),] %>% 
  ggplot(aes(x=fct_reorder(depAdmin,NSE10),y=NSE10))+
  geom_violin(col='grey80',alpha=.2)+
  geom_boxplot()+
  theme_minimal()+
  geom_hline(yintercept = me_nse,lty=4,col="red")+
  labs(x="",y="NSE")+
  theme(axis.text.x = element_text(size = 10,angle = 20),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size=14)
  )+
  facet_wrap(~tipo)

# Save as pdf
pdf(file = "2_outp/bplot1.pdf",width = 8,height = 6)
plot(bplot1)
dev.off()

# Matriz de dispersão -------

m_data <- escolas %>% select(NSE10,IEE,IAD,IDEB,ICG,tipo,area) %>% na.exclude()

m_data <- m_data %>% mutate(.,
                            NSE10 = scale(NSE10),
                            IEE = scale(IEE),
                            IDEB = scale(IDEB),
                            IAD = scale(IAD)
)

scater1 <- m_data %>% 
  ggplot(aes(x=NSE10,y=IEE))+
  geom_jitter(alpha=.05,color="grey70")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,0.5))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title="IEE",subtitle = paste("Cor = ",round(cor(m_data$NSE10,m_data$IEE),2)),
       x="INSE")+
  theme_minimal()

scater2 <- m_data %>% 
  ggplot(aes(x=NSE10,y=IAD))+
  geom_jitter(alpha=.05,color="grey70")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,0.5))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title="IAD",subtitle = paste("Cor = ",round(cor(m_data$NSE10,m_data$IAD),2)),
       x="INSE")+
  theme_minimal()

scater3 <- m_data %>% 
  ggplot(aes(x=NSE10,y=IDEB))+
  geom_jitter(alpha=.05,color="grey70")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,0.5))+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  labs(title='IDEB',
       subtitle = paste("Cor = ",round(cor(m_data$NSE10,m_data$IDEB),2)),
       x="INSE")+
  theme_minimal()

scaters <- scater2 + scater1 + scater3

# Save as pdf
pdf(file = "2_outp/scaters.pdf",width = 8,height = 3)
plot(scaters)
dev.off()

lm(data = m_data,)











library(pacman)

p_load(dplyr, ggplot2, gridExtra)


loc <- "C:/Users/caio0001/Documents/ArcGIS/Projects/disty_RivEX/"

df <- read.csv(paste0(loc, "all_data_synchrony.csv"), sep=";", dec=",")

key <- read.csv(paste0(loc, "SERS_allsites_2022.csv"), sep=";", dec=",")

df2 <- left_join(df, key, by = c("fromXKOORLOK" = "XKOORLOK", "fromYKOORLOK" = "YKOORLOK"))

write.csv(df2, file=paste0(loc, "all_data_synchrony_more_info.csv"), row.names = F)

####

SubNetID <- read.csv(paste0(loc, "subnetid_key.csv"), sep=";", dec=",")

key_SubNetID <- SubNetID %>% group_by(SubNetID) %>% summarise(length = sum(Shape_Length, na.rm=T))

df_rivex <- read.csv(paste0(loc, "disty_rivex_out.csv"), sep=";", dec=",")

hej <- df_rivex %>% group_by(unique_frag, SubNetID) %>% summarize(count = n())

hej <- hej %>% left_join(key_SubNetID, by="SubNetID")

frag_sizes <- hej %>% group_by(unique_frag) %>% summarise(frag_size = sum(length, na.rm=T))

df_updated <- df %>% left_join(frag_sizes, by = "unique_frag")


#p_load(riverdist, tidyr, outliers, dplyr, ggplot2, regclass, sjPlot, lme4, gridExtra, ggExtra, lmerMultiMember, MASS, scales, PerformanceAnalytics)


df_trutta <- read.csv("C:/data/hela_disty_från_LNU/disty2023_remaster/NatComm/data/fragment_data_strutta.csv", sep=";", dec=".")
df_phoxinus <- read.csv("C:/data/hela_disty_från_LNU/disty2023_remaster/NatComm/data/fragment_data_pphoxinus.csv", sep=";", dec=".")
df_lucius <- read.csv("C:/data/hela_disty_från_LNU/disty2023_remaster/NatComm/data/fragment_data_elucius.csv", sep=";", dec=".")


df_trutta2 <- df_trutta %>% left_join(frag_sizes, by =  c("unique_fragment"="unique_frag"))
df_phoxinus2 <- df_phoxinus %>% left_join(frag_sizes, by =  c("unique_fragment"="unique_frag"))
df_lucius2 <- df_lucius %>% left_join(frag_sizes, by =  c("unique_fragment"="unique_frag"))


## How fragment size affects synchrony

p1 <- df_trutta2 %>% ggplot(aes(x=log(frag_size), y=arith_rho)) + 
  #geom_point(aes(size=n_occasions)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(y="Synchrony trout (rho)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="a)")+
  theme(legend.position = "none")

p2 <- df_phoxinus2 %>% ggplot(aes(x=log(frag_size), y=arith_rho)) + 
  #geom_point(aes(size=n_occasions)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(y="Synchrony minnow (rho)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="b)")+
  theme(legend.position = "none")

p3 <- df_lucius2 %>% ggplot(aes(x=log(frag_size), y=arith_rho)) + 
  #geom_point(aes(size=n_occasions)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(y="Synchrony pike (rho)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="c)")+
  theme(legend.position = "none")

summary(lm(data=df_trutta2, arith_rho ~ log(frag_size)))
summary(lm(data=df_phoxinus2, arith_rho ~ log(frag_size)))
summary(lm(data=df_lucius2, arith_rho ~ log(frag_size)))

# summary(lm(data=df_trutta2, arith_rho ~ log(frag_size), weights=log(n_occasions)))
# summary(lm(data=df_phoxinus2, arith_rho ~ log(frag_size), weights=log(n_occasions)))
# summary(lm(data=df_lucius2, arith_rho ~ log(frag_size), weights=log(n_occasions)))

## How fragment size affects stability

p4 <- df_trutta2 %>% ggplot(aes(x=log(frag_size), y=trout_sd)) + 
  #geom_point(aes(size=n_occasions)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(y="Variability trout (sd)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="d)")+
  theme(legend.position = "none")

p5 <- df_phoxinus2 %>% ggplot(aes(x=log(frag_size), y=minnow_sd)) +
  #geom_point(aes(size=n_occasions)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(y="Variability minnow (sd)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="e)")+
  theme(legend.position = "none")

p6 <- df_lucius2 %>% ggplot(aes(x=log(frag_size), y=pike_sd)) +
  #geom_point(aes(size=n_occasions)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(y="Variability pike (sd)", x="Fragment size (log)")+
  theme_classic()+
  labs(tag="f)")+
  theme(legend.position = "none")

summary(lm(data=df_trutta2, trout_sd ~ log(frag_size)))
summary(lm(data=df_phoxinus2, minnow_sd ~ log(frag_size)))
summary(lm(data=df_lucius2, pike_sd ~ log(frag_size)))

# summary(lm(data=df_trutta2, trout_sd ~ log(frag_size), weights=log(n_occasions)))
# summary(lm(data=df_phoxinus2, minnow_sd ~ log(frag_size), weights=log(n_occasions)))
# summary(lm(data=df_lucius2, pike_sd ~ log(frag_size), weights=log(n_occasions)))

## How synchrony affects stability

p7 <- df_trutta2 %>% ggplot(aes(x=arith_rho, y=trout_sd)) +
  #geom_point(aes(size=n_occasions)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="Synchrony trout (rho)", y="Variability trout (sd)")+
  theme_classic()+
  labs(tag="g)")+
  theme(legend.position = "none")

p8 <- df_phoxinus2 %>% ggplot(aes(x=arith_rho, y=minnow_sd)) +
  #geom_point(aes(size=n_occasions)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="Synchrony minnow (rho)", y="Variability minnow (sd)")+
  theme_classic()+
  labs(tag="h)")+
  theme(legend.position = "none")

p9 <- df_lucius2 %>% ggplot(aes(x=arith_rho, y=pike_sd)) +
  #geom_point(aes(size=n_occasions)) +
  geom_point() + 
  geom_smooth(method="lm") +
  labs(x="Synchrony pike (rho)", y="Variability pike (sd)")+
  theme_classic()+
  labs(tag="i)")+
  theme(legend.position = "none")

summary(lm(data=df_trutta2, trout_sd ~ arith_rho))
summary(lm(data=df_phoxinus2, minnow_sd ~ arith_rho))
summary(lm(data=df_lucius2, pike_sd ~ arith_rho))

# summary(lm(data=df_trutta2, trout_sd ~ arith_rho, weights=log(n_occasions)))
# summary(lm(data=df_phoxinus2, minnow_sd ~ arith_rho, weights=log(n_occasions)))
# summary(lm(data=df_lucius2, pike_sd ~ arith_rho, weights=log(n_occasions)))

grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3)


####
####
####



summary(lm(data=df_trutta2, trout_occ ~ arith_rho))
summary(lm(data=df_phoxinus2, minnow_occ ~ arith_rho))
summary(lm(data=df_lucius2, pike_occ ~ arith_rho))

df_trutta2 %>% ggplot(aes(x=log(frag_size), y=trout_occ)) +
  geom_point(aes(size=n_occasions)) +
  geom_smooth(method="lm") +
  #labs(x="Synchrony trout (rho)", y="Variability trout (sd)")+
  theme_classic()+
  theme(legend.position = "none")

df_phoxinus2 %>% ggplot(aes(x=log(frag_size), y=minnow_occ)) +
  geom_point(aes(size=n_occasions)) +
  geom_smooth(method="lm") +
  #labs(x="Synchrony minnow (rho)", y="Variability minnow (sd)")+
  theme_classic()+
  theme(legend.position = "none")

df_lucius2 %>% ggplot(aes(x=log(frag_size), y=pike_occ)) +
  geom_point(aes(size=n_occasions)) +
  geom_smooth(method="lm") +
  #labs(x="Synchrony pike (rho)", y="Variability pike (sd)")+
  theme_classic()+
  theme(legend.position = "none")








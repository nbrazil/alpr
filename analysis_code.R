###############################################################
#This R Script runs multivariate regression models
#for network ALPR camera project
#
#
#1. Regress ALPR Cameras Per area measured at
# a. Own neighborhood
# b. Network neighbors 
#
# Main ind variables: % white, % black, % hisp, % fb, poverty
#
#2. Differences by poor/non poor
#
#
#Created by: Noli Brazil
###############################################################


#load in packages
library(tidyverse)
library(emmeans)

#set working directory

#turn off scientific notation
options(scipen = 999)

#bring in data
#demographics
file <- read_csv("final_analysis_file.csv")

#Main regressions
#Figure 3

#residential
#white
lm_ownw <- glm(flockarea ~ pwhite+PC1+phome+
                tpop  + vcrimed + totestd + NAMEc, data = file)

#black
lm_ownb <- glm(flockarea ~ pblack+PC1+phome+
                 tpop  + vcrimed + totestd + NAMEc, data = file)

#hisp
lm_ownh <- glm(flockarea ~ phisp+PC1+phome+
                 tpop  + vcrimed + totestd + NAMEc, data = file)

#asian
lm_owna <- glm(flockarea ~ pasian+PC1+phome+
                 tpop  + vcrimed + totestd + NAMEc, data = file)

#net/visited
#white
lm_netw <- glm(meanflockarea ~ pwhite+PC1+phome+
                 tpop  + meanvcrimed + meanestd + NAMEc, data = file)

#black
lm_netb <- glm(meanflockarea ~ pblack+PC1+phome+
                 tpop  + meanvcrimed + meanestd + NAMEc, data = file)

#hisp
lm_neth <- glm(meanflockarea ~ phisp+PC1+phome+
                 tpop  + meanvcrimed + meanestd + NAMEc, data = file)

#asian
lm_neta <- glm(meanflockarea ~ pasian+PC1+phome+
                 tpop  + meanvcrimed + meanestd + NAMEc, data = file)

#get coefficients
ownw <-summary(emtrends(lm_ownw,  ~ 1, var = "pwhite")) %>%
  dplyr::select(lower.CL, upper.CL, pwhite.trend) %>%
  mutate(neigh = "Residential",
         race = "White") %>%
  rename(Estimate = pwhite.trend)

ownh <-summary(emtrends(lm_ownh,  ~ 1, var = "phisp")) %>%
  dplyr::select(lower.CL, upper.CL, phisp.trend) %>%
  mutate(neigh = "Residential",
         race = "Hispanic") %>%
  rename(Estimate = phisp.trend)

ownb <-summary(emtrends(lm_ownb,  ~ 1, var = "pblack")) %>%
  dplyr::select(lower.CL, upper.CL, pblack.trend) %>%
  mutate(neigh = "Residential",
         race = "Black") %>%
  rename(Estimate = pblack.trend)

owna <-summary(emtrends(lm_owna,  ~ 1, var = "pasian")) %>%
  dplyr::select(lower.CL, upper.CL, pasian.trend) %>%
  mutate(neigh = "Residential",
         race = "Asian") %>%
  rename(Estimate = pasian.trend)

own <- rbind(ownw, ownh, ownb, owna)

netw <-summary(emtrends(lm_netw,  ~ 1, var = "pwhite")) %>%
  dplyr::select(lower.CL, upper.CL, pwhite.trend) %>%
  mutate(neigh = "Visited",
         race = "White") %>%
  rename(Estimate = pwhite.trend)

neth <-summary(emtrends(lm_neth,  ~ 1, var = "phisp")) %>%
  dplyr::select(lower.CL, upper.CL, phisp.trend) %>%
  mutate(neigh = "Visited",
         race = "Hispanic") %>%
  rename(Estimate = phisp.trend)%>%
  mutate(Estimate = Estimate+0.005, Estimate,
         lower.CL = lower.CL +0.005, lower.CL,
         upper.CL = upper.CL+0.005, upper.CL)

netb <-summary(emtrends(lm_netb,  ~ 1, var = "pblack")) %>%
  dplyr::select(lower.CL, upper.CL, pblack.trend) %>%
  mutate(neigh = "Visited",
         race = "Black") %>%
  rename(Estimate = pblack.trend)

neta <-summary(emtrends(lm_neta,  ~ 1, var = "pasian")) %>%
  dplyr::select(lower.CL, upper.CL, pasian.trend) %>%
  mutate(neigh = "Visited",
         race = "Asian") %>%
  rename(Estimate = pasian.trend)

net <- rbind(netw, neth, netb, neta)

race_trend <- rbind(own, net)

#percent foreign born

#residential
lm_own <- glm(flockarea ~ pwhite+pfb+PC1+phome+
                tpop  + vcrimed + totestd + NAMEc, data = file)

#net
lm_net <- glm(meanflockarea ~ pwhite+pfb+PC1+phome+
                tpop  + meanvcrimed + meanestd + NAMEc, data = file)

ownd <-summary(emtrends(lm_own,  ~ 1, var = "pfb")) %>%
  dplyr::select(lower.CL, upper.CL, pfb.trend) %>%
  mutate(neigh = "Residential") %>%
  rename(Estimate = pfb.trend)

netd <-summary(emtrends(lm_net,  ~ 1, var = "pfb")) %>%
  dplyr::select(lower.CL, upper.CL, pfb.trend) %>%
  mutate(neigh = "Visited") %>%
  rename(Estimate = pfb.trend)

pfb_trend <- rbind(ownd, netd)
pfb_trend <- pfb_trend %>%
  mutate(race = "Foreign Born")


#poverty
#residential
lm_own <- glm(flockarea ~ pwhite+ppov+PC1+phome+
                tpop  + vcrimed + totestd + NAMEc, data = file)

#net
lm_net <- glm(meanflockarea ~ pwhite+ppov+PC1+phome+
                tpop  + meanvcrimed + meanestd + NAMEc, data = file)

ownd <-summary(emtrends(lm_own,  ~ 1, var = "ppov")) %>%
  dplyr::select(lower.CL, upper.CL, ppov.trend) %>%
  mutate(neigh = "Residential") %>%
  rename(Estimate = ppov.trend)

netd <-summary(emtrends(lm_net,  ~ 1, var = "ppov")) %>%
  dplyr::select(lower.CL, upper.CL, ppov.trend) %>%
  mutate(neigh = "Visited") %>%
  rename(Estimate = ppov.trend) %>%
  mutate(Estimate = Estimate+0.005, Estimate,
         lower.CL = lower.CL +0.005, lower.CL,
         upper.CL = upper.CL+0.005, upper.CL)

ppov_trend <- rbind(ownd, netd)
ppov_trend <- ppov_trend %>%
  mutate(race = "Poverty")

race_trend <- rbind(race_trend, pfb_trend, ppov_trend)

#make into factors
race_trend$neigh <- factor(race_trend$neigh, levels = c('Visited', 'Residential'))

race_trend$race <- factor(race_trend$race, levels = c("White",
                                                      "Black",
                                                      "Hispanic",
                                                      "Asian",
                                                      "Poverty",
                                                      "Foreign Born"
))


#plot
ggplot(race_trend, aes(x = neigh, y = Estimate, color = race, group = race)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.4),
    width = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Neighborhood",
    y = "Coefficient Estimate",
    color = ""
  ) +   
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=10))


#######
#interact poverty and race
#race continuous, poor categorical
#Figure 4


#residential
lm_ownw <- glm(flockarea ~ pwhite*poor+PC1+phome+
                 tpop + vcrimed + totestd + NAMEc, data = file)

lm_ownb <- glm(flockarea ~ pblack*poor+PC1+phome+
                 tpop + vcrimed + totestd + NAMEc, data = file)

lm_ownh <- glm(flockarea ~ phisp*poor+PC1+phome+
                 tpop + vcrimed + totestd + NAMEc, data = file)

lm_owna <- glm(flockarea ~ pasian*poor+PC1+phome+
                 tpop + vcrimed + totestd + NAMEc, data = file)

#net/visited
lm_netw <- glm(meanflockarea ~ pwhite*poor+PC1+phome+
                 tpop + meanvcrimed + meanestd + NAMEc, data = file)

lm_netb <- glm(meanflockarea ~ pblack*poor+PC1+phome+
                  tpop + meanvcrimed + meanestd + NAMEc, data = file)

lm_neth <- glm(meanflockarea ~ phisp*poor+PC1+phome+
                 tpop + meanvcrimed + meanestd + NAMEc, data = file)

lm_neta <- glm(meanflockarea ~ pasian*poor+PC1+phome+
                 tpop + meanvcrimed + meanestd + NAMEc, data = file)

#coefficients
ownw <- summary(emtrends(lm_ownw,  ~ poor, var = "pwhite")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pwhite.trend) %>%
  mutate(neigh = "Residential",
         race = "White") %>%
  rename(Estimate = pwhite.trend)

ownb <- summary(emtrends(lm_ownb,  ~ poor, var = "pblack")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pblack.trend) %>%
  mutate(neigh = "Residential",
         race = "Black") %>%
  rename(Estimate = pblack.trend)

ownh <- summary(emtrends(lm_ownh,  ~ poor, var = "phisp")) %>%
  dplyr::select(poor, lower.CL, upper.CL, phisp.trend) %>%
  mutate(neigh = "Residential",
         race = "Hispanic") %>%
  rename(Estimate = phisp.trend)

owna <- summary(emtrends(lm_owna,  ~ poor, var = "pasian")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pasian.trend) %>%
  mutate(neigh = "Residential",
         race = "Asian") %>%
  rename(Estimate = pasian.trend)

netw <- summary(emtrends(lm_netw,  ~ poor, var = "pwhite")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pwhite.trend) %>%
  mutate(neigh = "Visited",
         race = "White") %>%
  rename(Estimate = pwhite.trend)

netb <- summary(emtrends(lm_netb,  ~ poor, var = "pblack")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pblack.trend) %>%
  rename(Estimate = pblack.trend) %>%
  mutate(neigh = "Visited",
         race = "Black")

neth <- summary(emtrends(lm_neth,  ~ poor, var = "phisp")) %>%
  dplyr::select(poor, lower.CL, upper.CL, phisp.trend) %>%
  rename(Estimate = phisp.trend) %>%
  mutate(neigh = "Visited",
         race = "Hispanic"
  ) 

neta <- summary(emtrends(lm_neta,  ~ poor, var = "pasian")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pasian.trend) %>%
  mutate(neigh = "Visited",
         race = "Asian") %>%
  rename(Estimate = pasian.trend)

#foreign born
#residential
lm_ownfb <- glm(flockarea ~ pfb*poor+PC1+pwhite+phome+
                  tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_netfb <- glm(meanflockarea ~ pfb*poor+PC1+pwhite+phome+
                  tpop + meanvcrimed + meanestd + NAMEc, data = file)


ownfb <- summary(emtrends(lm_ownfb,  ~ poor, var = "pfb")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pfb.trend) %>%
  mutate(neigh = "Residential",
         race = "Foreign Born") %>%
  rename(Estimate = pfb.trend)

netfb <- summary(emtrends(lm_netfb,  ~ poor, var = "pfb")) %>%
  dplyr::select(poor, lower.CL, upper.CL, pfb.trend) %>%
  mutate(neigh = "Visited",
         race = "Foreign Born") %>%
  rename(Estimate = pfb.trend)

poor_trend <- rbind(ownw, ownb, ownh, owna, ownfb,
                    netw, netb, neth, neta, netfb)

poor_trend$neigh <- factor(poor_trend$neigh, levels = c('Visited','Residential'))

poor_trend$race <- factor(poor_trend$race, levels = c("White",
                                                      "Black",
                                                      "Hispanic",
                                                      "Asian",
                                                      "Foreign Born"))

poor_trend$poor <- factor(poor_trend$poor, levels = c('Poor',
                                                      'Non Poor'))


#plot
ggplot(poor_trend, aes(x = neigh, y = Estimate, color = race, group = race)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    position = position_dodge(width = 0.4),
    width = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Neighborhood",
    y = "Coefficient Estimate",
    color = ""
  ) +   
  coord_flip() +
  facet_grid(  
    cols = vars(poor),
    scales = "free") +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=10))

ggsave("tot_poor_flock_cameras_race_city_wtot.png")


###########
#categorical majority race
#plot race differences
#Figure 5

#residential
lm_own <- glm(flockarea ~ race+PC1+phome+
                tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_net <- glm(meanflockarea ~ race+PC1+phome+
                tpop + meanvcrimed + meanestd + NAMEc, data = file)

#marginal means
emmeans1<- summary(emmeans(lm_own, ~ race,
                           type = "response")) %>%
  dplyr::select(race, emmean, lower.CL, upper.CL) %>%
  filter(race != "Mixed") %>%
  mutate(neigh = "Visited") %>%
  mutate(emmean = ifelse(race == "White", emmean-0.25, emmean),
         lower.CL = ifelse(race == "White", lower.CL-0.25, lower.CL),
         upper.CL = ifelse(race == "White", upper.CL-0.25, upper.CL)
  )

emmeans2<- summary(emmeans(lm_net, ~ race,
                           type = "response")) %>%
  dplyr::select(race, emmean, lower.CL, upper.CL) %>%
  filter(race != "Mixed") %>%
  mutate(neigh = "Residential") %>%
  mutate(emmean = ifelse(race == "White", emmean-0.25, emmean),
         lower.CL = ifelse(race == "White", lower.CL-0.25, lower.CL),
         upper.CL = ifelse(race == "White", upper.CL-0.25, upper.CL)
  )

#foreign born

#residential
lm_ownfb <- glm(flockarea ~ fb+race+PC1+phome+
                  tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_netfb <- glm(meanflockarea ~ fb+race+PC1+phome+
                  tpop + meanvcrimed + meanestd + NAMEc, data = file)

emmeans1fb<- summary(emmeans(lm_ownfb, ~ fb,
                             type = "response")) %>%
  dplyr::select(fb, emmean, lower.CL, upper.CL) %>%
  filter(fb != "Non Foreign Born") %>%
  rename(race = fb) %>%
  mutate(neigh = "Visited")

emmeans2fb<- summary(emmeans(lm_netfb, ~ fb,
                             type = "response")) %>%
  dplyr::select(fb, emmean, lower.CL, upper.CL) %>%
  filter(fb != "Non Foreign Born" ) %>%
  rename(race = fb) %>%
  mutate(neigh = "Residential")

#poor

#residential
lm_ownp <- glm(flockarea ~ poor+race+PC1+phome+
                 tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_netp <- glm(meanflockarea ~ poor+race+PC1+phome+
                 tpop + meanvcrimed + meanestd + NAMEc, data = file)

emmeans1p<- summary(emmeans(lm_ownp, ~ poor,
                            type = "response")) %>%
  dplyr::select(poor, emmean, lower.CL, upper.CL) %>%
  filter(poor != "Non Poor") %>%
  rename(race = poor) %>%
  mutate(neigh = "Visited")

emmeans2p<- summary(emmeans(lm_netp, ~ poor,
                            type = "response")) %>%
  dplyr::select(poor, emmean, lower.CL, upper.CL) %>%
  filter(poor != "Non Poor" ) %>%
  rename(race = poor) %>%
  mutate(neigh = "Residential")


#put results into a single dataframe
emmeans_v <-rbind(emmeans1, emmeans2, emmeans1fb, emmeans2fb,
                  emmeans1p, emmeans2p)

#remove Asian. Estimates unstable due to small sample
emmeans_v <- emmeans_v %>%
  filter(race != "Asian")

#create factors
emmeans_v$neigh <- factor(emmeans_v$neigh, levels = c('Residential',
                                                      'Visited'))

emmeans_v$race <- factor(emmeans_v$race, levels = c("Poor",
                                                    "Foreign Born",
                                                    "Hispanic",
                                                    "Black",
                                                    "White"))

#plot
ggplot(emmeans_v) +
  geom_bar(aes(x=race, y=emmean, fill = neigh),  stat="identity", 
           position=position_dodge(), alpha=0.7) +
  geom_errorbar(aes(x=race,ymin = lower.CL, ymax=upper.CL, group = neigh), width=0.5,
                position=position_dodge(0.9), alpha=0.5) +
  xlab("Group") +
  ylab("ALPR Cameras Per Square Mile") +
  coord_flip() +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12))


#######
#interact poverty and race
#race categorical, poor categorical

emmeans_v_tot <- emmeans_v

#residential
lm_own <- glm(flockarea ~ race*poor+PC1+phome+
                tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_net <- glm(meanflockarea ~ race*poor+PC1+phome+
                tpop + meanvcrimed + meanestd + NAMEc, data = file)

emmeans1<- summary(emmeans(lm_own, ~ race*poor,
                           type = "response")) %>%
  dplyr::select(race, poor, emmean, lower.CL, upper.CL) %>%
  filter(race != "Mixed") %>%
  mutate(neigh = "Visited") %>%
  mutate(emmean = ifelse(race == "White", emmean-0.25, emmean),
         lower.CL = ifelse(race == "White", lower.CL-0.25, lower.CL),
         upper.CL = ifelse(race == "White", upper.CL-0.25, upper.CL)
  )


emmeans2<- summary(emmeans(lm_net, ~ race*poor,
                           type = "response")) %>%
  dplyr::select(race, poor, emmean, lower.CL, upper.CL) %>%
  filter(race != "Mixed") %>%
  mutate(neigh = "Residential") %>%
  mutate(emmean = ifelse(race == "White", emmean-0.25, emmean),
         lower.CL = ifelse(race == "White", lower.CL-0.25, lower.CL),
         upper.CL = ifelse(race == "White", upper.CL-0.25, upper.CL)
  )


#foreign born

#residential
lm_ownfb <- glm(flockarea ~ fb*poor+race+PC1+phome+
                  tpop + vcrimed + totestd + NAMEc, data = file)

#net
lm_netfb <- glm(meanflockarea ~ fb*poor+race+PC1+phome+
                  tpop + meanvcrimed + meanestd + NAMEc, data = file)

emmeans1fb<- summary(emmeans(lm_ownfb, ~ fb*poor,
                             type = "response")) %>%
  dplyr::select(fb, poor, emmean, lower.CL, upper.CL) %>%
  filter(fb != "Non Foreign Born") %>%
  rename(race = fb) %>%
  mutate(neigh = "Visited")

emmeans2fb<- summary(emmeans(lm_netfb, ~ fb*poor,
                             type = "response")) %>%
  dplyr::select(fb, poor, emmean, lower.CL, upper.CL) %>%
  filter(fb != "Non Foreign Born" ) %>%
  rename(race = fb) %>%
  mutate(neigh = "Residential")

emmeans_v_tot <- emmeans_v_tot %>%
  mutate(poor = "Total") %>%
  filter(race != "Poor")

emmeans_v <-rbind(emmeans1, emmeans2, emmeans1fb, emmeans2fb, emmeans_v_tot)

#no asian
emmeans_v <- emmeans_v %>%
  filter(race != "Asian")

#create factors
emmeans_v$neigh <- factor(emmeans_v$neigh, levels = c('Residential',
                                                      'Visited'))

emmeans_v$race <- factor(emmeans_v$race, levels = c("Foreign Born",
                                                    "Hispanic",
                                                    "Black",
                                                    "White"))

emmeans_v$poor <- factor(emmeans_v$poor, levels = c('Total', 'Poor',
                                                    'Non Poor'))

ggplot(emmeans_v) +
  geom_bar(aes(x=race, y=emmean, fill = neigh),  stat="identity", 
           position=position_dodge(), alpha=0.7) +
  geom_errorbar(aes(x=race,ymin = lower.CL, ymax=upper.CL, group = neigh), width=0.5,
                position=position_dodge(0.9), alpha=0.5) +
  xlab("Group") +
  ylab("ALPR Cameras Per Square Mile") +
  coord_flip() +
  facet_grid(cols = vars(poor)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12))
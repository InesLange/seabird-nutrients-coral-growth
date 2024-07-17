####
## data analysis for:
## Seabird-nutrients increase coral calcification rates and boost reef carbonate production
##
## Lange ID, Benkwitt, CE 
## 2024. under review

#
# Author Ines Lange, Casey Benkwitt
#
# This code compares coral growth and calcification rates of Isopora palifera and Acropora vermiculata colonies
# next to an island with high seabird densities and an island with few seabirds in the Chagos Archipelago, BIOT.
# Data were collected on lagoonal reefs (1.5m) of Ile de la Passe (high seabird density) and Ile Anglaise (low sebird density)
# in January 2022 and Oct/Nov 2023 by Ines Lange (I. palifera photographs, ReefBudget surveys)
# and in March 2020 and April 2021 by Casey Benkwitt (A. vermiculata photographs)


## load packages
# organise and plot data
library(tidyverse)
library(plyr)
library(patchwork) #multipanel figures

# Bayesian analysis and interpretation
library(Rcpp)
library(brms)
library(tidybayes)
library(emmeans)

## load coral growth data
coral <- read.csv("data/coral_growth.csv")%>%
  mutate(status = as.factor(status)) 

coral$status <- factor(coral$status, 
                         levels = c("low", "high"))
coral$Species <- factor(coral$Species, 
                       levels = c("Isopora", "Acropora"))

isopora <- coral[coral$Species=="Isopora",] 
acropora <- coral[coral$Species=="Acropora",]

# load ReefBudget data
salomon <- read.csv("data/RB_salomon.csv")%>%
  mutate(status = as.factor(status)) 

salomon$status <- factor(salomon$status,levels = c("low", "high"))


####
# Fig 1: Plot growth metrics for Isopora and Acropora
####

# linear extension rate
linear.plot <- ggplot(coral, aes(x = status, y = linear, fill=status))
(linear <- linear.plot + 
    geom_boxplot(width=0.8)+
    labs(y=expression("Linear extension (cm "~yr^-1*")")) + xlab(NULL) +
    theme_classic() +  scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales = "free_y")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# planar area increase
planar_cm2.plot <- ggplot(coral, aes(x = status, y = planar_cm2, fill=status))
(planar <- planar_cm2.plot +
    geom_boxplot(width=0.8)+    
    labs(y=expression("Planar area increase ("*cm^2~cm^-2~yr^-1*")")) + xlab(NULL) +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales="free_y")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# surface area increase
area_cm2.plot <- ggplot(coral, aes(x = status, y = area_cm2, fill=status))
(area <- area_cm2.plot +
    geom_boxplot(width=0.8)+    
    labs(y=expression("Surface area increase ("*cm^2~cm^-2~yr^-1*")")) + xlab(NULL) +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales="free_y")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# volume increase
volume_cm2.plot <- ggplot(coral, aes(x = status, y = volume_cm2, fill=status))
(volume <- volume_cm2.plot +
    geom_boxplot(width=0.8)+    
    labs(y=expression("Volume increase ("*cm^3~cm^-2~yr^-1*")")) + xlab("Seabird density") +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales="free_y")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# skeletal density
density.plot <- ggplot(coral, aes(x = status, y = density, fill=status))
(density <- density.plot + 
    geom_boxplot(width=0.8)+    
    labs(y=expression("Skeletal density (g"~cm^-3*")")) + xlab("Seabird density") +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales = "free_y")+
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# calcification rate
calcification_cm2.plot <- ggplot(coral, aes(x = status, y = calcification_cm2, fill=status))
(calcification <- calcification_cm2.plot + 
    geom_boxplot(width=0.8)+    
    labs(y=expression("Calcification (g"~cm^-2~yr^-1*")")) + xlab("Seabird density") +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales = "free_y")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# plot growth metrics in Figure 1
Fig1_metrics <- (linear+planar+area)/(volume+density+calcification) +
  plot_annotation(tag_levels="A")
Fig1_metrics

ggsave("figures/Fig1_metrics.jpg", width = 10, height = 8)
ggsave("figures/Fig1_metrics.pdf", width = 10, height = 8)


#############
#### Fig 2: Plot reef-scale coral carbonate production
######

# structural complexity
rugosity.plot <- ggplot(salomon, aes(x = status, y = rugosity, fill=status))
(rugosity <- rugosity.plot + geom_boxplot(show.legend=TRUE) + 
    stat_summary(fun=mean, geom="point", shape=23, size=5, color="black", fill="white") +
    ylab("Rugosity") + xlab(NULL) + 
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

# coral cover
cover.plot <- ggplot(salomon, aes(x = status, y = cover, fill=status))
(cover <- cover.plot + geom_boxplot(show.legend=TRUE) + 
    stat_summary(fun=mean, geom="point", shape=23, size=5, color="black", fill="white") +
    ylab("Coral cover (%)") + xlab(NULL) + 
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    ylim(20,70)+
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

## carbonate production using average growth rates for both sites
coral.prod.avg <- ggplot(salomon, aes(x = status, y = coral_prod_avg, fill=status))
(coral.prod.avg <- coral.prod.avg + geom_boxplot(show.legend=TRUE) + 
    stat_summary(fun=mean, geom="point", shape=23, size=5, color="black", fill="white") +
    labs(y="Coral carbonate production (kg "~m^-2~yr^-1*")") + xlab(NULL) + 
    theme_classic() + scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    ylim(4,22)+
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

## carbonate production using site-specific growth rates
coral.prod.sites <- ggplot(salomon, aes(x = status, y = coral_prod_sites, fill=status))
(coral.prod.sites <- coral.prod.sites + geom_boxplot(show.legend=TRUE) + 
    stat_summary(fun=mean, geom="point", shape=23, size=5, color="black", fill="white") +
    labs(y="Coral carbonate production (kg "~m^-2~yr^-1*")") + xlab(NULL) +
    theme_classic() + scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    ylim(4,22)+
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

## carbonate production using low seabird growth rates but multiplying carbonate production by 2.2 for high seabird site
coral.prod.2.2 <- ggplot(salomon, aes(x = status, y = coral_prod_2.2, fill=status))
(coral.prod.2.2 <- coral.prod.2.2 + geom_boxplot(show.legend=TRUE) + 
    stat_summary(fun=mean, geom="point", shape=23, size=5, color="black", fill="white") +
    labs(y="Coral carbonate production (kg "~m^-2~yr^-1*")") + xlab(NULL) +
    theme_classic() +   scale_fill_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    ylim(4,22)+
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

Fig2_Reefbudget <- (cover+coral.prod.avg+coral.prod.sites+coral.prod.2.2) + 
  plot_layout(ncol = 4) + 
  plot_annotation(tag_levels="A")

Fig2_Reefbudget <- Fig2_Reefbudget + plot_annotation(
  caption = "Seabird density",
  theme = theme(
    plot.caption = element_text(hjust = 0.5, size = 12)))
Fig2_Reefbudget

ggsave("figures/Fig2_Reefbudget.jpg", width = 10, height = 4)
ggsave("figures/Fig2_Reefbudget.pdf", width = 10, height = 4)


######
# Figure S1: plot skeletal density against coral growth
#####

density_linear <- ggplot(coral, aes(x = density, y = linear))
(density_linear <- density_linear + geom_point(aes(size=2, colour=status), legend=TRUE) + 
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(y=expression("Linear extension (cm "~yr^-1*")"), x=expression("Skeletal density (g"~cm^-3*")")) + 
    theme_classic() + scale_color_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales = "free")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))

#test correlation
dens_linear_iso <- lm(linear~density, data=isopora)
summary(dens_linear_iso) # p<0.001, R2adj=0.57, 
# significant relationship between linear extension and density
dens_linear_acr <- lm(linear~density, data=acropora)
summary(dens_linear_acr) # p= 0.027, R2adj=0.34
# significant relationship between linear extension and density

density_planar <- ggplot(coral, aes(x = density, y = planar_cm2))
(density_planar <- density_planar + geom_point(aes(size=2, colour=status), legend=TRUE) + 
    geom_smooth(method = 'lm', formula = y ~ x) +
    labs(y=expression("Planar area increase ("~cm^2~cm^-2~yr^-1*")"), x=expression("Skeletal density (g "~cm^-3*")")) + theme_classic() +   
    scale_color_manual(values = alpha(c("#BE0032","#0067A5"),0.7)) + 
    facet_wrap(~Species, scales = "free")+ 
    theme(legend.position = "none")+
    theme(axis.text=element_text(size=12), axis.title=element_text(size=12)))
#test correlation
dens_planar_iso <- lm(planar_cm2~density, data=isopora)
summary(dens_planar_iso) #p=0.008, R2adj=0.38
# significant relationship between planar increase and density 
dens_planar_acr<- lm(planar_cm2~density, data=acropora)
summary(dens_planar_acr) # p=0.018, R2adj=0.39
# significant relationship between planar area increase and density

FigS1_density <- (density_linear+density_planar) + 
  plot_annotation(tag_levels="A")
FigS1_density

ggsave("figures/FigS1_density.jpg", width = 10, height = 4)
ggsave("figures/FigS1_density.pdf", width = 10, height = 4)



########
# Bayesian models and effect size plots for growth metrics (Fig S2)
########

# Bayesian models
# use uninformative (default) prior because there is no prior data on nutrient effects on Isopora growth (and to be conservative)
# use weakly informative half-cauchy for variance components due to small sample size 
# based on McNeish 2016 (https://www.tandfonline.com/doi/full/10.1080/10705511.2016.1186549?casa_token=8aPyiZ0aEU0AAAAA%3AAoo2jZ547UxohRAt8eKf4bOWGJ7xdK_jc3wp4t3t2OPeUtkMmOwzEUSE2PGALmxlC9jY3V-hKWj0) 
# log-transform response parameters for easy comparison of growth metrics and coral taxa
# based on McElreath 2016 (https://www.taylorfrancis.com/books/mono/10.1201/9781315372495/statistical-rethinking-richard-mcelreath)

# set prior
pr_default_cauchy <- 
  set_prior("cauchy(0,2)", class = "sigma")

###ISOPORA - calcification rate----------
growth_mod_iso_log_0<- brm(
  log(calcification_cm2)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/growth_mod_iso_log_0") 
print(growth_mod_iso_log_0) #0 divergent trans

pp_check(growth_mod_iso_log_0)
plot(growth_mod_iso_log_0, ask = FALSE)
#all looks good

growth_mod_iso_log_0.rg <- update(ref_grid(growth_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

growth_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
#birds / no birds  1.56     0.958      2.29

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#so birdy corals have 1.56 times higher calcification rates than ratty corals (95% HPDI 0.958 to 2.29).

growth_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.762     0.551      1.02
#birds       1.193     0.882      1.57

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(growth_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) > 0      0.45      0.21      0.1     0.79      47.19      0.98    *

#so estimated posterior probability of positive bird effect = 0.98 (NOTE - this is identical as to non-log model)

###ISOPORA - linear extension rate----------
linear_mod_iso_log_0<- brm(
  log(linear)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/linear_mod_iso_log_0") 
print(linear_mod_iso_log_0) #0 divergent trans

pp_check(linear_mod_iso_log_0)
plot(linear_mod_iso_log_0, ask = FALSE)
#all looks good

linear_mod_iso_log_0.rg <- update(ref_grid(linear_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

linear_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
#birds / no birds  1.38     0.783      2.08

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

linear_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.597     0.405     0.814
#birds       0.825     0.603     1.130

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(linear_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) > 0     0.32      0.23    -0.06      0.7      12.11      0.92  

###ISOPORA - planar area ----------
planar_mod_iso_log_0<- brm(
  log(planar_cm2)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/planar_mod_iso_log_0") 
print(planar_mod_iso_log_0) #0 divergent trans

pp_check(planar_mod_iso_log_0)
plot(planar_mod_iso_log_0, ask = FALSE)
#all looks good

planar_mod_iso_log_0.rg <- update(ref_grid(planar_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

planar_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds  1.75     0.812      3.01

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

planar_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.326     0.189     0.481
#birds       0.568     0.359     0.840

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(planar_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 ((statusbirds) > 0     0.56      0.31     0.04     1.06      24.16      0.96    *

###ISOPORA - surface area ----------
surface_mod_iso_log_0<- brm(
  log(area_cm2)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/surface_mod_iso_log_0") 
print(surface_mod_iso_log_0) #0 divergent trans

pp_check(surface_mod_iso_log_0)
plot(surface_mod_iso_log_0, ask = FALSE)
#all looks good

surface_mod_iso_log_0.rg <- update(ref_grid(surface_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

surface_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds  2.13     0.792       4.1

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

surface_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.320     0.162      0.52
#birds       0.677     0.349      1.08

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(surface_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) > 0     0.75      0.38     0.13     1.39      38.41      0.97    *

###ISOPORA - volume----------
volume_mod_iso_log_0<- brm(
  log(volume_cm2)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/volume_mod_iso_log_0") 
print(volume_mod_iso_log_0) #0 divergent trans

pp_check(volume_mod_iso_log_0)
plot(volume_mod_iso_log_0, ask = FALSE)
#all looks good

volume_mod_iso_log_0.rg <- update(ref_grid(volume_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

volume_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds 1.66     0.984      2.57

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

volume_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.500     0.345     0.685
#birds       0.832     0.572     1.112

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(volume_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#(statusbirds) > 0     0.51      0.24     0.12      0.9      54.94      0.98    *

###ISOPORA - density----------
density_mod_iso_log_0<- brm(
  log(density)~status,
  data = isopora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/density_mod_iso_log_0") 
print(density_mod_iso_log_0) #0 divergent trans

pp_check(density_mod_iso_log_0)
plot(density_mod_iso_log_0, ask = FALSE)
#all looks good

density_mod_iso_log_0.rg <- update(ref_grid(density_mod_iso_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

density_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
#  birds / no birds 0.941     0.843      1.04

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#reverse:
density_mod_iso_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("pairwise")

#contrast         ratio lower.HPD upper.HPD
#  no birds / birds  1.06     0.959      1.18

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

density_mod_iso_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds     1.52      1.41      1.64
#birds        1.43      1.33      1.54

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(density_mod_iso_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 ((statusbirds) > 0    -0.06      0.05    -0.15     0.03       0.13      0.12 

hypothesis(density_mod_iso_log_0, "statusbirds<0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) < 0    -0.06      0.05    -0.15     0.03       7.47      0.88    



##### ACROPORA - calcification--------------
growth_mod_acr_log_0<- brm(
  log(calcification_cm2)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  # control = list(adapt_delta = 0.999, max_treedepth = 15),  #don't need this because super simple models
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/growth_mod_acr_log_0") 
print(growth_mod_acr_log_0) #0 divergent trans

pp_check(growth_mod_acr_log_0)
plot(growth_mod_acr_log_0, ask = FALSE)
#all looks good

growth_mod_acr_log_0.rg <- update(ref_grid(growth_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

growth_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
#birds / no birds  2.74      0.99      5.11

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

growth_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds     3.33      1.72      5.28
#birds        9.10      4.60     14.40

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(growth_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#         (statusbirds) > 0     1.01      0.37      0.4     1.62     209.53         1    *

###acropora - linear extension rate----------
linear_mod_acr_log_0<- brm(
  log(linear)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/linear_mod_acr_log_0") 
print(linear_mod_acr_log_0) #0 divergent trans

pp_check(linear_mod_acr_log_0)
plot(linear_mod_acr_log_0, ask = FALSE)
#all looks good

linear_mod_acr_log_0.rg <- update(ref_grid(linear_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

linear_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
#birds / no birds  3.19       1.8      5.05
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

linear_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds     1.92      1.28      2.69
#birds        6.14      4.05      8.42
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(linear_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#(statusbirds) > 0     1.16      0.25     0.75     1.58       1999         1    * 

###acropora - planar area ----------
planar_mod_acr_log_0<- brm(
  log(planar_cm2)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/planar_mod_acr_log_0") 
print(planar_mod_acr_log_0) #0 divergent trans

pp_check(planar_mod_acr_log_0)
plot(planar_mod_acr_log_0, ask = FALSE)
#all looks good

planar_mod_acr_log_0.rg <- update(ref_grid(planar_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

planar_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds   3.86       1.9      6.71
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

planar_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
# no birds    0.634     0.393     0.949
#birds       2.457     1.482     3.575
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(planar_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#(statusbirds) > 0     1.35      0.31     0.85     1.85    2665.67         1    *

###acropora - surface area ----------
surface_mod_acr_log_0<- brm(
  log(area_cm2)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/surface_mod_acr_log_0") 
print(surface_mod_acr_log_0) #0 divergent trans

pp_check(surface_mod_acr_log_0)
plot(surface_mod_acr_log_0, ask = FALSE)
#all looks good

surface_mod_acr_log_0.rg <- update(ref_grid(surface_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

surface_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds  4.1      2.08         7

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

surface_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds    0.994     0.628      1.47
#birds       4.083     2.595      5.99

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(surface_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) > 0     1.41       0.3     0.93      1.9       7999         1    *

###acropora - volume----------
volume_mod_acr_log_0<- brm(
  log(volume_cm2)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/volume_mod_acr_log_0") 
print(volume_mod_acr_log_0) #0 divergent trans

pp_check(volume_mod_acr_log_0)
plot(volume_mod_acr_log_0, ask = FALSE)
#all looks good

volume_mod_acr_log_0.rg <- update(ref_grid(volume_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

volume_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds 3.5      1.28      6.81

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

volume_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds     2.28      1.15       3.7
#birds        7.98      4.25      13.4
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(volume_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#(statusbirds) > 0     1.25      0.39     0.61     1.89     379.95         1    *

###acropora - density----------
density_mod_acr_log_0<- brm(
  log(density)~status,
  data = acropora, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/density_mod_acr_log_0") 
print(density_mod_acr_log_0) #0 divergent trans

pp_check(density_mod_acr_log_0)
plot(density_mod_acr_log_0, ask = FALSE)
#all looks good

density_mod_acr_log_0.rg <- update(ref_grid(density_mod_acr_log_0),  tran = "log") #back-transform estimates by telling it we log-transformed

density_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# birds / no birds 0.783     0.667     0.901

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#reverse:
density_mod_acr_log_0.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("pairwise")

#contrast         ratio lower.HPD upper.HPD
#  no birds / birds  1.28       1.1      1.48
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

density_mod_acr_log_0.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#no birds     1.45      1.29      1.60
#birds        1.14      1.02      1.26
#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(density_mod_acr_log_0, "statusbirds>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) > 0    -0.24      0.07    -0.36    -0.12          0         0  

hypothesis(density_mod_acr_log_0, "statusbirds<0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1 (statusbirds) < 0    -0.24      0.07    -0.36    -0.12        499         1    *   

#####
### PLOT EFFECT SIZES FOR GROWTH METRICS-------
###

#list of models, and then extract draws (and exponentiate to back-transform from log scale):
linear_mod_acr_log_0
planar_mod_acr_log_0
surface_mod_acr_log_0
volume_mod_acr_log_0
density_mod_acr_log_0
growth_mod_acr_log_0

linear_mod_iso_log_0
planar_mod_iso_log_0
surface_mod_iso_log_0
volume_mod_iso_log_0
density_mod_iso_log_0
growth_mod_iso_log_0

acr_linear_spread<-linear_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

acr_planar_spread<-planar_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

acr_surface_spread<-surface_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

acr_vol_spread<-volume_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

acr_dens_spread<-density_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

acr_calc_spread<-growth_mod_acr_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))


iso_linear_spread<-linear_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

iso_planar_spread<-planar_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

iso_surface_spread<-surface_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

iso_vol_spread<-volume_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

iso_dens_spread<-density_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

iso_calc_spread<-growth_mod_iso_log_0 %>%
  spread_draws(b_statusbirds)%>%
  mutate(exp_status_birds = exp(b_statusbirds))

##now plot: 

iso_plot<-
  iso_calc_spread%>%
  ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  stat_halfeye(aes(y = 1, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") +  
  stat_halfeye(data = iso_dens_spread, aes(y = 2, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") +  
  stat_halfeye(data = iso_vol_spread, aes(y = 3, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") +  
  stat_halfeye(data = iso_surface_spread, aes(y = 4, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") + 
  stat_halfeye(data = iso_planar_spread, aes(y = 5, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") + 
  stat_halfeye(data = iso_linear_spread, aes(y = 6, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#4477AA") +   
  theme_bw() + 
  xlab("Estimated seabird effect on Isopora (multiplicative)")+
  ylab("")+
  xlim(c(0,12))+
  scale_y_continuous(labels = c("calcification", "density", "volume", "surface area", "planar area", "linear extension"),
                     breaks = c(1,2,3,4,5,6))+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA))
iso_plot

acr_plot<-
  acr_calc_spread%>%
  ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  stat_halfeye(aes(y = 1, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") +  
  stat_halfeye(data = acr_dens_spread, aes(y = 2, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") +  
  stat_halfeye(data = acr_vol_spread, aes(y = 3, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") +  
  stat_halfeye(data = acr_surface_spread, aes(y = 4, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") + 
  stat_halfeye(data = acr_planar_spread, aes(y = 5, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") + 
  stat_halfeye(data = acr_linear_spread, aes(y = 6, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, fill = "#66CCEE") +  
  theme_bw() + 
  xlab("Estimated seabird effect on Acropora (multiplicative)")+
  ylab("")+
  xlim(c(0,12))+
  scale_y_continuous(labels = c("calcification", "density", "volume", "surface area", "planar area", "linear extension"),
                     breaks = c(1,2,3,4,5,6))+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA))
acr_plot

#combined plot: 
combined_plot<-
  acr_calc_spread%>%
  ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  stat_halfeye(aes(y = 1, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE", fill = "#66CCEE") +  
  stat_halfeye(data = acr_dens_spread, aes(y = 2, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE",fill = "#66CCEE") +  
  stat_halfeye(data = acr_vol_spread, aes(y = 3, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE",fill = "#66CCEE") +  
  stat_halfeye(data = acr_surface_spread, aes(y = 4, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE",fill = "#66CCEE") + 
  stat_halfeye(data = acr_planar_spread, aes(y = 5, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE",fill = "#66CCEE") + 
  stat_halfeye(data = acr_linear_spread, aes(y = 6, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#66CCEE",fill = "#66CCEE") +   
  
  stat_halfeye(data = iso_calc_spread, aes(y = 1, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2, colour = "#4477AA",fill = "#4477AA") +  
  stat_halfeye(data = iso_dens_spread, aes(y = 2, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2,  colour = "#4477AA",fill = "#4477AA") +  
  stat_halfeye(data = iso_vol_spread, aes(y = 3, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2,  colour = "#4477AA",fill = "#4477AA") +  
  stat_halfeye(data = iso_surface_spread, aes(y = 4, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2,  colour = "#4477AA",fill = "#4477AA") + 
  stat_halfeye(data = iso_planar_spread, aes(y = 5, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2,  colour = "#4477AA",fill = "#4477AA") + 
  stat_halfeye(data = iso_linear_spread, aes(y = 6, x = exp_status_birds), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.6, fatten_point = 2,  colour = "#4477AA",fill = "#4477AA") +
  theme_bw() + 
  #xlab("Estimated seabird effect (multiplicative)")+
  xlab(NULL)+
  ylab(NULL)+
  xlim(c(0,12))+
  scale_y_continuous(labels = c("Calcification", "Skeletal density", "Volume increase", "Surface area increase", "Planar area increase", "Linear extension"),
                     breaks = c(1,2,3,4,5,6))+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA))
combined_plot

ggsave("combined_plot.pdf", combined_plot, width =  5, height = 4)
ggsave( "combined_plot.jpg", combined_plot, width = 5, height = 4)

ggsave("acropora_plot.pdf", acr_plot, width = 4, height = 4)
ggsave( "acropora_plot.jpg", acr_plot, width = 4, height = 4)

ggsave("isopora_plot.pdf", iso_plot, width = 4, height = 4)
ggsave( "isopora_plot.jpg", iso_plot, width = 4, height = 4)


########
# Bayesian models and effect size plots for reef-scale carbonate production (Fig S2B)
########

#set prior for all, as per growth metrics:
pr_default_cauchy<-
  set_prior("cauchy(0,2)", class = "sigma")

### coral cover ----------
cover_mod_log<- brm(
  log(cover)~status,
  data = salomon, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  # control = list(adapt_delta = 0.999, max_treedepth = 15),  #don't need this because super simple models
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/cover_mod_log") 
print(cover_mod_log) #0 divergent trans

pp_check(cover_mod_log)
plot(cover_mod_log, ask = FALSE)
#all looks good

cover_mod_log.rg <- update(ref_grid(cover_mod_log),  tran = "log") #back-transform estimates by telling it we log-transformed

cover_mod_log.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# high / low      1.12     0.627      1.72

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#so seabird has 1.12 higher production than ratty (but 95% HPDI = 0.627 to 1.72)

cover_mod_log.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#low        41.2      28.3      56.2
#high       46.1      31.4      62.9

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#lots of overlap

hypothesis(cover_mod_log, "statushigh>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1   (statushigh) > 0     0.11      0.25    -0.27     0.49       2.42      0.71 

# coral cover is similar at both sites

#### Carbonate production using average average growth rates----------
prod_avg_mod_log<- brm(
  log(coral_prod_avg)~status,
  data = salomon, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  # control = list(adapt_delta = 0.999, max_treedepth = 15),  #don't need this because super simple models
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/prod_avg_mod_log") 
print(prod_avg_mod_log) #0 divergent trans

pp_check(prod_avg_mod_log)
plot(prod_avg_mod_log, ask = FALSE)
#all looks good

prod_avg_mod_log.rg <- update(ref_grid(prod_avg_mod_log),  tran = "log") #back-transform estimates by telling it we log-transformed

prod_avg_mod_log.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# high / low       1.1     0.547      1.81

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#so seabird has 1.1 higher coral production than ratty (but 95% HPDI = 0.547 to 1.81)

prod_avg_mod_log.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
# low        10.8      6.83      15.6
#high       11.9      7.51      17.5

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#lots of overlap

hypothesis(prod_avg_mod_log, "statushigh>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1  (statushigh) > 0      0.1      0.29    -0.36     0.56       1.87      0.65  

# carbonate production similar between sites when using average growth rates

### Carbonate production using site-specific rates ----------
prod_sites_mod_log<- brm(
  log(coral_prod_sites)~status,
  data = salomon, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  # control = list(adapt_delta = 0.999, max_treedepth = 15),  #don't need this because super simple models
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/prod_sites_mod_log") 
print(prod_sites_mod_log) #0 divergent trans

pp_check(prod_sites_mod_log)
plot(prod_sites_mod_log, ask = FALSE)
#all looks good

prod_sites_mod_log.rg <- update(ref_grid(prod_sites_mod_log),  tran = "log") #back-transform estimates by telling it we log-transformed

prod_sites_mod_log.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# high / low      2.17     0.992      3.58

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#so seabird has 2.17 higher production than ratty ( 95% HPDI = 0.992 to 3.58)

prod_sites_mod_log.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
# low        7.12      4.34      10.3
#high      15.47      9.65      22.6

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

hypothesis(prod_sites_mod_log, "statushigh>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1   (statushigh) > 0     0.78       0.3     0.31     1.24     120.21      0.99    *

# very strong evidence for positive seabird effect


### Carbonate production using 2.2 multiplication factor----------
prod_2.2_mod_log<- brm(
  log(coral_prod_2.2)~status,
  data = salomon, 
  iter = 3000, warmup = 1000, chains = 4, cores = 4,
  # control = list(adapt_delta = 0.999, max_treedepth = 15),  #don't need this because super simple models
  sample_prior="yes",
  prior = pr_default_cauchy,
  file = "output/prod_2.2_mod_log") 
print(prod_2.2_mod_log) #0 divergent trans

pp_check(prod_2.2_mod_log)
plot(prod_2.2_mod_log, ask = FALSE)
#all looks good

prod_2.2_mod_log.rg <- update(ref_grid(prod_2.2_mod_log),  tran = "log") #back-transform estimates by telling it we log-transformed

prod_2.2_mod_log.rg%>%
  emmeans("status", 
          type="response")%>%
  contrast("revpairwise")

#contrast         ratio lower.HPD upper.HPD
# high / low      2.13      1.17      3.28

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#so seabird has 2.13 higher coral cover than ratty (but 95% HPDI = 1.17 to 3.28)

prod_2.2_mod_log.rg %>% 
  emmeans(~ status,
          type = "response") 
#status   emmean lower.HPD upper.HPD
#low        7.13      4.86      9.84
#high      15.22     10.39     20.94

#Point estimate displayed: median 
#Results are back-transformed from the log scale 
#HPD interval probability: 0.95 

#no overlap

hypothesis(prod_2.2_mod_log, "statushigh>0")
#         Hypothesis Estimate Est.Error CI.Lower CI.Upper Evid.Ratio Post.Prob Star
#1   (statushigh) > 0     0.76      0.24     0.36     1.15        249         1    *

# very strong evidence for positive seabird effect



#####
### PLOT EFFECT SIZES FOR carbonate production-------
###

prod_2.2_spread<-prod_2.2_mod_log %>%
  spread_draws(b_statushigh)%>%
  mutate(exp_statushigh = exp(b_statushigh))

prod_sites_spread<-prod_sites_mod_log %>%
  spread_draws(b_statushigh)%>%
  mutate(exp_statushigh = exp(b_statushigh))

prod_avg_spread<-prod_avg_mod_log %>%
  spread_draws(b_statushigh)%>%
  mutate(exp_statushigh = exp(b_statushigh))

cover_spread<-cover_mod_log %>%
  spread_draws(b_statushigh)%>%
  mutate(exp_statushigh = exp(b_statushigh))

##now plot: 
reef_budget_plot<-
  prod_2.2_spread%>%
  ggplot() +
  geom_vline(xintercept = 1, linetype = "dashed") +
  stat_halfeye(aes(y = 1, x = exp_statushigh), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.5, fatten_point = 2, colour ="#2e5174", fill = "#2e5174") +  
  stat_halfeye(data = prod_sites_spread, aes(y = 2, x = exp_statushigh), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.5, fatten_point = 2, colour ="#2e5174", fill = "#2e5174") +  
  stat_halfeye(data = prod_avg_spread, aes(y = 3, x = exp_statushigh), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.5, fatten_point = 2, colour ="#2e5174", fill = "#2e5174") +  
  stat_halfeye(data = cover_spread, aes(y = 4, x = exp_statushigh), point_interval=median_hdi, .width=c(.95),  slab_alpha = 0.5, fatten_point = 2, colour ="#2e5174", fill = "#2e5174") +  
  theme_bw() + 
  #xlab("Estimated seabird effect on carbonate production (multiplicative)")+
  xlab(NULL)+
  ylab(NULL)+
  xlim(c(0,6))+
  scale_y_continuous(labels = c("multiplied by seabird effect", "site-specific growth rates", "average growth rates", "Coral cover"),
                     breaks = c(1,2,3,4))+
  theme(panel.grid.major = element_blank(), # remove gridlines
        panel.grid.minor = element_blank(), #remove gridlines
        strip.background = element_blank(), 
        legend.position = "none",  
        rect = element_rect(fill = "transparent"),  
        plot.background = element_rect(fill = "transparent", color = NA))
reef_budget_plot

ggsave("figures/FigS2B_RB_effect.pdf", reef_budget_plot, width = 5, height = 4)
ggsave( "figures/Fig_S2B_RB_effect.jpg", reef_budget_plot, width = 5, height = 4)

FigS2_effect_sizes <- (combined_plot + reef_budget_plot) + 
  plot_layout(ncol = 2) + 
  plot_annotation(tag_levels="A")

FigS2_effect_sizes <- FigS2_effect_sizes + plot_annotation(
  caption = "Estimated seabird effect (multiplicative)",
  theme = theme(
    plot.caption = element_text(hjust = 0.5, size = 12)))
FigS2_effect_sizes

ggsave("figures/FigS2_effect_size.pdf", FigS2_effect_sizes, width = 10, height = 5)
ggsave( "figures/FigS2_effect_size.jpg", FigS2_effect_sizes, width = 10, height = 5)

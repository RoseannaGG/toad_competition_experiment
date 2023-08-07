################## R code for by Roseanna Gamlen-Greene 06-08-2023
#roseanna.gamlen.greene@gmail.com


### MODELS FOR PAPER - 29-05-23
model_MJ_GR6						
model_MJ_AW6
model_MJ_MD6

#for UNPUBLISHED paper by Gamlen-Greene, Bufford, Todd and Richardson 


#R version 4.0.2 (2020-06-22) -- "Taking Off Again"
#Copyright (C) 2020 The R Foundation for Statistical Computing
#Platform: x86_64-w64-mingw32/x64 (64-bit)

setwd("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22")

rm(list=ls())




######################
######### PACKAGES #########
#######################

library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(plyr)
library(dplyr)
library(visreg) #
library(lme4) #
library(nlme)
library(MuMIn)#
library(lmerTest) #
library(sjPlot)#
library(sjmisc)#
library(effects)#
library(car)#
library(optimx)#
library(arules)#
library(rstanarm)#
library(tidyr)
library(dataRetrieval)#
library(cowplot)#
library(boot)
library(rstanarm)
library(utf8)
library(emmeans)#
library(naniar) #
library(phia) #
library(multcomp) #
library(data.table)
library(tidyverse)
library(RCurl) #
library(afex)#
library(tibble)
library(ggstatsplot) #
library(metafor) #
library(svglite)
library(outliers) #"REMEMBER TO DELETE"
library(performance) # colinearity
library(ggpubr)





rm(list=ls())

#################################
######## IMPORT DATA #################
#########################################

## 540- no data removed
Rawdata_experimentAugust2018_nolow<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/Rawdata_540_06_08_23.csv",header=T,row.names=NULL,sep=",")
dim(Rawdata_experimentAugust2018_nolow) #540




## name new name
Rawdata_experimentAugust2018_nolow$Competitor_Identity<-Rawdata_experimentAugust2018_nolow$Competition_level
Rawdata_experimentAugust2018_nolow$Competitor_Identity<-as.character(Rawdata_experimentAugust2018_nolow$Competitor_Identity)
Rawdata_experimentAugust2018_nolow$Competitor_Identity[Rawdata_experimentAugust2018_nolow$Competitor_Identity == "High_control"] <- "Toad_control" 
Rawdata_experimentAugust2018_nolow$Competitor_Identity[Rawdata_experimentAugust2018_nolow$Competitor_Identity== "FrogHG"] <- "NRLF_HG" 
Rawdata_experimentAugust2018_nolow$Competitor_Identity[Rawdata_experimentAugust2018_nolow$Competitor_Identity== "FrogML"] <- "NRLF_ML" 
Rawdata_experimentAugust2018_nolow$Competitor_Identity<-as.factor(Rawdata_experimentAugust2018_nolow$Competitor_Identity)




###################################
## add a couple of columns #####
####################################

## turn ave weight to mg
Rawdata_experimentAugust2018_nolow$Av_juv_biomass_mg<-Rawdata_experimentAugust2018_nolow$Av_juv_biomass*1000


## days amphibians have been tadpoles (from breeidng day to metamorphosis) (days)
Rawdata_experimentAugust2018_nolow$Larvalperiodsincebreed<-Rawdata_experimentAugust2018_nolow$Daysbeforestartedexpmt+Rawdata_experimentAugust2018_nolow$Days


#### percapita tadpole additions due to diving beetle predation
Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita<-(Rawdata_experimentAugust2018_nolow$Tadpoles_added_as_top.up_june7thand8th/Rawdata_experimentAugust2018_nolow$Total_number_individuals_start)



#### tadpole additions due to diving beetle predation (same as Tadpoles_added_as_top.up_june7thand8th - just dif name)
Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th<-Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita*Rawdata_experimentAugust2018_nolow$Total_number_individuals_start

#### Totalindivstart_PLUS_toadTadpoles_added_as_top.up_june7thand8th
Rawdata_experimentAugust2018_nolow$Totalindivstart_PLUS_toadTadpoles_added_as_top.up_june7thand8th<-Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th+Rawdata_experimentAugust2018_nolow$Total_number_individuals_start


# Count_juv_emerged_percapita
Rawdata_experimentAugust2018_nolow$Count_juv_emerged_percapita<-Rawdata_experimentAugust2018_nolow$Count_juv_emerged/Rawdata_experimentAugust2018_nolow$Totalindivstart_PLUS_toadTadpoles_added_as_top.up_june7thand8th





######## add mortality #################

#### NB mortality calculations are in section 4. Toad Mortality   below #####

Mortalitydataset_nolow_26_06_2021<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/Mortalitydataset_nolow_26_06_2021-06-08-23.csv",header=T,row.names=NULL,sep=",")


##############
Mortalitydataset_nolow_26_06_2021

Rawdata_experimentAugust2018_nolow<-left_join(Mortalitydataset_nolow_26_06_2021,Rawdata_experimentAugust2018_nolow,by="Tank")





## make sure all random effects are factors
#days as factor


levels(Rawdata_experimentAugust2018_nolow$Daysfactor)

levels(Rawdata_experimentAugust2018_nolow$Population)


Rawdata_experimentAugust2018_nolow$Tank_factor<-as.factor((Rawdata_experimentAugust2018_nolow$Tank))

levels(Rawdata_experimentAugust2018_nolow$Tank_factor)


Rawdata_experimentAugust2018_nolow$Block_factor<-as.factor((Rawdata_experimentAugust2018_nolow$Block))

Rawdata_experimentAugust2018_nolow$Region_factor<-as.factor((Rawdata_experimentAugust2018_nolow$Region))

levels(Rawdata_experimentAugust2018_nolow$Region_factor)


Rawdata_experimentAugust2018_nolow$Population_factor<-as.factor((Rawdata_experimentAugust2018_nolow$Population))

levels(Rawdata_experimentAugust2018_nolow$Population_factor)




#############################
#### outliers #########
############


#Create a boxplot that labels the outliers  
ggbetweenstats(Rawdata_experimentAugust2018_nolow,
               Competitor_Identity, Av_juv_biomass, outlier.tagging = TRUE)




outlier(Rawdata_experimentAugust2018_nolow$Av_juv_biomass)

boxplot(Rawdata_experimentAugust2018_nolow$Av_juv_biomass)

#Create a boxplot that labels the outliers  
ggbetweenstats(Rawdata_experimentAugust2018_nolow,
               Competitor_Identity, Av_juv_biomass, outlier.tagging = TRUE)


Q <- quantile(Rawdata_experimentAugust2018_nolow$Av_juv_biomass, probs=c(.25, .75), na.rm = TRUE)

iqr <- IQR(Rawdata_experimentAugust2018_nolow$Av_juv_biomass,na.rm = TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

eliminated_Av_juv_biomass<- subset(Rawdata_experimentAugust2018_nolow, Rawdata_experimentAugust2018_nolow$Av_juv_biomass > (Q[1] - 1.5*iqr) & Rawdata_experimentAugust2018_nolow$Av_juv_biomass < (Q[2]+1.5*iqr))

dim(eliminated_Av_juv_biomass) #425  55


ggbetweenstats(eliminated_Av_juv_biomass,Competitor_Identity, Av_juv_biomass, outlier.tagging = TRUE)



## take out one small outlier
#### dataset I used
Rawdata_experimentAugust2018_nolow<-Rawdata_experimentAugust2018_nolow[!(Rawdata_experimentAugust2018_nolow$Av_juv_biomass <= 0.009 & !is.na(Rawdata_experimentAugust2018_nolow$Av_juv_biomass)),]
dim(Rawdata_experimentAugust2018_nolow) #







##############################################
# average starting weights + percent metamorphosed ##########
##################################################



dim(Rawdata_experimentAugust2018_nolow) #714  59

#average staring weight toad tadpoles (g)
Rawdata_experimentAugust2018_nolow %>%
  group_by(Region) %>%
  summarise(mean_run = mean(X25.5.18_experiment_start_tapole_av_individual_weight))


#percent metamorphosed by end

subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_07_06_21/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23.csv",header=T,row.names=NULL,sep=",")




sum(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled)


dim(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23) #72 238

## NB this is how it was calc
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled<- (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Running_total_of_adults_emerged+                                                                                  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_tadpoles+
                                                                                  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_metamorphs+                                                                       subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_number_of_metamorphs)


### number unique starting weights - 70 unique starting weights weights
df_uniq <- unique(Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight)
length(df_uniq)



# average meta across tanks was 0.5748264
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start)

# distribution % metamorphosed across tanks
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start*100)


#png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/hist_precent_metamorphosed_pertank_atend_72.png", width = 200, height = 150, units = 'mm', res = 600)
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start*100)
#dev.off()

#svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/hist_precent_metamorphosed_pertank_atend_72.svg", width = 200, height = 150)
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start*100)
#dev.off()

subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Percent_metemorphosedbyend<-subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start*100



## including toads added as top up
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$toadTadpoles_added_as_top.up_june7thand8th<-subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$toadTadpoles_added_as_top.up_june7thand8th_percapita*subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start


subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Totalindivstart_PLUS_toadTadpoles_added_as_top.up_june7thand8th<-subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$toadTadpoles_added_as_top.up_june7thand8th+subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start



subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Percent_metemorphosedbyend_tadsadded<-(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Survived_counts_weighedandkilled/(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start+subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$toadTadpoles_added_as_top.up_june7thand8th))*100

mean(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Percent_metemorphosedbyend_tadsadded)
# 50.16079


#png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/hist_precent_metamorphosed_tadsadded_pertank_atend_72.png", width = 200, height = 150, units = 'mm', res = 600)
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Percent_metemorphosedbyend_tadsadded)
#dev.off()




#############################
### rename some variables
###############################
Rawdata_experimentAugust2018_nolow$Block<-as.factor(Rawdata_experimentAugust2018_nolow$Block)
Rawdata_experimentAugust2018_nolow$Population<-as.factor(Rawdata_experimentAugust2018_nolow$Population)
Rawdata_experimentAugust2018_nolow$Tank<-as.factor(Rawdata_experimentAugust2018_nolow$Tank)




##########
########### add new treatments ############
##########################################



Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History<-Rawdata_experimentAugust2018_nolow$Region
Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History<-as.character(Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History)
Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History[Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History == "HG"] <- "Short" 
Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History[Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History== "ML"] <- "Long" 
Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History<-as.factor(Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History)




Rawdata_experimentAugust2018_nolow$Competition_Treatment<-Rawdata_experimentAugust2018_nolow$Competitor_Identity


Rawdata_experimentAugust2018_nolow$Density<-Rawdata_experimentAugust2018_nolow$Total_number_individuals_start






Rawdata_experimentAugust2018_nolow$Toad_region<-Rawdata_experimentAugust2018_nolow$Region
Rawdata_experimentAugust2018_nolow$Toad_density<-Rawdata_experimentAugust2018_nolow$Density





#####################################
######## CORRECT SCALING  i.e from 0 to 1 ############
########################

Rawdata_experimentAugust2018_nolow$Mean_Average_Water_Temperature_Scaled<-(Rawdata_experimentAugust2018_nolow$Mean_averageTemp-min(Rawdata_experimentAugust2018_nolow$Mean_averageTemp))/(max(Rawdata_experimentAugust2018_nolow$Mean_averageTemp)-min(Rawdata_experimentAugust2018_nolow$Mean_averageTemp))


Rawdata_experimentAugust2018_nolow$Toad_Tadpoles_Top_up_Two_Weeks_per_Capita_Scaled<-(Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita-min(Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita))/(max(Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita)-min(Rawdata_experimentAugust2018_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita))


Rawdata_experimentAugust2018_nolow$Average_Individual_Toad_Tadpole_Starting_Weight_Scaled<-(Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight-min(Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight))/(max(Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight)-min(Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight))


Rawdata_experimentAugust2018_nolow$Toad_Mortality_per_Capita_Scaled<-(Rawdata_experimentAugust2018_nolow$Mortality_06_05_20_percapita-min(Rawdata_experimentAugust2018_nolow$Mortality_06_05_20_percapita))/(max(Rawdata_experimentAugust2018_nolow$Mortality_06_05_20_percapita)-min(Rawdata_experimentAugust2018_nolow$Mortality_06_05_20_percapita))


Rawdata_experimentAugust2018_nolow$Days_Since_Experiment_Began_Scaled<-(Rawdata_experimentAugust2018_nolow$Days-min(Rawdata_experimentAugust2018_nolow$Days))/(max(Rawdata_experimentAugust2018_nolow$Days)-min(Rawdata_experimentAugust2018_nolow$Days))




########################################################
#########################################################
############### 1. TOAD GROWTH RATE ##########
##################################################
######################################################

#### VERY IMPORTANT FOR CUSTOM CONTRASTS #####
#order levels of factor
Rawdata_experimentAugust2018_nolow$Region<- factor(Rawdata_experimentAugust2018_nolow$Region, levels = c("HG","ML"))
Rawdata_experimentAugust2018_nolow<- droplevels(Rawdata_experimentAugust2018_nolow)


############ calculate toad growth rate
Rawdata_experimentAugust2018_nolow$GR_finalmin_intial_degreedays_mgDD<-((Rawdata_experimentAugust2018_nolow$Av_juv_biomass-Rawdata_experimentAugust2018_nolow$X25.5.18_experiment_start_tapole_av_individual_weight)/Rawdata_experimentAugust2018_nolow$SumDD)*1000


############ calculate toad growth rate - Richter Boix 2011
Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay<-(Rawdata_experimentAugust2018_nolow$Av_juv_biomass/Rawdata_experimentAugust2018_nolow$Days)*1000



# log (natural log!!!) ## not what i used
Rawdata_experimentAugust2018_nolow$log_GR_final_days_mgDay<-log(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)
hist(Rawdata_experimentAugust2018_nolow$log_GR_final_days_mgDay)


# log10
Rawdata_experimentAugust2018_nolow$log10_GR_final_days_mgDay<-log10(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)

### growth rate as richtoer biox
Rawdata_experimentAugust2018_nolow$GR_final_larvalperiod_mgDay<-(Rawdata_experimentAugust2018_nolow$Av_juv_biomass/Rawdata_experimentAugust2018_nolow$Larvalperiodsincebreed)*1000


# log10
Rawdata_experimentAugust2018_nolow$log10_GR_final_larvalperiod_mgDay<-log10(Rawdata_experimentAugust2018_nolow$GR_final_larvalperiod_mgDay)






### add column that is the number of animals left in tank at time of weighing (i.e. last number weighed )

plot(Rawdata_experimentAugust2018_nolow$Days,Rawdata_experimentAugust2018_nolow$Count_juv_emerged)

##############################################################
########## transformations ############
######  test normality #########
qqPlot(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)
hist(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)


shapiro.test(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)


# log
# Rawdata_experimentAugust2018_nolow$log1-_GR_final_days_mgDay<-log10(Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay)
hist(Rawdata_experimentAugust2018_nolow$log10_GR_final_days_mgDay)

qqPlot(Rawdata_experimentAugust2018_nolow$log10_GR_final_days_mgDay)
hist(Rawdata_experimentAugust2018_nolow$log10_GR_final_days_mgDay)


shapiro.test(Rawdata_experimentAugust2018_nolow$log10_GR_final_days_mgDay)





### get means and standard errors
Rawdata_experimentAugust2018_nolow_noNA<-Rawdata_experimentAugust2018_nolow[!(Rawdata_experimentAugust2018_nolow$Av_juv_biomass_mg=="NA"),]

dim(Rawdata_experimentAugust2018_nolow_noNA)

GR_final_days_mgDay_539 <- Rawdata_experimentAugust2018_nolow %>%
  group_by(Region,Competitor_Identity)

GR_final_days_mgDay_539<-GR_final_days_mgDay_539 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), GR_final_days_mgDay)

GR_final_days_mgDay_539<-as.data.frame(GR_final_days_mgDay_539)
GR_final_days_mgDay_539

write.csv(GR_final_days_mgDay_539,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/Mean_sd_se_GR_final_days_mgDay_539.csv")






#######################################
############# LMMR - TOAD GROWTH RATE ############
#########################################


### order factors this way.... key!!!!!!!
Rawdata_experimentAugust2018_nolow$Competitor_Identity<- factor(Rawdata_experimentAugust2018_nolow$Competitor_Identity, levels = c("Toad_control","NRLF_HG","NRLF_ML"))
Rawdata_experimentAugust2018_nolow<- droplevels(Rawdata_experimentAugust2018_nolow)


Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History<- factor(Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History, levels = c("Long","Short"))
Rawdata_experimentAugust2018_nolow<- droplevels(Rawdata_experimentAugust2018_nolow)


summary(Rawdata_experimentAugust2018_nolow)


Rawdata_experimentAugust2018_nolow$Block<-as.factor(Rawdata_experimentAugust2018_nolow$Block)
Rawdata_experimentAugust2018_nolow$Population<-as.factor(Rawdata_experimentAugust2018_nolow$Population)
Rawdata_experimentAugust2018_nolow$Tank<-as.factor(Rawdata_experimentAugust2018_nolow$Tank)

### BEST 16_02_22
#### model_MJ_GR6 ####
model_MJ_GR6<-lmer(log10_GR_final_days_mgDay~
                     Toad_Cooccurrence_History*Competitor_Identity+ 
                     Toad_Tadpoles_Top_up_Two_Weeks_per_Capita_Scaled+ 
                     Average_Individual_Toad_Tadpole_Starting_Weight_Scaled+
                     Toad_Mortality_per_Capita_Scaled+
                     Days_Since_Experiment_Began_Scaled+
                  Block+
                    Mean_Average_Water_Temperature_Scaled+
                  #(1|Daysfactor/Tank_factor)+
                  (1+Days_Since_Experiment_Began_Scaled|Tank)+
                  (1|Population), 
                #(1|Block_factor), #random effects
                data=Rawdata_experimentAugust2018_nolow,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(model_MJ_GR6)
car::Anova(model_MJ_GR6)

AIC(model_MJ_GR6) #  -695.8682
summary(model_MJ_GR6)



#539
capture.output(anova(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/anova_growthrate_model_MJ_GR6_539.txt")
write.csv(anova(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/anova_growthrate_model_MJ_GR6_539.csv")
write.csv(Anova(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/Anova_chisq_growthrate_model_MJ_GR6_539.csv")
capture.output(Anova(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/Anova_chisq_growthrate_model_MJ_GR6_539.txt")
capture.output(summary(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/summary_growthrate_model_MJ_GR6_539.txt")

capture.output(summary(model_MJ_GR6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/summary_growthrate_model_MJ_GR6_539.csv")


tab_model(model_MJ_GR6,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/model_tab_growthrate_model_MJ_GR6_539.doc")



### test residuals - mostly ok
qqnorm(resid(model_MJ_GR6))
hist(resid(model_MJ_GR6))
plot(model_MJ_GR6)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/Residualsplot_model_MJ_GR6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_MJ_GR6)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/Residualshist_model_MJ_GR6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_MJ_GR6))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/QQnorm_model_MJ_GR6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_MJ_GR6))
dev.off()


## interaction means
(model_MJ_GR6.means <- interactionMeans(model_MJ_GR6))
plot(model_MJ_GR6.means)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/interactionMeans_model_MJ_GR6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_MJ_GR6.means <- interactionMeans(model_MJ_GR6))
plot(model_MJ_GR6.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/interactionMeans_model_MJ_GR6_539.svg", width = 12, height = 6.5)
(model_MJ_GR6.means <- interactionMeans(model_MJ_GR6))
plot(model_MJ_GR6.means)
dev.off()


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/visreg_model_MJ_GR6_539.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_GR6)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/visreg_model_MJ_GR6_539.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_GR6)
dev.off()

## vis reg plots one at a time
par(mfrow=c(1,1))
par(mar = c(10, 5,2, 2))
#visreg(model_MJ_GR6)





L.S_model_MJ_GR6_frog <- pairs(lsmeans(model_MJ_GR6, ~ Toad_Cooccurrence_History | Competitor_Identity))
test(L.S_model_MJ_GR6_frog, adjust = "tukey")

capture.output(test(L.S_model_MJ_GR6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/L.S_model_MJ_GR6_frog_539.txt")
write.csv(test(L.S_model_MJ_GR6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/L.S_model_MJ_GR6_frog_539.csv")


L.S_model_MJ_GR6_region <- pairs(lsmeans(model_MJ_GR6, ~ Competitor_Identity | Toad_Cooccurrence_History ))
test(L.S_model_MJ_GR6_region, adjust = "tukey")


capture.output(test(L.S_model_MJ_GR6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/L.S_model_MJ_GR6_region_539.txt")
write.csv(test(L.S_model_MJ_GR6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/L.S_model_MJ_GR6_region_539.csv")



### AVERAVE/means per treatment - not transformed data
names(Rawdata_experimentAugust2018_nolow)


Rawdata_experimentAugust2018_nolow$GR_final_days_mgDay

Rawdata_experimentAugust2018_nolow %>%
  group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
  summarise_at(vars(GR_final_days_mgDay),na.rm=TRUE, list(GR_final_days_mgDay_mean = mean))



GR_final_days_mgDay_meanDF<-as.data.frame(Rawdata_experimentAugust2018_nolow %>%
                                          group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
                                          summarise_at(vars(GR_final_days_mgDay),na.rm=TRUE, list(GR_final_days_mgDay_mean = mean)))


# differences
GR_final_days_mgDay_meanDF
str(GR_final_days_mgDay_meanDF)

# long 
#control - frog HG
GR_final_days_mgDay_meanDF[1,3] - GR_final_days_mgDay_meanDF[2,3]


# % 
((GR_final_days_mgDay_meanDF[1,3] - GR_final_days_mgDay_meanDF[2,3])/GR_final_days_mgDay_meanDF[1,3])*100



# long 
# control - frog ML
GR_final_days_mgDay_meanDF[1,3] - GR_final_days_mgDay_meanDF[3,3]



# short
# control- frog HG
GR_final_days_mgDay_meanDF[4,3] - GR_final_days_mgDay_meanDF[5,3]

# short 
# control - frog ML
GR_final_days_mgDay_meanDF[4,3] - GR_final_days_mgDay_meanDF[6,3]


# long - short
# frog HG
GR_final_days_mgDay_meanDF[2,3] - GR_final_days_mgDay_meanDF[5,3]

# % 
((GR_final_days_mgDay_meanDF[2,3] - GR_final_days_mgDay_meanDF[5,3])/GR_final_days_mgDay_meanDF[2,3])*100

# long - short
# frog ML
GR_final_days_mgDay_meanDF[3,3] - GR_final_days_mgDay_meanDF[6,3]


# % 
((GR_final_days_mgDay_meanDF[3,3] - GR_final_days_mgDay_meanDF[6,3])/GR_final_days_mgDay_meanDF[3,3])*100



GR_final_days_mgDay_meanDF[GR_final_days_mgDay_meanDF$Toad_Cooccurrence_History=="Long",GR_final_days_mgDay_meanDF$Toad_Cooccurrence_History=="Toad_control"]





################################
#################### PLOTS ######
################################





colabels <- c(Long = "Long co-occurrence history", Short = "Short co-occurrence history")
Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History = factor(Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History, levels=c('Short','Long'))


#### beautiful
#### 4 comp no facet
png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet_539_600res.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet<- Rawdata_experimentAugust2018_nolow%>%
    ggplot(aes(x=Competitor_Identity, y=GR_final_days_mgDay,fill=Toad_Cooccurrence_History)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("Short", "Long"),
                      labels=c("Short", "Long"))+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    #coord_cartesian(ylim = ylim1*1.1)+
    labs(x="Competition Treatment")+
    labs(y="Toad growth rate (mg/day)")+
    scale_x_discrete(breaks=c( "Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("High density\nToad control","NRLFs\nMainland","NRLFs\nHaida Gwaii" ))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GrowthRate/boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet_539_600res.svg", width = 200, height = 150, units = 'mm')
(boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet<- Rawdata_experimentAugust2018_nolow%>%
    ggplot(aes(x=Competitor_Identity, y=GR_final_days_mgDay,fill=Toad_Cooccurrence_History)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("Short", "Long"),
                      labels=c("Short", "Long"))+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    #coord_cartesian(ylim = ylim1*1.1)+
    labs(x="Competition Treatment")+
    labs(y="Toad growth rate (mg/day)")+
    scale_x_discrete(breaks=c( "Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("Toad high\ncontrol","NRLFs\nMainland","NRLFs\nHaida Gwaii" ))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()







##############################################################################
##############################################################################
########## 2. Toad weight at metamorphosis ###################################
##############################################################################
##############################################################################

#### VERY IMPORTANT FOR CUSTOM CONTRASTS #####
#order levels of factor
Rawdata_experimentAugust2018$Region<- factor(Rawdata_experimentAugust2018$Region, levels = c("HG","ML"))
Rawdata_experimentAugust2018<- droplevels(Rawdata_experimentAugust2018)




######  test normality #########
qqPlot(Rawdata_experimentAugust2018$Av_juv_biomass)
hist(Rawdata_experimentAugust2018$Av_juv_biomass)
shapiro.test(Rawdata_experimentAugust2018$Av_juv_biomass)

#log - looks good



Rawdata_experimentAugust2018$log10_Av_juv_biomass<-log10(Rawdata_experimentAugust2018$Av_juv_biomass)


Rawdata_experimentAugust2018$log10_Av_juv_biomass_mg<-log10(Rawdata_experimentAugust2018$Av_juv_biomass_mg)




png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/hist_log10_Av_juv_biomass_mg_model_AW23_539.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(Rawdata_experimentAugust2018$log10_Av_juv_biomass_mg)
dev.off()



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/hist_Av_juv_biomass_mg_539.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(Rawdata_experimentAugust2018$Av_juv_biomass_mg)
dev.off()





dim(Rawdata_experimentAugust2018) #539  87





#Rawdata_experimentAugust2018_nolow$log10_Av_juv_biomass<-log10(Rawdata_experimentAugust2018_nolow$Av_juv_biomass)


#Rawdata_experimentAugust2018_nolow$log10_Av_juv_biomass_mg<-log10(Rawdata_experimentAugust2018_nolow$Av_juv_biomass_mg)






### get means and standard errors
Rawdata_experimentAugust2018_nolow_noNA<-Rawdata_experimentAugust2018_nolow[!(Rawdata_experimentAugust2018_nolow$Av_juv_biomass_mg=="NA"),]

dim(Rawdata_experimentAugust2018_nolow_noNA)

avweight_539 <- Rawdata_experimentAugust2018_nolow_noNA %>%
  group_by(Region,Competitor_Identity)

avweight_539<-avweight_539 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), Av_juv_biomass_mg)

avweight_539<-as.data.frame(avweight_539)
avweight_539

write.csv(avweight_539,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/Mean_sd_se_avjuvweight_539.csv")



##############################################################
############# LMMR - TOAD WEIGHT AT METAMORPHOSIS ##########################################################
##########################################



### order factors this way.... key!!!!!!!
Rawdata_experimentAugust2018_nolow$Competitor_Identity<- factor(Rawdata_experimentAugust2018_nolow$Competitor_Identity, levels = c("Toad_control","NRLF_HG","NRLF_ML"))
Rawdata_experimentAugust2018_nolow<- droplevels(Rawdata_experimentAugust2018_nolow)


Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History<- factor(Rawdata_experimentAugust2018_nolow$Toad_Cooccurrence_History, levels = c("Long","Short"))
Rawdata_experimentAugust2018_nolow<- droplevels(Rawdata_experimentAugust2018_nolow)


summary(Rawdata_experimentAugust2018_nolow)


Rawdata_experimentAugust2018_nolow$Block<-as.factor(Rawdata_experimentAugust2018_nolow$Block)
Rawdata_experimentAugust2018_nolow$Population<-as.factor(Rawdata_experimentAugust2018_nolow$Population)
Rawdata_experimentAugust2018_nolow$Tank<-as.factor(Rawdata_experimentAugust2018_nolow$Tank)


## best 16_02_22
model_MJ_AW6<-lmer(log10_Av_juv_biomass_mg~
                     Toad_Cooccurrence_History*Competitor_Identity+ 
                     Toad_Tadpoles_Top_up_Two_Weeks_per_Capita_Scaled+ 
                     Average_Individual_Toad_Tadpole_Starting_Weight_Scaled+
                     Toad_Mortality_per_Capita_Scaled+
                     Days_Since_Experiment_Began_Scaled+
                     Block+
                     Mean_Average_Water_Temperature_Scaled+
                     #(1|Daysfactor/Tank_factor)+
                     (1+Days_Since_Experiment_Began_Scaled|Tank)+
                     (1|Population), 
                   #(1|Block_factor), #random effects
                   data=Rawdata_experimentAugust2018_nolow,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

anova(model_MJ_AW6)
car::Anova(model_MJ_AW6)

AIC(model_MJ_AW6) #  -701.7067
summary(model_MJ_AW6)



#539
capture.output(anova(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/anova_WeightatMetamorphosis_model_MJ_AW6_539.txt")
write.csv(anova(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/anova_WeightatMetamorphosis_model_MJ_AW6_539.csv")
write.csv(Anova(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/Anova_chisq_WeightatMetamorphosis_model_MJ_AW6_539.csv")
capture.output(Anova(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/Anova_chisq_WeightatMetamorphosis_model_MJ_AW6_539.txt")
capture.output(summary(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/summary_WeightatMetamorphosis_model_MJ_AW6_539.txt")

capture.output(summary(model_MJ_AW6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/summary_WeightatMetamorphosis_model_MJ_AW6_539.csv")


tab_model(model_MJ_AW6,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/model_tab_WeightatMetamorphosis_model_MJ_AW6_539.doc")



### test residuals - mostly ok
qqnorm(resid(model_MJ_AW6))
hist(resid(model_MJ_AW6))
plot(model_MJ_AW6)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/Residualsplot_model_MJ_AW6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_MJ_AW6)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/Residualshist_model_MJ_AW6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_MJ_AW6))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/QQnorm_model_MJ_AW6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_MJ_AW6))
dev.off()


## interaction means
(model_MJ_AW6.means <- interactionMeans(model_MJ_AW6))
plot(model_MJ_AW6.means)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/interactionMeans_model_MJ_AW6_539.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_MJ_AW6.means <- interactionMeans(model_MJ_AW6))
plot(model_MJ_AW6.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/interactionMeans_model_MJ_AW6_539.svg", width = 12, height = 6.5)
(model_MJ_AW6.means <- interactionMeans(model_MJ_AW6))
plot(model_MJ_AW6.means)
dev.off()


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/visreg_model_MJ_AW6_539.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_AW6)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/visreg_model_MJ_AW6_539.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_AW6)
dev.off()

## vis reg plots one at a time
par(mfrow=c(1,1))
par(mar = c(10, 5,2, 2))
#visreg(model_MJ_AW6)





L.S_model_MJ_AW6_frog <- pairs(lsmeans(model_MJ_AW6, ~ Toad_Cooccurrence_History | Competitor_Identity))
test(L.S_model_MJ_AW6_frog, adjust = "tukey")

capture.output(test(L.S_model_MJ_AW6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/L.S_model_MJ_AW6_frog_539.txt")
write.csv(test(L.S_model_MJ_AW6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/L.S_model_MJ_AW6_frog_539.csv")


L.S_model_MJ_AW6_region <- pairs(lsmeans(model_MJ_AW6, ~ Competitor_Identity | Toad_Cooccurrence_History ))
test(L.S_model_MJ_AW6_region, adjust = "tukey")


capture.output(test(L.S_model_MJ_AW6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/L.S_model_MJ_AW6_region_539.txt")
write.csv(test(L.S_model_MJ_AW6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/L.S_model_MJ_AW6_region_539.csv")







### means per treatment - not transformed data
names(Rawdata_experimentAugust2018_nolow)

Rawdata_experimentAugust2018_nolow$Av_juv_biomass_mg


Rawdata_experimentAugust2018_nolow %>%
  group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
  summarise_at(vars(Av_juv_biomass_mg),na.rm=TRUE, list(Av_juv_biomass_mg_mean = mean))



Av_juv_biomass_mg_meanDF<-as.data.frame(Rawdata_experimentAugust2018_nolow %>%
  group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
  summarise_at(vars(Av_juv_biomass_mg),na.rm=TRUE, list(Av_juv_biomass_mg_mean = mean)))


# differences
Av_juv_biomass_mg_meanDF
str(Av_juv_biomass_mg_meanDF)

# long 
#control - frog HG
Av_juv_biomass_mg_meanDF[1,3] - Av_juv_biomass_mg_meanDF[2,3]


# % 
((Av_juv_biomass_mg_meanDF[1,3] - Av_juv_biomass_mg_meanDF[2,3])/Av_juv_biomass_mg_meanDF[1,3])*100



# long 
# control - frog ML
Av_juv_biomass_mg_meanDF[1,3] - Av_juv_biomass_mg_meanDF[3,3]



# short
# control- frog HG
Av_juv_biomass_mg_meanDF[4,3] - Av_juv_biomass_mg_meanDF[5,3]

# short 
# control - frog ML
Av_juv_biomass_mg_meanDF[4,3] - Av_juv_biomass_mg_meanDF[6,3]


# long - short
# frog HG
Av_juv_biomass_mg_meanDF[2,3] - Av_juv_biomass_mg_meanDF[5,3]

# % 
((Av_juv_biomass_mg_meanDF[2,3] - Av_juv_biomass_mg_meanDF[5,3])/Av_juv_biomass_mg_meanDF[2,3])*100

# long - short
# frog ML
Av_juv_biomass_mg_meanDF[3,3] - Av_juv_biomass_mg_meanDF[6,3]


Av_juv_biomass_mg_meanDF[Av_juv_biomass_mg_meanDF$Toad_Cooccurrence_History=="Long",Av_juv_biomass_mg_meanDF$Toad_Cooccurrence_History=="Toad_control"]










#################################
#################### PLOTS ######
################################




png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet_539_600res.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet<- Rawdata_experimentAugust2018_nolow%>%
    ggplot(aes(x=Competitor_Identity, y=Av_juv_biomass_mg,fill=Toad_Cooccurrence_History)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("Short", "Long"),
                      labels=c("Short", "Long"))+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    #coord_cartesian(ylim = ylim1*1.1)+
    labs(x="Competition Treatment")+
    labs(y="Average toad weight\nat metamorphosis (mg)")+
    scale_x_discrete(breaks=c( "Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("High density\nToad control","NRLFs\nMainland","NRLFs\nHaida Gwaii" ))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/WeightatMetamorphosis/boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet_539_600res.svg", width = 200, height = 150, units = 'mm')
(boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet<- Rawdata_experimentAugust2018_nolow%>%
    ggplot(aes(x=Competitor_Identity, y=Av_juv_biomass_mg,fill=Toad_Cooccurrence_History)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("Short", "Long"),
                      labels=c("Short", "Long"))+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    #coord_cartesian(ylim = ylim1*1.1)+
    labs(x="Competition Treatment")+
    labs(y="Average toad weight at metamorphosis (mg)")+
    scale_x_discrete(breaks=c( "Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("Toad high\ncontrol","NRLFs\nMainland","NRLFs\nHaida Gwaii" ))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()





labels <- c(ML = "Mainland Toads", HG = "Haida Gwaii Toads")
Rawdata_experimentAugust2018$Region = factor(Rawdata_experimentAugust2018$Region, levels=c('ML','HG'))


ylim1 = boxplot.stats(Rawdata_experimentAugust2018$Av_juv_biomass)$stats[c(1, 5)]




############################################################################
############################################################################
############################# 3. MEDIAN TIME TO METAMORPHOSIS #############
#########################################################################
##################################################################################

#rm(list=ls())
### KEEP USING THE FILE USED FORGROWTH RATE AND WEIGHT AT METAMORPHOSIS!

#Rawdata_experimentAugust2018_indivrows2_median2_large_nolow<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/Rawdata_experimentAugust2018_indivrows2_median2_large_nolow_06_08_23.csv",header=T,row.names=NULL,sep=",")

#subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23.csv",header=T,row.names=NULL,sep=",")





########## how to calc MedianDays ################################
#make each toad have its own row and then run the code below
Rawdata_experimentAugust2018_nolow_indivrows2 <- as.data.frame(lapply(Rawdata_experimentAugust2018_nolow, rep, Rawdata_experimentAugust2018_nolow$Count_juv_emerged))


#cal median
Rawdata_experimentAugust2018_nolow_indivrows2_median2<-setDT(Rawdata_experimentAugust2018_nolow_indivrows2)[,list(MeanDays=mean(Days), MaxDays=max(Days), MinDays=min(Days), MedianDays=as.numeric(median(Days)), StdDays=sd(Days)), by=Tank]
dim(Rawdata_experimentAugust2018_nolow_indivrows2_median2) #70

#join to large dataset
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow<-dplyr::left_join(Rawdata_experimentAugust2018_nolow_indivrows2_median2,subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23,by="Tank")

#calc toad tadpoles percentage top up
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percentagetotal<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tadpoles_added_as_top.up_june7thand8th/Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Total_number_individuals_start)*100

#### percapita tadpole additions due to diving beetle predation
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tadpoles_added_as_top.up_june7thand8th/Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Total_number_individuals_start)


## name new name
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity<-Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competition_level
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity<-as.character(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity)
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity[Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity == "High_control"] <- "Toad_control" 
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity[Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity== "FrogHG"] <- "NRLF_HG" 
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity[Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity== "FrogML"] <- "NRLF_ML" 
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity<-as.factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity)




#Write csv
#write.csv(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow,"Rawdata_experimentAugust2018_indivrows2_median2_large_nolow.csv")

######## add mortality

##############
Mortalitydataset_26_06_2021<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/Mortalitydataset_26_06_2021.csv",header=T,row.names=NULL,sep=",")

Rawdata_experimentAugust2018_indivrows2_median2_large_nolow<-left_join(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow,Mortalitydataset_26_06_2021,by="Tank")




Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Block_factor<-as.factor((Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Block))

Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tank_factor<-as.factor((Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tank))





#####################################
######## CORRECT SCALING  i.e from 0 to 1 ############
########################

Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mean_Average_Water_Temperature_Scaled<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mean_averageTemp-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mean_averageTemp))/(max(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mean_averageTemp)-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mean_averageTemp))


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Toad_Tadpoles_Top_up_Two_Weeks_per_Capita_Scaled<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita))/(max(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita)-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$toadTadpoles_added_as_top.up_june7thand8th_percapita))


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Average_Individual_Toad_Tadpole_Starting_Weight_Scaled<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$X25.5.18_experiment_start_tapole_av_individual_weight-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$X25.5.18_experiment_start_tapole_av_individual_weight))/(max(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$X25.5.18_experiment_start_tapole_av_individual_weight)-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$X25.5.18_experiment_start_tapole_av_individual_weight))


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Toad_Mortality_per_Capita_Scaled<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mortality_06_05_20_percapita-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mortality_06_05_20_percapita))/(max(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mortality_06_05_20_percapita)-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Mortality_06_05_20_percapita))


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Days_Since_Experiment_Began_Scaled<-(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Days-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Days))/(max(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Days)-min(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Days))






######  test normality #########
qqPlot(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$MedianDays)
shapiro.test(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$MedianDays)
hist(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$MedianDays)


######################################
############ LMMER ############
#########################################



### order factors this way.... key!!!!!!!


### order factors this way.... key!!!!!!!
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity<- factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity, levels = c("Toad_control","NRLF_HG","NRLF_ML"))
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow<- droplevels(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow)
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Competitor_Identity



Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Toad_Cooccurrence_History<- factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Toad_Cooccurrence_History, levels = c("Long","Short"))
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow<- droplevels(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow)


summary(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow)


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Block<-as.factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Block)
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Population<-as.factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Population)
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tank<-as.factor(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Tank)








### BEST 16_02_22
#### model_MJ_MD6 ####
model_MJ_MD6<-lmer(MedianDays~
                           Toad_Cooccurrence_History*Competitor_Identity+ 
                           Toad_Tadpoles_Top_up_Two_Weeks_per_Capita_Scaled+ 
                           Toad_Mortality_per_Capita_Scaled+
                           Block+
                           Mean_Average_Water_Temperature_Scaled+
                           #(1|Daysfactor/Tank_factor)+
                           (1|Population), 
                         #(1|Block_factor), #random effects
                         data=Rawdata_experimentAugust2018_indivrows2_median2_large_nolow,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))



car::Anova(model_MJ_MD6)
AIC(model_MJ_MD6) #   359.4783
summary(model_MJ_MD6)


capture.output(check_collinearity(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/collinearity_mediandaysmetamorph_model_MJ_MD6_54.txt")


#54
capture.output(anova(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/anova_mediandaysmetamorph_model_MJ_MD6_54.txt")
capture.output(car::Anova(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/Anova_chisq_mediandaysmetamorph_model_MJ_MD6_54.txt")
write.csv(anova(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/anova_mediandaysmetamorph_model_MJ_MD6_54.csv")
write.csv(car::Anova(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/Anova_chisq_mediandaysmetamorph_model_MJ_MD6_54.csv")
capture.output(summary(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/summary_mediandaysmetamorph_model_MJ_MD6_54.txt")

capture.output(summary(model_MJ_MD6),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/summary_mediandaysmetamorph_model_MJ_MD6_54.csv")

tab_model(model_MJ_MD6, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/tab_model_mediandaysmetamorph_model_MJ_MD6_54.doc")

tab_model(model_MJ_MD6,show.df = TRUE,p.val = "kr",  file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/tab_model_mediandaysmetamorph_model_MJ_MD6_54_df_pkr.doc")

#file didn't open
#tab_model(model_MJ_MD6,show.df = TRUE,p.val = "kr",  file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/tab_model_mediandaysmetamorph_model_MJ_MD6_54_df_pkr.svg")



### test residuals - mostly ok
qqnorm(resid(model_MJ_MD6))
hist(resid(model_MJ_MD6))
plot(model_MJ_MD6)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/Residualsplot_model_MJ_MD6_54.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_MJ_MD6)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/Residualshist_model_MJ_MD6_54.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_MJ_MD6))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/QQnorm_model_MJ_MD6_54.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_MJ_MD6))
dev.off()



## interaction plots
(model_MJ_MD6.means <- interactionMeans(model_MJ_MD6))
plot(model_MJ_MD6.means)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/interactionMeans_model_MJ_MD6_54.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_MJ_MD6.means <- interactionMeans(model_MJ_MD6))
plot(model_MJ_MD6.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/interactionMeans_model_MJ_MD6_54.svg", width = 8, height = 6.5)
(model_MJ_MD6.means <- interactionMeans(model_MJ_MD6))
plot(model_MJ_MD6.means)
dev.off()

## visreg
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_MD6)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/visreg_model_MJ_MD6_54.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_MD6)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/visreg_model_MJ_MD6_54.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_MJ_MD6)
dev.off()



L.S_model_MJ_MD6_frog <- pairs(lsmeans(model_MJ_MD6, ~ Toad_Cooccurrence_History | Competitor_Identity))
test(L.S_model_MJ_MD6_frog, adjust = "tukey")

capture.output(test(L.S_model_MJ_MD6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/L.S_model_MJ_MD6_frog_539.txt")
write.csv(test(L.S_model_MJ_MD6_frog, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/L.S_model_MJ_MD6_frog_539.csv")


L.S_model_MJ_MD6_region <- pairs(lsmeans(model_MJ_MD6, ~ Competitor_Identity | Toad_Cooccurrence_History ))
test(L.S_model_MJ_MD6_region, adjust = "tukey")


capture.output(test(L.S_model_MJ_MD6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/L.S_model_MJ_MD6_region_539.txt")
write.csv(test(L.S_model_MJ_MD6_region, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/L.S_model_MJ_MD6_region_539.csv")











### means per treatment - not transformed data
names(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow)
Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$Toad_Cooccurrence_History

Rawdata_experimentAugust2018_indivrows2_median2_large_nolow$MedianDays


Rawdata_experimentAugust2018_indivrows2_median2_large_nolow %>%
  group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
  summarise_at(vars(MedianDays),na.rm=TRUE, list(MedianDays_mean = mean))



MedianDays_meanDF<-as.data.frame(Rawdata_experimentAugust2018_indivrows2_median2_large_nolow %>%
                                          group_by(Toad_Cooccurrence_History,Competitor_Identity) %>%
                                          summarise_at(vars(MedianDays),na.rm=TRUE, list(MedianDays_mean = mean)))


# differences
MedianDays_meanDF
str(MedianDays_meanDF)

# long 
#control - frog HG
MedianDays_meanDF[4,3] - MedianDays_meanDF[5,3]



# % 
((MedianDays_meanDF[4,3] - MedianDays_meanDF[5,3])/MedianDays_meanDF[4,3])*100



# long 
# control - frog ML
MedianDays_meanDF[4,3] - MedianDays_meanDF[6,3]



# short
# control- frog HG
MedianDays_meanDF[1,3] - MedianDays_meanDF[2,3]

# short 
# control - frog ML
MedianDays_meanDF[1,3] - MedianDays_meanDF[3,3]


# long - short
# frog HG
MedianDays_meanDF[5,3] - MedianDays_meanDF[2,3]

# % 
((MedianDays_meanDF[5,3] - MedianDays_meanDF[2,3])/MedianDays_meanDF[5,3])*100

# long - short
# frog ML
MedianDays_meanDF[6,3] - MedianDays_meanDF[3,3]


# % 
((MedianDays_meanDF[6,3] - MedianDays_meanDF[3,3])/MedianDays_meanDF[6,3])*100


MedianDays_meanDF[MedianDays_meanDF$Toad_Cooccurrence_History=="Long",MedianDays_meanDF$Toad_Cooccurrence_History=="Toad_control"]







################################
#################### PLOTS ######
################################

#order levels of factor


#labels <- c(ML = "Mainland Toads", HG = "Haida Gwaii Toads")

#ylim1 = boxplot.stats(Rawdata_experimentAugust2018_indivrows2_median2_large$MedianDays)$stats[c(1, 5)]



### 3 levels
png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/boxplot_MedianDays_metamorph_COMP3XREGION_nolow_nofacet_54.png", width = 200, height = 150, units = 'mm', res = 600)
(boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet <-Rawdata_experimentAugust2018_indivrows2_median2_large_nolow %>%

    ggplot(aes(x=Competitor_Identity, y=MedianDays, fill=Region)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("HG", "ML"),
                      labels=c("Short", "Long"))+
    #facet_wrap(~Region,labeller = labeller(Region = labels))+
    labs(x="Competition Treatment")+
    labs(y="Median time to toad metamorphosis (days)")+
    scale_x_discrete(breaks=c("Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("High density\nToad control","NRLFs\nMainland","NRLFs\nHaida Gwaii"))+
    #scale_fill_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #     labels=c("Haida Gwaii", "Mainland"))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/MedianDays/boxplot_MedianDays_metamorph_COMP3XREGION_nolow_nofacet_54.svg", width = 200, height = 150, units = 'mm')
(boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet <-Rawdata_experimentAugust2018_indivrows2_median2_large_nolow %>%

    ggplot(aes(x=Competitor_Identity, y=MedianDays, fill=Region)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad co-occurrence history\nwith the NRLF",breaks=c("HG", "ML"),
                      labels=c("Short", "Long"))+
    #facet_wrap(~Region,labeller = labeller(Region = labels))+
    labs(x="Competition Treatment")+
    labs(y="Median time to toad metamorphosis (days)")+
    scale_x_discrete(breaks=c("Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("Toad high\ncontrol","NRLFs\nMainland","NRLFs\nHaida Gwaii"))+
    #scale_fill_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #     labels=c("Haida Gwaii", "Mainland"))+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))
dev.off()


######################################
###### plot all 3 toad responses together ###
######################################



####### _NOLOW
### LEGEND
(boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet_LEGEND <-Rawdata_experimentAugust2018_indivrows2_median2_large_nolow %>%
    #mutate(Competition_level = fct_relevel(Treatment_three, 
    #                                          "Low_control", "Frog"#,"High_control")) %>%
    ggplot(aes(x=Competition_level, y=MedianDays, fill=Region)) +
    geom_boxplot(lwd=0.6)+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History\nwith the NRLF",breaks=c("HG", "ML"),
                      labels=c("Short (Haida Gwaii)", "Long (Mainland)"))+
    #facet_wrap(~Region,labeller = labeller(Region = labels))+
    labs(x="Competition Treatment")+
    labs(y="Median time to toad metamorphosis (days)")+
    scale_x_discrete(breaks=c("Toad_control","NRLF_ML","NRLF_HG"),
                     labels=c("High density\nToad control","NRLFs\nMainland","NRLFs\nHaida Gwaii"))+
    #scale_fill_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #     labels=c("Haida Gwaii", "Mainland"))+
    theme_bw()+
    theme(legend.key.size = unit(4, 'cm'),axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))





#### GET LEGEND ON SECOND PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend_NOLOW<-g_legend(boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet_LEGEND)


##3 MOVE LABELS
png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GR_AW_MT_nolow_n54_onelegend_cooccurrencehistory.png", width = 400, height = 350, units = 'mm', res = 600)
ggarrange(boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet  + theme(legend.position="none"), boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet + theme(legend.position="none"), boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_NOLOW,nrow = 2) 
dev.off()

ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Results_19_01_22/GR_AW_MT_nolow_n54_onelegend_cooccurrencehistory.svg", width = 400, height = 350, units = 'mm')
ggarrange(boxplot_GR_final_days_mgDay_3comp_nolow_logscale_nofacet  + theme(legend.position="none"), boxplot_Av_juv_biomass_mg_3comp_nolow_logscale_nofacet + theme(legend.position="none"), boxplot_timing_toad_MedianDays_comp3region_nolow_19_01_22_nofacet + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_NOLOW,nrow = 2) 
dev.off()






#####################################################################
##############################################################################
########## 4. Toad Mortality #################################################
##############################################################################
##############################################################################

#rm(list=ls())

#same data as Rawdata_experimentAugust2018 but only have one mortality point per tank, so it's a short dataframe (don't have repeated measures of mortality per tank)

subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23.csv",header=T,row.names=NULL,sep=",")




#order levels of factor
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Competition_level<- factor(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Competition_level, levels = c("FrogML","FrogHG","High_control"))
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23<- droplevels(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23)

#--------------------------------
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$toadTadpoles_added_as_top.up_june7thand8th_percentagetotal<-
  (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Tadpoles_added_as_top.up_june7thand8th/
     subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start)*100

#-----------------------------
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_tadsadded_26_06_21<-
  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start+
  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Tadpoles_added_as_top.up_june7thand8th-
  
  (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Running_total_of_adults_emerged+
     subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_tadpoles+
     subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_metamorphs+
     subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_number_of_metamorphs)

#---------
subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_tadsadded_26_06_21_percapita<-
  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_tadsadded_26_06_21/
  
  (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start +                                                                                                   subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Tadpoles_added_as_top.up_june7thand8th)
#-------------------

subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20<-
  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start-
  
  (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Running_total_of_adults_emerged+                                                                                                       subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_tadpoles+                                                                                                      subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_endexperiment_number_metamorphs+                                                                                             subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$X23.8.18_number_of_metamorphs)

#-----------

subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20_percapita<-
  subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20/
  
  (subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Total_number_individuals_start)
#----------------------------------



#save as file
Mortalitydataset_nolow_26_06_2021<-subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23 %>%
  dplyr::select(Tank,toadTadpoles_added_as_top.up_june7thand8th_percentagetotal,Mortality_tadsadded_26_06_21,Mortality_tadsadded_26_06_21_percapita,Mortality_06_05_20,Mortality_06_05_20_percapita)

write.csv(Mortalitydataset_nolow_26_06_2021,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/Rcode_plots_RGG_experiment_MS_sept8th2020/Mortalitydataset_nolow_26_06_2021-06-08-23.csv")



#test normality 
qqPlot(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20_percapita)
shapiro.test(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20_percapita)
hist(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20_percapita)


### get ave mortality

mean(subset_dataRGRdd_renamedsomecolumns_2_10_20_nolow_06_08_23$Mortality_06_05_20_percapita)

0.4270833 toad mortality per capita


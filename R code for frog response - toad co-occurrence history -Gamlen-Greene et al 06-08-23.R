################## R code for by Roseanna Gamlen-Greene 29 05 2023
#roseanna.gamlen.greene@gmail.com

## FROG RESPONSES ##

### MODELS FOR PAPER - 29-05-23
model_FGR1S				
model_FAW1S				
model_FMD1S	


#for UNPUBLISHED paper by Gamlen-Greene, Bufford, Todd and Richardson 


##########


      # frog response variable


#########

rm(list=ls())

library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(plyr)
library(dplyr)
library(visreg)
library(lme4)
library(nlme)
library(MuMIn)
library(lmerTest)
library(sjPlot)
library(sjmisc)
library(effects)
library(car)
library(ggbeeswarm)
library(optimx)
library(arules)
library(rstanarm)
library(tidyr)
library(dataRetrieval)
library(cowplot)
library(forcats)
library(performance) # colinearity
library(ggpubr)

setwd("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20")

##########################################
############## SHORTCUT ###########
########################################

data_frogs_long_enviro_sumDD_01_05_20<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20.csv",header=T,row.names=NULL,sep=",") 

dim(data_frogs_long_enviro_sumDD_01_05_20) #360  45

data_frogs_long_enviro_sumDD_01_05_20$Block<-as.factor(data_frogs_long_enviro_sumDD_01_05_20$Block)

#data_frogs_long_enviro_sumDD_01_05_20<-data_frogs_long_enviro_sumDD_01_05_20

data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass[data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass == 0] <- NA


data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg<-data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass*1000

data_frogs_long_enviro_sumDD_01_05_20$log10_Av_frog_juv_biomass_mg<-
  log10(data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg)


data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay<-data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg/data_frogs_long_enviro_sumDD_01_05_20$Days

data_frogs_long_enviro_sumDD_01_05_20$log10_GR_final_days_mgDay<-
  log10(data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay)






## final - initial / just degree days in experiment 
data_frogs_long_enviro_sumDD_01_05_20$GR_frog_finalmin_intial_degreedays_mgDD<-((data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass-data_frogs_long_enviro_sumDD_01_05_20$FROG_25.5.18_experiment_start_tadpole_av_individual_weight)/data_frogs_long_enviro_sumDD_01_05_20$SumDD)*1000



##########################
#### short dataset
##################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$ï..Tank


#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA



subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished_percapita<-subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished/(subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th)



####################################
##### join with morality data
subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[c(233:239)]

data_frogs_long_enviro_sumDD_01_05_20_2<-left_join(data_frogs_long_enviro_sumDD_01_05_20,subset_dataRGRdd_frogs2_sub,by="Tank")




###### scaled ########
################

#scale Ctadpole top up
Xtemp9 <-data_frogs_long_enviro_sumDD_01_05_20_2$Tadpoles_added_as_top.up_june7thand8th_percapita

Xscaled9 <- (Xtemp9 - mean(Xtemp9))/sd(Xtemp9)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_Tadpoles_added_as_top.up_june7thand8th_percapita  <- Xscaled9

#starting weight
Xtemp <- data_frogs_long_enviro_sumDD_01_05_20_2$FROG_25.5.18_experiment_start_tadpole_av_individual_weight
Xtemp

Xscaled <- (Xtemp - mean(Xtemp))/sd(Xtemp)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight  <- Xscaled



######### Mean_averageTemp
Xtemp8 <-data_frogs_long_enviro_sumDD_01_05_20_2$Mean_averageTemp

Xscaled8 <- (Xtemp8 - mean(Xtemp8))/sd(Xtemp8)
data_frogs_long_enviro_sumDD_01_05_20_2$scaled_Mean_averageTemp  <- Xscaled8


## morality scaled

data_frogs_long_enviro_sumDD_01_05_20_2$Frog_Mortality_Percapita_Scaled<-(data_frogs_long_enviro_sumDD_01_05_20_2$Frogmortality_percapita-min(data_frogs_long_enviro_sumDD_01_05_20_2$Frogmortality_percapita))/(max(data_frogs_long_enviro_sumDD_01_05_20_2$Frogmortality_percapita)-min(data_frogs_long_enviro_sumDD_01_05_20_2$Frogmortality_percapita))


##  days scaled
data_frogs_long_enviro_sumDD_01_05_20_2$Days_Since_Experiment_Began_Scaled<-(data_frogs_long_enviro_sumDD_01_05_20_2$Days-min(data_frogs_long_enviro_sumDD_01_05_20_2$Days))/(max(data_frogs_long_enviro_sumDD_01_05_20_2$Days)-min(data_frogs_long_enviro_sumDD_01_05_20_2$Days))




#################
### END SHORTCUT #######
##########################






######################################
########## see how did it (MADE SHORT DATASET) ... TURN DATASET LONG #############
################################
subset_dataRGRdd_frogs2_long_enviro<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs2_long_enviro.csv",header=T,row.names=NULL,sep=",") 


subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank",
           "X19.7.18_._average_weight_of_adults"                                                 
           , "X31.7.18_._average_weight_of_adults"                                                 
           , "X3.8.18_._average_weight_of_adults"                                                  
           , "X7.8.18_._average_weight_of_adults"                                                  
           , "X10.8.18_._average_weight_of_adults"                                                 
           , "X13.8.18_._average_weight_of_adults"                                                 
           , "X16.8.18_._average_weight_of_adults"                                                 
           , "X20.8.18_._average_weight_of_adults"                                                 
           , "X23.8.18_._average_weight_of_adults" 
     )      



subset_dataRGRdd_frogs2_juvweights<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_juvweights)

subset_dataRGRdd_frogs2_juvweightslong<-gather(subset_dataRGRdd_frogs2_juvweights, "Days", "Av_frog_juv_biomass", 2:10)



subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X19.7.18_._average_weight_of_adults"] <- "55"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X31.7.18_._average_weight_of_adults"] <- "67"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X3.8.18_._average_weight_of_adults"] <- "70"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X7.8.18_._average_weight_of_adults"] <- "74"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X10.8.18_._average_weight_of_adults"] <- "77"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X13.8.18_._average_weight_of_adults"] <- "80"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X16.8.18_._average_weight_of_adults"] <- "83"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X20.8.18_._average_weight_of_adults"] <- "87"
subset_dataRGRdd_frogs2_juvweightslong$Days[subset_dataRGRdd_frogs2_juvweightslong$Days == "X23.8.18_._average_weight_of_adults"] <- "90"

subset_dataRGRdd_frogs2_juvweightslong$Days<-as.numeric(subset_dataRGRdd_frogs2_juvweightslong$Days)


#write.csv(subset_dataRGRdd_frogs2_juvweightslong,"subset_dataRGRdd_frogs2_juvweightslong.csv")


##########################
######## add counts in
#################################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank",
           "Frog_popsource", "Population" ,"Region","Block"
           ,"Tadpoles_added_as_top.up_june7thand8th"
           ,"Tadpoles_added_as_top.up_june7thand8th_percapita"
           ,"X19.7.18_._number_adults"                                                  
           ,"X27.7.18_._NUMBER_ADULTS._WERE_NOT_WEIGHED"                                
           ,"X31.7.18_._number_adults"                                                  
           ,"X3.8.18_._number_adults"                                                   
          ,"X7.8.18_._number_adults"                                                   
          , "X10.8.18_._number_adults"                                                  
          , "X13.8.18_._number_adults"                                                  
           , "X16.8.18_._number_adults"                                                  
           ,"X20.8.18_._number_adults"                                                  
           , "X23.8.18_._number_adults"  
           ,"May_MaxwaterTemp"                                                          
           ,"May_MinwaterTemp"                                                          
           ,"May_MeanwaterTemp"                                                         
           ,"June_MaxwaterTemp"                                                         
           ,"June_MinwaterTemp"                                                         
           ,"June_MeanwaterTemp"                                                        
           ,"July_MaxwaterTemp"                                                         
           ,"July_MinwaterTemp"                                                         
           ,"July_MeanwaterTemp"                                                        
           ,"August_MaxwaterTemp"                                                       
           ,"August_MinwaterTemp"                                                       
           ,"August_MeanwaterTemp"                                                      
           ,"Max_averageTemp"                                                           
           ,"Min_averageTemp"                                                           
           ,"Mean_averageTemp" 
           ,"Total_chla_conc_8.6.18_ug.l"                                               
           , "Total_chla_conc_26.7.18_ug.l" 
           ,"FROG_25.5.18_experiment_start_tadpole_av_individual_weight"
           ,"Average_weight_of_tadpole_replacements_june7thand8th"
           ,"Total_number_individuals_start" 
          ,"Survived_counts_weighedandkilled"                                          
, "Perished_total_onsideoftank"                                               
, "Survived_and_perished"                                                     
,"DIED_unexplaineddeath"                                                     
,"DIED_no_extras"                                                            
,"DIED_no_extras_divided_bystartingnumber"                                   
,"DIED_all"                                                                  
,"DIED_all_percapita"                                                        
,"Extra_number_at_end"  
,"Running_total_of_adults_emerged"
,"Daysbeforestartedexpmt"
,"Daysincaptivitybeforeexpmtstart"
,"Daysbeforeminusdayscap")      



subset_dataRGRdd_frogs2_juvcounts<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_juvcounts)

subset_dataRGRdd_frogs2_juvcountslong<-gather(subset_dataRGRdd_frogs2_juvcounts, "Days", "Count_juvfrog_emerged", 8:17)



subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X19.7.18_._number_adults"] <- "55"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X27.7.18_._NUMBER_ADULTS._WERE_NOT_WEIGHED"] <- "63"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X31.7.18_._number_adults"] <- "67"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X3.8.18_._number_adults"] <- "70"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X7.8.18_._number_adults"] <- "74"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X10.8.18_._number_adults"] <- "77"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X13.8.18_._number_adults"] <- "80"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X16.8.18_._number_adults"] <- "83"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X20.8.18_._number_adults"] <- "87"
subset_dataRGRdd_frogs2_juvcountslong$Days[subset_dataRGRdd_frogs2_juvcountslong$Days == "X23.8.18_._number_adults"] <- "90"

subset_dataRGRdd_frogs2_juvcountslong$Days<-as.numeric(subset_dataRGRdd_frogs2_juvcountslong$Days)


head(subset_dataRGRdd_frogs2_juvcountslong)


#write.csv(subset_dataRGRdd_frogs2_juvcountslong,"subset_dataRGRdd_frogs2_juvcountslong.csv")




############## top down join between subset_dataRGRdd_frogs2_juvcountslong and subset_dataRGRdd_frogs2_juvweightslong
data_frogs_long_enviro_01_05_20<-left_join(subset_dataRGRdd_frogs2_juvcountslong,subset_dataRGRdd_frogs2_juvweightslong)



########################################
################ add in SumDD #############
###################################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs


subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"

names(subset_dataRGRdd_frogs2)
#SUBSET TO JUST DATASET i WANT
myvars<- c("Tank"
          , "SumDD_23_8"                                                                ,"SumDD_20_8"                                                                
           ,"SumDD_16_8"                                                                
           ,"SumDD_13_8"                                                                
           ,"SumDD_10_8"                                                                
           , "SumDD_7_8"                                                                 
          ,"SumDD_3_8"                                                                 
          ,"SumDD_31_7"                                                                
         , "SumDD_27_7"                                                                
         , "SumDD_19_7")                                                            
  



subset_dataRGRdd_frogs2_sumDD<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_sumDD)

subset_dataRGRdd_frogs2_sumDDlong<-gather(subset_dataRGRdd_frogs2_sumDD, "Days", "SumDD", 2:11)



subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_19_7"] <- "55"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_27_7"] <- "63"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_31_7"] <- "67"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_3_8"] <- "70"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_7_8"] <- "74"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_10_8"] <- "77"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_13_8"] <- "80"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_16_8"] <- "83"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_20_8"] <- "87"
subset_dataRGRdd_frogs2_sumDDlong$Days[subset_dataRGRdd_frogs2_sumDDlong$Days == "SumDD_23_8"] <- "90"

subset_dataRGRdd_frogs2_sumDDlong$Days<-as.numeric(subset_dataRGRdd_frogs2_sumDDlong$Days)


head(subset_dataRGRdd_frogs2_sumDDlong)


##########join

data_frogs_long_enviro_sumDD_01_05_20<-left_join(data_frogs_long_enviro_01_05_20,subset_dataRGRdd_frogs2_sumDDlong)


#write.csv(data_frogs_long_enviro_sumDD_01_05_20,"data_frogs_long_enviro_sumDD_01_05_20.csv")




############## #############
####### 1. FROG WEIGHT at metamorph ###########
###################################
ggbetweenstats(data_frogs_long_enviro_sumDD_01_05_20,
               Frog_popsource, Av_frog_juv_biomass_mg, outlier.tagging = TRUE)








#################
####### mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_2[!(data_frogs_long_enviro_sumDD_01_05_20_2$Av_frog_juv_biomass_mg=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_2_noNA)

Av_juv_biomass_mg_360 <- data_frogs_long_enviro_sumDD_01_05_20_2_noNA %>%
  group_by(Frog_popsource,Region)

Av_juv_biomass_mg_360<-Av_juv_biomass_mg_360 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), Av_frog_juv_biomass_mg)

Av_juv_biomass_mg_360<-as.data.frame(Av_juv_biomass_mg_360)
Av_juv_biomass_mg_360

write.csv(Av_juv_biomass_mg_360,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Mean_sd_se_FROG_Av_juv_biomass_mg_36.csv")



########## HG frog ####
466.500-395.9881

((466.500-395.9881)/395.9881)*100


############## 
####### SCALING ###########
###############


############## 
####### ANOVA ###########
###############

### very important
data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/hist_log10_Av_frog_juv_biomass_mg.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$log10_Av_frog_juv_biomass_mg)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/hist_Av_frog_juv_biomass_mg.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$Av_frog_juv_biomass_mg)
dev.off()

### best
model_FAW1S<-lmer(
  log10_Av_frog_juv_biomass_mg~
    Region*Frog_popsource+ 
    Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ 
    Frog_Mortality_Percapita_Scaled+
    Days_Since_Experiment_Began_Scaled+ #scaled
    Block+
    Mean_averageTemp+
    (1|Population)+ 
    (1+Days_Since_Experiment_Began_Scaled|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

Anova(model_FAW1S)
AIC(model_FAW1S) #-58.09773
summary(model_FAW1S)


#360
capture.output(anova(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/anova_avjuvweight_model_FAW1S_360.txt")
capture.output(Anova(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Anova_chisq_avjuvweight_model_FAW1S_360.txt")
write.csv(anova(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/anova_avjuvweight_model_FAW1S_360.csv")
write.csv(Anova(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Anova_chisq_avjuvweight_model_FAW1S_360.csv")
capture.output(summary(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/summary_avjuvweight_model_FAW1S_360.txt")


capture.output(summary(model_FAW1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/summary_avjuvweight_model_FAW1S_360.csv")

tab_model(model_FAW1S, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/tab_model_avjuvweight_model_FAW1S_360.doc")

tab_model(model_FAW1S,show.df = TRUE,p.val = "kr",
          file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/tab_model_avjuvweight_model_FAW1S_360_df_pkr.doc")


### test residuals - mostly ok
qqnorm(resid(model_FAW1S))
hist(resid(model_FAW1S))
plot(model_FAW1S)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Residualsplot_model_FAW1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FAW1S)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/Residualshist_model_FAW1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FAW1S))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/QQnorm_model_FAW1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FAW1S))
dev.off()



(model_FAW1S.means <- interactionMeans(model_FAW1S))
plot(model_FAW1S.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/interactionMeans_model_FAW1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FAW1S.means <- interactionMeans(model_FAW1S))
plot(model_FAW1S.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/interactionMeans_model_FAW1S_360.svg", width = 12, height = 6.5)
(model_FAW1S.means <- interactionMeans(model_FAW1S))
plot(model_FAW1S.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1S)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/visreg_model_FAW1S_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1S)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/visreg_model_FAW1S_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FAW1S)
dev.off()




L.S_model_FAW1S <- pairs(lsmeans(model_FAW1S, ~ Region | Frog_popsource))
test(L.S_model_FAW1S, adjust = "tukey")

capture.output(test(L.S_model_FAW1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1S_360.txt")
write.csv(test(L.S_model_FAW1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1S_360.csv")

L.S_model_FAW1S_2 <- pairs(lsmeans(model_FAW1S, ~  Frog_popsource | Region))
test(L.S_model_FAW1S_2, adjust = "tukey")

capture.output(test(L.S_model_FAW1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1S_2_360.txt")
write.csv(test(L.S_model_FAW1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/WeightatMetamorphosis/L.S_model_FAW1S_2_360.csv")





###########################
############# 2. FROG GROWTH RATE ###############
#############################



#################
####### mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_2[!(data_frogs_long_enviro_sumDD_01_05_20_2$Av_frog_juv_biomass_mg=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_2_noNA)

GR_final_days_mgDay_360 <- data_frogs_long_enviro_sumDD_01_05_20_2_noNA %>%
  group_by(Frog_popsource,Region)

GR_final_days_mgDay_360<-GR_final_days_mgDay_360 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), GR_final_days_mgDay)

GR_final_days_mgDay_360<-as.data.frame(GR_final_days_mgDay_360)
GR_final_days_mgDay_360

write.csv(GR_final_days_mgDay_360,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Mean_sd_se_FROG_GR_final_days_mgDay_36.csv")



########## HG frog ####
6.653252 - 4.952009
((6.653252 - 4.952009)/4.952009)*100

############# 
####### ANOVA ###########
###############

### very important
data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)


data_frogs_long_enviro_sumDD_01_05_20_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_2)




png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/hist_log10_GR_final_days_mgDay.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$log10_GR_final_days_mgDay)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/hist_GR_final_days_mgDay.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(data_frogs_long_enviro_sumDD_01_05_20$GR_final_days_mgDay)
dev.off()


### best added scaling #########
model_FGR1S<-lmer(
  log10_GR_final_days_mgDay~
    Region*Frog_popsource+ 
    scaled_Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ 
    Frog_Mortality_Percapita_Scaled+
    Days_Since_Experiment_Began_Scaled+ #scaled
    Block+
    scaled_Mean_averageTemp+
    (1|Population)+ 
    (1+Days_Since_Experiment_Began_Scaled|Tank), #random effects
  data=data_frogs_long_enviro_sumDD_01_05_20_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

Anova(model_FGR1S)
AIC(model_FGR1S) # -55.85908
summary(model_FGR1S)


#360
capture.output(anova(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/anova_growthrate_model_FGR1S_360.txt")
capture.output(Anova(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Anova_chisq_growthrate_model_FGR1S_360.txt")
write.csv(anova(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/anova_growthrate_model_FGR1S_360.csv")
write.csv(Anova(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Anova_chisq_growthrate_model_FGR1S_360.csv")
capture.output(summary(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/summary_growthrate_model_FGR1S_360.txt")

capture.output(summary(model_FGR1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/summary_growthrate_model_FGR1S_360.csv")


tab_model(model_FGR1S, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/tab_model_growthrate_model_FGR1S_360.doc")


tab_model(model_FGR1S,show.df = TRUE,p.val = "kr", file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/tab_model_growthrate_model_FGR1S_360_df_pkr.doc")


### test residuals - mostly ok
qqnorm(resid(model_FGR1S))
hist(resid(model_FGR1S))
plot(model_FGR1S)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Residualsplot_model_FGR1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FGR1S)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/Residualshist_model_FGR1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FGR1S))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/QQnorm_model_FGR1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FGR1S))
dev.off()



(model_FGR1S.means <- interactionMeans(model_FGR1S))
plot(model_FGR1S.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/interactionMeans_model_FGR1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FGR1S.means <- interactionMeans(model_FGR1S))
plot(model_FGR1S.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/interactionMeans_model_FGR1S_360.svg", width = 12, height = 6.5)
(model_FGR1S.means <- interactionMeans(model_FGR1S))
plot(model_FGR1S.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1S)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/visreg_model_FGR1S_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1S)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/visreg_model_FGR1S_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FGR1S)
dev.off()




L.S_model_FGR1S <- pairs(lsmeans(model_FGR1S, ~ Region | Frog_popsource))
test(L.S_model_FGR1S, adjust = "tukey")

capture.output(test(L.S_model_FGR1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1S_360.txt")
write.csv(test(L.S_model_FGR1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1S_360.csv")

L.S_model_FGR1S_2 <- pairs(lsmeans(model_FGR1S, ~  Frog_popsource | Region))
test(L.S_model_FGR1S_2, adjust = "tukey")

capture.output(test(L.S_model_FGR1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1S_2_360.txt")
write.csv(test(L.S_model_FGR1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/GrowthRate/L.S_model_FGR1S_2_360.csv")






#################
###3. FROG MEDIAN TIME to meta #####
################


######### shortcut #####
##################

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large.csv",header=T,row.names=NULL,sep=",")

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large$Tadpoles_added_as_top.up_june7thand8th_percapita<-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large$Tadpoles_added_as_top.up_june7thand8th



############
#### join with mortality #######
#############################



##########################
#### short dataset
##################
subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$ï..Tank


#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA






####################################
##### join with morality data
subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[c(233:239)]

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-left_join(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large,subset_dataRGRdd_frogs2_sub,by="Tank")






########################
####### this is how i calcualted median days #########
#############

subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

#subset_dataRGRdd_frogs2$Tank<-subset_dataRGRdd_frogs2$?..Tank


names(subset_dataRGRdd_frogs2)[names(subset_dataRGRdd_frogs2) == "?..Tank"] <- "Tank"



data_frogs_long_enviro_sumDD_01_05_20<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/data_frogs_long_enviro_sumDD_01_05_20.csv",header=T,row.names=NULL,sep=",") 

#make each toad have its own row and then run the code below
data_frogs_long_enviro_sumDD_01_05_20_indivrows2 <- as.data.frame(lapply(data_frogs_long_enviro_sumDD_01_05_20, rep, data_frogs_long_enviro_sumDD_01_05_20$Count_juvfrog_emerged))


#calc median
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2<-setDT(data_frogs_long_enviro_sumDD_01_05_20_indivrows2)[,list(MeanDays=mean(Days), MaxDays=max(Days), MinDays=min(Days), MedianDays=as.numeric(median(Days)), StdDays=sd(Days)), by=Tank]

#ry chaning tank to chracter to see if that fixes error
subset_dataRGRdd_frogs2$Tank<-as.character(subset_dataRGRdd_frogs2$Tank)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2$Tank<-as.character(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2$Tank)


myvars<- c("Tank", "Population" ,"Region","Block","Frog_popsource","Total_number_individuals_start","Tadpoles_added_as_top.up_june7thand8th"          
           ,"Average_weight_of_tadpole_replacements_june7thand8th"                      
           , "Total_chla_conc_8.6.18_ug.l"                                               
           , "Total_chla_conc_26.7.18_ug.l"   
           ,"Daysbeforestartedexpmt"                                                    
           , "Daysincaptivitybeforeexpmtstart"                                           
           , "Daysbeforeminusdayscap"                                                    
           , "Daysbeforedividedayscap"
           ,"May_MaxwaterTemp"                                                          
           ,"May_MinwaterTemp"                                                          
           ,"May_MeanwaterTemp"                                                         
           ,"June_MaxwaterTemp"                                                         
           ,"June_MinwaterTemp"                                                         
           ,"June_MeanwaterTemp"                                                        
           ,"July_MaxwaterTemp"                                                         
           ,"July_MinwaterTemp"                                                         
           ,"July_MeanwaterTemp"                                                        
           ,"August_MaxwaterTemp"                                                       
           ,"August_MinwaterTemp"                                                       
           ,"August_MeanwaterTemp"                                                      
           ,"Max_averageTemp"                                                           
           ,"Min_averageTemp"                                                           
           ,"Mean_averageTemp"
           
           ,"Perished_total_onsideoftank"
           ,"Survived_counts_weighedandkilled"                                
           
           ,"Survived_and_perished"                                                     
           , "DIED_unexplaineddeath"                                                     
           , "DIED_no_extras"                                                            
           , "DIED_no_extras_divided_bystartingnumber"                                   
           , "DIED_all"                                                                  
           , "Extra_number_at_end"
           ,"X23.8.18_endexperiment_number_tadpoles_metas_excluding_adults_weighedmetas"
           ,"X23.8.18_endexperiment_number_metamorphs"                                  
           ,"X23.8.18_endexperiment_number_tadpoles"    
           , "X25.5.18_experiment_start_tapole_av_individual_weight"                     
           , "FROG_25.5.18_experiment_start_tadpole_av_individual_weight"  
           ,"Running_total_of_adults_emerged" ) 

subset_dataRGRdd_frogs2_sub<-subset_dataRGRdd_frogs2[myvars]
head(subset_dataRGRdd_frogs2_sub)


#join to large dataset - use sub to deal with error and use for plotting only 
#data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large<-left_join(subset_dataRGRdd_frogs2_sub,data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2,by="Tank")



#write.csv(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large,"data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large.csv")


############## 
####### SCALING ###########
###############


#################
########## mean sd #########
########################


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA<-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2[!(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$MedianDays=="NA"),]

dim(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA)

MedianDays <- data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2_noNA %>%
  group_by(Frog_popsource,Region)

MedianDays<-MedianDays %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), MedianDays)

MedianDays<-as.data.frame(MedianDays)
MedianDays

write.csv(MedianDays,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Mean_sd_se_FROG_MedianDays.csv")



########## HG frog ####
71.12500-82.92857

((71.12500-82.92857)/82.92857)*100

############## 
####### ANOVA ###########
###############

### very important
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History, levels = c("Long","Short"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region, levels = c("ML","HG"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<-droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Block<-as.factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Block)




### scaled


#scale Ctadpole top up
Xtemp9 <-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Tadpoles_added_as_top.up_june7thand8th_percapita

Xscaled9 <- (Xtemp9 - mean(Xtemp9))/sd(Xtemp9)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$scaled_Tadpoles_added_as_top.up_june7thand8th_percapita  <- Xscaled9

#starting weight
Xtemp <- data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$FROG_25.5.18_experiment_start_tadpole_av_individual_weight
Xtemp

Xscaled <- (Xtemp - mean(Xtemp))/sd(Xtemp)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight  <- Xscaled



######### Mean_averageTemp
Xtemp8 <-data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Mean_averageTemp

Xscaled8 <- (Xtemp8 - mean(Xtemp8))/sd(Xtemp8)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$scaled_Mean_averageTemp  <- Xscaled8


## morality scaled

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_Mortality_Percapita_Scaled<-(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frogmortality_percapita-min(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frogmortality_percapita))/(max(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frogmortality_percapita)-min(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frogmortality_percapita))



### best
model_FMD1S<-lmer(
  MedianDays~
    Region*Frog_popsource+ 
    scaled_Tadpoles_added_as_top.up_june7thand8th_percapita+ 
    #scaled_FROG_25.5.18_experiment_start_tadpole_av_individual_weight+ raises AIC
    Frog_Mortality_Percapita_Scaled+
    Block+
    scaled_Mean_averageTemp+
    (1|Population),
  data=data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2,na.action=na.exclude,REML=FALSE,control=lmerControl(optimizer="optimx", optCtrl=list(method='nlminb'),check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

Anova(model_FMD1S)
AIC(model_FMD1S) # 194.578
summary(model_FMD1S)


#360
capture.output(anova(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/anova_mediandaysmeta_model_FMD1S_360.txt")
capture.output(Anova(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Anova_chisq_mediandaysmeta_model_FMD1S_360.txt")
write.csv(anova(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/anova_mediandaysmeta_model_FMD1S_360.csv")
write.csv(Anova(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Anova_chisq_mediandaysmeta_model_FMD1S_360.csv")
capture.output(summary(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/summary_mediandaysmeta_model_FMD1S_360.txt")

capture.output(summary(model_FMD1S),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/summary_mediandaysmeta_model_FMD1S_360.csv")


tab_model(model_FMD1S, file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/tab_model_mediandaysmeta_model_FMD1S_360.doc")


tab_model(model_FMD1S,show.df = TRUE,p.val = "kr", file = "C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/tab_model_mediandaysmeta_model_FMD1S_360_df_pkr.doc")





### test residuals - mostly ok
qqnorm(resid(model_FMD1S))
hist(resid(model_FMD1S))
plot(model_FMD1S)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Residualsplot_model_FMD1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
plot(model_FMD1S)
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/Residualshist_model_FMD1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
hist(resid(model_FMD1S))
dev.off()

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/QQnorm_model_FMD1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
qqnorm(resid(model_FMD1S))
dev.off()



(model_FMD1S.means <- interactionMeans(model_FMD1S))
plot(model_FMD1S.means)

png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/interactionMeans_model_FMD1S_360.png", width = 12, height = 6.5, units = 'in', res = 300)
(model_FMD1S.means <- interactionMeans(model_FMD1S))
plot(model_FMD1S.means)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/interactionMeans_model_FMD1S_360.svg", width = 12, height = 6.5)
(model_FMD1S.means <- interactionMeans(model_FMD1S))
plot(model_FMD1S.means)
dev.off()


par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1S)



png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/visreg_model_FMD1S_360.png", width = 8, height = 6.5, units = 'in', res = 300)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1S)
dev.off()

svg("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/visreg_model_FMD1S_360.svg", width = 8, height = 6.5)
par(mar = c(5, 5,2, 2))
par(mfrow=c(3,3))
visreg(model_FMD1S)
dev.off()




L.S_model_FMD1S <- pairs(lsmeans(model_FMD1S, ~ Region | Frog_popsource))
test(L.S_model_FMD1S, adjust = "tukey")

capture.output(test(L.S_model_FMD1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1S_360.txt")
write.csv(test(L.S_model_FMD1S, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1S_360.csv")

L.S_model_FMD1S_2 <- pairs(lsmeans(model_FMD1S, ~  Frog_popsource | Region))
test(L.S_model_FMD1S_2, adjust = "tukey")

capture.output(test(L.S_model_FMD1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1S_2_360.txt")
write.csv(test(L.S_model_FMD1S_2, adjust = "tukey"),file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/MedianDays/L.S_model_FMD1S_2_360.csv")







#################################
################# 4. FROG MORTALITY ###############
#################################################
rm(list=ls())

subset_dataRGRdd_frogs2<-read.csv(file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/subset_dataRGRdd_frogs_added_toadpopulations.csv",header=T,row.names=NULL,sep=",") #frog_hg changed N to NA for ones that didn't have frogs

#order levels of factor
subset_dataRGRdd_frogs2$Treatment_nameC<- factor(subset_dataRGRdd_frogs2$Treatment_nameC, levels = c("HGF_HGT","HGF_MLT","MLF_MLT","MLF_HGT"))
subset_dataRGRdd_frogs2 <- droplevels(subset_dataRGRdd_frogs2)


#NB two metamorph numbers for 23.8.18 - some were weighed and then realized that some were left out at the end
#X23.8.18_endexperiment_number_metamorphs <- ones that were found when we emptied the tanks - not weighed
#X23.8.18_number_of_metamorphs - number that were weighed


subset_dataRGRdd_frogs2$Frogmortality<-subset_dataRGRdd_frogs2$Total_number_individuals_start-subset_dataRGRdd_frogs2$Survived_counts_weighedandkilled

subset_dataRGRdd_frogs2$Frogmortality_percapita<-subset_dataRGRdd_frogs2$Frogmortality/36

subset_dataRGRdd_frogs2$Frogmortality_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

subset_dataRGRdd_frogs2$Frogmortality_tadsadded_yesperished<-subset_dataRGRdd_frogs2$Total_number_individuals_start+subset_dataRGRdd_frogs2$Tadpoles_added_as_top.up_june7thand8th-(subset_dataRGRdd_frogs2$Running_total_of_adults_emerged+subset_dataRGRdd_frogs2$Perished_total_onsideoftank+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_number_of_metamorphs+subset_dataRGRdd_frogs2$X23.8.18_endexperiment_number_tadpoles)

#remove negative value
subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished[subset_dataRGRdd_frogs2$Frogmortality_tadsadded_noperished < 0] <- NA




################################
###### PLOTS ########
################################

############
####### ggarrange ####
########################

library(ggpubr)




################################################################


#order levels of factor

# LONG VS SHORT

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History<- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Toad_Cooccurrence_History, levels = c("Short","Long"))
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<- droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)

data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region <- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Region,
                                                                             levels = c("HG","ML"),ordered = TRUE)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<- droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)


data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource <- factor(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2$Frog_popsource,
                                                                                  levels = c("HG","ML"),ordered = TRUE)
data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2<- droplevels(data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2)















labels <- c(ML = "Mainland Toads", HG = "Haida Gwaii Toads")

### 1
(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog<-
   data_frogs_long_enviro_sumDD_01_05_20 %>%
   ggplot(aes(x=Frog_popsource, y=GR_final_days_mgDay, fill=Region)) +
   geom_boxplot(lwd=0.6) +
   #scale_colour_hue(l=50) +
   #coord_cartesian(ylim = ylim1*1.15)+
   scale_y_continuous(trans = 'log10')+
   annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History \nwith the frog",breaks=c("HG", "ML"),
                      labels=c("Short (Haida Gwaii)", "Long (Mainland)"))+
   labs(x="Interspecific Competition Treatment")+
   labs(y="Frog growth rate (mg/day)")+
   #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
   scale_x_discrete(breaks=c("HG",  "ML"),
                    labels=c("Toad +\nHaida Gwaii Frog","Toad +\nMainland Frog" ))+
   # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
   #          labels=c("Haida Gwaii", "Mainland"),
   #              guide=FALSE)+    
   #scale_fill_manual("grey")+
   theme_bw()+
   theme(axis.title.x = element_text(margin = margin(t = 10)))+
   theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


##### 2
(frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog<-
    data_frogs_long_enviro_sumDD_01_05_20 %>%
    ggplot(aes(x=Frog_popsource, y=Av_frog_juv_biomass_mg, fill=Region)) +
    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    scale_y_continuous(trans = 'log10')+
    annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History \nwith the frog",breaks=c("HG", "ML"),
                      labels=c("Short (Haida Gwaii)", "Long (Mainland)"))+
    labs(x="Interspecific Competition Treatment")+
    labs(y="Average frog weight\nat metamorphosis (mg)")+
    #facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Toad +\nHaida Gwaii Frog","Toad +\nMainland Frog" ))+
    # scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #          labels=c("Haida Gwaii", "Mainland"),
    #              guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### 3
(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog<- 
    data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2%>% ggplot(aes(x=Frog_popsource, y=MedianDays, fill=Region)) +    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    #scale_y_continuous(trans = 'log10')+
    #annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History \nwith the frog",breaks=c("HG", "ML"),
                      labels=c("Short (Haida Gwaii)", "Long (Mainland)"))+
    labs(x="Interspecific Competition Treatment")+
    
    labs(y="Median time to frog\nmetamorphosis (days)")+
    #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Toad +\nHaida Gwaii Frog","Toad +\nMainland Frog" ))+
    #scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
                     # breaks=c("HG", "ML"),
                      #labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))


### legend
(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog_LEGEND<- 
    data_frogs_long_enviro_sumDD_01_05_20_indivrows2_median2_large_2%>% ggplot(aes(x=Frog_popsource, y=MedianDays, fill=Region)) +    geom_boxplot(lwd=0.6) +
    #scale_colour_hue(l=50) +
    #coord_cartesian(ylim = ylim1*1.15)+
    #scale_y_continuous(trans = 'log10')+
    #annotation_logticks(sides="l")+
    scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad Co-occurrence History \nwith the frog",breaks=c("HG", "ML"),
                      labels=c("Short - Haida Gwaii", "Long - Mainland"))+
    labs(x="Interspecific Competition Treatment")+
    
    labs(y="Median time to frog\nmetamorphosis (days)")+
    #   scale_fill_manual(values=c("#a2ccb6ff","#979080ff"),name="Toad population",breaks=c("HG", "ML"),
    #                  labels=c("Haida Gwaii", "Mainland"))+
    # facet_grid(~Frog_popsource,labeller = labeller(Frog_popsource = labels))+
    scale_x_discrete(breaks=c("HG",  "ML"),
                     labels=c("Toad +\nHaida Gwaii Frog","Toad +\nMainland Frog" ))+
    #scale_fill_manual(values=c("#a65830ff","#deaa87ff"),name="Frog population",
    # breaks=c("HG", "ML"),
    #labels=c("Haida Gwaii", "Mainland"))+
    #scale_colour_discrete(name="Toad Region",breaks=c("HG", "ML"),
    #    labels=c("Haida Gwaii", "Mainland"),
    #      guide=FALSE)+    
    #scale_fill_manual("grey")+
    theme_bw()+
    theme(axis.title.x = element_text(margin = margin(t = 10)))+
    theme(legend.key.size = unit(3, 'cm'),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),text = element_text(size = 20)))




#### GET LEGEND ON SECOND PLOT
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend_frog2<-g_legend(boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog_LEGEND)


png("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend_swaptoadfrog_cooccurencehist-07-05-23.png", width = 300, height = 300, units = 'mm', res = 600)
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog2,nrow = 2) 
dev.off()



ggsave("C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Frog_growthrate_avweightatmeta_mediantime_2x2_onelegend_swaptoadfrog_cooccurencehist-07-05-23.svg", width = 300, height = 300, units = 'mm')
ggarrange(frogs_GR_final_days_mgDay_logscale_nofacet_29_01_22_swaptoadfrog  + theme(legend.position="none"), frogs_meta_weightplot_logscale_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"), boxplot_frogs_mediandays_nofacet_29_01_22_swaptoadfrog + theme(legend.position="none"),
          labels = c("a)", "b)", "c)"), font.label = list(size = 17),hjust =-0.25,
          ncol = 2,  mylegend_frog2,nrow = 2) 
dev.off()


############
####### initial weight frogs ######
#################



### get means and standard errors
subset_dataRGRdd_frogs2_noNA<-subset_dataRGRdd_frogs2[!(subset_dataRGRdd_frogs2$FROG_25.5.18_experiment_start_tadpole_av_individual_weight=="NA"),]

dim(subset_dataRGRdd_frogs2_noNA)

avweight_36 <- subset_dataRGRdd_frogs2_noNA %>%
  group_by(Frog_popsource)

avweight_36<-avweight_36 %>%
  summarise_each(funs(mean, sd, se=sd(.)/sqrt(n())), FROG_25.5.18_experiment_start_tadpole_av_individual_weight)

avweight_36<-as.data.frame(avweight_36)
avweight_36

write.csv(avweight_36,file="C:/Users/Roseanna/Documents/Haida_Gwaii_RAAU_ANBO/Experiment_2018/NEW_13_3_19_WITHOUT20.8ERROR/Frog_response_01_05_20/Mean_sd_se_STARTING_WEIGHT_FROGS_avjuvweight_539.csv")



0.09290278-0.10357986


(0.09290278-0.10357986/0.10357986)*100



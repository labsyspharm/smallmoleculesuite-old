## query all cmpds for a certain gene

#detach(package:dplyr, unload=TRUE)
library(plyr)
library(dplyr)
library(ggplot2)

######################################################################################T
# Set directories and import files -----
######################################################################################T
dir_compound_similarity_app<-paste0("/Users/nienke/Dropbox/Harvard/CBDM-SORGER/Collaborations/LINCS_Compound_Database_NM/custom_informer_set/",
                                                                 "drug_browser/compound_similarity_app")
dir_all_cmpds_per_gene_app<-paste0("/Users/nienke/Dropbox/Harvard/CBDM-SORGER/Collaborations/LINCS_Compound_Database_NM/custom_informer_set/",
                                                                "drug_browser/all_cmpds_per_gene_app")

setwd(dir_compound_similarity_app)
affinity_selectivity<-read.csv("affinity_selectivity_table_ChemblV22_1_20170804.csv",stringsAsFactors = F)
affinity_selectivity[is.na(affinity_selectivity$selectivity_class)==T,]$selectivity_class<-
  "unknown"


######################################################################################T
# Example user input -----
######################################################################################T

c.gene<-2475 #MTOR
c.gene<-6198 #S6K1

min_affinity<-0
max_affinity<-1000000
max_sd<-100000
min_measurements<-2

selectivity_order<-c("best_class","second_class","non_specific","unknown","other")

c.binding_data<-affinity_selectivity%>%filter(gene_id==c.gene)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(selectivity_class,mean_affinity)

#View(c.binding_data)

## plot data
##! we should include a solution for NA points as well.

ggplot(c.binding_data,aes(x=selectivity,y=mean_affinity,color=selectivity_class))+
  geom_point()+
  scale_y_log10(limits=c((min_affinity+0.01),max_affinity))+
  scale_x_continuous(limits=c(0,
                              max(c.binding_data$selectivity,na.rm=T)))

## show selection table -- user selects up to three compounds
selection_table<-c.binding_data[c("name","hms_id","symbol","selectivity_class","mean_affinity","selectivity",
                                  "ontarget_IC50_Q1","offtarget_IC50_Q1","offtarget_IC50_N")]
View(selection_table) ## option to download c.binding_data

selected_1<-10411
selected_2<-10103
selected_3<-10175

selection1.binding_data<-affinity_selectivity%>%filter(hms_id==selected_1)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(mean_affinity,selectivity_class)
selection1.title<-paste0(unique(selection1.binding_data$hms_id),";",unique(selection1.binding_data$name))
selection1.display_table<-selection1.binding_data[1:7,c(3,4,5)]

selection2.binding_data<-affinity_selectivity%>%filter(hms_id==selected_2)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(mean_affinity,selectivity_class)
selection2.title<-paste0(unique(selection2.binding_data$hms_id),";",unique(selection2.binding_data$name))
selection2.display_table<-selection2.binding_data[1:7,c(3,4,5)]

selection3.binding_data<-affinity_selectivity%>%filter(hms_id==selected_3)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(mean_affinity,selectivity_class)
selection3.title<-paste0(unique(selection3.binding_data$hms_id),";",unique(selection3.binding_data$name))
selection3.display_table<-selection3.binding_data[1:7,c(3,4,5)]

selection1.title
selection1.display_table
selection2.title
selection2.display_table
selection3.title
selection3.display_table

## option to download full binding data


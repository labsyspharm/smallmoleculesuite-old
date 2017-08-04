## compound similarity example

library(dplyr)

######################################################################################T
# Set directories and import files -----
######################################################################################T
dir_compound_similarity_app<-paste0("/Users/nienke/Dropbox/Harvard/CBDM-SORGER/Collaborations/LINCS_Compound_Database_NM/custom_informer_set/",
                                    "drug_browser/compound_similarity_app")
setwd(dir_compound_similarity_app)
similarity_table<-read.csv("similarity_table_ChemblV22_1_20170804.csv",stringsAsFactors = F)
affinity_selectivity<-read.csv("affinity_selectivity_table_ChemblV22_1_20170804.csv",stringsAsFactors = F)

######################################################################################T
# Example user input -----
######################################################################################T

## subset current data
c.compound<-10020
c.data<-similarity_table%>%filter(hmsID_1==c.compound)

View(c.data)

## display scatterplots
## show c.data plotted w/selection boxes

## after selecting 2 additional compounds
selected_1<-10023
selected_2<-10052

## show affinity data of reference compound+ selected compounds
min_affinity<-0
max_affinity<-1000000
max_sd<-100000
min_measurements<-2

selectivity_order<-c("best_class","second_class","non_specific","unknown","other")

c.binding_data<-affinity_selectivity%>%filter(hms_id==c.compound)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(selectivity_class,mean_affinity)
c.title<-paste0(unique(c.binding_data$hms_id),";",unique(c.binding_data$name))
c.display_table<-c.binding_data[1:7,c(3,4,5)]

selection1.binding_data<-affinity_selectivity%>%filter(hms_id==selected_1)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(selectivity_class,mean_affinity)
selection1.title<-paste0(unique(selection1.binding_data$hms_id),";",unique(selection1.binding_data$name))
selection1.display_table<-selection1.binding_data[1:7,c(3,4,5)]

selection2.binding_data<-affinity_selectivity%>%filter(hms_id==selected_2)%>%
  filter(mean_affinity>=min_affinity & mean_affinity<= max_affinity)%>%
  filter(SD_affinity<=max_sd)%>%
  filter(n_measurements>=min_measurements)%>%mutate(selectivity_class=factor(selectivity_class,levels=selectivity_order))%>%
  arrange(selectivity_class,mean_affinity)
selection2.title<-paste0(unique(selection2.binding_data$hms_id),";",unique(selection2.binding_data$name))
selection2.display_table<-selection2.binding_data[1:7,c(3,4,5)]

c.title
c.display_table
selection1.title
selection1.display_table
selection2.title
selection2.display_table


## option to download c.binding_data

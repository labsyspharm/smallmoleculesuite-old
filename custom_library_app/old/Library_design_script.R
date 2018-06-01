## this script is example code for generating a custom library

library(dplyr)
################################################################################################T
# import tables ------------
################################################################################################T
dir_selection_tables<-"add_directory_path"

dir_selection_tables<-paste0("/Users/nienke/Dropbox/Harvard/CBDM-SORGER/Collaborations/LINCS_Compound_Database_NM/custom_informer_set",
                             "/drug_browser/custom_library_app")

setwd(dir_selection_tables)
selection_table_selectivity<-read.csv("selection_table_selectivity.csv",stringsAsFactors = F)
selection_table_clindev<-read.csv("selection_table_clinical_development.csv",stringsAsFactors = F)
merge_cmpd_info<-read.csv("cmpd_info_library_designer.csv")
merge_table_geneinfo<-read.csv("gene_info_library_designer.csv")

################################################################################################T
# example ------------
################################################################################################T

# 1. user can specify what type of data to be included in each tier
# 2. a table will appear on screen with an option to download
# 3. figures that represent the coverage in structure, pheno and target selectivity will appear (scripts not included in this example)

# user input genes -----
genes<-c(25, 673, 2475) #c(ABL1,BRAF,MTOR)

# user input selectivity -----
## user checks boxer which category to be included
input_selectivity<-
  data.frame(option=c("Best class","Second class","Non-specifc","Unknown selectivity"),
             source_name_1=c("bestclass_I","secondclass_I","TBD","TBD"),
             source_name_2=c("bestclass_II","secondclass_II","TBD","TBD"),
             included=c(TRUE,FALSE,FALSE,FALSE),stringsAsFactors = F)

c.sources_selectivity<-c(input_selectivity[input_selectivity$included==TRUE,]$source_name_1,
                         input_selectivity[input_selectivity$included==TRUE,]$source_name_2)

# user input clinical phase-----

## user checks boxer which category to be included
input_clinical_phase<-
  data.frame(option=c("Approved", "Phase III","Phase II","Phase I"),
             source_name=c("approved","max_phase_3","max_phase_2","max_phase_1"),
             included=c(TRUE,FALSE,FALSE,FALSE),stringsAsFactors = F)
c.sources_clindev<-c(input_clinical_phase[input_clinical_phase$included==TRUE,]$source_name)

## user can specify thresholds for inclusion
max_mean_affinity_threshold<-1000
max_sd_treshold<-100
min_measurement_threshold<-1

# outputs -----
output_selectivity<-selection_table_selectivity%>%filter(source %in% c.sources_selectivity)%>%
  filter(gene_id %in% genes)
output_clindev<-selection_table_clindev%>%filter(source %in% c.sources_clindev)%>%
  filter(gene_id %in% genes)%>%
  filter(mean_aff<=max_mean_affinity_threshold)%>%
  filter(SD_aff<=max_sd_treshold)%>%
  filter(n_measurement>=min_measurement_threshold)
names(output_clindev)[5]<-"mean_Kd"

output_table<-rbind(output_selectivity[c("gene_id","molregno","mean_Kd","n_measurement","source")],
                    output_clindev[c("gene_id","molregno","mean_Kd","n_measurement","source")])

table_display_per_entry<-unique(output_table%>%
                                  merge(merge_cmpd_info[c("molregno","chembl_id","pref_name","max_phase")],by="molregno")%>%
                                  merge(merge_table_geneinfo,by="gene_id"))
table_display_per_entry<-table_display_per_entry[c("symbol","chembl_id","pref_name","source",
                                                   "max_phase","mean_Kd","n_measurement","gene_id","tax_id")]
table_display_per_cmpd<-unique(output_table%>%
                                 merge(merge_cmpd_info[c("molregno","chembl_id","pref_name","max_phase","alt_names","inchi")],
                                       by="molregno")%>%
                                 merge(merge_table_geneinfo,by="gene_id"))%>%
  group_by(molregno,chembl_id,pref_name,alt_names,inchi,max_phase)%>%
  summarise(sources=toString(paste0(symbol,";",source)))

View(table_display_per_cmpd)
View(table_display_per_entry)

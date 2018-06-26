## this script calculates the PFP correlation from the phenotypic data obtained with script '02_collecting_data_chembl.R'

library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)

`%nin%`<-Negate(`%in%`)
################################################################################################T
# set connection, directories and impost files ------------
################################################################################################T
dir_chembl_data<-"mychembldatafolder"
dir_PFPtable<-"myPFPtablefolder"

setwd(dir_chembl_data)
phenotypic_data<-readRDS("phenotypic_assaydata_mycompounds_chembl.RDS")

################################################################################################T
# convert assay results to r-scores ------------
################################################################################################T

pheno_data.g<-dlply(phenotypic_data,.(assay_id),c)

rscore_table<-list()
index_table<-0
for(index in 1:length(pheno_activities.g)){
  c.data<-as.data.frame(pheno_activities.g[[index]])
  if(dim(c.data)[1]>2){
    index_table<-index_table+1
    c.med<-median(c.data$log10_value)
    c.mad<-mad(c.data$log10_value)
    c.result<-list()
    c.result$assay_id<-unique(c.data$assay_id)
    c.result$molregno<-c.data$molregno
    c.result$rscore<-(c.data$log10_value - c.med)/c.mad
    c.result$rscore_tr<-ifelse(c.result$rscore<0,
                               5 * (1 / (1+ exp(-(c.result$rscore + 2.5)*2)) - 1) + 0.0335, 
                               5 * (1 / (1+ exp(-(c.result$rscore - 2.5)*2))) - 0.0335)
    rscore_table[[index]]<-as.data.frame(c.result)
  }
  print(paste0(index,"-",length(pheno_activities.g)))
}

rscore_table.b<-rbindlist(rscore_table)%>%arrange(molregno)
dim(rscore_table.b)

setwd(dir_chembl_data)
write.csv(file="phenotypic_rscoredata_chembl.csv",rscore_table.b, row.names=F)
saveRDS(rscore_table.b,file = "phenotypic_rscoredata_chembl.RDS")

################################################################################################T
# calculate pearson correlation ------------
################################################################################################T

loop_table_1<-rscore_table.b%>%group_by(molregno)%>%filter(n()>5)
loop_table<-dlply(loop_table_1,.(molregno),c)

correlation_table<-list()
corr_index<-0
for(index_1 in 1:length(loop_table)){
  c.data_1<-as.data.frame(loop_table[[index_1]])
  for(index_2 in 1:length(loop_table)){
    if(index_1 != index_2){
      c.data_2<-as.data.frame(loop_table[[index_2]])
      c.vector<-inner_join(c.data_1,c.data_2,by="assay_id")
      c.length_both_active<-c.vector%>%
        filter(rscore.x != -Inf & rscore.x != Inf & rscore.y != -Inf & rscore.y != Inf)%>%
        filter(rscore_tr.x >2.5 |rscore_tr.x < -2.5|rscore_tr.y >2.5 |rscore_tr.y < -2.5)
      if(dim(c.length_both_active)[1]>5){
        c.result<-list()
        c.result$molregno_1<-unique(c.data_1$molregno)
        c.result$molregno_2<-unique(c.data_2$molregno)
        c.result$pearson_corr<-cor(c.vector$rscore_tr.x,c.vector$rscore_tr.y,use = "pairwise.complete.obs")
        c.result$n_common<-dim(c.vector)[1]
        c.result$n_common_active<-dim(c.length_both_active)[1]
        corr_index<-corr_index+1
        correlation_table[[corr_index]]<-as.data.frame(c.result)
      }
    }
    print(paste0(index_1,"-",index_2,"-",length(loop_table)))
  }
}

correlation_table.b<-rbindlist(correlation_table)
dim(correlation_table.b)


setwd(dir_output)
saveRDS(correlation_table_names,file="PFP_correlationtable_chembl.RDS")
write.csv(correlation_table_names,file="PFP_correlationtable_chembl.csv",row.names = F)


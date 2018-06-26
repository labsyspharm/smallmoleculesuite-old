## this script calculates selectivity scores from biochemical data obtained with scripts '02_collecting_data_chembl.R'


library(plyr)
library(dplyr)
library(data.table)

`%nin%`<-Negate(`%in%`)
################################################################################################T
# set directories, import files------------
################################################################################################T
dir_chembl_data<-"mychembldatafolder"
dir_selectivity_data<-"myselectivitydatafolder"

setwd(dir_chembl_data)
activities_molregno_geneid<-read.csv("biochemicaldata_mycompounds_chembl.csv",stringsAsFactors = F)

################################################################################################T
# set toolscore function ------------
################################################################################################T

calc_toolscore<-function(data, molregno, gene_id) 
{
  example_subset<-data[data$molregno==molregno,]
  
  ontarget<-example_subset[example_subset$gene_id == gene_id,]
  offtarget<-example_subset[example_subset$gene_id !=  gene_id,]
  
  chembl_active_data<-as.numeric(as.character(ontarget$standard_value))
  chembl_active_low_nM<-chembl_active_data[chembl_active_data<=100]
  if (length(chembl_active_low_nM) > 1) {
    chembl_strength<-7
  }else if (length(chembl_active_data[chembl_active_data<=1000]) > 4) {
    chembl_strength<-4
  }else {
    chembl_strength<-1
  }
  
  strength<-chembl_strength + 1 # bonus for multiple sources
  
  ontarget_IC50<-as.numeric(as.character(ontarget[!is.na(ontarget$standard_value),]$standard_value))
  ontarget_IC50_N<-length(ontarget_IC50)
  ontarget_IC50_Q1<-quantile(ontarget_IC50, probs=0.25, na.rm=TRUE)
  offtarget_IC50<-as.numeric(as.character(offtarget[!is.na(offtarget$standard_value),]$standard_value))
  offtarget_IC50_N<-length(offtarget_IC50)
  offtarget_IC50_Q1<-quantile(offtarget_IC50, probs=0.25, na.rm=TRUE)
  Q1_IC50_diff<-log10(offtarget_IC50_Q1)-log10(ontarget_IC50_Q1);
  if (length(ontarget_IC50)==0 || length(offtarget_IC50)==0) {
    wilcox_pval<-10;
  } else {
    w<-wilcox.test(ontarget_IC50, offtarget_IC50, alternative='less');
    if (w$p.value>=1E-15){
      wilcox_pval<-w$p.value;
    } else {
      wilcox_pval<-1E-15;
    }
  }
  
  IC50<-c(ontarget_IC50, offtarget_IC50)
  IC50_Q1<-quantile(IC50, probs=0.25, na.rm=TRUE)
  investigation_bias<-length(ontarget_IC50)/length(IC50)
  
  selectivity<-(Q1_IC50_diff/3 + (1-investigation_bias) - log10(wilcox_pval)/15)/3
  tool_score<- strength * selectivity
  #names(tool_score)<-'tool score'
  return (list(tool_score[[1]],strength,selectivity[[1]],investigation_bias,wilcox_pval,
               Q1_IC50_diff[[1]],offtarget_IC50_Q1[[1]],ontarget_IC50_Q1[[1]],ontarget_IC50_N,offtarget_IC50_N))
}

################################################################################################T
# calculate toolscores ------------
################################################################################################T

toolscore<-list()
toolscore_index<-0

molregno_list<-dlply(activities_molregno_geneid,.(molregno),c)

for(index_cmpd in 1:length(molregno_list)){
  c.data<-as.data.frame(c.molregno_list[[index_cmpd]])
  c.molregno<-unique(c.data$molregno)
  c.targets<-unique(c.data$gene_id)
  for(index_target in 1:length(c.targets)){
    c.df<-list()
    c.gene_id<-c.targets[index_target]
    c.df$molregno<-c.molregno
    c.df$gene_id<-c.gene_id
    c.return<-calc_toolscore(c.data, c.molregno, c.gene_id)
    if(is.na(c.return[[3]])==F){
      toolscore_index<-toolscore_index+1
      c.df$tool_score<-c.return[[1]]
      c.df$strength<-c.return[[2]]
      c.df$selectivity<-c.return[[3]]
      c.df$investigation_bias<-c.return[[4]]
      c.df$wilcox_pval<-c.return[[5]]
      c.df$IC50_diff<-c.return[[6]]
      c.df$ontarget_IC50_Q1<-c.return[[8]]
      c.df$offtarget_IC50_Q1<-c.return[[7]]
      c.df$ontarget_IC50_N<-c.return[[9]]
      c.df$offtarget_IC50_N<-c.return[[10]]
      c.df$N_total<-c.return[[9]]+c.return[[10]]
      toolscore[[toolscore_index]]<-as.data.frame(c.df)
    }
  }
  print(paste0(index_cmpd,"-",length(c.molregno_list)))
}

toolscore.b<-rbindlist(toolscore)

setwd(dir_selectivity_data)
write.csv(toolscore.b,file="selectivitydata_mycompounds_chembl.csv",row.names = F)


## this script obtains biochemical and phenotypic data from the chembl22_1 database

require("RPostgreSQL")
library(dplyr)
library(data.table)

`%nin%`<-Negate(`%in%`)
################################################################################################T
# set connection, directories and impost files ------------
################################################################################################T
dir_chembl_data<-"mychembldatafolder"
dir_compound_list<-"mycompoundlistfolder"
dir_mapping_tables<-"mydirmappingtables" ## mapping tables can be found in supplementary material

##connect to local copy of chembl database
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "chembl_22_1",
                 host = "localhost", port = 5433,
                 user = "myusername")

setwd(dir_compound_list)
compound_list<-read.csv("02_list_example.csv")

setwd(dir_mapping_tables)
map_tid_geneID<-read.csv("Suppl_Table03C_CHEMBL_target_map.csv",stringsAsFactors = F)
################################################################################################T
# get biochemical data ------------
################################################################################################T

activities_biochem_1<-dbGetQuery(con, paste0("select ACT.molregno,ACT.standard_relation, ACT.standard_type,
                                              ACT.standard_value,ACT.standard_units,
                                             A.tid,
                                             A.description,A.chembl_id as chembl_id_assay
                                             from activities as ACT
                                             left join assays as A
                                             on ACT.assay_id = A.assay_id
                                             where ACT.molregno in (",toString(unique(compound_list$molregno)),")
                                             and ACT.standard_value is not null
                                             and A.assay_type = 'B'
                                             and A.relationship_type in ('D', 'H', 'M', 'U')
                                             and ACT.standard_units = 'nM'
                                             and ACT.standard_type in ('IC50','Ki','EC50','Kd','IC90','CC50','ID50','AC50','Inhibition','MIC','Potency','Activity','ED50')
                                             and A.assay_cell_type is NULL
                                             "))

activities_biochem_2<-dbGetQuery(con, paste0("select ACT.molregno,ACT.standard_relation, ACT.standard_type,
                                             ACT.standard_value,ACT.standard_units,
                                             A.tid,
                                             A.description,A.chembl_id as chembl_id_assay
                                             from activities as ACT
                                             left join assays as A
                                             on ACT.assay_id = A.assay_id
                                             where ACT.molregno in (",toString(unique(compound_list$molregno)),")
                                             and ACT.standard_value is not null
                                             and A.assay_type = 'F'
                                             and A.description like '%Navigating the Kinome%'
                                             "))

activities_biochem<-list()
activities_biochem[[1]]<-activities_biochem_1
activities_biochem[[2]]<-activities_biochem_2

activities_biochem<-rbindlist(activities_biochem)%>%data.table(.)
activities_biochem_geneid<-left_join(activities_biochem,map_tid_geneID,by="tid")%>%as.data.frame(.)

setwd(dir_chembl_data)
write.csv(activities_biochem_geneid,file = "biochemicaldata_mycompounds_chembl.csv",row.names = F)


################################################################################################T
# get phenotypic data ------------
################################################################################################T
standard_units<-dbGetQuery(con, paste0("
                          select distinct(standard_units) 
                          from activities as ACT
                          where ACT.molregno in (",toString(compound_list$molregno),")"))

standard_units_ok<-c('M','mol/L','nM','nmol/L',
                       'nmol.L-1','pM','pmol/L','pmol/ml','um',
                       'uM','umol/L','umol/ml','umol/uL')

assay_freq<-dbGetQuery(con, paste0(" select assay_id, count(molregno)
                                    from activities
                                     where standard_units in ('",paste(standard_units_ok,collapse="','"),"')
                                    group by assay_id
                                    "))

units_per_assay<-dbGetQuery(con, paste0(" select assay_id, count(distinct(standard_units)) as count_units,
                                        count(molregno) as count_molregno
                                        from activities
                                        where standard_units in ('",paste(standard_units_ok,collapse="','"),"')
                                        group by assay_id
                                        "))
assays_qualified<-units_per_assay%>%filter(count_units==1)%>%filter(count_molregno>2)

activities_1<-dbGetQuery(con, paste0("select ACT.molregno,ACT.standard_relation, ACT.standard_type,ACT.standard_value,ACT.standard_units,
                                      A.tid,A.assay_type,A.assay_test_type,A.assay_category,A.description,A.assay_tax_id,A.assay_strain,
                                     A.assay_tissue,A.assay_cell_type,A.assay_subcellular_fraction,A.relationship_type, A.assay_id
                                     from activities as ACT
                                     left join assays as A
                                     on ACT.assay_id = A.assay_id
                                     where ACT.standard_value is not null
                                     and A.relationship_type like 'N'
                                     and A.assay_id in (",toString(assays_qualified$assay_id),")
                                     and ACT.standard_units in ('",paste(standard_units_ok,collapse="','"),"')
                                     "))
dim(activities_1)

activities_2<-dbGetQuery(con, paste0("select ACT.molregno,ACT.standard_relation, ACT.standard_type,ACT.standard_value,ACT.standard_units,
                                     A.tid,A.assay_type,A.assay_test_type,A.assay_category,A.description,A.assay_tax_id,A.assay_strain,
                                     A.assay_tissue,A.assay_cell_type,A.assay_subcellular_fraction,A.relationship_type, A.assay_id
                                     from activities as ACT
                                     left join assays as A
                                     on ACT.assay_id = A.assay_id
                                     where ACT.standard_value is not null
                                     and A.relationship_type in ('D', 'H', 'M', 'U')
                                     and A.assay_type in ('F','A','T','U')
                                     and A.description not like '%Navigating the Kinome%'
                                     and A.assay_id in (",toString(assays_qualified$assay_id),")
                                     and ACT.standard_units in ('",paste(standard_units_ok_2,collapse="','"),"')
                                     "))
dim(activities_2)

dbDisconnect(con)

pheno_activities<-list()
pheno_activities[[1]]<-activities_1
pheno_activities[[2]]<-activities_2
pheno_activities<-rbindlist(pheno_activities)
pheno_activities$log10_value<-log10(pheno_activities$standard_value)
pheno_activities<-pheno_activities%>%filter(log10_value != "NaN")%>%filter(standard_units %in% standard_units_ok_2)

dim(pheno_activities)

setwd(dir_output)
write.csv(file="phenotypic_assaydata_mycompounds_chembl.csv",pheno_activities, row.names=F)
saveRDS(pheno_activities,file = "phenotypic_assaydata_mycompounds_chembl.RDS")

dbDisconnect(con)
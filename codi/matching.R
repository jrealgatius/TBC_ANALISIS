

# 4. Preparar matching i setriskmatching  ----------------------------

#4.	Exact matching the exposed cohort (EC) to the candidate non-exposed patients (CNE)
#   with a ratio 1:10 (EC:CNE) by year of birth (+/-1year), sex, and practice,
#   without replacement (each candidate non-exposed patient can be only matched once). 
#   This is the matched cohort (MC), and the index date is the same as the matched exposed patient.


dt_matching<-dt_matching %>% 
  transmute(idp,dnaix,sexe,grup,dtevent=data_index,entrada,sortida,
            DG18.cancer,DG18.prevalent_CVD,DG18.prevalent_KD,DG18.prevalent_MET) %>%
  left_join(LLEGIR.variables_geo_sanitaries,by="idp")


# Generar data de sortida (Data event / Data de censura)     
dt_matching<-dt_matching %>% 
  mutate(dtindex_case=ifelse(grup==1, as.Date(as.character(dtevent),format="%Y%m%d"),NA)) 

## dtindex_control
dt_matching<-dt_matching %>% 
  mutate (dtindex_control=as.Date(as.character(sortida),format="%Y%m%d")%>% as.numeric())

## Generar any de naixament 
dt_matching<-dt_matching %>% mutate (
  any_naix=lubridate::year(lubridate::ymd(dnaix))) 

#vii [Parametres d'aparellament: llistaPS=c("sexe","any_naix","iddap")] 

# Parametres d'aparellament
llistaPS<-extreure.variables("matching",conductor)


num_controls<-10
llavor<-125
set.seed(llavor)
gc()


# viii)					[MATCHING 1:10]   -----------

#heaven::riskSetMatch

detach("package:tidyr", unload = TRUE)


dades_match<-heaven::riskSetMatch(ptid="idp"                   # Unique patient identifier
                                  ,event="grup"                # 0=Control, 1=case
                                  ,terms=llistaPS              # terms c("n1","n2",...) - list of vairables to match by
                                  ,dat=dt_matching              # dataset with all variables
                                  ,Ncontrols=num_controls         # number of controls to provide
                                  ,oldevent="oldevent"            # To distinguish cases used as controls
                                  ,caseid="caseid"                # variable to group cases and controls (case-ptid)
                                  ,reuseCases=F                   # T og F or NULL - can a case be a control prior to being a case?
                                  ,reuseControls=F                # T or F or NULL - can controls be reused?
                                  ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                                  ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                                  ,NoIndex=FALSE                # If T ignore index
                                  ,cores=1                      # Number of cores to use, default 1
                                  ,dateterms=c("DG18.cancer","DG18.prevalent_CVD","DG18.prevalent_KD","DG18.prevalent_MET")) 
library(tidyr)
gc()





dt_matching<-KK %>% 
  mutate(dtindex_case=ymd(data) %>% as.numeric(), dtindex_control=ifelse(grup==0,data_sortida,NA)) %>% 
  mutate(any_naix=year(dNaixement)) %>% 
  unique() %>% # Eliminar repetits
  group_by(idp) %>% arrange(DET_TB) %>% dplyr::slice(1L) %>% ungroup()

llistaPS<-c("sexe","any_naix")
num_controls<-1


dades_match<-heaven::riskSetMatch(ptid="idp"                   # Unique patient identifier
                                  ,event="grup"                # 0=Control, 1=case
                                  ,terms=llistaPS              # terms c("n1","n2",...) - list of vairables to match by
                                  ,dat=dt_matching              # dataset with all variables
                                  ,Ncontrols=num_controls         # number of controls to provide
                                  ,oldevent="oldevent"            # To distinguish cases used as controls
                                  ,caseid="caseid"                # variable to group cases and controls (case-ptid)
                                  ,reuseCases=F                   # T og F or NULL - can a case be a control prior to being a case?
                                  ,reuseControls=F                # T or F or NULL - can controls be reused?
                                  ,caseIndex="dtindex_case"       # Integer or date, date where controls must be prior
                                  ,controlIndex="dtindex_control" # controlIndex - Index date for controls
                                  ,NoIndex=FALSE                # If T ignore index
                                  ,cores=1                      # Number of cores to use, default 1
)


# Aviam repetits
# Número de controls per cas

dades_match<-dades_match %>% group_by(idp) %>% mutate(id_num=seq(1:n()), n=n()) %>% ungroup()


dades_match %>% distinct(idp)


heaven::matchReport(dades_match, id="idp",case="grup",caseid="caseid",oldcase = "oldevent")

dades_match %>% group


dades_match<-dades_match %>% group_by(idp) %>% mutate(n=n()) %>% ungroup()

table(dades_match$n)



###### Heaven2

library("heaven",lib="C:/Rpackages/heavenv2")












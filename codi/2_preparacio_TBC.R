# Preparació de dades 

# 

# 1. Càrrega de funcions ----------------------  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


source("codi/global_TBC.R")

# FUNCIÓ 
valida_quanti<-function(dt=dades,y="valor_basal.GLICADA",grup="constant") {
  dt$constant=1
  # dt=data_long
  # y="valor_basal.GLICADA"
  # grup="SEXE"
  summ1 <- paste0('min(', y, ',na.rm=T)')
  summ2<-paste0('max(',y,',na.rm=T)')
  
  dt %>% dplyr::group_by_(grup) %>% 
    dplyr::summarise_(min=summ1,
                      max=summ2,
                      n="n()") %>%  rename("group"=grup) }


# 2. Paràmetres  ----------------------

conductor_variables<-"variables_tbc.xls"

# 3. Lectura -------------

load("dades/output/output.Rdata")

# Generar conductor dades 
#    PREPARACIÓ          -----------------------
library(lubridate)


# Genero variable grup (Si un ja te antecedent DM aquest ja no potser un event )
dt_ecap<- dt_ecap %>% 
  mutate(grup=if_else(DG.DM<=ymd(dtindex),"DM","Control"),
         grup=if_else(is.na(DG.DM),"Control",grup))
dt_ecap<- dt_ecap %>% 
  mutate(EV.DM=ifelse(grup=="DM",NA,EV.DM),
         EV.DM=as_date(EV.DM)) 

# Truncament a 31/12/2016  -------
# Generar variable antecedent TBC / EVENT TBC (<31/12/2016) -------
dt_ecap <-dt_ecap %>% 
  mutate (DG.TBC=ifelse(DET_TB>ymd(dtindex) | is.na(DET_TB) ,NA,DET_TB),
          EV.TBC=ifelse(DET_TB<=ymd(dtindex) | is.na(DET_TB),NA,DET_TB),
          DG.TBC=as_date(DG.TBC),
          EV.TBC=as_date(EV.TBC)) 


dt_ecap %>% valida_quanti("DET_TB")
dt_ecap %>% valida_quanti("DG.TBC")

# Actualització situacio o data final de seguiment 31/12/2016 o mort --------------
data_maxima<-20181231

dt_ecap<-
  dt_ecap %>% 
  mutate(dsituacio=ifelse(dsituacio>ymd(data_maxima),ymd(data_maxima),dsituacio),
         dsituacio=ifelse(situacio=="D",dsituacio,ymd(data_maxima)),
         dsituacio=ifelse(is.na(dsituacio),ymd(data_maxima),dsituacio),
         dsituacio=as_date(dsituacio),
         situacio=ifelse(is.na(situacio),"A",situacio)) 

dt_ecap %>% valida_quanti("dsituacio","situacio")

table(dt_ecap$situacio)
dt_ecap %>% filter(situacio=="D")%>% select(idp,situacio,dsituacio,EV.DM)
dt_ecap %>% filter(situacio=="D" & EV.DM>dsituacio) %>% select(situacio,dsituacio,EV.DM)

# Si situacio=="D" &  EV.DM>dsituacio ---> situacio="A" i data situacio = data_maxima
dt_ecap<-dt_ecap %>% 
  mutate(situacio=ifelse(situacio=="D" & (EV.DM>dsituacio & !is.na(EV.DM)),"A",situacio))

# Actualitzar data de situacio postmortem per data de DG
dt_ecap<-dt_ecap %>% 
  mutate(dsituacio=ifelse(dsituacio>EV.DM | is.na(EV.DM),dsituacio,ymd(data_maxima)),
         dsituacio=as_date(dsituacio))

dt_ecap %>% valida_quanti("dsituacio","situacio")

# Generar data fi de seguiment / event principal TBC previ a 31/12/2016 / o data de DM en controls   -------

# Si un CONTROL passa a DM s'acaba seguiment a data de DIABETIS
dt_ecap<-
  dt_ecap %>% 
  mutate(datafi=ifelse(grup=="Control" & EV.DM>=ymd(dtindex),EV.DM,dsituacio),
         datafi=ifelse(grup=="Control" & is.na(EV.DM),dsituacio,datafi),
         datafi=as_date(datafi)) 

dt_ecap %>% valida_quanti("datafi","situacio")


#  filtre 1: mort inici de seguiment ----------------
dt_ecap<-
  dt_ecap %>% mutate(filtre_exitus=ifelse(situacio=="D" & dsituacio<=ymd(dtindex),1,0))

# Temps de seguiment  ----------
dt_ecap<-dt_ecap %>% mutate(temps_seguiment=datafi-ymd(dtindex))

dt_ecap %>% valida_quanti("temps_seguiment","filtre_exitus")


# Diagnostics/Events basals (Recode) (NA --> 0 (Else=1) (No hi ha 0))   ----------------
dt_ecap<-dt_ecap %>% mutate_at(vars(starts_with("DG.")),funs('cat'=if_else(is.na(.) | 0,0,1))) 
dt_ecap<-dt_ecap %>% mutate_at(vars(starts_with("EV.")),funs("cat"=if_else(is.na(.) | 0,0,1))) 


#  filtre 2: TBC inici de seguiment ----------------
dt_ecap<-
  dt_ecap %>% mutate(filtre_TBC=ifelse(DG.TBC_cat==1,1,0))


# Calculo temps de seguiment  ----------
dt_ecap<-dt_ecap %>% mutate(temps_seguiment=datafi-ymd(dtindex))

dt_ecap %>% valida_quanti("temps_seguiment","filtre_exitus")


# Temps lliure d'esdeveniment TBC i event_tbc -----------------------

dt_ecap<-dt_ecap %>% 
  mutate(dt_lliure_TBC=pmin(datafi,EV.TBC,na.rm = T),
         temps_tbc=dt_lliure_TBC-ymd(dtindex),
         event_tbc=ifelse(EV.TBC<=datafi,1,0),
         event_tbc=ifelse(is.na(event_tbc) | event_tbc==0,0,1))

# Surv
dt_ecap$surv_tbc<-with(dt_ecap,Surv(temps_tbc, event_tbc))


# Any de TBC diagnosticada  ------------------
dt_ecap<-dt_ecap %>% mutate(any_TBC=year(EV.TBC))


# Evolucion de la DM (dias/años)   --------------
dt_ecap<-dt_ecap %>% mutate(DM_dias=ymd(dtindex)-DG.DM,
                            DM_anys=interval(DG.DM,ymd(dtindex))/years(1)) 


# Variable edat  -----------
dt_ecap<-dt_ecap %>% mutate(age=(ymd(dtindex)-dNaixement)/365.25 %>% as.numeric())

# Data index numerica --------
dt_ecap<-dt_ecap %>% mutate(anyindex=year(ymd(dtindex)))

# Generar matching   -----------------
descrTable(grup~sexe+age,data=dt_ecap)


dt_ecap_matchejades<-dt_ecap %>% generar_matching(vars_matching=c("sexe","age","anyindex"),grup="DG.DM_cat",ratio=1)

descrTable(grup~sexe+age+anyindex,data=dt_ecap_matchejades)

dt_ecap<-dt_ecap %>% left_join(select(dt_ecap_matchejades,CIP,PS),by="CIP")



# Arreglar dt_agencia ---------------

# Filtro aquells que tenen data de TBC en qualsevol moment o marcats com a DM o control en variable
dt_agencia<-dt_agencia %>% filter(!is.na(DET_TB) | !is.na(DIABETIS)) 

# Marco repetits 
dt_agencia<-dt_agencia %>% left_join(dt_agencia %>% group_by(CIP) %>% summarize(repe=n(), by="CIP") %>% ungroup())

# Afegeixo edat + sexe de dt_ecap ------------------
dt_agencia<-dt_agencia %>% left_join(select(dt_ecap,CIP,sexe,age,PS,grup,event_tbc,DG.TBC_cat),by="CIP") %>% select(-EDAT,-GENERE)


save(dt_agencia, file="dt_agencia.Rdata")



# Conductors de dt_ecap
# Conductor de dt_agencia

dades<-dt_ecap

ActualitzarConductor2(dades,"variables_tbc_ver2.xlsx")

ActualitzarConductor2(d=dades,taulavariables = "variables_tbc_ver2.xlsx")

write.csv2(names(dt_ecap),"lll.csv")








# Aparello dades de l'agencia  -----------------
descrTable(grup~sexe+age,data=dt_agencia)

dt_agencia_matchejades<-select(dt_ecap,-PS) %>% 
  filter(filtre_exitus==0 & filtre_TBC==0 & dt_agencia==1) %>% 
  generar_matching(vars_matching=c("sexe","age"),grup="DG.DM_cat",ratio=1)

descrTable(grup~sexe+age,data=dt_agencia_matchejades)

## 



##  ANALISIS PRELIMINAR-----
#  CURVAS DE SUPERVIVENCIA 
library(survminer)

dades<-dt_ecap %>% filter(filtre_exitus==0 & filtre_TBC==0 & dt_agencia==1)


survfit(surv_tbc ~ grup, data=dt_agencia_matchejades) %>%
  ggsurvplot(title="TB detection in patient with or without DM",
             p.val=T, xlab="Time (days)", censor=F, linetype="strata",
             fun="event", cumevents = T)

descrTable(surv_tbc~grup,data=dt_agencia_matchejades,show.ratio = T, byrow = T )


dades<-dt_ecap %>% filter(filtre_exitus==0 & filtre_TBC==0 & dt_agencia==1)

survfit(surv_tbc ~ grup, data=dades) %>%
  ggsurvplot(title="TB detection in patient with or without DM",
             p.val=T, xlab="Time (days)", censor=F, linetype="strata",
             fun="event", cumevents = T)

descrTable(surv_tbc~grup,data=dades,show.ratio = T, byrow = T )

descrTable(grup~sexe+age,data=dades)


#  ------------















































# 4. Format: Netejar espais en blanc   -----------

dt_ecap<-netejar_espais(dt_ecap)
dt_agencia<-netejar_espais(dt_agencia)

# 5. Eliminar duplicats  --------------

#         dt_agencia 




# Filtro per CASOS
dt_agencia<-dt_agencia %>% filter(repe==1 | (repe>1 & Mostra=="CASOS"))

# Torno a marcar repes
dt_agencia<-dt_agencia %>% left_join(dt_agencia %>% group_by(CIP) %>% summarize(N=n()) %>% ungroup())

# Agafo registre unic 
dt_agencia<-dt_agencia %>% filter (N==1 | INDIGENT=="NO") %>% select(-N)


# 6. Càlculs   -----------------
dades<-dt_ecap

# Tots els DM (CASOS) -- > data DM post = NA 
dades<-dades %>% mutate(EV.DM=ifelse(Mostra=="CASOS",NA,EV.DM),EV.DM=lubridate::as_date(EV.DM)) 

# Data_DM
dades<-dades %>% mutate(data_ANT_DM=DG.DM)
dades<-dades %>% mutate(data_EV_DM=EV.DM)

# Diagnostics/Events basals (Recode) (NA --> 0 (Else=1) (No hi ha 0))
dades<-dades %>% mutate_at(vars(starts_with("DG.")),funs(if_else(is.na(.) | 0,0,1))) 
dades<-dades %>% mutate_at(vars(starts_with("EV.")),funs(if_else(is.na(.) | 0,0,1))) 

# Antedecentes / Eventos / Final de Diabeticos 
dades<-dades %>% mutate(ant_dm=if_else(is.na(DG.DM),0,1))
dades<-dades %>% mutate(event_dm=if_else(is.na(EV.DM),0,1))
dades<-dades %>% mutate(final_dm=if_else(event_dm | ant_dm,1,0))


# Evento/Antecedente Tuberculosis SI / tuberculosis NO 
dades<-dades %>% mutate(event_tb = ifelse(DET_TB>lubridate::ydm(20070101),1,0),
                        event_tb = ifelse(event_tb==0 | is.na(event_tb),0,1),
                        dt_event_tb=ifelse(event_tb,DET_TB,NA) %>% lubridate::as_date())

#
# Temps de seguiment (data_fi de seguiment)

#TIEMPOS; / Si se detecta TB fecha fin esa fecha / #Si muere fecha fin esa fecha, / #Si es control y se detecta DM , esa fecha de diagnostico 
# SINO Fecha Fin (Ultima detección de TB)

dades<-dades %>% mutate(data_exitus=ifelse(situacio=="D",dsituacio,NA) %>%lubridate::as_date() ,
                     data_trasllat=ifelse(situacio=="T",dsituacio,NA) %>%lubridate::as_date() ) 


# Data final--> minima entre data exitus / event / DM
dades<-dades %>% mutate(data_final=pmin(data_exitus,
                         
                         dt_event_tb,
                         data_EV_DM,
                         as.Date("2018-02-15"),
                         na.rm=T)) 

dades %>% count(event_tb)
dades %>% mutate(pp=ifelse(dt_event_tb>data_EV_DM,0,event_tb)) %>% select(event_tb,pp) %>% count(event_tb)



#CRITERIOS EXCLUSION 
#Todos los que se les haya detectado TB antes del 01/01/2007
dt_total<-dt_total %>% filter((DET_TB >= dat_inici | is.na(DET_TB))) #Eliminados todos los TB antes del 2007
#Todos los muertos (situacio = D) antes del 01/01/2007 el resto los dejamos 
dt_total <- dt_total %>% filter((situacio == "D" & dsituacio > dat_inici) | situacio %in% c("A", "T"))







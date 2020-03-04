# Preparació de dades 

# 

# 1. Càrrega de funcions ----------------------  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# 2. Paràmetres  ----------------------

conductor_variables<-"variables_tbc.xls"

# 3. Lectura -------------

load("dades/output/output.Rdata")

# Generar conductor dades 

# 4. Format: Netejar espais en blanc   -----------

dt_ecap<-netejar_espais(dt_ecap)
dt_agencia<-netejar_espais(dt_agencia)

# 5. Eliminar duplicats  --------------

#         dt_ecap  

# Marco repetits 
dt_ecap<-dt_ecap %>% left_join(dt_ecap %>% group_by(CIP) %>% summarize(repe=n()) %>% ungroup())

# Primer valor valid
dt_ecap <- dt_ecap %>% group_by(CIP) %>% summarise_each(funs(first(.[!is.na(.)]))) %>% ungroup()

#         dt_agencia 

# Marco repetits 
dt_agencia<-dt_agencia %>% left_join(dt_agencia %>% group_by(CIP) %>% summarize(repe=n()) %>% ungroup())

# Filtro per CASOS
dt_agencia<-dt_agencia %>% filter(repe==1 | (repe>1 & Mostra=="CASOS"))

# Torno a marcar repes
dt_agencia<-dt_agencia %>% left_join(dt_agencia %>% group_by(CIP) %>% summarize(N=n()) %>% ungroup())

# Agafo registre unic 
dt_agencia<-dt_agencia %>% filter (N==1 | INDIGENT=="NO") %>% select(-N)


# 6. Càlculs   -----------------
dades<-dt_ecap


# Casos/controls en data basal 
dades<-dades %>% mutate(Mostra=
                          case_when(DG.DM<=lubridate::ymd(20070101)~"CASOS",
                                    EV.DM>lubridate::ymd(20070101)~"CONTROLS",
                                    is.na(DG.DM)~"CONTROLS",
                                    TRUE~"CONTROLS"))

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







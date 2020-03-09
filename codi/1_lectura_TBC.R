#                               FASE LECTURA                            ---------------------
#  Aplicació lectura de dades / aplanament / criteris d'inclusió 

memory.size(max=160685)
#
# 1. Càrrega de funcions ----------------------  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

#CANVI

# 2. Paràmetres  ----------------------

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg_tbc.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables_tbc.xls"
# fitxer conductor GENERAL
fitxer_conductor_general<-"variables_general.xls"



# 
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg,col_types = "text")


# 3. Lectura fitxers  ----------------------------

# "farmacia.csv", "Laboratori.csv", "PSalut.csv", "Vacunes.csv", "Variables.csv", "vardemografiques.csv" 

# Actualització de 4 nous fitxers: "PSalut2.csv", "Laboratori2.csv" , "pob ciutat vella ene 2007.csv" "farma2_fisdmtbv.csv"

# Actualització de 4 nous fitxers: "PSalut3.csv", "Laboratori3.csv" , "pob ciutat vella ene 2007_3.csv" "farma2_fisdmtbv_3.csv"

# Actualització d'un fitxer: "extracció tbc dm metode antic-1.csv"

library(dplyr)

#     Problemes de salut ----------
dt_psalut3<-read.csv2(here::here("dades","PSalut3.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
dt_psalut<-read.csv(here::here("dades","extracció tbc dm metode antic-1.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
# Fusió problemes
dt_psalut<-dt_psalut %>% bind_rows(dt_psalut3) %>% select(cipACT,codiPSalut,ddeteccio)
rm(dt_psalut3)

#     Laboratori/Variables -------------
# dt_laboratori_antic<-read.csv(here::here("dades","Laboratori.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
dt_laboratori<-read.csv(here::here("dades","Laboratori3.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
dt_variables<-read.csv2(here::here("dades","Variables.csv"), header=T, sep=";",dec=".",fileEncoding="utf-16") %>% as_tibble()
dt_variables<-dt_variables %>% mutate (VU_VAL= VU_VAL %>% as.character() %>% as.numeric())
# fusionar ambdues fonts
dt_variables<-dt_variables %>% bind_rows(dt_laboratori)
rm(dt_laboratori)

#     Vacunes ---------------
dt_vacunes<-read.csv(here::here("dades","Vacunes.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

#     Farmacia  -------------
dt_farmacia<-read.csv(here::here("dades","farmacia.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
dt_farmacia_nou<-read.csv(here::here("dades","farma2_fisdmtbv.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()
dt_insuline<-read.csv(here::here("dades","insulina.csv"), sep=";") %>% as_tibble()

# Formatejo farmacia
dt_farmacia<-dt_farmacia %>% select(CIP=PPFMC_PMC_USUARI_CIP,ATC=PPFMC_ATCCODI,datainici=PPFMC_DATA_INI_SIRE,datafi=PPFMC_DATA_FI_SIRE)
dt_farmacia_nou<-dt_farmacia_nou %>% select(CIP=PPFMC_PMC_USUARI_CIP,ATC=PPFMC_ATCCODI,datainici=PPFMC_PMC_DATA_INI,datafi=PPFMC_DATA_FI)
dt_insuline<-dt_insuline %>% select(CIP=PPFMC_PMC_USUARI_CIP,ATC=PPFMC_ATCCODI,datainici=PPFMC_DATA_INI_SIRE,datafi=PPFMC_DATA_FI_SIRE)

# Fusiono farmacia
dt_farmacia<-dt_farmacia_nou %>% bind_rows(dt_farmacia) %>% bind_rows(dt_insuline)
rm(dt_farmacia_nou,dt_insuline)

#     Demografiques -------------------
dt_demografiques_antic <- read.csv2(here::here("dades", "vardemografiques.csv"), sep=";") %>% as_tibble()
dt_demografiques_antic <- read_xls(here::here("dades", "vardemografiques.xls")) %>% as_tibble() %>% 
  mutate(dNaixement= as.Date(dNaixement),dsituacio= as.Date(dsituacio))

dt_demografiques <- read.csv2(here::here("dades", "pob_ciutat_vella_ene_2007.csv"), sep=";",header=T,fileEncoding="utf-16") %>% as_tibble()
dt_demografiques<-dt_demografiques %>% rename (CIP=cipACT) %>% 
  mutate(dsituacio=stringr::str_sub(as.character(dsituacio),1,10),
         dNaixement=stringr::str_sub(as.character(dNaixement),1,10))

# Agrego per CIP I agafo la última situació 
dt_demografiques<-dt_demografiques %>% 
  transmute(CIP,sexe,situacio,dNaixement=lubridate::ymd(dNaixement),ABS,dsituacio=lubridate::ymd(dsituacio)) %>% 
  group_by(CIP) %>% dplyr::slice(which.max(dsituacio))

# Fusiono ambdues bases de dades (dt_demografiques_antic + dt_demografiques)
dt_demografiques<-dt_demografiques %>% bind_rows(dt_demografiques_antic)

# Elimino duplicats (primer valor vàlid)  
dt_demografiques <- dt_demografiques %>%  
  group_by(CIP) %>%
  summarise_each(funs(first(.[!is.na(.)]))) %>% ungroup()

rm(dt_demografiques_antic)

#     Cataleg Medea  -------------
cataleg_medea<-read_excel(here::here("dades", "catalegEAP_BCN_MEDEA3.xls"),skip=3) %>% as_tibble()

# Visites     ------------
dt_visites <- read.csv(here::here("dades", "visites.csv"), sep=";") %>% as_tibble()

# Verificació de CIPS   ------------
dt_cips_antic <- readxl::read_excel("./dades/Cips.xls") %>% mutate(CIP=CIP2)
dt_cips <- dt_demografiques %>% select(CIP)
dt_cips_antic %>% anti_join(dt_cips)

#     Fitxer general de TBC (dt_dades) ----------------
dt_dades <- openxlsx::read.xlsx("./dades/General.xlsx",sheet=1) %>% as_tibble()

# Formatar dates
dt_dades<-dt_dades %>% mutate(DATA_NAIX=as.Date(DATA_NAIX, origin = "1899-12-30"),
                    DAT_INCL=as.Date(DAT_INCL, origin = "1899-12-30"),
                    DAT_MORT=as.Date(DAT_MORT, origin = "1899-12-30"),
                    DET_TB=as.Date(DET_TB, origin = "1899-12-30"),
                    DAT_LLEGADA=as.Date(DAT_LLEGADA, origin = "1899-12-30"))


# Actualitzar dt_demografiques  amb dades generals de l'agencia (sobretot els 1643 que no teniem en E-cap)
# Formatejo dades de dt_dades
dades_temp<-
  dt_dades %>% transmute(CIP,
                       sexe=if_else(str_sub(CIP,5,5)=="1","D","H"),
                       dNaixement=paste0("19",str_sub(CIP,6,7),str_sub(CIP,8,9),str_sub(CIP,10,11)) %>% lubridate::ymd(),
                       situacio=MORT,
                       dsituacio=lubridate::as_date(DAT_MORT),
                       Mostra,
                       ABS,
                       descUP=BARRI,
                       INCL_CVELLA)

# selecciono només aquells CIPS que no estan a dades demografiques (n=1643)
dades_temp<-dades_temp %>% anti_join(dt_demografiques,by="CIP") 

# I ara afegeixo aquestos a demografiques amb total (Actualitzo dades )
dt_demografiques<-dt_demografiques %>% bind_rows(dades_temp)
rm(dades_temp)

#     Capturo data_TBC de dades_dt i afegeixo en dt_demografiques  ---------------
dt_demografiques<-dt_demografiques %>% left_join(select(dt_dades,CIP,DET_TB),by="CIP")

# Neteja 
rm(dt_cips_antic)

# 4. Formatar dades  -------------
# 4.1 DT_PSALUT
dt_psalut<- 
  dt_psalut %>% semi_join(dt_demografiques,by=c("cipACT"="CIP")) %>% 
  mutate(cod = ifelse(codiPSalut  %like% "C01-", substr(codiPSalut,5,15), as.character(codiPSalut)) %>% str_sub(1,5),
  dat=as.Date(ddeteccio)) %>% 
  select(CIP=cipACT,cod,dat) 
  

#4.2 de_variables
dt_variables<-dt_variables %>% mutate (cod=VU_COD_VS) %>% left_join(select(CATALEG,cod,grup))

dt_variables <- 
  dt_variables %>% 
  semi_join(dt_demografiques,by=c("VU_COD_U"="CIP")) %>% 
  transmute(CIP=VU_COD_U,
            cod=grup,
            dat=as.Date(VU_DAT_ACT),
            val=VU_VAL)
  
#4.3 DT_VACUNES
dt_vacunes<- 
  dt_vacunes %>% 
  semi_join(dt_demografiques,by=c("VA_U_USUA_CIP"="CIP")) %>% 
  transmute (CIP=VA_U_USUA_CIP,
             cod=VA_U_COD,
             dat=as.Date(VA_U_DATA_VAC)) 


#4.5 DT_FARMACIA 
dt_farmacia<-dt_farmacia %>% 
  semi_join(dt_demografiques,by="CIP") %>% 
  transmute(
    CIP,
    cod=ATC,
    dataini=as.Date(datainici),
    datafi=as.Date(datafi)) 
  

#4.7 DT_VISITES
dt_visites<-
  dt_visites %>% 
  semi_join(dt_demografiques,by=c("cip"="CIP")) %>% 
  transmute(
  CIP=cip,any,servei=as.character(servei),tipus.visita =as.character(tipus.visita),N)
  

# 5. Agregar cada base de dades en data index (01/01/2007 o movil ) ------

# 5.1. Agregar problemes de salut filtro previ a 20161231
dt_psalut<-
  dt_psalut %>% mutate(idp=CIP) %>% 
  filter((dat<=lubridate::ymd(20161231)))


# 5.2. Generar data_index segons primer DM2 detectat en problemes de salut  --------
dt_dtindexDM<-
  dt_psalut %>% 
  semi_join(CATALEG %>% select(cod,grup) %>% filter(grup=="DM2"),by="cod") %>% 
  group_by(idp) %>% 
  dplyr::slice(which.min(dat)) %>% 
  ungroup() 

# Definició de data index 
dt_dtindexDM<-dt_dtindexDM %>% filter(lubridate::year(dat)<2017)

# 5.3. data index (Dinamica o estatica)   -----------------
data_index_agregacio<-dt_dtindexDM %>% select(idp,data=dat) %>% mutate(data=data.to.string(data))

dt_demografiques<-dt_demografiques %>% mutate(dtindex="20070101")
data_index_agregacio<-dt_demografiques %>% select(idp=CIP,data=dtindex)

# 5.4. Agregacions   -----------------------
# Antecedents  -----------------
dt_antecedents.agregada<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                      finestra.dies = c(-Inf,+1),prefix="DG.",camp_agregador="grup")

# Antecedents 2n nivell
dt_antecedents.agregada2<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                                           finestra.dies = c(-Inf,+1),prefix="DG.",camp_agregador="subgrup") %>% select(-dtindex)

# Antecedents 3n nivell
dt_antecedents.agregada3<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                                            finestra.dies = c(-Inf,+1),prefix="DG.",camp_agregador="DM") %>% select(-dtindex)

# Juntar totes les agregades
dt_antecedents.agregada<-dt_antecedents.agregada %>% left_join(dt_antecedents.agregada2) %>% left_join(dt_antecedents.agregada3) %>% select(-dtindex)
rm(dt_antecedents.agregada2,dt_antecedents.agregada3)

# Events --------------
dt_events.agregada<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                                           finestra.dies = c(+1,+Inf),prefix="EV.",camp_agregador="grup") %>% select(-dtindex)

# Events 2n nivell 
temp<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                                      finestra.dies = c(+1,+Inf),prefix="EV.",camp_agregador="DM") %>% select(-dtindex)

# Events 3 nivell 
temp2<-agregar_problemes(dt_psalut,bd.dindex =data_index_agregacio,dt.agregadors=CATALEG,
                         finestra.dies = c(+1,+Inf),prefix="EV.",camp_agregador="subgrup") %>% select(-dtindex)


dt_events.agregada<-dt_events.agregada %>% left_join(temp) %>% left_join(temp2)
rm(temp,temp2)



# 5.2. Agregar variables basals 
dt_variables <-dt_variables %>% mutate(idp=CIP) 

dt_variables.agregada<-
  agregar_analitiques(dt_variables,bd.dindex =data_index_agregacio,finestra.dies=c(-365,+1),sufix = c(".valor", ".dies")) %>% select(-dtindex)

# 5.4. Agregar vacunes
dt_vacunes <- dt_vacunes %>% mutate(idp=CIP,val=1) 
dt_vac.agregada<-
  agregar_analitiques(dt_vacunes,bd.dindex =data_index_agregacio,finestra.dies=c(-Inf,+1),sufix = c(".valor", ".dies")) %>% select(-dtindex)

# 5.5 Farmacs --------------------
dt_farmacia<-dt_farmacia %>% mutate(idp=CIP,dat=data.to.string(dataini),dbaixa=data.to.string(datafi)) 
  
dt_farmacs_agregada<-
  agregar_prescripcions(dt=dt_farmacia,bd.dindex=data_index_agregacio,dt.agregadors=CATALEG,prefix="FP.",finestra.dies=c(-45,+45),camp_agregador="grup",agregar_data=F) %>% select(-dtindex)

temp<-
  agregar_prescripcions(dt=dt_farmacia,bd.dindex=data_index_agregacio,dt.agregadors=CATALEG,prefix="FP.",finestra.dies=c(-45,+45),camp_agregador="subgrup",agregar_data=F) %>% select(-dtindex)

dt_farmacs_agregada<-dt_farmacs_agregada %>% left_join(temp,by="idp")
rm(temp)

# 5.6 Visites   ----------------
dt_visites <- dt_visites %>% mutate(idp=CIP,dat=lubridate::make_date(any),cod=servei) 

dt_visites.agregada <- 
  agregar_visites(dt_visites, bd.dindex = data_index_agregacio, finestra.dies = c(-365, +Inf),N="N") %>% select(-dtindex)

# 6. Fusionar taules en una --------------
dt_total<-
  dt_demografiques %>% mutate(idp=CIP) %>% 
  left_join(dt_antecedents.agregada,by="idp") %>% 
  left_join(dt_events.agregada,by="idp") %>% 
  left_join(dt_variables.agregada,by="idp") %>% 
  left_join(dt_vac.agregada,by="idp") %>%
  left_join(dt_visites.agregada,by="idp") %>% 
  left_join(dt_farmacs_agregada,by="idp") 
  

# 7. Elimino duplicats 
dt_total<-dt_total %>% group_by(idp) %>% slice(1) %>% ungroup()

# 8. Oju dades d'agencia També te pacients duplicats 
temp<-dt_dades %>% group_by(CIP) %>% mutate(num=n()) %>% ungroup() %>% filter(num>1)


# 9. Afegeixo variable indicadors de dades_agencia

# Eliminar Espais en blanc de base de dades 
dt_total<-dt_total %>% netejar_espais()
dt_dades<-dt_dades %>% netejar_espais()

# Afegir variable dt_Agencia (1.Si / 0. No) en dades_ecap
dt_total<-
  dt_total %>% left_join(dt_dades %>% select(CIP) %>% distinct() %>% mutate(dt_agencia=1),by="CIP") %>% 
  mutate(dt_agencia=ifelse(is.na(dt_agencia),0,1))


# 7. Salvar fitxers  ------------------- 
# Renombro i salvo 
dt_ecap<-dt_total
dt_agencia<-dt_dades


# Crear directori output si no existeix
if (!dir.exists("dades/output")) {dir.create("dades/output")}

save(dt_ecap,dt_agencia, file=here::here("dades/output","output.Rdata"))

# Salvar variables en excel 
# write.csv2(names(dt_ecap),file="var_ecap.csv")
# write.csv2(names(dt_agencia),file="var_agencia.csv")


# Netejo objectes   --------------------
rm(dt_cips,dt_insuline,dt_vac.agregada,dt_events.agregada,dt_visites.agregada,dt_variables,dt_variables.agregada,dt_psalut,
   dt_antecedents.agregada,data_index_agregacio,dt_dtindexDM)



#  ------------






































# 
#     FINS AQUÍ VERIFICAT           ----------------------------------
#     FINS AQUÍ VERIFICAT           ----------------------------------
#     FINS AQUÍ VERIFICAT           ----------------------------------

#DT_TOTAL tenemos lo necesario para calcular la incidencia; Tenemos en cuenta 5 escenarios; 
# 1. Evento Tuberculosis--> Fecha Fin de Seguimiento DET_TB
# 2. Ni muere ni TB --> Fecha Fin de Seguimiento ultima detección de TB 
# 3. Muere --> Fecha Fin de Seguimiento Fecha Muerte
# 4. Controls y detecció de DM --> Fecha Diagnostic DM 
# 5. Controls, detecció DM i TBC --> Fecha Diagnostic TBC

#CIPS duplicados; 
length(unique(dt_total$CIP)) == nrow(dt_total) # FALSE, por lo tanto hay duplicados, 
repet1 <- data.frame(table(dt_total$CIP))
repet1 <- repet1[repet1$Freq>1, ]
length(repet1$Var1)
#Eliminamos 29 duplicados 
dt_total <- dt_total[!duplicated(dt_total$CIP),]
dim(dt_total)
table(dt_total$Mostra)



dt_total<-dt_total %>% 
  mutate(Mostra=if_else(DG.DM<=lubridate::ymd(20070101),"CASOS","CONTROLS"),
         Mostra=if_else(is.na(DG.DM),"CONTROLS",Mostra)) 

#Convertimos DET_TB en Fecha 
dt_total$DET_TB <- as.Date(as.numeric(dt_total$DET_TB), origin="1899-12-30")

#Creo una Variable con fecha de Inicio en 01/01/2007, Inclusión de todos los pacientes, Casos (DM) / Controles (No DM)
dt_total <- dt_total %>% mutate (dat_inici="2007-01-01",dat_inici=as.Date(dat_inici, origin = "1899-12-30"))


#Creamos las Variables Antedecentes / Eventos / Final de Diabeticos 
dt_total<-dt_total %>% mutate(ant_dm=if_else(is.na(DG.DM),0,1))
dt_total<-dt_total %>% mutate(event_dm=if_else(is.na(EV.DM),0,1))
dt_total<-dt_total %>% mutate(final_dm=if_else(event_dm | ant_dm,1,0))

#Convierto las variables en caracter; 
dt_total$ant_dm <- as.character(dt_total$ant_dm)
dt_total$event_dm <- as.character(dt_total$event_dm)
dt_total$final_dm <- as.character(dt_total$final_dm)

#Creo el Evento Tuberculosis SI / tuberculosis NO 
dt_total <- dt_total %>% mutate(event_tb = if_else(is.na(DET_TB), 0,1))
dt_total$event_tb <- as.character(dt_total$event_tb)

table(dt_total$event_tb,dt_total$Mostra)


require(data.table)

#CRITERIOS EXCLUSION 
#Todos los que se les haya detectado TB antes del 01/01/2007

dt_total<-dt_total %>% filter((DET_TB >= dat_inici | is.na(DET_TB))) #Eliminados todos los TB antes del 2007
#Todos los muertos (situacio = D) antes del 01/01/2007 el resto los dejamos 
dt_total <- dt_total %>% filter((situacio == "D" & dsituacio > dat_inici) | situacio %in% c("A", "T"))




# Elimino Espais en blanc de base de dades 
dt_total<-dt_total %>% netejar_espais()

#TIEMPOS; 
#Si se detecta TB fecha fin esa fecha, 
#Si muere fecha fin esa fecha, 
#Si es control y se detecta DM , esa fecha de diagnostico 
#SINO Fecha Fin (Ultima detección de TB)

dt_total <- dt_total %>% 
  mutate(dat_fi=as.Date("2018-02-15"),
                   data_final = case_when(
                     Mostra == "CONTROLS" & EV.DM < DET_TB ~ EV.DM,   # Controls converits a DM --> Data DM
                      !is.na(DET_TB) ~ DET_TB,                         # Si TBC --> data de TBC 
                      is.na(DET_TB) & situacio != "D" ~ dat_fi,        # Si no TBC --> data fi
                      is.na(DET_TB) & situacio == "D" ~ dsituacio)     # Si no TBC --> data defunció
                       )

# Verificació de data final 
dplyr::select(dt_total, DET_TB, situacio, Mostra, dsituacio, dat_fi, data_final) %>% filter(Mostra == "CONTROLS")
dplyr::select(dt_total, DET_TB, situacio, Mostra, dsituacio, dat_fi, data_final) %>% filter(DET_TB>0)
dplyr::select(dt_total, DET_TB, situacio, Mostra, dsituacio, dat_fi, data_final) %>% filter(is.na(DET_TB) & situacio == "D")

#Calcular Temps de Seguiment 
summary(dt_total$data_final)
sum(dt_total$data_final > dt_total$dat_fi)
#Tenemos 409 individuos que tienes fecha de Defuncion despues de 15/02/2018, ultima detección de TB 

# Individuos que cualquier evento (TBC o muerte es posterior a fin 15/02/2018, se censura)
# Fecha máxima de censura --> 15/02/2018 
dt_total<-dt_total %>% mutate(data_final=if_else(data_final>as.Date("2018-02-15"),as.Date("2018-02-15"),data_final))

#

#TRABAJAMOS LOS TIEMPOS EN GENERAL 
dt_total$dies <- dt_total$data_final - dt_total$dat_inici    #DIES DE SEGUIMENT 
dt_total$dies <- as.numeric(dt_total$dies)

dt_total$mesos <- dt_total$dies / 30.4
dt_total$mesos <- as.numeric(dt_total$mesos)

dt_total$anys <- dt_total$mesos / 12
dt_total$anys <- as.numeric(dt_total$anys)
dt_total$anys <- round(dt_total$anys, 1)


#CURVAS DE SUPERVIVENCIA 

dt_total$event_tb <- as.numeric(dt_total$event_tb)
Y_Surv <- Surv(dt_total$anys, dt_total$event_tb)

plot(Y_Surv, lty=c(1), col=c("Grey"), lwd=c(3), las=1, font=2, cex=2, 
     xlab = "Time (Years)", ylab= "Probability of TB detection")

library(survminer)
survfit(Y_Surv ~ dt_total$Mostra, dt_total, conf.type = "log-log") %>%
  ggsurvplot(title="TB detection in patient with or without DM",
             p.val=T, xlab="Time (Years)", censor=F, linetype="strata",
             fun="event", cumevents = T, xlim=c(0,13))


#SI AHORA CORTO LOS TIEMPOS TODOS A 15/02/2018
dt_total <- mutate(dt_total, data_final_TB = 
                     case_when(
                       dt_total$data_final > dt_total$dat_fi ~ dt_total$dat_fi, 
                       dt_total$data_final <= dt_total$dat_fi ~ dt_total$data_final
                       ))
                       

summary(dt_total$data_final_TB)
dt_total$dies_TB <- dt_total$data_final_TB - dt_total$dat_inici
dt_total$dies_TB <- as.numeric(dt_total$dies_TB)

dt_total$mesos_TB <- dt_total$dies_TB / 30.4
dt_total$mesos_TB <- as.numeric(dt_total$mesos_TB)

dt_total$any_TB <- dt_total$mesos_TB/12
dt_total$any_TB <- as.numeric(dt_total$any_TB)
dt_total$any_TB <- round(dt_total$any_TB, 1)

Z_Surv  <- Surv(dt_total$any_TB, dt_total$event_tb)
library(survminer)
survfit(Z_Surv ~ dt_total$Mostra, dt_total, conf.type = "log-log") %>%
  ggsurvplot(title="TB detection in patient with or without DM",
             p.val=T, xlab="Time (Years)", censor=F, linetype="strata",
             fun="event", cumevents = T, xlim=c(0,13))


# 8. Salvar fitxer ---------------

#SI AHORA CORTO LOS TIEMPOS TODOS A 15/02/2018
dt_total <- mutate(dt_total, data_final_TB = 
                     case_when(
                       dt_total$data_final >= dt_total$dat_fi ~ dt_total$dat_fi, 
                       dt_total$data_final < dt_total$dat_fi ~ dt_total$data_final
                     ))



dt_total$mesos_TB <- dt_total$dies_TB / 30.4
dt_total$mesos_TB <- as.numeric(dt_total$mesos_TB)

dt_total$any_TB <- dt_total$mesos_TB/12
dt_total$any_TB <- as.numeric(dt_total$any_TB)
dt_total$any_TB <- round(dt_total$any_TB, 1)

Z_Surv  <- Surv(dt_total$any_TB, dt_total$event_tb)
library(survminer)
survfit(Z_Surv ~ dt_total$Mostra, dt_total, conf.type = "log-log") %>%
  ggsurvplot(title="TB detection in patient with or without DM",
             p.val=T, xlab="Time (Years)", censor=F, linetype="strata",
             fun="event", cumevents = T, xlim=c(0,13))



saveRDS(dt_total,here::here("dades","dades.RDS"))


# 9 Generar fitxer de variables 

write.csv2(names(dt_total),"variables.csv")


# 10 Capturar historics de problemes de salut i de farmacia

#Agafats CIPS de dt_total i aqui he agregat els problemes de salut per CIP i despres el Agregadors de CATALEG per codi, 
dt1 <- dt_total %>% select(CIP)
dt_psalut <- dt_psalut %>% rename(CIP = cipACT)

dt_psalut_historic<-dt_psalut %>% 
  left_join(select(CATALEG,cod,agr)) %>% select(codiPSalut,cod,dat,agr,CIP) %>% 
  semi_join(dt1,by="CIP")

library(DT)
dt_psalut %>% group_by(cod) %>% summarize(n(), min(dat),max(dat)) %>% datatable()
dt_psalut_historic %>% group_by(cod,agr) %>% summarize(n(), min(dat),max(dat)) %>% datatable()


dt_historic_farmacs<-dt_farmacia %>% 
  select(CIP=idp,cod,data_ini=PPFMC_DATA_INI_SIRE,data_fi=PPFMC_DATA_FI_SIRE) %>% 
  semi_join(dt1) %>% 
  left_join(select(CATALEG,cod,agr) %>% unique())

dt_farmacs<-dt_farmacia %>% 
  select(CIP=idp,cod,data_ini=PPFMC_DATA_INI_SIRE,data_fi=PPFMC_DATA_FI_SIRE)%>% 
  left_join(select(CATALEG,cod,agr) %>% unique())

# Salvar historics
saveRDS(dt_psalut_historic, here::here("dades", "psalut_historic.RDS"))
saveRDS(dt_psalut, here::here("dades", "psalut.RDS"))
saveRDS(dt_historic_farmacs, here::here("dades", "dt_historic_farmacs.RDS"))
saveRDS(dt_farmacs, here::here("dades", "dt_farmacs.RDS"))


# Descriptiva Nomes TB 
#(event_tb)

#BBDD donde solo nos quedamos con los 201 incidentes TB 
dt_tb <- dt_total %>% filter(event_tb == TRUE)

#Cruzamos los CIPS de esta BBDD de tuberculosos con la dt_dades donde tenemos toda la información y lo hacemos por CIP 
dt_tb <- dt_tb %>% select(CIP)
dt_dades

dim(dt_tb) #201 Tuberculosos

dt_tuberculosos <- dt_tb %>% 
  left_join(dt_dades, by="CIP")

dim(dt_tuberculosos)
length(unique(dt_tuberculosos$CIP)) == nrow(dt_tuberculosos) # FALSE, por lo tanto hay duplicados, 
repet1 <- data.frame(table(dt_tuberculosos$CIP))
repet1 <- repet1[repet1$Freq>1, ]
length(repet1$Var1)
dt_tuberculosos <- dt_tuberculosos[!duplicated(dt_tuberculosos$CIP),]
dim(dt_tuberculosos)

descrTable(dt_tuberculosos)                      


# Factoritzar variables                              
dt_tuberculosos<-factoritzar.NO.YES(dt_tuberculosos,"factoritzarSINO",fitxer_conductor_general)
dt_tuberculosos<-factoritzar(dt_tuberculosos,extreure.variables("factoritzar",fitxer_conductor_general))


saveRDS(dt_tuberculosos, here::here("dades", "dt_tuberculosos.RDS"))



#Descriptiva de la Taula General 
#Formatejar les dades
names(dt_dades)
str(dt_dades)
dt_dades$data_naixement <- as.Date(dt_dades$DATA_NAIX, origin="1899-12-30")
dt_dades$data_inclusio <- as.Date(dt_dades$DAT_INCL, origin="1899-12-30")
dt_dades$data_mort <- as.Date(dt_dades$DAT_MORT, origin="1899-12-30")
dt_dades$data_tbc <- as.Date(dt_dades$DET_TB, origin= "1899-12-30")

summary(dt_dades)
descrTable(dt_dades)


# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables_tbc.xls"
# fitxer conductor GENERAL
fitxer_conductor_general<-"variables_general.xls"

dt_dades <- factoritzar.NO.YES(dt_dades, "factoritzarSINO", fitxer_conductor_general)
dt_dades <-factoritzar(dt_dades,extreure.variables("factoritzar",fitxer_conductor_general))


descrTable(dt_dades)
datatable(dt_dades)
descrTable(dt_dades) %>% export2md(caption = "Descriptivo de TBC")



#Creuament dt_demografiques_antis & dt_demografiques
tabla_demo_antic <- dt_demografiques_antic    #31491
tabla_demo_nueva <- dt_demografiques          #67437

tabla_demo_antic$EstaNueva <- ifelse(is.na(match(tabla_demo_antic$CIP, tabla_demo_nueva$CIP)), 0, 1)
table(tabla_demo_antic$EstaNueva)

dt_demografiques %>% 
  semi_join(dt_demografiques_antic, by="CIP")

dt_demografiques_antic %>% 
  semi_join(dt_demografiques, by="CIP")



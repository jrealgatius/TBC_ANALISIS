#                               FASE LECTURA                            ---------------------
#  Aplicació lectura de dades / aplanament / criteris d'inclusió 

memory.size(max=160685)
#
# 1. Càrrega de funcions ----------------------  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)


# 2. Paràmetres  ----------------------

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables_tbc.xls"

# 
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg,col_types = "text")


# 3. Lectura fitxers  ----------------------------

# "farmacia.csv", "Laboratori.csv", "PSalut.csv", "Vacunes.csv", "Variables.csv", "vardemografiques.csv" 

dt_psalut<-read.csv(here::here("dades","PSalut.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_laboratori<-read.csv(here::here("dades","Laboratori.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_vacunes<-read.csv(here::here("dades","Vacunes.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_variables<-read.csv(here::here("dades","Variables.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_farmacia<-read.csv(here::here("dades","farmacia.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_insuline<-read.csv(here::here("dades","insulina.csv"), sep=";") %>% as_tibble()

dt_demografiques <- read.csv(here::here("dades", "vardemografiques.csv"), sep=";") %>% as_tibble()

#dt_cips<-read_excel("./dades/Cips.xls")

#Unió Farmacia amb Insulina
dt_farmacia<-rbind(dt_farmacia,dt_insuline)
table(dt_farmacia$PPFMC_ATCCODI)

dt_cips <- readxl::read_excel("./dades/Cips.xls")
dt_dades <- openxlsx::read.xlsx("./dades/General.xlsx",sheet=1)

dt_visites <- read.csv(here::here("dades", "visites.csv"), sep=";") %>% as_tibble()

#Comprobar CIPS 
#tabla1 <- dt_cips  
#tabla2 <- dt_dades

#tabla1$EstaTabla2 <- ifelse(is.na(match(tabla1$CIP2, tabla2$CIP)), 0, 1)
#table(tabla1$EstaTabla2)

#tabla2$EstaTabla1 <- ifelse(is.na(match(tabla2$CIP, tabla1$CIP2)), 0, 1)
#table(tabla2$EstaTabla1)

#Comprobar CIPS; 
# CREADA (Violeta) con DEMOGRAFICA (Jose)
tabla3 <- dt_dades
tabla4 <- dt_demografiques

tabla3$EstaTabla4 <- ifelse(is.na(match(tabla3$CIP, tabla4$CIP)), 0, 1)
table(tabla3$EstaTabla4)

tabla4$EstaTabla3 <- ifelse(is.na(match(tabla4$CIP, tabla3$CIP)), 0, 1)
table(tabla4$EstaTabla3)
#Todos los de Tabla Demografica estan en Tabla de Dades; 
#No tenemos CIPS en Tabla demografica que no esten en Dades (BBDD Creada)

# 4. Formatar dades  -------------

# 4.1 DT_PSALUT
dt_psalut$new_codiPSalut <- ifelse(dt_psalut$codiPSalut  %like% "C01-", substr(dt_psalut$codiPSalut,5,15), as.character(dt_psalut$codiPSalut))
dt_psalut$ddeteccio <- as.Date(dt_psalut$ddeteccio, origin = "1899-12-30")    #Convertit a Data
dt_psalut$cipACT <- as.character(dt_psalut$cipACT)                            #Convertir en Caracter 

#4.2 DT_LABORATORI
dt_laboratori$VU_COD_U <- as.character(dt_laboratori$VU_COD_U)          
dt_laboratori$VU_COD_VS <- as.character(dt_laboratori$VU_COD_VS)        
dt_laboratori$VU_DAT_ACT <- as.Date(dt_laboratori$VU_DAT_ACT, origin = "1899-12-30")
dt_laboratori$VU_VAL <- as.numeric(dt_laboratori$VU_VAL)
dt_laboratori<-dt_laboratori %>% left_join(dplyr::select(CATALEG,VU_COD_VS=cod,agr),by="VU_COD_VS") 

#4.3 DT_VACUNES
dt_vacunes$VA_U_USUA_CIP <- as.character(dt_vacunes$VA_U_USUA_CIP)
dt_vacunes$VA_U_COD <- as.character(dt_vacunes$VA_U_COD)
dt_vacunes$VA_U_DATA_VAC <- as.Date(dt_vacunes$VA_U_DATA_VAC)

#4.4 DT_VARIABLES
dt_variables$VU_COD_U <- as.character(dt_variables$VU_COD_U)
dt_variables$VU_COD_VS <- as.character(dt_variables$VU_COD_VS)
dt_variables$VU_DAT_ACT <- as.Date(dt_variables$VU_DAT_ACT, origin = "1899-12-30")
dt_variables$VU_VAL <- as.numeric(as.character(dt_variables$VU_VAL))
dt_variables<-dt_variables %>% left_join(dplyr::select(CATALEG,VU_COD_VS=cod,agr),by="VU_COD_VS") 

#4.5 DT_FARMACIA 
dt_farmacia$ATC <- as.character(dt_farmacia$ATC)
dt_farmacia$PPFMC_DATA_INI_SIRE <- as.Date(dt_farmacia$PPFMC_DATA_INI_SIRE, origin = "1899-12-30")
dt_farmacia$PPFMC_DATA_FI_SIRE <- as.Date(dt_farmacia$PPFMC_DATA_FI_SIRE, origin = "1899-12-30")
dt_farmacia$PPFMC_PMC_USUARI_CIP <- as.character(dt_farmacia$PPFMC_PMC_USUARI_CIP)
dt_farmacia$PPFMC_ATCCODI <- as.character(dt_farmacia$PPFMC_ATCCODI)
dt_farmacia <- dt_farmacia %>% left_join(dplyr::select(CATALEG, PPFMC_ATCCODI=cod, agr), by= "PPFMC_ATCCODI")

#4.6 DT_DEMOGRAFIQUES
names(dt_demografiques)
dt_demografiques$CIP <- as.character(dt_demografiques$CIP)
dt_demografiques$Mostra <- as.character(dt_demografiques$Mostra)
dt_demografiques$sexe <- as.character(dt_demografiques$sexe)
#Convertir dNaixement en Dates (R no llegia bé les dates)
dt_demografiques$dNaixement <- gsub("ene", "jan", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("feb", "feb", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("mar", "mar", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("abr", "apr", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("may", "may", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("jun", "jun", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("jul", "jul", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("ago", "aug", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("sep", "sep", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("oct", "oct", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("nov", "nov", dt_demografiques$dNaixement)
dt_demografiques$dNaixement <- gsub("dic", "dec", dt_demografiques$dNaixement)
dt_demografiques$dNaixement<-dt_demografiques$dNaixement %>% as.character() %>% lubridate::dmy() 
dt_demografiques$situacio <- as.character(dt_demografiques$situacio)
#Convertir dsituació en Dates (R no llegeix bé les dates)
dt_demografiques$dsituacio <- gsub("ene", "jan", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("feb", "feb", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("mar", "mar", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("abr", "apr", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("may", "may", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("jun", "jun", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("jul", "jul", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("ago", "aug", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("sep", "sep", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("oct", "oct", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("nov", "nov", dt_demografiques$dsituacio)
dt_demografiques$dsituacio <- gsub("dic", "dec", dt_demografiques$dsituacio)
dt_demografiques$dsituacio<-dt_demografiques$dsituacio %>% as.character() %>% lubridate::dmy() 
dt_demografiques$descNacionalitat <- as.character(dt_demografiques$descNacionalitat)
dt_demografiques$paisNaixement <- as.character(dt_demografiques$paisNaixement)
dt_demografiques$descUP <- as.character(dt_demografiques$descUP)

#4.7 DT_VISITES
dt_visites
dt_visites$cip <- as.character(dt_visites$cip)
dt_visites$any <- as.numeric(as.character(dt_visites$any))
dt_visites$servei <- as.character(dt_visites$servei)
dt_visites$tipus.visita <- as.character(dt_visites$tipus.visita)
dt_visites$N <- as.numeric(as.character(dt_visites$N))

# 5. Agregar / Aplanar cada base de dades en data index (01/01/2007) ------

# 5.1. Agregar problemes de salut
dt_psalut<-dt_psalut %>% mutate(cod=stringr::str_sub(new_codiPSalut,1,5),
                                idp=cipACT,
                                dat=ddeteccio)

# Antecedents
dt_antecedents.agregada<-agregar_problemes(dt_psalut,bd.dindex ="20070101",dt.agregadors=CATALEG,
                      finestra.dies = c(-Inf,0),prefix="DG.",camp_agregador="agr")

# Events 
dt_events.agregada<-agregar_problemes(dt_psalut,bd.dindex ="20070101",dt.agregadors=CATALEG,
                                           finestra.dies = c(+1,+Inf),prefix="EV.",camp_agregador="agr")

# 5.2. Agregar variables basals 
dt_variables <-dt_variables %>% mutate(idp=VU_COD_U,dat=VU_DAT_ACT,val=VU_VAL,cod=agr)
dt_variables.agregada<-agregar_analitiques(dt_variables,bd.dindex ="20070101",finestra.dies=c(-365,0),sufix = c(".valor", ".dies"))

# 5.3. Agregar laboratori 
dt_laboratori <-dt_laboratori %>% mutate(idp=VU_COD_U,dat=VU_DAT_ACT,val=VU_VAL,cod=agr)
dt_lab.agregada<-agregar_analitiques(dt_laboratori,bd.dindex ="20070101",finestra.dies=c(-365,0),sufix = c(".valor", ".dies"))

# 5.4. Agregar vacunes
dt_vacunes <- dt_vacunes %>% mutate(idp=VA_U_USUA_CIP,dat=VA_U_DATA_VAC,val=1,cod=VA_U_COD)
dt_vac.agregada<-agregar_analitiques(dt_vacunes,bd.dindex ="20171231",finestra.dies=c(-Inf,0),sufix = c(".valor", ".dies"))

# 5.5 Farmacs 
dt_farmacia<-dt_farmacia %>% mutate(idp=PPFMC_PMC_USUARI_CIP,cod=PPFMC_ATCCODI,dat=data.to.string(PPFMC_DATA_INI_SIRE),dbaixa=data.to.string(PPFMC_DATA_FI_SIRE))
dt_farmacs_agregada<-agregar_prescripcions(dt=dt_farmacia,bd.dindex=20091231,dt.agregadors=CATALEG,prefix="FP.",finestra.dies=c(0,0),camp_agregador="agr",agregar_data=F)

# 5.6 Visites
dt_visites$any <- make_date(year=dt_visites$any)
str(dt_visites$any)
dt_visites

dt_visites <- dt_visites %>% mutate(idp=cip, dat=any, cod=servei)
dt_visites.agregada <- agregar_visites(dt_visites, bd.dindex = "20070101", finestra.dies = c(-365, Inf))


# 6. Fusionar taules en una --------------

#De la DT_DADES (BBDD Violeta), solo necesitamos DET_TB, 
#Seleccionamos de BBD Dades CIP y DET_TB para juntar con el resto; 
dt_datos <- dt_dades[,c(1,26)]   

dt_cips <- dt_cips %>% rename(CIP=CIP2)
dt_antecedents.agregada<-dt_antecedents.agregada %>% rename(CIP=idp)
dt_events.agregada<-dt_events.agregada %>% rename(CIP=idp)
dt_variables.agregada<-dt_variables.agregada %>% rename(CIP=idp)
dt_lab.agregada<-dt_lab.agregada %>% rename(CIP=idp)
dt_vac.agregada<-dt_vac.agregada %>% rename(CIP=idp)
dt_dem.agregada <- dt_demografiques %>% rename(CIP = CIP) 
dt_datos.agregada <- dt_datos %>% rename (CIP = CIP)

dt_total <- dt_cips %>% 
  left_join(dt_antecedents.agregada,by="CIP") %>% 
  left_join(dt_events.agregada,by="CIP") %>% 
  left_join(dt_lab.agregada,by="CIP") %>% 
  left_join(dt_variables.agregada,by="CIP") %>% 
  left_join(dt_vac.agregada,by="CIP") %>%
  left_join(dt_dem.agregada, by="CIP") %>%
  left_join(dt_datos.agregada,by="CIP")

names(dt_total)
#DT_TOTAL tenemos lo necesario para calcular la incidencia; Tenemos en cuenta 5 escenarios; 
# 1. Evento Tuberculosis--> Fecha Fin de Seguimiento DET_TB
# 2. Ni muere ni TB --> Fecha Fin de Seguimiento ultima detección de TB 
# 3. Muere --> Fecha Fin de Seguimiento Fecha Muerte
# 4. Controls y detecció de DM --> Fecha Diagnostic DM 
# 5. Controls, detecció DM i TBC --> Fecha Diagnostic TBC
str(dt_total)

#CIPS duplicados; 
length(unique(dt_total$CIP)) == nrow(dt_total) # FALSE, por lo tanto hay duplicados, 
repet1 <- data.frame(table(dt_total$CIP))
repet1 <- repet1[repet1$Freq>1, ]
length(repet1$Var1)
#Eliminamos 29 duplicados 
dt_total <- dt_total[!duplicated(dt_total$CIP),]
dim(dt_total)
table(dt_total$Mostra)


#Convertimos DET_TB en Fecha 
dt_total$DET_TB <- as.Date(dt_total$DET_TB, origin="1899-12-30")

#Creo una Variable con fecha de Inicio en 01/01/2007, Inclusión de todos los pacientes, Casos (DM) / Controles (No DM)
dt_total$dat_inici <- rep("2007-01-01", 33258 )
dt_total$dat_inici <- as.Date(dt_total$dat_inici, origin = "1899-12-30")

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


require(data.table)
dt_total <- data.table(dt_total)

#CRITERIOS EXCLUSION 
#Todos los que se les haya detectado TB antes del 01/01/2007
dt_total <- dt_total[(DET_TB >= dat_inici | is.na(DET_TB))]   #Eliminados todos los TB antes del 2007
#Todos los muertos (situacio = D) antes del 01/01/2007 el resto los dejamos 
dt_total <- dt_total[(situacio == "D" & dsituacio > dat_inici) | situacio %in% c("A", "T")] 


#TIEMPOS; 
#Si se detecta TB fecha fin esa fecha, 
#Si muere fecha fin esa fecha, 
#Si es control y se detecta DM , esa fecha de diagnostico 
#SINO Fecha Fin (Ultima detección de TB)

dt_total$dat_fi <- rep("2018-02-15", 30275)
dt_total$dat_fi <- as.Date(dt_total$dat_fi)

dt_total <- mutate(dt_total, data_final = 
                    case_when(
                      (Mostra == "CONTROLS " & EV.DM < DET_TB) ~ EV.DM,
                      !is.na(DET_TB) ~ DET_TB, 
                      is.na(DET_TB) & situacio != "D" ~ dat_fi, 
                      is.na(DET_TB) & situacio == "D" ~ dsituacio,
                       ))

dplyr::select(dt_total, DET_TB, situacio, Mostra, dsituacio, dat_fi, data_final) %>% filter(Mostra == "CONTROLS ")

#Calcular Temps de Seguiment 
summary(dt_total$data_final)
sum(dt_total$data_final > dt_total$dat_fi)
#Tenemos 409 individuos que tienes fecha de Defuncion despues de 15/02/2018, ultima detección de TB 

#TRABAJAMOS LOS TIEMPOS EN GENERAL 
dt_total$dies <- dt_total$data_final - dt_total$dat_inici    #DIES DE SEGUIMENT 
dt_total$dies <- as.numeric(dt_total$dies)

dt_total$mesos <- dt_total$dies / 30.4
dt_total$mesos <- as.numeric(dt_total$mesos)

dt_total$anys <- dt_total$mesos / 12
dt_total$anys <- as.numeric(dt_total$anys)
dt_total$anys <- round(dt_total$anys, 1)

names(dt_total)

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

sum(dt_total$DET_TB > dt_total$dat_fi, na.rm = T)
(dt_total$dsituacio > dt_total$dat_fi)

#SI AHORA CORTO LOS TIEMPOS TODOS A 15/02/2018
dt_total <- mutate(dt_total, data_final_TB = 
                     case_when(
                       dt_total$data_final >= dt_total$dat_fi ~ dt_total$dat_fi, 
                       dt_total$data_final < dt_total$dat_fi ~ dt_total$data_final
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



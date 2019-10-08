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

# Ahir el PEPE no va venir

# fitxer conductor cataleg 
fitxer_conductor_cataleg<-"cataleg.xls"

# fitxer conductor variables
fitxer_conductor_variables<-"variables_tbc.xls"

# 
CATALEG<-readxl::read_excel(fitxer_conductor_cataleg,col_types = "text")


# 3. Lectura fitxers  ----------------------------

# "farmacia.csv", "Laboratori.csv", "PSalut.csv", "Vacunes.csv", "Variables.csv"   

dt_psalut<-read.csv(here::here("dades","PSalut.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_laboratori<-read.csv(here::here("dades","Laboratori.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_vacunes<-read.csv(here::here("dades","Vacunes.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_variables<-read.csv(here::here("dades","Variables.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_farmacia<-read.csv(here::here("dades","farmacia.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()


#dt_cips<-read_excel("./dades/Cips.xls")


dt_cips <- readxl::read_excel("./dades/Cips.xls")
dt_dades <- openxlsx::read.xlsx("./dades/General.xlsx",sheet=1)

# Falta fitxer de població (Caracteristiques demografiques)
# Falta fitxer de TBC ()


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
dt_laboratori<-dt_laboratori %>% left_join(select(CATALEG,VU_COD_VS=cod,agr),by="VU_COD_VS") 


#4.3 DT_VACUNES
dt_vacunes$VA_U_USUA_CIP <- as.character(dt_vacunes$VA_U_USUA_CIP)
dt_vacunes$VA_U_COD <- as.character(dt_vacunes$VA_U_COD)
dt_vacunes$VA_U_DATA_VAC <- as.Date(dt_vacunes$VA_U_DATA_VAC)

#4.4 DT_VARIABLES
dt_variables$VU_COD_U <- as.character(dt_variables$VU_COD_U)
dt_variables$VU_COD_VS <- as.character(dt_variables$VU_COD_VS)
dt_variables$VU_DAT_ACT <- as.Date(dt_variables$VU_DAT_ACT, origin = "1899-12-30")
dt_variables$VU_VAL <- as.numeric(as.character(dt_variables$VU_VAL))
dt_variables<-dt_variables %>% left_join(select(CATALEG,VU_COD_VS=cod,agr),by="VU_COD_VS") 

#4.5 DT_FARMACIA 
dt_farmacia$ATC <- as.character(dt_farmacia$ATC)
dt_farmacia$PPFMC_DATA_INI_SIRE <- as.Date(dt_farmacia$PPFMC_DATA_INI_SIRE, origin = "1899-12-30")
dt_farmacia$PPFMC_DATA_FI_SIRE <- as.Date(dt_farmacia$PPFMC_DATA_FI_SIRE, origin = "1899-12-30")
dt_farmacia$PPFMC_PMC_USUARI_CIP <- as.character(dt_farmacia$PPFMC_PMC_USUARI_CIP)
dt_farmacia$PPFMC_ATCCODI <- as.character(dt_farmacia$PPFMC_ATCCODI)

# 5. Descriptiu --------------- 

# Per variable (codi) dates i valors:  max i min 
# Per codi frequencies

#Descriptiu DT_PSALUT
names(dt_psalut)

data.frame(table(dt_psalut$new_codiPSalut)) #Codi per Freq?encies
min(dt_psalut$ddeteccio)        #Data Minima de deteccio de la Diabetis   
max(dt_psalut$ddeteccio)        #Data Maxima de detecci? de la Diabetis 
data.frame(sum(table(dt_psalut$cipACT)))

#Descriptiu DT_LABORATORI
sum(table(dt_laboratori$VU_COD_U))
data.frame(table(dt_laboratori$VU_COD_VS))
min(dt_laboratori$VU_DAT_ACT)
max(dt_laboratori$VU_DAT_ACT)
summary(dt_laboratori$VU_VAL)

tapply(dt_laboratori$VU_VAL, dt_laboratori$agr, summary)

#Descriptiu DT_VACUNES
dt_vacunes
sum(table(dt_vacunes$VA_U_USUA_CIP))
data.frame(table(dt_vacunes$VA_U_COD))
min(dt_vacunes$VA_U_DATA_VAC)
max(dt_vacunes$VA_U_DATA_VAC)

#Descriptiu DT_VARIABLES
dt_variables
sum(table(dt_variables$VU_COD_U))
data.frame(table(dt_variables$VU_COD_VS))
summary(dt_variables$VU_DAT_ACT)
summary(dt_variables$VU_VAL)

tapply(dt_variables$VU_VAL, dt_variables$agr, summary)

#Descriptiu DT_FARMACIA 
dt_farmacia
sum(table(dt_farmacia$ATC))
summary(dt_farmacia$PPFMC_DATA_INI_SIRE)
summary(dt_farmacia$PPFMC_DATA_FI_SIRE)
sum(table((dt_farmacia$PPFMC_PMC_USUARI_CIP)))
table(dt_farmacia$PPFMC_ATCCODI)


#Descriptiu Dades
names(dt_dades)
table(dt_dades$Mostra)
dt_dades$DATA_NAIX <- as.Date(dt_dades$DATA_NAIX, origin = "1899-12-30")
summary(dt_dades$DATA_NAIX)
dt_dades$DAT_INCL <- as.Date(dt_dades$DAT_INCL, origin = "1899-12-30")
summary(dt_dades$DAT_INCL)
summary(dt_dades$EDAT)
table(dt_dades$GENERE)
table(dt_dades$TABAC)
table(dt_dades$ALCOHOL)
table(dt_dades$DROGUES)
table(dt_dades$PRESO)
table(dt_dades$MORT)
dt_dades$DAT_MORT <- as.Date(dt_dades$DAT_MORT, origin="1899-12-30")
summary(dt_dades$DAT_MORT)
dt_dades$DET_TB <- as.numeric(dt_dades$DET_TB)
dt_dades$DET_TB <- as.Date(dt_dades$DET_TB, origin="1899-12-30")
summary(dt_dades$DET_TB)
table(dt_dades$BARRI)
table(dt_dades$INDIGENT)
table(dt_dades$ABS)
table(dt_dades$GRUP_OCUP)

#TIPUS DE TB
table(dt_dades$TB._PULMONAR)
table(dt_dades$TB_PLEURAL)
table(dt_dades$TB_PLEUROPULM)
table(dt_dades$TB_LIMFATICA)
table(dt_dades$TB_OSTEOART)
table(dt_dades$TB_MILIAR)
table(dt_dades$TB_GENITOURIN)
table(dt_dades$TB_MENINGEA)
table(dt_dades$TB_LARINGEA)
table(dt_dades$TB_ALTRESLOC)

# 6. Agregar / Aplanar cada base de dades en data index (31/06/2006 ?) ------------


# 6.1. Agregar problemes de salut

dt_psalut<-dt_psalut %>% mutate(cod=stringr::str_sub(new_codiPSalut,1,5),
                                idp=cipACT,
                                dat=ddeteccio)

# Antecedents

dt_antecedents.agregada<-agregar_problemes(dt_psalut,bd.dindex ="20070101",dt.agregadors=CATALEG,
                      finestra.dies = c(-Inf,0),prefix="DG.",camp_agregador="agr")

# Events 
dt_events.agregada<-agregar_problemes(dt_psalut,bd.dindex ="20070101",dt.agregadors=CATALEG,
                                           finestra.dies = c(+1,+Inf),prefix="EV.",camp_agregador="agr")

# 6.2. Agregar variables basals 

dt_variables <-dt_variables %>% mutate(idp=VU_COD_U,dat=VU_DAT_ACT,val=VU_VAL,cod=agr)
dt_variables.agregada<-agregar_analitiques(dt_variables,bd.dindex ="20070101",finestra.dies=c(-365,0),sufix = c(".valor", ".dies"))

# 6.3. Agregar laboratori 

dt_laboratori <-dt_laboratori %>% mutate(idp=VU_COD_U,dat=VU_DAT_ACT,val=VU_VAL,cod=agr)
dt_lab.agregada<-agregar_analitiques(dt_laboratori,bd.dindex ="20070101",finestra.dies=c(-365,0),sufix = c(".valor", ".dies"))


# 6.4. Agregar vacunes
dt_vacunes <- dt_vacunes %>% mutate(idp=VA_U_USUA_CIP,dat=VA_U_DATA_VAC,val=1,cod=VA_U_COD)
dt_vac.agregada<-agregar_analitiques(dt_vacunes,bd.dindex ="20171231",finestra.dies=c(-Inf,0),sufix = c(".valor", ".dies"))


# 6.5 Farmacs 

dt_farmacia<-dt_farmacia %>% mutate(idp=PPFMC_PMC_USUARI_CIP,cod=PPFMC_ATCCODI,dat=data.to.string(PPFMC_DATA_INI_SIRE),dbaixa=data.to.string(PPFMC_DATA_FI_SIRE))

 
dt_farmacs_agregada<-agregar_prescripcions(dt=dt_farmacia,bd.dindex=20091231,dt.agregadors=CATALEG,prefix="FP.",finestra.dies=c(0,0),camp_agregador="agr",agregar_data=F)



# 7. Fusionar taules en una --------------

dt_cips <- dt_cips %>% rename(CIP=CIP2)
dt_antecedents.agregada<-dt_antecedents.agregada %>% rename(CIP=idp)
dt_variables.agregada<-dt_variables.agregada %>% rename(CIP=idp)
dt_lab.agregada<-dt_lab.agregada %>% rename(CIP=idp)
dt_vac.agregada<-dt_vac.agregada %>% rename(CIP=idp)

dt_total <- dt_cips %>% 
  left_join(dt_dades,by="CIP") %>%
  left_join(dt_antecedents.agregada,by="CIP") %>% 
  left_join(dt_lab.agregada,by="CIP") %>% 
  left_join(dt_variables.agregada,by="CIP") %>% 
  left_join(dt_vac.agregada,by="CIP") 

dt_dades %>% filter(Mostra=="CONTROLS ") %>% select(CIP,Mostra)
dt_dades %>% filter(Mostra!="CONTROLS ") %>% select(CIP,Mostra)

# 8. Salvar fitxer ---------------






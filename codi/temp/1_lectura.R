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

# "farmacia.csv", "Laboratori.csv", "PSalut.csv", "Vacunes.csv", "Variables.csv"   

dt_psalut<-read.csv(here::here("dades","PSalut.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_laboratori<-read.csv(here::here("dades","Laboratori.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_vacunes<-read.csv(here::here("dades","Vacunes.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_variables<-read.csv(here::here("dades","Variables.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_farmacia<-read.csv(here::here("dades","farmacia.csv"), header=T, sep=";",fileEncoding="utf-16") %>% as_tibble()

dt_cips<-read_excel("./dades/Cips.xls")


# Falta fitxer de població (Caracteristiques demografiques)
# Falta fitxer de TBC ()


# 4. Formatar dades  -------------


# 5. Descriptiu --------------- 

# Per variable (codi) dates i valors:  max i min 
# Per codi frequencies

table(dt_psalut$codiPSalut)


# 6. Agregar / Aplanar cada base de dades en data index (31/06/2006 ?) ------------


# 7. Fusionar taules en una --------------


# 8. Salvar fitxer ---------------






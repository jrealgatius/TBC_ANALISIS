
##############
#[10.03.2020]#
##############



#                               FASE LECTURA                            ---------------------
#  Aplicació lectura de dades / aplanament / criteris d'inclusió 

#memory.size(max=160685)
#
# 1. Càrrega de funcions ----------------------  
gc()
rm(list=ls())

link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
devtools::source_url(link_source)

#CANVI
# 2. Paràmetres  ----------------------

load("dt_agencia.Rdata")

#-----------------------------------------------#
dt_plana_agencia<-dt_agencia
#-----------------------------------------------#
#variable.names(dt_plana_agencia)


dt_plana_agencia<-netejar_espais(dt_plana_agencia)


##1."CIP"             
#dt_plana_agencia$CIP
#table(dt_plana_agencia$CIP)
N_CIP<-dt_plana_agencia%>%filter(repe>1)%>%select(CIP)

##2."Mostra"          
#dt_plana_agencia$Mostra

##3 "anynai"  
#dt_plana_agencia$anynai

##4 "mesnai"  No         
#dt_plana_agencia$mesnai

##5 "dianaix" NO        
#dt_plana_agencia$dianaix

##6 "DATA_NAIX"       
#dt_plana_agencia$DATA_NAIX

##7"DAT_INCL"        
#dt_plana_agencia$DAT_INCL

##8 "PAIS"           
#dt_plana_agencia$PAIS
#table(dt_plana_agencia$PAIS)

##9 "NACIONALITAT"    
#dt_plana_agencia$NACIONALITAT
#table(dt_plana_agencia$NACIONALITAT)

##10 "ETNIA"           
#dt_plana_agencia$ETNIA

##11 "DAT_LLEGADA"     
#dt_plana_agencia$DAT_LLEGADA

##12 "TABAC"          
#dt_plana_agencia$TABAC

##13 "ALCOHOL"         
#dt_plana_agencia$ALCOHOL

###14 "DROGUES"         
#dt_plana_agencia$DROGUES
#table(dt_plana_agencia$DROGUES)

##15 "PRESO"           
#dt_plana_agencia$PRESO

##16 "INCL_CVELLA"    
#dt_plana_agencia$INCL_CVELLA

##17 "ABS"            
#dt_plana_agencia$ABS
#table(dt_plana_agencia$ABS)

##18 "BARRI"          
#dt_plana_agencia$Mostra
#table(dt_plana_agencia$BARRI)

##19 "MEDEA"          
#dt_plana_agencia$MEDEA

##20 "INDIGENT"       
#dt_plana_agencia$INDIGENT

##21 "GRUP_OCUP"  ?    
#dt_plana_agencia$GRUP_OCUP

##22 "MORT"           
#dt_plana_agencia$MORT
#table(dt_plana_agencia$MORT)

##23 "DAT_MORT"        
#dt_plana_agencia$DAT_MORT

##24 "DET_TB"         
#dt_plana_agencia$DET_TB

##25 "TB._PULMONAR"    
#table(dt_plana_agencia$TB._PULMONAR)
#0129

##26 "TB_PLEURAL"      
#table(dt_plana_agencia$TB_PLEURAL)
#0129

##27 "TB_PLEUROPULM"   
#table(dt_plana_agencia$TB_PLEUROPULM)
#0129

##28 "TB_LIMFATICA"   
#table(dt_plana_agencia$TB_LIMFATICA)
#0129

##29 "TB_OSTEOART"     
#table(dt_plana_agencia$TB_OSTEOART)
#0129

##30 "TB_MILIAR"       
#table(dt_plana_agencia$TB_MILIAR)
#0129

##31 "TB_GENITOURIN"   
#table(dt_plana_agencia$TB_GENITOURIN)
#0129

##32 "TB_MENINGEA"    
#table(dt_plana_agencia$TB_MENINGEA)
#0129

##33 "TB_LARINGEA"    
#table(dt_plana_agencia$TB_LARINGEA)
#0129

##34 "TB_ALTRESLOC"   
#table(dt_plana_agencia$TB_ALTRESLOC)
#0129

##35 "PPD"             
#table(dt_plana_agencia$PPD)

##36 "RADIOLOGIA"     
#table(dt_plana_agencia$RADIOLOGIA)

##37 "BACTERIOL"      
#table(dt_plana_agencia$BACTERIOL)

##38 "VSG"            
#table(dt_plana_agencia$VSG)

##39 "PCR"             
#table(dt_plana_agencia$PCR)

##40 "Leucocits"      
#table(dt_plana_agencia$Leucocits)

##41 "EXC_Urinaria"    
#table(dt_plana_agencia$EXC_Urinaria)

##42 "Filt.Glomerular" 
#table(dt_plana_agencia$Filt.Glomerular)

##43"IMC"             
#table(dt_plana_agencia$IMC)

##44 "HIV"            
#table(dt_plana_agencia$HIV)
# 0   1   2   3   9

##45 "SIDA"           
#table(dt_plana_agencia$SIDA)
# 0   1   2   3   9

##46 "HB1AC"           
#table(dt_plana_agencia$HB1AC)

##47 "DIABETIS"        
#table(dt_plana_agencia$DIABETIS)
#NO  SI

##48 "repe"           
#table(dt_plana_agencia$repe)
#1   2

##49 "by"             
#table(dt_plana_agencia$by)

##50"sexe"            
#table(dt_plana_agencia$sexe)
#D   H 
##51 "age"             
#table(dt_plana_agencia$age)

##52 "PS"             
#table(dt_plana_agencia$PS)

##53 "grup"            
#table(dt_plana_agencia$grup)
#Control      DM 

##54 "event_tbc"      
#table(dt_plana_agencia$event_tbc)
#0 no   1 si

##55 "DG.TBC_cat"
#table(dt_plana_agencia$DG.TBC_cat)
#0 no   1 si


#RECODES / CALCULS     

dt_plana_agencia<-dt_plana_agencia%>%mutate(MORT2=if_else(is.na(MORT),0,1))
#dt_plana_agencia$MORT2

dt_plana_agencia<-dt_plana_agencia%>%mutate(Leucocits=as.numeric(Leucocits))
#table(dt_plana_agencia$Leucocits)


#-------
#exemples recordatori de recodificacions[]
#-------------------------------------------------------------------------------------------------#
#dt_plana<-dt_plana%>%mutate(dtindex=as_date(dtindex))
#dt_plana<-dt_plana %>% mutate(any_index=lubridate::year(lubridate::as_date(dtindex)))
#dt_plana<-dt_plana %>% mutate(agein=(as_date(dtindex)-ymd(dnaix))/365.25)
#dt_plana<-dt_plana %>% mutate(exitus=if_else(situacio=="D",1,0))
#dt_plana<-dt_plana %>% mutate(temps_FU=ymd(sortida)-as_date(dtindex))
#dt_plana<-dt_plana %>% mutate(temps_FU2=(ymd(sortida)-as_date(dtindex))/365.25)
#dt_plana<-dt_plana %>% mutate(agein2=as.numeric(as_date(dtindex)-ymd(dnaix))/365.25)
#dt_plana$dnaix<-as.character(dt_plana$dnaix)
#dt_plana$any_index2<-as.character(dt_plana$any_index)
#-------------------------------------------------------------------------------------------------#
#recodificació1:
#dt_plana<-dt_plana%>%mutate(agein3.cat6=case_when(  agein2<40~ 1,
#                                                    agein2>=40 & agein2<50 ~ 2,  
#                                                    agein2>=50 & agein2<60 ~ 3,
#                                                    agein2>=60 & agein2<70 ~ 4,
#                                                    agein2>=70  ~ 5))
#-------------------------------------------------------------------------------------------------#
#recodificació2:
#dt_plana<-dt_plana%>%mutate(IMC.valor2=case_when(   IMC.valor   <15~ 1,
#                                                    IMC.valor   >=15 & IMC.valor   <25 ~ 2,  
#                                                    IMC.valor   >=25 & IMC.valor   <30 ~ 3,
#                                                    IMC.valor   >=30  ~ 4))
#-------------------------------------------------------------------------------------------------#
# Farmacs 
#dt_plana<-mutate_at(dt_plana, vars( starts_with("FF.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))
#dt_plana<-mutate_at(dt_plana, vars( starts_with("FP.") ), funs( if_else(.==0  | is.na(.)  ,0,1)))
#-------------------------------------------------------------------------------------------------#




# 6. La cohort d'estudi final [dt_plana] --------------

#és la combinació de la cohorte exposada (EC) i la cohort final no exposada (FNE) 

#6.	The final study cohort is the combination of the exposed cohort (EC) and the final non-exposed cohort (FNE).


# ANALISIS  --------------

#6.	The final study cohort is the combination of the exposed cohort (EC) 
#   and the final non-exposed cohort (FNE).



#This is a retrospective cohort study. 
#All people diagnosed with diabetes 
#between 01/01/2006 and the latest specific capture 31/12/2018
#entrada>=20060101  & entrada<=20181231



#i) [[Flowchart]]

#flow_chart1
#flow_chart2
#flow_chart3






#ii)


#dt_plana<-recodificar(dt_plana,taulavariables =conductor,"recode",missings = T)

#variable.names(dt_plana)


conductor<-"conductor_agencia.xls"

dt_plana_agencia2<-dt_plana_agencia

###

#dt_plana_agencia2<-convertir_dates(d=dt_plana2,taulavariables=conductor)

dt_plana_agencia2<-etiquetar(d=dt_plana_agencia2,taulavariables=conductor)
dt_plana_agencia2<-etiquetar_valors(dt=dt_plana_agencia2,variables_factors=conductor,fulla="etiquetes",camp_etiqueta="etiqueta2")


#dt_plana_agencia2$grup
#dt_plana_agencia2$MORT2
#dt_plana_agencia2$event_tbc

variables_noconductuals<-extreure.variables("taula00",taulavariables = conductor)[!extreure.variables("taula00",taulavariables = conductor)%in%names(dt_plana_agencia2)]
formula_taula00<-formula.text("taula00",y="",taulavariables = conductor,elimina = variables_noconductuals)

T00<-descrTable(formula_taula00,
                data=dt_plana_agencia2,
                max.xlev = 100, 
                show.p.overall=FALSE,
                show.n = T,
                hide.no="NO",simplify=F)





dt_plana_agencia3<-dt_plana_agencia2%>%filter(!is.na(DET_TB))

variables_noconductuals<-extreure.variables("taula00",taulavariables = conductor)[!extreure.variables("taula00",taulavariables = conductor)%in%names(dt_plana_agencia3)]
formula_taula00<-formula.text("taula00",y="",taulavariables = conductor,elimina = variables_noconductuals)


T01<-descrTable(formula_taula00,
                data=dt_plana_agencia3,
                max.xlev = 100, 
                show.p.overall=FALSE,
                show.n = T,
                hide.no="NO",simplify=F)



save(N_CIP,T00,T01, file="dt_plana_agencia2.Rdata")





# Retorna dades matchejades 1:1 en funció de una llista de variables 
generar_matching<-function(dt=dt_ecap,vars_matching=c("sexe","age"),grup="DG.DM_cat",ratio=1) {

  # dt=dt_ecap
  # vars_matching=c("sexe","age")
  # grup="DG.DM_cat"
  # ratio=1
  
  dadesmatching<-dt %>% select(vars_matching) %>% bind_cols(select_(dt,"idp",grup))
  
  # Genero formula amb covariables 
  formulaPS<-as.formula(paste(grup, paste(vars_matching, collapse=" + "), sep=" ~ "))
  
  dt.matched<-formulaPS %>% 
    matchit(method="nearest",data=dadesmatching,ratio=ratio,caliper=0.01,distance = "logit") %>%    # FAig el matching 1 a 1
    weights() %>%                                                            # Guardo els pesos 
    data.table() %>% 
    'colnames<-'(c("PS")) %>%  as_tibble() %>% 
    cbind(dt) %>%                                                 # Ho junto al dt.total 
    filter(PS==1) %>% 
    as_tibble() 
  }



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

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




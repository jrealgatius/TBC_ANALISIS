


Esquema_ggplot<-function(dt=dt_temp,datainicial="data",datafinal="datafi",id="idp_temp",grup_color="Sexo",grup_shape="Group",lim_inf=-Inf,lim_sup=Inf,lab_y="lab_SexEdat") {
  
  # dt=dt_temp
  # datainicial= "data0"
  # datafinal="dtindex"
  # id="num"
  # grup_shape="Group"
  # grup_color = "Sexo"
  # lim_inf=-Inf
  # lim_sup="20181231"
  # lab_y="lab_SexEdat"
  
  if (is.na(grup_shape)) dt<- dt %>% mutate(Overall="Overall")
  if (is.na(grup_shape)) grup_shape<- "Overall"
  
  if (is.na(grup_color)) dt<- dt %>% mutate(Overall2="Overall2")
  if (is.na(grup_color)) grup_color<- "Overall2"
  
  # # Configuro limits finestra
  if (lim_inf==-Inf) porca1<-min(dt %>% dplyr::pull(datainicial) %>% lubridate::ymd())
  if (lim_sup==+Inf) porca2<-max(dt %>% dplyr::pull(datafinal) %>% lubridate::ymd())
  # # 
  if (lim_inf!=-Inf) porca1<-lim_inf
  if (lim_sup!=+Inf) porca2<-lim_sup
  
  porca1=lubridate::ymd(porca1)
  porca2=lubridate::ymd(porca2)
  
  # Conversio a Sym per evaluar  
  datainicial<-rlang::sym(datainicial)
  datafinal<-rlang::sym(datafinal)
  id<-rlang::sym(id)
  grup_color<-rlang::sym(grup_color)
  grup_shape<-rlang::sym(grup_shape)
  
  # Calculo dies de duracó  
  dt<-dt %>% 
    mutate(
      dia0=lubridate::ymd(!!datainicial),
      diaf=lubridate::ymd(!!datafinal),
      days_duration=lubridate::interval(dia0,diaf) %>% lubridate::as.duration()/lubridate::ddays()
    )
  
  num_cips<-dt$CIP %>% length()
  
  # Grafico el tema 
  figura<-ggplot(dt,aes(x =dia0,y =!!id)) +
    geom_segment(aes(x =diaf, xend=dia0, y =!!id, yend = !!id,color=!!grup_color)) +
    geom_point(aes(diaf, !!id,color=!!grup_color),size=4,shape=18) + 
    geom_linerange(aes(x=diaf,ymax=num_cips,ymin=0),linetype="dashed") + 
    xlim(porca1,porca2)+
    scale_y_continuous(breaks=dt %>% pull(num) ,labels=dt %>% pull(lab_y))+
    geom_rect(aes(xmin = lubridate::ymd(20070101), xmax = lubridate::ymd(20161231), ymin = min(num), ymax = max(num)),colour=NA,alpha=0.01)+
    theme_minimal()+
    theme(legend.position="top")
  
  figura
  
  }
  
"Cox proportional hazards"

extreure_HR<-function(a="grup2",x="DM_ajust",c=c,...) { 
  
  # a="grup2"
  # x="DM_ajust3"
  # event = "event_tbc"
  # t="temps_tbc"
  # d=dades
  # taulavariables = conductor_variables
  # c=cluster

  ## Etiquetar variables
  if (x!="") { covariables<-c(extreure.variables(x,conductor_variables)) 
               cov_labels<-etiquetar_vector(covariables,taulavariables=conductor_variables,camp_descripcio="Descripcio")
               cov_labels<-cov_labels %>% unique() %>% paste0(collapse = ", ")}
  
  if (x=="") {cov_labels<-"Unadjusted"}

  if (c=="") method="Cox-PH" else method="Cox PH by clusters"
  
  num_nivells<-levels(as.factor(dades[[a]])) %>% length()
  
  taula_HR<- HR.COX(a=a,x=x,...) %>% as_tibble(rownames="id")
  # taula_HR<- HR.COX(a=a,x=x,d=dades,taulavariables=conductor_variables,t="temps_tbc",event = "event_tbc") %>% as_tibble(rownames="id")

  taula_HR %>% 
    transmute(var=id,HR=HRadjusted,`Li95%CI`=IC951,`Ls95%CI`=IC952,`p-value`=p,method=method,adjustedby=cov_labels) %>% 
    filter(row_number() %in% c(1,num_nivells-1))
  
}

Estima_HR_RCrisk_clusters<-function(dt=dades,cov1="DM_ajust",a="grup2",failcode = "Event",cencode = "End of follow-up") {
  
  # failcode = "Event"
  # cencode = "End of follow-up"
  # dt=dades
  # cov1="DM_ajust4"
  # a="grup2"
 
  gc()
  
  if (cov1!="") covariables<-c(a,extreure.variables(cov1,conductor_variables,variable_camp = "camp")) %>% unique() else covariables<-c(a)
  
  ## Etiquetar variables
  if (cov1!="") {
    lab_covariables<-covariables[!covariables==a] %>% 
      etiquetar_vector(taulavariables=conductor_variables,camp_descripcio="Descripcio") } else {lab_covariables<-"Unadjusted"}
  
  
  nomscovariables<-colnames(stats::model.matrix(formula_vector(covariables,""),data = dt))[-1]
  cov1 <- stats::model.matrix(formula_vector(covariables,""),data = dt)[, -1]
  
  model<-crrSC::crrc(ftime=dt$temps_tbc,
                     fstatus=dt$status,
                     cov1=cov1,
                     cluster=dt$case.id, 
                     failcode = failcode, 
                     cencode = cencode)
  
  tab<-cmprsk::summary.crr(model) 
  tab<- tibble(var=nomscovariables) %>% bind_cols(tab$coef %>% as_tibble())
  
  num_nivells<-levels(as.factor(dt[[a]])) %>% length()
  
  x<-tab %>% 
    transmute(var,
              HR=`exp(coef)`, 
              `Li95%CI`=exp(coef- qnorm(1 - (1-0.95)/2)*`se(coef)`),
              `Ls95%CI`=exp(coef+ qnorm(1 - (1-0.95)/2)*`se(coef)`),
              `p-value`) %>% 
    filter(row_number() %in% c(1,num_nivells-1))
  

  as_tibble(x) %>% bind_cols(method="Competing risk",adjustedby=paste0("",paste0(unique(lab_covariables),collapse = ", "), collapse = " ,"))
  
}

plot_cmprisk_cuminc<-function(dt=dades,a="grup2",failcode = "Event",cencode = "End of follow-up") {
  
  # dt=dades
  # a="grup"
  # failcode = "Event"
  # cencode = "End of follow-up"
  model_cuminc<-cmprsk::cuminc(ftime=dt$temps_tbc,
                        group=dt[[a]],
                        fstatus=dt[["status"]],
                        cencode = cencode)
 
  model_cuminc$`Control Exitus`<-NULL
  model_cuminc$`Diabetes Exitus`<-NULL
  model_cuminc$`Control Switch group`<-NULL
  model_cuminc$`Diabetes Switch group`<-NULL
  
  p<-survminer::ggcompetingrisks(model_cuminc,conf.int = F,multiple_panels=F, 
                                 xlab="Days",ylab="Cumulative incidence of Tuberculusis",
                                 title = "Cumulative incidence curve")
  
  p$mapping <- aes(x = time, y = est, colour = group, linetype = event)
  p + labs(linetype = "Tuberculosis", colour = "")
  }




forest.plot.HR<-function(dadesmodel,label="Categoria",mean="estimate",lower="Linf",upper="Lsup",label_X="OR (95% CI)",
                         intercept=1,
                         nivell="outcome", factor1="type",color=F, label_Xvertical="Method",
                         nolabels=TRUE,
                         title = "Forest plot of hazard ratios by method",
                         label_Favors="Favors Controls        Favors DM",add_table=F) {
  
  # dadesmodel=taula_models
  # label="var"
  # mean="HR"
  # lower="Li95%CI"
  # upper="Ls95%CI"
  # label_X="Hazard ratio (95% CI)"
  # intercept=1
  # nivell="method"
  # factor1="Model"
  # label_Xvertical = "Method"
  # color=F
  # nolabels=TRUE
  # label_Favors="Favors SGLT-2        Favors oGLD-2"
  # title = "Forest plot of hazard ratios by method"
  
  # Generar data set 
  dadesmodel <- dadesmodel %>% select(valor=!!mean,Linf=!!lower,Lsup=!!upper,nivell=!!nivell, factor1=!!factor1)
  
  ## Preparar taula (Genero etiqueta)
  taula_betas<-dadesmodel %>% mutate(etiqueta=paste0("   ",factor1),
                                     Group = paste0(factor1))
  
  # Afegir fila com un punt nivell per outcome i genero label de group
  taula_betas<-taula_betas %>% split(.$nivell) %>% 
    purrr::map_dfr(~add_row(.x,.before = 0),.id = "outcome" ) %>% 
    dplyr::mutate (etiqueta2=if_else(is.na(etiqueta),outcome,"")) %>% 
    dplyr::mutate (etiqueta=if_else(is.na(etiqueta),outcome,etiqueta))
  
  # AFegir etiqueta 3 mes centrada
  taula_betas<-taula_betas %>% mutate(etiqueta3=lag(etiqueta2),
                                      etiqueta3=if_else(is.na(etiqueta3),"",etiqueta3))
  
  # Reordenar outcomes segons origen de taula inicial
  dt_ordre<-dadesmodel %>% distinct(outcome=nivell) %>% mutate(seq=seq(1:n()))
  taula_betas<-taula_betas %>% left_join(dt_ordre,by="outcome") %>% arrange(seq)
  
  # Generar id 
  taula_betas<-taula_betas %>% mutate(id=seq(n())) %>% mutate(id=n()-id+1)
  
  # REomplir missings en factor1 i factor2
  taula_betas<-taula_betas %>% tidyr::fill(c(factor1,Group),.direction="updown")
  
  # Relevel mateix ordre tal com surt taula   
  ordre_levels<-taula_betas %>% pull(Group) %>% unique()
  taula_betas$Group<-factor(taula_betas$Group, levels = ordre_levels)
  
  # per defecte agafo etiqueta 3 (Si no agafo etiqueta buida)
  if (nolabels) labels_scaleX=taula_betas %>% pull(etiqueta3) else labels_scaleX=taula_betas %>% pull(etiqueta)  
  
  #limits màxims d'eixos
  xmax=max(taula_betas$Lsup,na.rm = T) %>% max(4) 
  xmin=min(taula_betas$Linf,na.rm = T) %>% min(0.2)
  ymaxim=taula_betas %>% count() %>% as.numeric()
  
  fp <- ggplot(data=taula_betas,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    # geom_pointrange(size=0.6) + 
    # geom_pointrange(size=0.6) + 
    geom_errorbar(size=0.6,width =0.4) + 
    geom_hline(yintercept=intercept, lty=1,colour="grey") +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    scale_x_continuous(breaks=taula_betas %>% pull(id),labels=labels_scaleX)  +
    ylim(xmin,xmax)
  
  fp<-fp + theme_minimal(base_size = 12) + theme(axis.text.y = element_text(hjust = 0,vjust=0,size=12)) +
    labs(title = title, x=label_Xvertical,y=label_X, col="Method \n") +
    theme(legend.position="top") +
    annotate("text", x=ymaxim+1,y=1,label=label_Favors, colour = "black",size=2.5)
  
  # caption = "SGLT-2: sodium-glucose co-transporter-2 inhibitors | oGLD-2 \n created by Jordi Real & Rai Puig ")
  
  if (color) {fp<-fp + geom_point(aes(color=Group),size=3)} 
  
  # Add banda d'error
  # fp<-fp + geom_hline(yintercept = c(intercept+0.1,intercept-0.1),linetype=2)
  
  # Afegir Taula de coeficients
  if (add_table) {
    taula_betas<-taula_betas %>% mutate(HR_IC=if_else(!is.na(valor),paste0(sprintf("%04.2f",valor)," (",sprintf("%04.2f",Linf),";",sprintf("%04.2f",Lsup),")"),""))
    
    fp<- 
      fp + geom_text(aes(x=id,y=xmax+1), label=taula_betas$HR_IC) + 
      annotate("text", x=ymaxim+0.5,y=xmax+1,label=" HR (95% CI)") +
      ylim(xmin,xmax+1.5)
     
    } else fp
    
  
  fp
  
  # plotly::ggplotly(fp) 
  
  
}


model_HR_RCrisk_clusters<-function(dt=dades,cov1="DM_ajust",a="grup2",failcode = "Event",cencode = "End of follow-up") {
  
  # failcode = "Event"
  # cencode = "End of follow-up"
  # dt=dades
  # cov1="DM_ajust6"
  # a="grup"
  
  if (cov1!="") covariables<-c(a,extreure.variables(cov1,conductor_variables,variable_camp = "camp")) %>% unique() else covariables<-c(a)
  
  # rename etiquetes per diferent valor 
  nousnoms<-etiquetar_taula(as_tibble(covariables),camp="value",taulavariables=conductor_variables,camp_descripcio= "Descripcio") %>% pull(value)
  nousnoms<-paste0(nousnoms,".")
  dt<-dt %>% rename_at(vars(covariables),~nousnoms)

  nomscovariables<-colnames(stats::model.matrix(formula_vector(nousnoms,""),data = dt))[-1]
  cov1 <- stats::model.matrix(formula_vector(nousnoms,""),data = dt)[, -1]
  
  # nomscovariables<-colnames(stats::model.matrix(formula_vector(covariables,""),data = dt))[-1]
  # cov1 <- stats::model.matrix(formula_vector(covariables,""),data = dt)[, -1]

  model<-crrSC::crrc(ftime=dt$temps_tbc,
                     fstatus=dt$status,
                     cov1=cov1,
                     cluster=dt$case.id, 
                     failcode = failcode, 
                     cencode = cencode,maxiter=3)
  
  tab<-cmprsk::summary.crr(model) 
  tab<- tibble(var=nomscovariables) %>% bind_cols(tab$coef %>% as_tibble())
  
  num_nivells<-levels(as.factor(dt[[a]])) %>% length()
  
  x<-tab %>% 
    transmute(var,
              HR=`exp(coef)`, 
              `Li95%CI`=exp(coef- qnorm(1 - (1-0.95)/2)*`se(coef)`),
              `Ls95%CI`=exp(coef+ qnorm(1 - (1-0.95)/2)*`se(coef)`),
              `p-value`) 
  
  # Etiquetar variables
  covariables<-etiquetar_vector(covariables,taulavariables=conductor_variables,camp_descripcio="Descripcio")
  
  resum<-as_tibble(x) %>% bind_cols(method="Competing risk",adjustedby=paste0("",paste0(unique(covariables),collapse = ", "), collapse = " ,"))
  
  list(model_crrsc=model,resum_crrsc=resum)
  
  
}


forest.plot.modelcomplet<-function(dadesmodel=dadesmodel,label="Categoria",mean="HR",lower="Li95%CI",upper="Ls95%CI",label_X="OR (95% CI)", intercept=1,
                                   add_table=F) {
  
  # dadesmodel=dadesmodel
  # label="Categoria"
  # mean="HR"
  # lower="Li95%CI"
  # upper="Ls95%CI"
  # label_X="HR (95% CI)"
  # intercept=1
  # add_table=T
  
  # dadesmodel<-dadesmodel %>% mutate(id=seq(length(dadesmodel[[label]])))
  
  # Generar data set 
  dadestemp <- dadesmodel %>% select(etiqueta=!!label,valor=!!mean,Linf=!!lower,Lsup=!!upper,id) %>% arrange(id)

  
  #limits màxims d'eixos
  xmax=max(dadestemp$Lsup,na.rm = T) %>% max(4) 
  xmin=min(dadestemp$Linf,na.rm = T) %>% min(0.2)
  ymaxim=dadestemp %>% count() %>% as.numeric()
  
  fp <- ggplot(data=dadestemp,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    geom_errorbar(size=0.6,width =0.4) +
    geom_pointrange() + 
    geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Variable") + ylab(label_X) +
    scale_x_continuous(breaks=dadestemp %>% pull(id),labels=dadestemp %>% pull(etiqueta))
  
  # Afegir Taula de coeficients
  if (add_table) {
    
    dadestemp<-dadestemp %>% mutate(HR_IC=if_else(!is.na(valor),paste0(sprintf("%04.2f",valor)," (",sprintf("%04.2f",Linf),";",sprintf("%04.2f",Lsup),")"),""))
    
    fp<- fp + ylim(xmin,xmax+15) +
      geom_text(aes(x=id,y=xmax+5),label=dadestemp$HR_IC,hjust=0) + 
      annotate("text", x=ymaxim+1,y=xmax+5,label=" HR (95% CI)",hjust=0) 
      
  
    } else fp
  
  }



# Formateig ----------------
# Calcul de variables noves edat etc..

Formateig_dades<-function(dt=dades) {

  
    dt<-netejar.noms.variables(dt) %>% as_tibble()
    dt<-dt %>% netejar_espais()
  
  
    # Recode dates / factorització 
    dt<-dt %>% mutate_at(vars(starts_with("P_G_")), ~if_else(is.na(.),"No","Si")) # Vacunes
    
    dt<-dt %>% mutate_at(vars(starts_with("FP.")), ~if_else(is.na(.),"No","Si")) # Farmacs
    
    dt<-dt %>% mutate(PS=if_else(is.na(PS),0,1))
    
    dt<-dt %>% factoritzar.NO.YES("factoritzarSINO",taulavariables=conductor_variables)
    
    # Recode rangs de valors fora de l'interval a missings Reals (missings de valors reals)
    
    dt<-recode_to_missings(dt,taulavariables = conductor_variables,rang="rang_valid")
    
    # Calcul de dies i anys lliure de TBC 
    dt<-dt %>% mutate (dies_lliure_tbc=as.numeric(temps_tbc),anys_lliure_tbc=dies_lliure_tbc/365.25)
    
    # Numero de vegada que apareix un CIP
    dt<-dt %>% group_by(CIP) %>% arrange(dtindex) %>% mutate(seq=seq(1:n())) %>% ungroup()
    
    # Dades basals (Prevalents vs incidents )
    dt<-dt %>% mutate(any2007=ifelse(anyindex<=2007,"Si","No"))
    
    # Tractament  (Sense tractament)
    dt<-
      dt %>% mutate(FP_TRACTAMENT_AD=case_when(
        FP.ADO=="No" & FP.ADO_ALT=="No" & FP.ADO_MET=="No" & FP.ADO_SU=="No" & FP.INSU=="No"~"Sense ADO ni Insu",
        (FP.ADO=="Si" | FP.ADO_ALT=="Si" | FP.ADO_MET=="Si" | FP.ADO_SU=="Si") & FP.INSU=="No"~"Algun ADO",
        FP.INSU=="Si"~"Insulina")) 
    
    # VACUNACIO
    dt<-dt %>% mutate(VACUNA= if_else(P_G_60.valor == "Si" | 
                                              P_G_AD.valor== "Si" | 
                                              P_G_AR.valor=="Si" | 
                                              P_G_NE.valor=="Si","Si","No",missing = "No"))
    # Etiquetes de valors
    dt<-etiquetar_valors(dt, variables_factors= conductor_variables,fulla="etiquetes")
    # Recode paisos dos agrupacions
    dt<-etiquetar_valors(dt,conductor_variables,fulla = "nacionalitats",camp_etiqueta = "etiqueta2",missings = TRUE,new_vars = T,sufix = ".2")
    dt<-etiquetar_valors(dt,conductor_variables,fulla = "nacionalitats",camp_etiqueta = "etiqueta3",missings = TRUE)
    
    # Recode missings --> Espanya
    dt<-dt %>% 
      mutate(descNacionalitat.2=as.character(descNacionalitat.2)) %>% 
      mutate(descNacionalitat.2=if_else(descNacionalitat.2=="None","1. Espanya",descNacionalitat.2))
    
    # Recode visites - missings--0
    dt<-dt %>% mutate_at(vars(starts_with("visites_")), ~ifelse(is.na(.),0,.)) 
    
    # # Relevel 
    # dades2<- dades2 %>% mutate(grup=relevel(grup, "Control"))
    # Recodificar
    dt<-recodificar(dt,conductor_variables,"recode")
    #
    dt<-recodificar2(dt,conductor_variables,"recode2",missings = T,prefix="cat")
    
    # Recodificar en quartils i + categoria missings
    vars<-c("IMC.valor","PAD.valor","PAS.valor","visites_TOT","visites_MG")
    dt<-dt %>% 
      mutate_at(vars,.funs = list(CATQ=~Hmisc::cut2(.,g=3,na.rm = T))) %>% 
      mutate_at(vars(ends_with("_CATQ")), ~ifelse(!is.na(.),levels(.),"None") %>% as.factor())
    
    
    # Mana DMT2
    dt<-dt %>% mutate(grup2=case_when(grup=="Control"~"Control",
                                            DG.DM1_cat=="Yes"~"DM1",
                                            DG.DM2_cat=="Yes"~"DM2"))
    # Mana DMT2 en cas de cap DG.
    dt<-dt %>% mutate (grup2=if_else(is.na(grup2),"DM2",grup2))
    
    # DM Prevalent/incident
    dt<-dt %>% 
      mutate(grup3=case_when(grup=="Diabetes" & any2007=="Si"~ "DM Prevalent",
                             grup=="Diabetes" & any2007=="No"~ "DM Incident", 
                             grup=="Control"~"Control")) 
    
  }


Formateig_agencia<-function(dt) {
  
  #variable.names(dt_plana_agencia)
  dt<-netejar_espais(dt)
  dt<-netejar.noms.variables(dt)
  
  # Repetits
  N_CIP<-dt%>% filter(repe>1)%>% select(CIP)
  

  #RECODES / CALCULS     
  dt<-dt %>% mutate(MORT2=if_else(is.na(MORT),0,1))
  
  # Variable indicadora dades TBC provinents agencia
  dt<-dt %>% mutate(dt_agencia=ifelse(is.na(NUM_REG),0,1))
  
  # Factoritzar
  vars_factor<-extreure.variables("factoritzar",conductor_agencia)
  dt<-dt %>% mutate_at(vars_factor,as.factor)
  
  # Etiquetar valors
  dt<-etiquetar_valors(dt=dt,variables_factors=conductor_agencia,fulla="etiquetes",camp_etiqueta="etiqueta2")
  
}


roc_curve<-function(dt=dades,valor="HBA1c.valor",event="event_tbc"){
  
  # dt=dades
  # valor="HBA1c.valor"
  # event="event_tbc"
  
  dt_temp<-dt %>% select(valor=valor,event=event) %>% na.omit()
  
  g <- pROC::roc(event ~ valor, data = dt_temp)
  auc=pROC::auc(g)
  auc_ci=pROC::ci(g) 
  
  ggplot(dt_temp, aes(d = event, m = valor)) + 
    plotROC::geom_roc(n.cuts = 0) +
    annotate("text", x = .75, y = .25, label = paste("AUC:",round(auc_ci[2],2), "; 95 CI%:",round(auc_ci[1],2),"-",round(auc_ci[3],2)))
  
  
}


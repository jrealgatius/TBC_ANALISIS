


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
  
  # ConversiÃ³ a Sym per evaluaciÃ³  
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
  
  # Grafico el tema 
  figura<-ggplot(dt,aes(x =dia0,y =!!id)) +
    geom_segment(aes(x =diaf, xend=dia0, y =!!id, yend = !!id,color=!!grup_color)) +
    geom_point(aes(diaf, !!id,color=!!grup_color),size=4,shape=18) + 
    geom_linerange(aes(x=diaf,ymax=10,ymin=0),linetype="dashed") + 
    xlim(porca1,porca2)+
    scale_y_continuous(breaks=dt %>% pull(num) ,labels=dt %>% pull(lab_y))+
    geom_rect(aes(xmin = lubridate::ymd(20070101), xmax = lubridate::ymd(20161231), ymin = min(num), ymax = max(num)),colour=NA,alpha=0.01)+
    theme_minimal()+
    theme(legend.position="top")
  
  figura
  
  }
  
"Cox proportional hazards"

extreure_HR<-function(a="grup2",x="DM_ajust",c=c,...) { 
  
  if (x!="") covariables<-c(a,extreure.variables(x,conductor_variables)) %>% unique() %>% paste0(collapse = ", ")
  if (x=="") covariables<-c(a) 
  
  if (c=="") method="Cox-PH" else method="Cox PH by clusters"
  
  num_nivells<-levels(as.factor(dades[[a]])) %>% length()
  
  HR.COX(a=a,x=x,...) %>% as_tibble(rownames="id") %>% 
    transmute(var=id,HR=HRadjusted,`Li95%CI`=IC951,`Ls95%CI`=IC952,`p-value`=p,method=method,adjustedby=covariables) %>% 
    filter(row_number() %in% c(1,num_nivells-1))
  
}

Estima_HR_RCrisk_clusters<-function(dt=dades,cov1="DM_ajust",a="grup2",failcode = "Event",cencode = "End of follow-up") {
  
  # failcode = "Event"
  # cencode = "End of follow-up"
  # dt=dades
  # cov1="DM_ajust3"
  # a="grup"
  
  gc()
  
  if (cov1!="") covariables<-c(a,extreure.variables(cov1,conductor_variables,variable_camp = "camp")) %>% unique() else covariables<-c(a)
  
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
  
  
  as_tibble(x) %>% bind_cols(method="Competing risk",adjustedby=paste0("",paste0(unique(covariables),collapse = ", "), collapse = " ,"))
  
}

plot_cmprisk_cuminc<-function(dt=dades,a="grup2",failcode = "Event",cencode = "End of follow-up") {
  
  # dt=dades
  # cov1=""
  # a="grup"
  # failcode = "Event"
  # cencode = "End of follow-up"
  model_cuminc<-cmprsk::cuminc(ftime=dt$temps_tbc,
                        group=dt[[a]],
                        fstatus=dt[["status"]],
                        cencode = cencode)
 
  model_cuminc$`Control Exitus`<-NULL
  model_cuminc$`Diabetis Exitus`<-NULL
  model_cuminc$`Control Swich group`<-NULL
  model_cuminc$`Diabetis Swich group`<-NULL
  
  p<-survminer::ggcompetingrisks(model_cuminc,conf.int = F,multiple_panels=F, 
                                 xlab="Days",ylab="Cumulative incidence of Tuberculusis",
                                 title = "Competing risks analysis")
  
  p$mapping <- aes(x = time, y = est, colour = group, linetype = event)
  p + labs(linetype = "Tuberculosis", colour = "")
  }




forest.plot.HR<-function(dadesmodel,label="Categoria",mean="estimate",lower="Linf",upper="Lsup",label_X="OR (95% CI)",
                         intercept=1,
                         nivell="outcome", factor1="type",color=F, label_Xvertical="Method",nolabels=TRUE,
                         title = "Forest plot of hazard ratios by method",
                         label_Favors="Favors Controls        Favors DM") {
  
  # dadesmodel=taula_models_COX
  # label="var"
  # mean="HR"
  # lower="Li95%CI"
  # upper="Ls95%CI"
  # label_X="Hazard ratio (95% CI)"
  # intercept=1
  # nivell="method"
  # factor1="adjust"
  # label_Xvertical = "Method"
  # color=F
  # nolabels=TRUE
  # label_Favors="Favors SGLT-2        Favors oGLD-2"
  
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
  taula_betas<-taula_betas %>% fill(c(factor1,Group),.direction="updown")
  
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
  
  resum<-as_tibble(x) %>% bind_cols(method="Competing risk",adjustedby=paste0("",paste0(unique(covariables),collapse = ", "), collapse = " ,"))
  
  list(model_crrsc=model,resum_crrsc=resum)
  
  
}


forest.plot.modelcomplet<-function(dadesmodel=dadesmodel,label="Categoria",mean="HR",lower="Li95%CI",upper="Ls95%CI",label_X="OR (95% CI)", intercept=1) {
  
  # dadesmodel=dt_dif
  # label="lipo"
  # mean="dif_st"
  # lower ="ci1"
  # upper="ci2"
  # label_X="Differences standardized (95% CI)"
  # intercept=0
  
  # dadesmodel<-dadesmodel %>% mutate(id=seq(length(dadesmodel[[label]])))
  
  # Generar data set 
  dadestemp <- dadesmodel %>% select(etiqueta=!!label,valor=!!mean,Linf=!!lower,Lsup=!!upper,id) %>% arrange(id)
  
  fp <- ggplot(data=dadestemp,aes(x=id, y=valor, ymin=Linf, ymax=Lsup)) +
    geom_errorbar(size=0.6,width =0.4) +
    geom_pointrange() + 
    geom_hline(yintercept=intercept, lty=2) +  # add a dotted line at x=1 after flip
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Variable") + ylab(label_X) +
    scale_x_continuous(breaks=dadestemp %>% pull(id),labels=dadestemp %>% pull(etiqueta))
  
  fp
  
}




  
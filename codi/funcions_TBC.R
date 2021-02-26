


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
  
  
  
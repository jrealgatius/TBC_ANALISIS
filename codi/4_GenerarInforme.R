# GENERAR INFORME 

# Funció --------------

# metode = "dinamica" / "estatica" / "PS"

Generar_informe=function(metode="dinamica") {
  
  if (metode=="dinamica") {
    output="informe_Cohort_Din"
    subtitul="Cohorte DINÁMICA" }
  
  if (metode=="estatica") {
    output="informe_C_estat"
    subtitul="Cohorte ESTÁTICA" }
  
  if (metode=="PS") {
    output="informe_C_estatPS"
    subtitul="Cohorte ESTÁTICA MATCHING por PS" }
  
  rmarkdown::render("./codi/3_analisis_TBC.Rmd", 
                    output_file = output,
                    output_dir = "codi/informes",
                    params=list(metode=metode,subtitul=subtitul),
                    envir = parent.frame() # Truc per que render dins funció no peti 
                    )
  }


metode="PS"

rmarkdown::render("./codi/1_lectura_TBC.Rmd",params = list(metode=metode))

rmarkdown::render("./codi/2_preparacio_TBC.Rmd",params = list(metode=metode))

Generar_informe(metode)




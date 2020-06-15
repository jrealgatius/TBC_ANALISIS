# GENERAR INFORME 

# Funció --------------

# metode = "dinamica" / "estatica" / "PS"

Generar_informe=function(metode="dinamica2") {
  
  if (metode=="dinamica2") {
    output="informe_Cohort_Din"
    subtitul="Cohorte DINÁMICA: Controles sin reemplazo" }
  
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

metode="dinamica2"
rmarkdown::render("./codi/1_lectura_TBC.Rmd",params = list(metode=metode,cas_com_controls=TRUE))
rmarkdown::render("./codi/2_preparacio_TBC.Rmd",params = list(metode=metode))
Generar_informe("dinamica2")

metode="PS"
rmarkdown::render("./codi/1_lectura_TBC.Rmd",params = list(metode=metode,cas_com_controls=TRUE))
rmarkdown::render("./codi/2_preparacio_TBC.Rmd",params = list(metode=metode))
Generar_informe(metode)

metode="estatica"
rmarkdown::render("./codi/1_lectura_TBC.Rmd",params = list(metode=metode,cas_com_controls=TRUE))
rmarkdown::render("./codi/2_preparacio_TBC.Rmd",params = list(metode=metode))
Generar_informe(metode)




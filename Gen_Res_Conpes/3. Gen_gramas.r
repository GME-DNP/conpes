rm(list = ls())

#################
### Librerías ###
#################

library(dplyr)
library(purrr)
library(tibble)
library(quanteda)
library(qdapRegex)
library(stringr)
library(writexl)
library(rstudioapi)  
library(tidyr)

#####################################
### Funciones de preprocesamiento ###
#####################################

### 1. Lectura de Funciones ###

setwd("C:/Users/user/Google Drive/6. DNP/2. Proyectos/2. Terminados/3. Conpes/2. Códigos R/1. PreProcesamiento")
source("4. Fun_Base.r")

setwd("..")
setwd("2. Gen_Res_Conpes")
source("1. Funciones asociadas.r")

### 2. Cargando los TCP limpios ###

setwd("..")
setwd("..")
setwd("3. Salidas/1. Limpieza Texto")
load("salidas_PND.Rdata")

##############################
### Generación de N-Gramas ###
##############################

l_unigramas <- l_conpes_depur %>% map(f_ngramas, ngramas = 1)
l_bigramas  <- l_conpes_depur %>% map(f_ngramas, ngramas = 2)
l_trigramas <- l_conpes_depur %>% map(f_ngramas, ngramas = 3)

##########################
### Conteo de N-Gramas ###
##########################

l_dfFreq <- l_unigramas %>% map(f_Frecuencias)
l_dfBigrama <- l_bigramas %>% map(f_Frecuencias)

base_conteos = c()
for (i in 1:length(l_dfBigrama)) {
  l_dfBigrama[[i]]$doc = lista_archivos[i]
  base_conteos = rbind(base_conteos,l_dfBigrama[[i]])
}

#########################################
### Selección de bigramas específicos ###
#########################################

palabras_buscadas = c("evaluacion_politicas","evaluacion_politica",
                      "evaluar_politicas","evaluar_politica",
                      "evaluacion_programas","evaluacion_programa",
                      "evaluar_programas","evaluar_programa",
                      "evaluacion_proyectos","evaluacion_proyecto",
                      "evaluar_proyectos","evaluar_proyecto",
                      "evaluacion_resultados","evaluacion_resultado",
                      "evaluar_resultados","evaluar_resultado",
                      "evaluacion_operaciones","evaluacion_operacion",
                      "evaluar_operaciones","evaluar_operacion",
                      "medicion_impactos","medicion_impacto","medir_efectos",
                      "medir_efecto","evidencia_empirica","costo_beneficios",
                      "costo_beneficio","costo_efectividad", "impacto_proyecto",
                      "impacto_programa","impacto_politica","efecto_proyecto",
                      "efecto_programas","efecto_política","diseno_experimental",
                      "diseno_experimentos","experimento_impacto")

base_bigramas = subset(base_conteos,Palabra %in% palabras_buscadas)

### Generación de tabla ###

salida <- base_bigramas %>% select(doc, Palabra, n) %>% spread(Palabra, n) 
salida[is.na(salida)] <- 0

##############
### Salida ###
##############

setwd("..")
setwd("2. Entrega final")
openxlsx::write.xlsx(file =  "Bigramas.xlsx", salida)



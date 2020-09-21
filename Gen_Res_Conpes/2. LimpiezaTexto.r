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
library(pdftools)

#####################################
### Funciones de preprocesamiento ###
#####################################

### 1. Lectura de Funciones ###

setwd("C:/Users/dflemusp/Google Drive/6. DNP/2. Proyectos/1. En proceso/2. Conpes/2. Códigos R/1. PreProcesamiento")
source("4. Fun_Base.r")

# Palabras a eliminar (incluir manualmente) #

terminos_remover <- c("www","http","https","asociado","dnp","nacional", 
                      "plan","paises","periodo","uso","colombiano","con todo",
                      "capitulo","pacto","y","además","también","asimismo", 
                      "por añadidura","igualmente","encima","es","más","aún",
                      "incluso","hasta","para colmo","a pesar de todo", "pero", 
                      "aun así","ahora bien","de cualquier modo", "sin embargo", 
                      "al mismo tiempo","no obstante","en cierto modo",
                      "en cierta medida","hasta cierto punto","si bien",
                      "por otra parte","empero","por el contrario","por ende",
                       "en cambio","por tanto","por consiguiente", "de ahí que",
                      "en consecuencia"," así pues","por lo tanto","por eso",
                      "por consiguiente","por lo que sigue", "por esta razón",
                      "entonces","resulta que", "de manera que","porque", "pues",
                      "puesto que", "ya que","a causa de","visto que","dado que", 
                      "como","considerando que", "a causa de", "por culpa de", 
                      "Del mismo modo","igualmente","análogamente","a saber",
                      "de modo similar","es decir","o sea","esto es","en resumen",
                      "en otras palabras","de hecho","en resumidas cuentas", 
                      "en definitiva", "en suma", "total", "en una palabra",
                      "en otras palabras", "en breve", "en síntesis", 
                       "dicho de otro modo", "recapitulando", "brevemente", "en pocas palabras", "globalmente",
                       "en conjunto", "recogiendo lo más importante", "así pues", "como se ha dicho", "por ejemplo", "así", "así como", 
                       "verbigracia", "por ejemplo", "particularmente","en particular","específicamente","incidentalmente","para ilustrar",
                       "en el caso de","vale la pena decir", "hay que hacer notar", "conviene destacar", "lo más importante", 
                       "la idea central es","en efecto","efectivamente","mejor dicho","o sea","bueno","bien","ante todo","para comenzar",
                       "primeramente", "antes de nada", "primero", "en fin", "por último", "en suma", "finalmente", "por fin", "al final",
                       "terminando","para resumir", "en conclusión", "para finalizar", "en definitiva","por otro lado", "por otra parte",
                       "en otro orden de cosas", "a continuación", "después","luego", "además", "con respecto a", "en cuanto a", "acerca de",
                       "otro punto es", "por lo que se refiere a", "por cierto", "a propósito","a todo esto", "a partir
                       de","antes de","antes que", "hasta que", "en cuanto", "al principio", "en el comienzo", "a continuación",
                       "inmediatamente", "temporalmente", "actualmente", "finalmente", "por último", "cuando", "al mismo tiempo",
                      "anteriormente", "acto seguido", "más adelante", "más tarde", "afecta", "especialmente","alle",
                      'plan','paises','ia','especialmente','periodo','uso','colombiano','nacional',
                      'siguientes','tales','caso','lograr','cuadro','us','nivel','forma','mil','bases',"ods",
                      'dg','hac','dgsarrollo','tar','fin','capitulo','cion','base','meta','cuenta','dnp',
                      'iii','vii','xii','xiii','xvi','xvii','xviii','xxi','xxii','xxiii','pacto',"asociado",'politica','entidades','politicas','sistema','pacto','colombia','pais')

palabras_eliminar <- ListaStopwords(useTM = TRUE, # Usar lista stopwords paquete tm
                                    useranks.nl = TRUE, # Usar lista ranks.nl
                                    useGH = TRUE, # Usar lista github
                                    useNombres = TRUE, # Incluir nombres propios como sws
                                    # Stopwords adicionales; ejm: aaa
                                    vectorAddi = terminos_remover, # Vector terminos a?adir # geocol
                                    listAddi=NA, # Path lista terminos a?adir
                                    # No son stopwords; ejm: empleo
                                    vectorExclus=c(), # Vector terminos excluir
                                    listExclus=NA, # Path lista terminos excluir
                                    saveFile=FALSE # Guardar lista en archivo de texto
)

setwd("..")
setwd("2. Gen_Res_Conpes")
source("1. Funciones asociadas.r")

################################################################################
        #################### Listado de archivos #################### 
################################################################################

setwd("..")
setwd("..")
setwd("1. Docs")

lista_archivos <- list.files()
lista_archivos <- lista_archivos[grepl(".pdf", lista_archivos )]

###############################
### Lectura de los archivos ###
###############################

l_conpes <- lista_archivos %>% map(f_lectura)

################################
### Depuración de los textos ###
################################

l_conpes_depur <- l_conpes %>% map(f_limpieza_texto)
l_conpes_depur <- chartr('áéíóúñ','aeioun',l_conpes_depur)
l_conpes_sinEspacios <- l_conpes_depur %>% map(f_limpieza_espacios)

l_conpes_noespaciosCarrNewl <- l_conpes_sinEspacios %>% gsub("((?![\\r\\n])\\s){2,}", "", ., perl = T)
l_conpes_noespaciosCarrNewl <- as.list(l_conpes_noespaciosCarrNewl)

###############
### Salidas ###
###############

setwd("..")
setwd("3. Salidas/1. Limpieza Texto")
save(lista_archivos, l_conpes, l_conpes_depur, l_conpes_sinEspacios, 
     l_conpes_noespaciosCarrNewl,
     file = "salidas_PND.Rdata")


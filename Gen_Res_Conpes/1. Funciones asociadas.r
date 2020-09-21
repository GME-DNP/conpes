
f_lectura <- function(x){
  archivo <- pdftools::pdf_text(x)
  archivo <- paste(archivo, collapse = " ")  
}

f_ngramas <- function(x, ngramas = 1) {
  vector_ngramas <- tokens(x)#[[1]] 
  #vector_ngramas <- gsub("_", " ", vector_ngramas)
  vector_ngramas=tokens_ngrams(vector_ngramas, n = ngramas, skip = 0L, concatenator = "_")
  vector_ngramas
}


f_extraer_mpio <- function(x){
  x %>% gsub("Relatoria Mesas de Trabajo", "", .) %>% gsub("Relatoria Mesas Trabajo", "", .) %>% 
    gsub(".pdf", "", .) %>% gsub("_", ", ", .) %>% paste(., ", Colombia") %>%  trimws
}


f_Frecuencias <- function(x){
  df <- as_tibble(as.data.frame(table(as.character(x)))) %>%  arrange(-Freq)
  #df <- as_tibble(as.data.frame(table(x))) %>%  arrange(-Freq)
  names(df) <- c("Palabra", "n")
  df <- df %>%  mutate(porc =  n / sum(n), cumpor = cumsum(porc))
  df
}

f_extraer_fecha <- function(x){
  x %>% gsub(".* MESAS DE TRABAJO (.*) CONSEJER?A PRESIDENCIAL.*", "\\1", .) %>% gsub("?", "", .) %>% trimws
}


f_ultimapalabra <- function(x) { 
  word(x,-1)
}


f_limpieza_espacios <- function(x){
  x %>%  gsub("[^[:print:]]", " ", .) %>% 
    gsub("[[:space:]]{1,}", " ", .) %>%  # Limpiar siglas de dos letras 
    trimws 
}

f_limpieza_texto <- function(x){
  x %>%  gsub("[^[:print:]]", " ", .) %>% tolower %>% 
    gsub("[^[:lower:]^[:space:]]", " ", .)  %>% 
    gsub("[[:space:]]{1,}", " ", .)  %>% 
    gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", .)   %>%  # Limpiar siglsa de dos letras 
    trimws %>% RemoveStopwordsFromText(palabras_eliminar)
}

f_extraerEntre <- function(x, inicio, fin, extremos) rm_between(x, inicio, fin, extract=TRUE, include.markers = extremos)[[1]]


f_extraerEntre_Spanish <- function(x, inicio, fin, extremos){
  
  # Texto y delimitadores sin acentos
 

  inicio_sinacento <- inicio %>% gsub("?", "a", .) %>% gsub("?", "e", .) %>% gsub("?", "i", .) %>%
    gsub("?", "o", .) %>% gsub("?", "u", .) %>%
    gsub("?", "A", .) %>% gsub("?", "E", .) %>% gsub("?", "I", .) %>% gsub("?", "O", .) %>% gsub("?", "U", .)  %>% 
    gsub("?", "n", .) %>% gsub("?", "N", .) %>% gsub("?", "u", .)  %>% gsub("?", "U", .)
  
  fin_sinacento <- fin %>% gsub("?", "a", .) %>% gsub("?", "e", .) %>% gsub("?", "i", .) %>%
    gsub("?", "o", .) %>% gsub("?", "u", .) %>%
    gsub("?", "A", .) %>% gsub("?", "E", .) %>% gsub("?", "I", .) %>% gsub("?", "O", .) %>% gsub("?", "U", .)  %>% 
    gsub("?", "n", .) %>% gsub("?", "N", .) %>% gsub("?", "u", .)  %>% gsub("?", "U", .)  %>%
    gsub("(", "", . , fixed=TRUE) %>%  gsub(")", "", ., fixed=TRUE)
  
  texto <- gsub(inicio, inicio_sinacento,x)
  texto <- gsub(fin, fin_sinacento,texto)
  
    # Extraer texto sin acento
  extrae_texto_sinacentos <-  rm_between(text.var = texto, 
                                         left = inicio_sinacento, right = fin_sinacento, extract = T,
                                         include.markers = extremos)[[1]]

  extrae_texto_sinacentos
}



f_secciones <- function(x, extremos = T, inicio_seccion = '\\r\\n', fin_seccion = '\\r\\nVocero') { 
  # Remover solo espacios, no remueve newlines ni carriages, luego remueve
  x <- gsub("Vocera", "Vocero", x)
  x %>% gsub("((?![\\r\\n])\\s){2,}", "", ., perl = T) %>%
    f_extraerEntre(inicio_seccion, fin_seccion, extremos = extremos )
}


# i = 1, j = 1
# listaSinEspaciosConNewLineCarriage <- l_talleres2_noespaciosCarrNewl
# lista_secciones <- l_secciones
# vector_mpios <- nombres_mpios
# vector_ultimapalabra <- v_ultimapalabra

# Extraer secciones

f_conformacion_lista <- function(listaSinEspaciosConNewLineCarriage, vector_mpios, lista_secciones,
                                 lista_secciones_arregladas,
                                 vector_ultimapalabra, hay_texto_interior = T,
                                 inicio_texto_interior =  "Necesidades:",
                                 fin_texto_interior =  "Entidades que pueden dar soluci?n:"){

l_AnalisisTextoXmpio <- vector("list", length(vector_mpios)) 
names(l_AnalisisTextoXmpio) <- vector_mpios

# Inicializar el contenido para los textos de las listas


for(i in 1:length(vector_mpios)){
  for(j in 1:length(lista_secciones[[i]])){
    l_AnalisisTextoXmpio[[i]] <-  vector("list", length(lista_secciones[[i]]))
  }
}

# names(l_AnalisisTextoXmpio[[i]][[j]]) <- l_secciones_arregladas[[i]][[j]]
#i = 1; j = 6
for(i in 1:length(vector_mpios)){
  for(j in 1:(length(lista_secciones[[i]]) - 1)){
    
    inic <- gsub("\\n", "\\\\\\\\n", unlist(lista_secciones[[i]])[j])
    inic <- gsub("\\r", "\\\\\\\\r", inic)
    inic <- gsub("(", "", inic, fixed = T)
    inic <- gsub(")", "", inic, fixed = T)
    #inic <- gsub("-", " ", inic, fixed = T)
    inic <- gsub("Vocera", "Vocero", inic)
    
    
    fn <- gsub("\\n", "\\\\\\\\n",  unlist(lista_secciones[[i]])[j+1])
    fn <- gsub("\\r", "\\\\\\\\r", fn)
    fn <- gsub("(", "", fn, fixed = T)
    fn <- gsub(")", "", fn, fixed = T)
    #fn <- gsub("-", "", fn, fixed = T)
    fn <- gsub("Vocera", "Vocero", fn)
    
    
    texto <- listaSinEspaciosConNewLineCarriage[[i]]
    texto <- gsub("\\n", "\\\\n",  texto)
    texto <- gsub("\\r", "\\\\r", texto)
    texto <- gsub("(", "", texto, fixed = T)
    texto <- gsub(")", "", texto, fixed = T)
    texto <- gsub("Vocera", "Vocero", texto)
    #texto <- gsub("-", "", texto, fixed = T)
    
    
     a <- f_extraerEntre_Spanish(x = texto, inicio = inic , fin = fn,  extremos = F) %>% 
      gsub("\\\\r", " ",.) %>%  gsub("\\\\n", " ", .)
    
     # Quitar vocero y entidades que dan soluci?n
     if(hay_texto_interior==T){
        a <- f_extraerEntre_Spanish(x = a, inicio = inicio_texto_interior ,
                                    fin = fin_texto_interior,
                                extremos = F)
     }
     
     a <- a %>%  gsub("[^[:print:]]", " ", .) %>% tolower %>% 
      gsub("[^[:lower:]^[:space:]]", " ", .)  %>% 
      gsub("[[:space:]]{1,}", " ", .)  %>% 
      gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", .)   %>%  # Limpiar siglas de dos letras 
      trimws %>% RemoveStopwordsFromText(palabras_eliminar)
    
    l_AnalisisTextoXmpio[[i]][[j]] <- a       
  }
}

# 
# # Ultima secci?n
# for(i in 1:length(vector_mpios)){
#   
#   inic <- gsub("\\n", "\\\\\\\\n", unlist(lista_secciones[[i]])[length(lista_secciones[[i]])])
#   inic <- gsub("\\r", "\\\\\\\\r", inic)
#   inic <- gsub("(", "", inic, fixed = T)
#   inic <- gsub(")", "", inic, fixed = T)
#   inic <- gsub("Vocera", "Vocero", inic)
#   
#   fn <- gsub("\\n", "\\\\\\\\n",  vector_ultimapalabra[i])
#   fn <- gsub("\\r", "\\\\\\\\r", fn)
#   fn <- gsub("(", "", fn, fixed = T)
#   fn <- gsub(")", "", fn, fixed = T)
#   fn <- gsub("Vocera", "Vocero", fn)
#   
#   texto <- listaSinEspaciosConNewLineCarriage[[i]]
#   texto <- gsub("\\n", "\\\\n",  texto)
#   texto <- gsub("\\r", "\\\\r", texto)
#   texto <- gsub("(", "", texto, fixed = T)
#   texto <- gsub(")", "", texto, fixed = T)
#   texto <- gsub("Vocera", "Vocero", texto)
#   
#   a <- f_extraerEntre_Spanish(x = texto, inicio = inic , fin = fn, extremos = F)
# 
#   
#   if(hay_texto_interior == T){
#     a <- f_extraerEntre_Spanish(x = a, inicio = inicio_texto_interior ,
#                                 fin = fin_texto_interior,
#                                 extremos = F)
#   }
#   
#     
#   # QUitar vocero y entidades que dan soluci?n
#   a <- a %>%  gsub("[^[:print:]]", " ", .) %>% tolower %>% 
#     gsub("[^[:lower:]^[:space:]]", " ", .)  %>% 
#     gsub("[[:space:]]{1,}", " ", .)  %>% 
#     gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", .)   %>%  # Limpiar siglas de dos letras 
#     trimws %>% RemoveStopwordsFromText(palabras_eliminar)
#   
#   l_AnalisisTextoXmpio[[i]][[length(lista_secciones[[i]])]] <- a   
#   
# }
  names(l_AnalisisTextoXmpio) <- vector_mpios
  
  for(i in 1:length(vector_mpios)){
    names(l_AnalisisTextoXmpio[[i]]) <- lista_secciones_arregladas[[i]]
  }
  
  return(l_AnalisisTextoXmpio)

}



# Revisar inconsisntencias
# x Debe ser una lista
funcion_incons_longLista <- function(x){
  Long_lista <- unlist(x %>% map(length))
  Long_lista <- Long_lista[Long_lista>=2 | Long_lista == 0]
  Long_lista
}



# l_AnalisisTextoXmpio <- vector("list", length(nombres_mpios)) 
# names(l_AnalisisTextoXmpio) <- nombres_mpios
# 
# # Inicializar el contenido para los textos de las listas
# 
# 
# for(i in 1:length(nombres_mpios)){
#   for(j in 1:length(l_secciones[[i]])){
#     l_AnalisisTextoXmpio[[i]] <-  vector("list", length(l_secciones[[i]]))
#   }
# }
# 
# 
# 
# 
# # names(l_AnalisisTextoXmpio[[i]][[j]]) <- l_secciones_arregladas[[i]][[j]]
# #i = 1; j = 6
# for(i in 1:length(nombres_mpios)){
#   for(j in 1:(length(l_secciones[[i]]) - 1)){
#     
#     inic <- gsub("\\n", "\\\\\\\\n", unlist(l_secciones[[i]])[j])
#     inic <- gsub("\\r", "\\\\\\\\r", inic)
#     inic <- gsub("(", "", inic, fixed = T)
#     inic <- gsub(")", "", inic, fixed = T)
#     
#     
#     
#     fn <- gsub("\\n", "\\\\\\\\n",  unlist(l_secciones[[i]])[j+1])
#     fn <- gsub("\\r", "\\\\\\\\r", fn)
#     fn <- gsub("(", "", fn, fixed = T)
#     fn <- gsub(")", "", fn, fixed = T)
#     
#     
#     texto <- l_talleres2_noespaciosCarrNewl[[i]]
#     texto <- gsub("\\n", "\\\\n",  texto)
#     texto <- gsub("\\r", "\\\\r", texto)
#     texto <- gsub("(", "", texto, fixed = T)
#     texto <- gsub(")", "", texto, fixed = T)
#     
#     
#     a <- f_extraerEntre_Spanish(x = texto, inicio = inic , fin = fn,  extremos = F) %>% 
#       gsub("\\\\r", " ",.) %>%  gsub("\\\\n", " ", .)
#     # QUitar vocero y entidades que dan soluci?n
#     a <- f_extraerEntre_Spanish(x = a, inicio = "Necesidades:" , fin = "Entidades que pueden dar soluci?n:",
#                                 extremos = F)
#     a <- a %>%  gsub("[^[:print:]]", " ", .) %>% tolower %>% 
#       gsub("[^[:lower:]^[:space:]]", " ", .)  %>% 
#       gsub("[[:space:]]{1,}", " ", .)  %>% 
#       gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", .)   %>%  # Limpiar siglas de dos letras 
#       trimws %>% RemoveStopwordsFromText(palabras_eliminar)
#     
#     l_AnalisisTextoXmpio[[i]][[j]] <- a       
#   }
# }
# 
# 
# # Ultima secci?n
# for(i in 1:length(nombres_mpios)){
#   
#   inic <- gsub("\\n", "\\\\\\\\n", unlist(l_secciones[[i]])[length(l_secciones[[i]])])
#   inic <- gsub("\\r", "\\\\\\\\r", inic)
#   
#   fn <- gsub("\\n", "\\\\\\\\n",  unlist(v_ultimapalabra[i]))
#   fn <- gsub("\\r", "\\\\\\\\r", fn)
#   
#   texto <- l_talleres2_noespaciosCarrNewl[[i]]
#   texto <- gsub("\\n", "\\\\n",  texto)
#   texto <- gsub("\\r", "\\\\r", texto)
#   
#   a <- f_extraerEntre(x = texto, inicio = inic , fin = fn, extremos = F)
#   
#   # QUitar vocero y entidades que dan soluci?n
#   a <- f_extraerEntre(x = a, inicio = "Necesidades:" , fin = "Entidades que pueden dar soluci?n:", extremos = F)
#   a <- a %>%  gsub("[^[:print:]]", " ", .) %>% tolower %>% 
#     gsub("[^[:lower:]^[:space:]]", " ", .)  %>% 
#     gsub("[[:space:]]{1,}", " ", .)  %>% 
#     gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", .)   %>%  # Limpiar siglas de dos letras 
#     trimws %>% RemoveStopwordsFromText(palabras_eliminar)
#   
#   l_AnalisisTextoXmpio[[i]][[length(l_secciones[[i]])]] <- a       
# }
# 
# 
# 
# for (i in 1:length(nombres_mpios)){
#   names(l_AnalisisTextoXmpio[[i]]) <- l_secciones_arregladas[[i]]
# }



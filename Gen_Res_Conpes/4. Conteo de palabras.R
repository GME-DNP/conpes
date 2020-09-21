library(readxl)
library(stringr)

######## Directorio de trabajo
setwd("~/Google Drive/Laboral/5.2019/Varios/7.DFL/2. Generacion Ngramas/")
rm(list = ls())

######## Bases de datos
## Objetivos
obj_conpes <- read_excel(path = "Palabras PND y CONPES.xlsx",sheet = 3)
obj_pnd <- read_excel(path = "Palabras PND y CONPES.xlsx",sheet = 4)

## Conpes
Conpes <- dir(path = "Conpes/")

for(k in 1:length(Conpes)){
  info <- read_excel(path = paste("Conpes/",Conpes[k],"/df_ngramas.xlsx",sep = ""))
  info$Palabra <- tolower(x = info$Palabra)
  assign(x = paste("conpes",Conpes[k],sep = ""),value = info); rm(info)
}

## PND
PND <- dir(path = "PNDs/")

for(k in 1:length(Conpes)){
  info <- read_excel(path = paste("PNDs/",PND[k],"/df_ngramas.xlsx",sep = ""))
  info$Palabra <- tolower(x = info$Palabra)
  assign(x = paste("pnd",PND[k],sep = ""),value = info); rm(info)
}

######## Busqueda de las palabras
## Conpes
for(k in 1:nrow(obj_conpes)){
  
  # k <- 1
  word <- obj_conpes[k,1]
  word <- as.character(word)
  
  for(j in 1:length(Conpes)){
    
    # j <- 1
    datos <- get(x = paste("conpes",Conpes[j],sep = ""))
    cruzan <- datos$n[datos$Palabra %in% word]
    obj_conpes[k,j + 2] <- sum(cruzan)
    
    print(paste(k,j,sep = "_"))
  }
}

## PND
for(k in 1:nrow(obj_pnd)){
  
  # k <- 1
  word <- obj_pnd[k,1]
  word <- as.character(word)
  
  for(j in 1:length(Conpes)){
    
    # j <- 1
    datos <- get(x = paste("pnd",PND[j],sep = ""))
    cruzan <- datos$n[datos$Palabra %in% word]
    obj_pnd[k,j + 2] <- sum(cruzan)
    
    print(paste(k,j,sep = "_"))
  }
}

######## Exportado
write.csv(x = obj_conpes,file = "Conteos conpes.csv",row.names = F)
write.csv(x = obj_pnd,file = "Conteos pnd.csv",row.names = F)
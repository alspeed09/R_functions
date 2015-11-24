# --- 
# title: "Split a Character Vector" 
# author: "Alfredo Ascanio [aut] [cre]", "Luis Miguel Montilla [aut]" 
# date: "08/04/2015"
# e-mail: "11-10060@usb.ve"
# --- 
##Descripción: CVsplit es una función cuya función es ser utilizada al requerir
##datos a partir de un archivo de referencias exportado en formato CSV.
##El problema con estos archivos es que son leídos como un dataframe en la cual
##cada observación es un paper y cada uno de estos tiene asociado una serie de 
##variables, cada uno de los cuales contiene el título, los autores, el ISBN, etc.

##CVsplit funciona para extraer los autores de un archivo de CSV dado por
##programas como Zotero o Mendeley. 
##El resultado será una matriz en la cual cada fila es un artículo y 
##cada columna es un autor de dicho artículo.

CVsplit <- function(x) {
        
        ## 'x' es un vector dentro del CSV exportad por manejadores de referencias
        ## dado que estos entienden cada variable como un conjunto de factores,
        ## todos los autores de cada paper están contenidos por las mismas comillas.
        
        y <- list()
        
        for(i in 1:length(x)) {
                coauthor <- as.character(x)  ##Asigna como un vector de caracteres
                                             ##cada fila del vector requerido
                z=strsplit(coauthor[i], split= ";")
                                             ##Luego separa los autores
                y[i] <- z
                                             ##Y los asigna como un elemento de
                                             ##la lista 'y'
        }
        
        vl <- NULL
        for(i in 1:length(y)) {
                vl[i] <- length(y[[i]])
        }
        b <- max(vl)
        
        ## 'vl' es un vector numerico en el cual se representa el
        ## número de elementos dentro de cada vector de la lista 'y'. Y 'b'
        ## representa el número más alto de estos.
        
        
        for(i in 1:length(y)) {
                length(y[[i]]) <- b
        }
        
        ##El paso anterior asigna la misma longitud ('b' o la máxima longitud)
        ##a cada vector de la lista 'y'
        
        z <- do.call("rbind", y)
        
        z
        
        ##Ahora que todos los elementos de la lista 'y' tienen la misma longitud
        ##pueden unirse en una matriz 'z'. Los elementos agregados en los últimos
        ##dos pasos serán colocados como 'NA'.
        
}
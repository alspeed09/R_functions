# --- 
# title: "CExtract" 
# author: "Alfredo Ascanio [cre]" "Mario González-Gil [ctb]"
# date: "08/06/2015"
# e-mail: "11-10060@usb.ve"
# --- 

##Descripción: Esta función tiene el objetivo de comparar dos conjuntos de 
##artículos científicos, en base a sus autores, año de publicación y DOI (si lo
##tiene). De esta manera se pueden hacer búsquedas rápidas de artículos en dos
##listas de referencias separadas sin necesidad de verificar uno a uno su presencia
#en ambas listas.

bibCompare <- function(x, y) {

##Donde 'x' es la matriz de prueba conteniendo los autores del segundo conjunto
##de referencias con su año y DOI correspondiente; y 'y' es la matriz que contiene
##el conjutno de autores, años y DOI de las referencias originales.

        test2 <- c(NULL)
        
        for (i in 1:nrow(x)) {
                for (j in 1:nrow(y)){
                        if (!is.na(x[i, ncol(x)]) == TRUE) {
                                if (!is.na(y[j, ncol(y)]) == TRUE) {
                                        if (x[i, ncol(x)] == y[j, ncol(y)]) {
                                                test2[[i]] = i
                                        }
                                        
                                }
                        } else {
                                v <- suppressWarnings(sum(x[, -c(ncol(x))][i, ] 
                                                          == y[, -c(ncol(y))][j, ], na.rm = T)
                                                      == sum(!is.na(y[, -c(ncol(y))][j, ])))
                                if (v == TRUE){
                                        test2[[i]] = i 
                                        
                                }
                        }
                        
                }
                
        }
        
        test2 <- test2[!is.na(test2)]
        
        test1 <- x[-c(test2), ]
        
        if(is.vector(test1)) {
                test1 <- as.matrix(test1)
                test1 <- t(test1)
        }
        
}
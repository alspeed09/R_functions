 # --- 
 # title: "CExtract" 
 # author: "Alfredo Ascanio [cre]" "Mario González-Gil [ctb]"
 # date: "08/06/2015"
 # e-mail: "11-10060@usb.ve"
 # --- 

##Descripción: Esta función tiene el objetivo de extraer palabras o elementos
##puntuales a partir una matriz de caracteres.

CExtract <- function(x, k) {
        
        ##Donde 'x' es una matriz de palabras y 'k' es un vector de palabras a extraer
        ##de la matriz
        
        y <- list()
        
        for(i in 1:nrow(x)) {
                w <- x[i,] %in% k
                z <- x[i,][w]
                y[[i]] <- z  
        }
        ## 'y' es una lista en la cual cada elemento es un vector que contiene 
        ## aquellas palabras del vector 'k' que estaban contenidas en 'x'
        
        vl <- NULL
        for(i in 1:length(y)) {
                vl[i] <- length(y[[i]])
        }
        e <- max(vl)
        
        ## 'vl' es un vector numerico en el cual cada elemento representa el
        ## número de palabras coincidentes entre la matrix 'x' y el vector 'k'
        ## por fila. 'e' representa el número de elementos coincidentes que
        ## presenta la fila con mayores coincidencias entre 'x' y 'k'
        
        for(i in 1:length(y)) {
                length(y[[i]]) <- e
        }
        
        ## El paso anterior le asigna a cada vector de la lista 'y' la longitud
        ## máxima expresada por 'e'. De esta manera los vectores de 'y' tendrán
        ## la misma longitud. Los elementos añadidos a cada vector serán
        ## colocados como 'NA'
        
        u <- do.call("rbind", y)
        
        t <- rep(1, nrow(u))
        
        for(i in 1:nrow(u)) {
                if(sum(is.na(u[i,]=="NA")) == ncol(u)) {
                        t[i] <- 0
                }
        }
        t <- t == 1
        u[t, ]
        
        ##Los pasos anteriores significan la creación de una matriz 'u' que
        ##contiene los vectores de la lista 'y' unidos como filas de igual
        ##longitud. La creación del vector lógico 't' se realiza para que el
        ##resultado u 'output' de la función sea una matriz que solo tome en
        ##cuenta aquellas filas que tuvieron elementos coincidentes. Por tanto,
        ##se eliminan las filas que no tuvieron coincidencia.
}


##Ejemplo

c1 <- c(letters)
x <- rbind(c1[1:11], c1[5:15], c1[10:20], c1[15:25])
k <- c("a", "b", "g")

CExtract(x, k)

# --- 
# title: "adj_to_edgelist" 
# author: "Alfredo Ascanio [cre]" 
# date: "09/12/2015"
# e-mail: "11-10060@usb.ve"
# --- 

##Descripción: 'adj_to_edgelist' permite generar una matriz de enlaces o 'edgelist'
##en base a una matriz de adyacencia. La matriz de adyacencia debe tener bien
##colocados los nombres de filas y columnas (row.names y col.names).
##La función genera una matriz de tres columnas que contiene todas los enlaces 
##posibles dados por la matriz de adyacencia, con el peso de dichos enlaces.
##En caso de que el enlace no exista y por tanto sea un 'NA', elimina dichas filas.


adj_to_edgelist <- function(x) {
        
## 'x' debe ser una matriz de adyacencia, donde cada fila y columna debe tener
## su identificacion o nombre particular.
        
        y <- list()
        
        for (i in 1:nrow(x)) { 
                w <- cbind(1:ncol(x), 1:ncol(x))
                w[, 1] <- rep(dimnames(x)[[1]][i], ncol(x))
                w[, 2] <- dimnames(x)[[2]]
                w <- as.data.frame(w)
                for (j in 1:ncol(x)) { w[j, 3] <- x[i, j] }
                y[[i]] <- w
        }
        
## Cada elemento de la lista 'y' es una matriz 'w' que tiene 3 columnas
## todas con tantas filas como columnas tenga la matriz 'x'. La primera 
## columna contiene repeticiones del nombre de la fila 'i', la segunda
## contiene los nombres de todas las columnas y la tercera los valores
## de dichos enlaces.
        
        z <- do.call("rbind", y)
        
## 'z' es una matriz que contiene todos los enlaces posibles. Es la unión
## de todos los elementos de la lista 'y' por filas.

        logicz <- !is.na(z[, 3])
        z1 <- z[logicz,]
        z1

## 'z1' es la matriz que no contiene NA's, dado que se hizo el subconjunto
## con el vector logico 'logicz' para saber cuales filas NO contienen NA's.
        
}
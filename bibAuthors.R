# --- 
# title: "bibAuthors" 
# author: "Alfredo Ascanio [cre]" 
# date: "08/06/2015"
# e-mail: "11-10060@usb.ve"
# --- 

##Descripción: bibAuthors es una función que permite hallar una matriz de autores
##a partir de un archivo '.bib' exportado desde un manejador de referencias.
##Cada fila de la matriz es un artículo y cada columna es un autor.

bibAuthors <- function(w) {
        
        ## 'w' es el nombre del archivo .bib sin su extensión y entre comillas
        ## por ejemplo "archivobib". Este debe encontrarse en el directorio
        ## de trabajo
        
        library(bibtex)
        w <- w
        file1 <- paste(w, ".bib", sep = "")
        x <- read.bib(file =  file1, encoding = "UTF-8")
        y <- x$author ## x es un objeto bibentry
        a <- list()
        b <- as.character(c())
        
        for(i in 1:length(y)) {
        
                for(j in 1:length(y[[i]])) {
        b[j] <- c(paste(c(y[[i]][[j]]$family, y[[i]][[j]]$given), collapse=", "))
        
                }
        b <- b[1:length(y[[i]])]
        a[[i]] <- b
        }
        
        ## 'b' es el vector de caracteres que contiene cada autor del artículo
        ## considerando el apellido y el nombre del mismo, separados por un ", ".
        ## Cada artículo tendra un vector asociado, y estos estarán dentro de la
        ##lista 'a'
        
        vl <- NULL
        for(i in 1:length(a)) {
                vl[i] <- length(a[[i]])
        }
        e <- max(vl)
        
        for(i in 1:length(a)) {
                length(a[[i]]) <- e
        }
        
        ##Luego, 'vl' será un vector que contendrá el número de autores por
        ##por artículo. 'e' será el número más alto de autores entre todos los
        ##artículos. Luego, se le asignará a cada vector de autores dicha longitud

        z <- do.call("rbind", a)
        
        z
        
        ##Por último, 'z' es una matriz de autores, donde cada fila es un artículo
        ##y cada columna es un autor de los mismos. Esto se logró gracias al
        ##paso anterior, que hace que todos los vectores tengan la misma longitud.
        ##Aquellos que tienen menos autores que el máximo, serán completados con
        ## 'NA's' hasta alcanzar la longitud máxima.
        
}

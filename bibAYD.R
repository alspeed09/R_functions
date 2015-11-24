# --- 
# title: "bibAuthors" 
# author: "Alfredo Ascanio [cre]" 
# date: "08/06/2015"
# e-mail: "11-10060@usb.ve"
# --- 

##Descripción: Esta función fue realizada para ser usada en conjunto con la
##función 'bibCompare'. En general, es una extensión de la función 'bibAuthors'
##que agrega a cada fila el año y doi del artículo correspondiente.

bibAYD <- function(x) {

##Donde 'x' es el nombre del archivo .bib que contiene las referencias
##de los artículos correspondientes
        
        require(bibtex)
        source("bibAuthors.R")
        file1 <- paste(x, ".bib", sep = "")
        authors <- read.bib(file =  file1, encoding = "UTF-8")
        mat <- bibAuthors(x)
        
        doi <- authors$doi
        for (i in 1:length(doi)) {
                
                if (is.null(doi[[i]]) == TRUE) {
                        doi[[i]] <- NA
                }
                
                
        }
        
        mat1 <- cbind(mat, year = do.call("rbind", authors$year), doi = unlist(doi))
        mat1
}
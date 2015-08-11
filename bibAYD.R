bibAYD <- function(x) {

##Where 'x' is an object of class 'bibentry' which contains all the metadata
##of the references stored in the bib file
        
        source("bibAuthors.R")
        mat <- bibAuthors(authors)
        
        doi <- authors$doi
        for (i in 1:length(doi)) {
                
                if (is.null(doi[[i]]) == TRUE) {
                        doi[[i]] <- NA
                }
                
                
        }
        
        mat1 <- cbind(mat, year = do.call("rbind", authors$year), doi = unlist(doi))
        mat1
}
# --- 
# title: "bibAuthors" 
# author: "Alfredo Ascanio [cre]" 
# date: "08/06/2015"
# e-mail: "11-10060@usb.ve"
# --- 

bibAuthors <- function(x) {
        
        y <- x$author
        a <- list()
        b <- as.character(c())
        
        for(i in 1:length(y)) {
        
                for(j in 1:length(y[[i]])) {
        b[j] <- c(paste(c(y[[i]][[j]]$family, y[[i]][[j]]$given), collapse=", "))
        
                }
        b <- b[1:length(y[[i]])]
        a[[i]] <- b
        }
        
        vl <- NULL
        for(i in 1:length(a)) {
                vl[i] <- length(a[[i]])
        }
        e <- max(vl)
        
        for(i in 1:length(a)) {
                length(a[[i]]) <- e
        }
        
        z <- do.call("rbind", a)
        
        z
        
        
}

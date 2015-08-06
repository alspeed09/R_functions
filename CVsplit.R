# --- 
# title: "Split a Character Vector" 
# author: "Alfredo Ascanio [aut] [cre]", "Luis Miguel Montilla [aut]" 
# date: "08/04/2015"
# e-mail: "11-10060@usb.ve"
# --- 

CVsplit <- function(x) {

        y <- list()
        
        for(i in 1:length(x)) {
                coauthor <- as.character(x)
                z=strsplit(coauthor[i], split= ";")
                y[i] <- z
                
        }
        
        vl <- NULL
        for(i in 1:length(y)) {
                vl[i] <- length(y[[i]])
        }
        b <- max(vl)
        
        for(i in 1:length(y)) {
                length(y[[i]]) <- b
        }
        
        z <- do.call("rbind", y)
        
        z
        
}

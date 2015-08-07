# --- 
# title: "cmbDFrame" 
# author: "Emanuel Valero [cre]", Alfredo Ascanio [cbt]" 
# date: "08/04/2015"
# e-mail: ""
# --- 

cmbDFrame <- function(x) {
        c <- x; m <- 2
        
        oput <- list()
        for(i in 1:nrow(c)) {
                out <- combn(as.matrix(c[i,]), m)
                oput[[i]] <- t(out)
        }
        
        for(i in 1:length(oput)) {
                
                if(sum(!is.na(oput[[i]][, 2])) == 0) {
                        oput[[i]] <- head(oput[[i]], 1)
                        oput[[i]][is.na(oput[[i]])] <- ""
                }
        }
        
        mrg <- do.call("rbind", oput)
        mrgC <- mrg[complete.cases(mrg),]
        
        op <- list(
                rawCombs = oput, mrgCombs = mrg, mrgCCombs = mrgC
        )
        
        return(op)
}

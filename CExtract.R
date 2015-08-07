 # --- 
 # title: "bibAuthors" 
 # author: "Alfredo Ascanio [cre]" "Mario González-Gil [ctb]"
 # date: "08/06/2015"
 # e-mail: "11-10060@usb.ve"
 # --- 

CExtract <- function(x, k) {
        
        ##Donde 'x' es una matriz de palabras y 'k' es un vector de palabras a extraer
        ##de la matriz
        
        y <- list()
        
        for(i in 1:nrow(a)) {
                w <- x[i,] %in% k
                z <- x[i,][w]
                y[[i]] <- z
        }
        
        vl <- NULL
        for(i in 1:length(y)) {
                vl[i] <- length(y[[i]])
        }
        e <- max(vl)
        
        for(i in 1:length(y)) {
                length(y[[i]]) <- e
        }
        
        u <- do.call("rbind", y)
        
        t <- rep(1, nrow(u))
        
        for(i in 1:nrow(u)) {
                if(sum(is.na(u[i,]=="NA")) == ncol(u)) {
                        t[i] <- 0
                }
        }
        t <- t == 1
        u[t, ]
        
        
}


##Example

c1 <- c(letters)
x <- rbind(c1[1:11], c1[5:15], c1[10:20], c1[15:25])
k <- c("a", "b", "g")

CExtract(x, k)

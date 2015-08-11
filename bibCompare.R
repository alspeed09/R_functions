bibCompare <- function(x, y) {

##Where 'x' is the test matrix containing the authors of the second set of references 
##with their corresponding year and doi; and 'y' is a matrix which contains
##the set of authors, years and doi from the original references.

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
        
        test1 <- test[-c(test2), ]
        
        if(is.vector(test1)) {
                test1 <- as.matrix(test1, nrow = 1)
        }
        
        test3 <- test1[, -c(ncol(test1), ncol(test1)-1)]
        
        test3
}
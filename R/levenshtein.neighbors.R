levenshtein.neighbors <-
function(source, targets){
    results<-list()
    distances<-levenshtein.distance(source, targets)
    for (distance in min(distances):max(distances)){
        results[distance]=list(names(which(distances==distance)))
    }
    return(results)
}


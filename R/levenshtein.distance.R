levenshtein.distance <-
function(source, targets){
    distances<-levenshteinDist(source, targets)
    names(distances)<-targets
    return(distances)
}


hamming.distance <-
function(source, targets){
    if (class(targets)!='preprocessed.targets'){
        targets<-tpreprocess(targets)
    }
    indexes<-which(targets$n.chars==nchar(source))
    source<-unlist(strsplit(as.character(source),c()))
    distances<-unlist(lapply(targets$spelling[indexes],function(target) return(sum(source!=target))))
    names(distances)<-targets$names[indexes]
    return(distances)
}


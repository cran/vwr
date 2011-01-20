coltheart.N <-
function(sources, targets, multicore=FALSE, distance=1, method='Hamming'){
    if(typeof(sources)!='character'){
        sources<-as.character(sources)
    }

    if(method=='Levenshtein'){
        do.one<-function(source, targets) return(sum(levenshtein.distance(source, targets)==distance))
    }
    else{
        targets<-tpreprocess(targets)
        do.one<-function(source, targets) return(sum(hamming.distance(source,targets)==distance))
    }
    if (multicore==TRUE){
        results<-unlist(mclapply(sources,do.one,targets))
        names(results)<-sources
    }
    else{
        results<-unlist(lapply(sources,do.one,targets))
        names(results)<-sources
    }
    return(results)
}


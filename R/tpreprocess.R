tpreprocess <-
function(targets){
        targets<-list(
        'names'=targets,
        'spelling'=lapply(as.character(targets),function(s) unlist(strsplit(s,c()))),
        'n.chars'=lapply(as.character(targets),nchar))
        class(targets)<-'preprocessed.targets'
        return(targets)
}


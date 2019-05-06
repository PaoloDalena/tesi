lemmaText <- function(x = NULL)
  {if(is.null(x)){return()}
  {pb <- txtProgressBar(min = 0,max = length(x),style=3)
    out <- character()
    for(j in 1:length(x)){
      setTxtProgressBar(pb,j)
      tx <- x[j]
      vtx <- strsplit(tx, ' ')[[1]]
      vtx <- vtx[vtx!=""]
      ltx <- character()
      for(i in 1:length(vtx)){
        frm <- vtx[i]
        ric <- dizTot[dizTot$Forma2==frm,c("Lemma2","Cat_Gramm.L")]
        if(dim(ric)[1]>0){
          ltx[i] <- ric[1,"Lemma2"]
        } else {
          ltx[i] <- frm
        }
      }
      out[j] <- paste(ltx, collapse = " ")
    }
    close(pb)}
  return(out)
}

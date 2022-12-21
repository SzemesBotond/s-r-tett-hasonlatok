library(dplyr)
library(tidytext)

# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

#fn stop
stop_gyanfn <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/gyanant_fnstop.txt",
                    what="character", sep="\f", quote = "", encoding = "UTF-8")
stop_gy <- paste(stop_gyanfn, collapse = "|")
stop_gyan <- data.frame(stop_gyanfn)


#ige stop
stop_gyanige <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/gyanant_igestop.txt",
                    what="character", sep="\f", quote = "", encoding = "UTF-8")
stop_gyi <- paste(stop_gyanige, collapse = "|")
stop_gyani <- data.frame(stop_gyanige)

# "gyanánt" és stopszavak pozíciója
gyanpoz <- list()
gyanpoz1 <- list()
gyanstop_poz <- list()
gyanistop_poz <- list()
gyan_kontext <- list()
for (i in 1:length(my.corpus.l)){
  gyanpoz[[i]] <- grep("gyanánt", my.corpus.l[[i]]$V2) 
  gyanpoz[[i]] <- gyanpoz[[i]]-1 
  gyanstop_poz[[i]] <- grep(stop_gy, my.corpus.l[[i]]$V2)
  gyanpoz1[[i]] <- gyanpoz[[i]][!gyanpoz[[i]]%in%gyanstop_poz[[i]]]
  gyanistop_poz[[i]] <-grep(stop_gyi, my.corpus.l[[i]]$V2)
}



# főnév stopszűrés utáni kontextusa a gyanánt szavaknak

# a kontextusok pozíciói
r2 <- list()
r1 <- list()
for(i in 1:length(gyanpoz)){
  r1[[i]]<-gyanpoz1[[i]]-2
  r2[[i]]<-gyanpoz1[[i]]+3
}

#a kontextus
gyan_kontext <- gyanpoz1 
for (i in 1:length(my.corpus.l)){
  gyan_kontext[[i]] <- list(gyanpoz1[[i]])
  for(j in 1:length(gyanpoz1[[i]])){
    if (length(gyanpoz1[[i]]) >0){
    gyan_kontext[[i]][[j]] <- r1[[i]][[j]]:r2[[i]][[j]]
    }
  else{
    gyan_kontext[[i]] <- 0  
  }
}}

# a kiszűrése annak, ha a kontextusban szerepel olyan ige, ami stopszó 
gyan_kontext1 <- gyan_kontext
for (i in 1:length(my.corpus.l)){
  for(j in 1:length(gyan_kontext1[[i]])) {
    if (length(intersect(gyan_kontext1[[i]][[j]],gyanistop_poz[[i]])) >0)
      {
      gyan_kontext1[[i]][[j]] <- 0
    }
  }
}

# összes példány összeszámolása
gyan_ossz <- gyan_kontext1
gyan_osszes <- list()
for(i in 1:length(gyan_kontext1)){
  for(j in 1:length(gyan_kontext1[[i]])){
    gyan_ossz[[i]][[j]]  <- length(gyan_kontext1[[i]][[j]])
  }
  gyan_osszes[[i]] <- length(gyan_ossz[[i]][which(gyan_ossz[[i]] > 1)])
}

unlist(gyan_osszes)


###
## minta hozása kézi elemzéshez
gyan_kontext2 <- gyan_kontext1
for (i in 1:length(my.corpus.l)){
  for(j in 1:length(gyan_kontext1[[i]])) {
    if (class(gyan_kontext1[[i]][[j]]) == "integer"){
      gyan_kontext2 [[i]][[j]] <-(gyan_kontext1[[i]][[j]][3]-10):(gyan_kontext1[[i]][[j]][3]+10) 
    }
  }
}

szavak <- list()
for(i in 1:length(my.corpus.l)){
  szavak [[i]] <- my.corpus.l[[i]]
}
gyan_szurt <- szavak
for(i in 1:length(my.corpus.l)){
  gyan_szurt[[i]] <- list(szavak[[i]])
  for(j in 1:length(gyan_kontext2[[i]])){
    if (length(gyan_kontext2[[i]][[j]]) >0){
      gyan_szurt[[i]][[j]] <- szavak[[i]][gyan_kontext2[[i]][[j]],1]}
    else{
      gyan_szurt[[i]] <- 0  
    }
  }}


gyan_minta <- unlist(gyan_minta, recursive = F)
gyan_minta <- gyan_minta[lapply(gyan_minta,length)>0]

gyan_minta1   <- gyan_szurt
for(i in 1:length(gyan_szurt)){
  for(j in 1:length(gyan_szurt[[i]])){
    if(class(gyan_szurt[[i]])== "list"){    
      gyan_minta1[[i]][[j]] <- paste(unlist(gyan_szurt[[i]][j]), collapse = " " )}
  }}

gyan_minta2 <- sample(unlist(gyan_minta1), size = 200, replace = F)
capture.output(gyan_minta2,file=paste("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/gyanant_minta_stop_uj.txt"))

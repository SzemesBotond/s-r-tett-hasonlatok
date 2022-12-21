library(dplyr)
library(tidytext)

# magyarlnc által elemzett szövegek
input.dir <- "C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/regenyek_magyarlanc"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}


#stopszavak
stop.kent <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/kent_stop.txt",
                  what="character", sep="\f", quote = "", encoding = "UTF-8")
stop_kent <- data.frame(stop.kent)


#stopszavak kiszűrése 
kent_stop <- list()
my.corpus.l1 <- list()
for (i in 1:length(my.corpus.l)) {
  my.corpus.l1[[i]] <- my.corpus.l[[i]]%>%
    unnest_tokens(szavak, V1)}
for (i in 1:length(my.corpus.l1)) {
  kent_stop [[i]] <- anti_join(my.corpus.l1 [[i]], stop_kent, by = c("szavak"="stop.kent"))
}


#ként ragozott főnvecek pozíciója
kent <- list()
kent2 <- list()
kent3 <- list()
kent_all <- list()
fonevkent <- list()
fn <- list()
for (i in 1:length(my.corpus.l)){
kent [[i]] <- grep("[a-zíéáűúőóüö](ként)$", kent_stop[[i]]$szavak, ignore.case=T)
kent2 [[i]] <- grep("[a-zíéáűúőóüö](nként)$", kent_stop[[i]]$szavak, ignore.case=T)
kent3[[i]] <- kent[[i]][!kent[[i]]%in%kent2[[i]]]
fn [[i]] <- grep("NOUN", kent_stop[[i]]$V3, ignore.case = T)
fonevkent[[i]] <- intersect(kent3 [[i]], fn[[i]])
}

unlist(lapply(fonevkent, length))

####
# minták kézi elemzésre

r2 <- list()
r1 <- list()
for(i in 1:length(my.corpus.l)){
  r1[[i]]<-fonevkent[[i]]-10
  r2[[i]]<-fonevkent[[i]]+10
}
# csak szavak a korpuszból (csak az első oszlopok), majd a kontextus által jelölt távolság
szavak <- list()
for(i in 1:length(my.corpus.l)){
  szavak [[i]] <- kent_stop[[i]]$szavak
}
kent_minta <- szavak
for(i in 1:length(szavak)){
  kent_minta[[i]] <- list(szavak[[i]])
  for(j in 1:length(r1[[i]])){
    if (length(r1[[i]]) >0){
      kent_minta[[i]][[j]] <- szavak[[i]][r1[[i]][[j]]:r2[[i]][[j]]]}
    else{
      kent_minta[[i]] <- 0  
    }
  }}

# a szavak listáját mondatokká  alakítani és unlistelni
kent_minta1   <- kent_minta
for(i in 1:length(kent_minta)){
  for(j in 1:length(kent_minta[[i]])){
    if(class(kent_minta[[i]])== "list"){    
      kent_minta1[[i]][[j]] <- paste(unlist(kent_minta[[i]][j]), collapse = " " )}
  }}

kent_mint2 <- sample(unlist(kent_minta1), size = 200, replace = F)

#minták kiírása
library(stringr)
for (i in 1:length(files.v)) {
  files.v[[i]] <- str_remove_all(files.v[[i]], ".txt")
  files.v[[i]] <- str_remove_all(files.v[[i]], "out_")
}
names(kent_mint2) <- files.v
capture.output(kent_minta1,file=paste("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/kent_osszes_0414.txt"))
capture.output(kent_mint2,file=paste("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/kent_minta_200stop_0414.txt"))







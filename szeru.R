library(dplyr)
# magyarálnc által elemzett szövegek

input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:100){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

#stopszavak és pozíciói
stop.szeru <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/szeru_stop.txt",
                  what="character", sep="\f", quote = "", encoding = "UTF-8")

stop_sz <- paste(stop.szeru, collapse = "|") #csak a scan után, nem dataframe
szeru_hely <- list()
for (i in 1:length(my.corpus.l)){
  szeru_hely [[i]] <- grep(stop_sz, my.corpus.l[[i]]$V1, ignore.case=T)
}

# szerű végződésű szavak
szeru_1 <- list()
szeru_all <- list()
szeru_nostop <- list()
szeru_szuk <- list()
for (i in 1:length(my.corpus.l)){
  szeru_1 [[i]] <- grep("[a-zíéáűúőóüö](szerű)$", my.corpus.l[[i]]$V1, ignore.case=T)
  szeru_all[[i]] <- my.corpus.l[[i]][szeru_1[[i]],]
  szeru_nostop [[i]] <- setdiff(szeru_1[[i]], szeru_hely[[i]])
}

unlist(lapply(szeru_nostop, length))

###

#mintahozás, először az összeből

r2 <- list()
r1 <- list()
for(i in 1:length(my.corpus.l)){
  r1[[i]]<-szeru_nostop[[i]]-10
  r2[[i]]<-szeru_nostop[[i]]+10
}
# csak szavak a korpuszból (csak az első oszlopok), majd a kontextus által jelölt távolság
szavak <- list()
for(i in 1:100){
  szavak [[i]] <- my.corpus.l[[i]]$V1
}
szeru_minta <- szavak
for(i in 1:length(szavak)){
  szeru_minta[[i]] <- list(szavak[[i]])
  for(j in 1:length(r1[[i]])){
    if (length(r1[[i]]) >0){
      szeru_minta[[i]][[j]] <- szavak[[i]][r1[[i]][[j]]:r2[[i]][[j]]]}
    else{
      szeru_minta[[i]] <- 0  
    }
  }}

# a szavak listáját mondatokká  alakítani és unlistelni: itt lehet összeszámolni, h melyikben mennyi van
szeru_minta1   <- szeru_minta
for(i in 1:length(szeru_minta)){
  for(j in 1:length(szeru_minta[[i]])){
    if(class(szeru_minta[[i]])== "list"){    
      szeru_minta1[[i]][[j]] <- paste(unlist(szeru_minta[[i]][j]), collapse = " " )}
  }}

szeru_minta2 <- sample(unlist(szeru_minta1), size = 200, replace = F)

library(stringr)
for (i in 1:length(files.v)) {
  files.v[[i]] <- str_remove_all(files.v[[i]], ".txt")
  files.v[[i]] <- str_remove_all(files.v[[i]], "out_")
}
names(szeru_minta2) <- files.v

capture.output(szeru_minta2,file=paste("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/kent_szeru/szeru_0413.txt"))

library(dplyr)
library(tidytext)

# magyarlnc által elemzett szövegek
input.dir <- "YOUR_DIR"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}


#stopszavak
stop.kent <- scan("YOUR_DIR/kent_stop.txt",
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






intersect_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(intersect,lapply(list(x, y),"[[",i)))
}

union_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(union,lapply(list(x, y),"[[",i)))
}

grep_list_df <- function (a, x, y){
  lapply(a, 
         function(df) grep(x, df[,y], ignore.case = T))
}

prev_pos <- function (a){
  lapply(a, function(x) x-1)
}

next_pos <- function (a,b){
  lapply(a, function(x) x+b)
}

my.corpus.l1 <- lapply(my.corpus.l, unnest_tokens, szavak, V1)
kent_stop <- lapply(my.corpus.l1, anti_join, stop_kent, by = c("szavak" = "stop.kent"))




#ként ragozott főnvecek pozíciója
kent <- grep_list_df(kent_stop,"[a-zíéáűúőóüö](ként)$", 4 )
kent2 <- grep_list_df(kent_stop,"[a-zíéáűúőóüö](nként)$", 4 )
kent3 <- lapply(seq(length(my.corpus.l)),
                function(i) Reduce(setdiff,lapply(list(kent, kent2),"[[",i)))
fn <- grep_list_df(kent_spot, "NOUN", 3)
fonevkent <- intersect_lists(kent3, fn)

unlist(lapply(fonevkent, length))

#minta
# a kontextus által jelölt távolságban a szavak sofonevkentata
szavak <- lapply(my.corpus.l, "[", , "V1")

kent_prev8 <- Map(`[`, szavak, prev_pos(fonevkent,8) )
kent_prev7 <- Map(`[`, szavak, prev_pos(fonevkent,7) )
kent_prev6 <- Map(`[`, szavak, prev_pos(fonevkent,6) )
kent_prev5 <- Map(`[`, szavak, prev_pos(fonevkent,5) )
kent_prev4 <- Map(`[`, szavak, prev_pos(fonevkent,4) )
kent_prev3 <- Map(`[`, szavak, prev_pos(fonevkent,3) )
kent_prev2 <- Map(`[`, szavak, prev_pos(fonevkent,2) )
kent_prev1 <- Map(`[`, szavak, prev_pos(fonevkent,1) )
kent <- Map(`[`, szavak, fonevkent )
kent_next1 <- Map(`[`, szavak, next_pos(fonevkent,1) )
kent_next2 <- Map(`[`, szavak, next_pos(fonevkent,2) )
kent_next3 <- Map(`[`, szavak, next_pos(fonevkent,3) )
kent_next4 <- Map(`[`, szavak, next_pos(fonevkent,4) )
kent_next5 <- Map(`[`, szavak, next_pos(fonevkent,5) )
kent_next6 <- Map(`[`, szavak, next_pos(fonevkent,6) )
kent_next7 <- Map(`[`, szavak, next_pos(fonevkent,7) )
kent_next8 <- Map(`[`, szavak, next_pos(fonevkent,8) )


kent_df <- lapply(seq(length(fonevkent)),
                  function(j)
                    lapply(seq(length(fonevkent[[j]])),
                           function (i) 
                             if (length(fonevkent[[j]]) >0)
                               data.frame( #nem tibble az egyszerűbb kiíráshoz
                                 kent_prev8 = kent_prev8[[j]][[i]],
                                 kent_prev7 = kent_prev7[[j]][[i]],
                                 kent_prev6 = kent_prev6[[j]][[i]],
                                 kent_prev5 = kent_prev5[[j]][[i]],
                                 kent_prev4 = kent_prev4[[j]][[i]],
                                 kent_prev3 = kent_prev3[[j]][[i]],
                                 kent_prev2 = kent_prev2[[j]][[i]],
                                 kent_prev1 = kent_prev1[[j]][[i]],
                                 kent = kent[[j]][[i]],
                                 kent_next1 = kent_next1[[j]][[i]],
                                 kent_next2 = kent_next2[[j]][[i]],
                                 kent_next3 = kent_next3[[j]][[i]],
                                 kent_next4 = kent_next4[[j]][[i]],
                                 kent_next5 = kent_next5[[j]][[i]],
                                 kent_next6 = kent_next6[[j]][[i]],
                                 kent_next7 = kent_next7[[j]][[i]],
                                 kent_next8 = kent_next8[[j]][[i]]
                               )
                           else 
                             0
                    ))

for(i in seq(my.corpus.l)){
  kent_df [[i]] <- do.call("rbind", kent_df[[i]])
}



#a fájlok nevéből a szerzőt és címet megtartani
library(stringr)
files.v <- lapply(files.v, str_remove_all, "(.txt|out_)")
names(kent_df) <- files.v
options(dplyr.print_max = 100)
options(width=10000)
capture.output(kent_df, file="eredmeny.txt")

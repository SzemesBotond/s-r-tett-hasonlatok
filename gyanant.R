# magyarlnc által elemzett szövegek
input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files.v)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

## használt függvények
intersect_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(intersect,lapply(list(x, y),"[[",i)))
}

grep_list_df <- function (x, y){
  lapply(my.corpus.l, 
         function(df) grep(x, df[,y], ignore.case = T))
}

prev_pos <- function (a,b){
  lapply(a, function(x) x-b)
}

next_pos <- function (a,b){
  lapply(a, function(x) x+b)
}

library(dplyr)
library(tidytext)


#fn stop
stop_gyanfn <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/gyanant_fnstop.txt",
                    what="character", sep="\f", quote = "", encoding = "UTF-8")
stop_gy <- paste(stop_gyanfn, collapse = "|")

#ige stop
stop_gyanige <- scan("C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/hasonlat/gyanant_igestop.txt",
                     what="character", sep="\f", quote = "", encoding = "UTF-8")
stop_gyi <- paste(stop_gyanige, collapse = "|")

# "gyanánt" előtt főnév és stopszavak pozíciója
gyanpoz <- prev_pos(grep_list_df("gyanánt", 2),1)
gyanstoppoz <- grep_list_df(stop_gy, 2)
igestoppoz <- grep_list_df(stop_gyi, 2)
#főnév stopszavak szűrése
gyanpoz1 <- lapply(seq(length(gyanpoz)),
                   function(i) Reduce(setdiff,lapply(list(gyanpoz, gyanstoppoz),"[[",i)))
#kontextus, amiben igei stopszavak előfordulhatnak
gyanpoz_prev10 <- prev_pos(gyanpoz1,10)
gyanpoz_next10 <- next_pos(gyanpoz1,10)
start_end_df <- lapply(seq(length(my.corpus.l)),
                       function(i) tibble(gyanpoz_next10[[i]], gyanpoz_prev10[[i]]))
start_end_df <- lapply(start_end_df, setNames, c("start", "end"))
start_end <- lapply(start_end_df, with, Map(`:`, start, end))
#igei stopszavak szűrése
igestop_match <- lapply(seq(length(my.corpus.l)), function (i)
  lapply(r[[i]], intersect, igestoppoz[[i]]))
igestop_match <- lapply(seq(length(my.corpus.l)), function (i)
  lapply(igestop_match[[i]], '>', 0))
gyanpoz_szurt <- lapply(seq(length(my.corpus.l)), function (i)
  which(lapply(igestop_match[[i]], '>', 0) != "TRUE"))
gyanpoz_szurt <- lapply(seq(length(my.corpus.l)), function (i)
  gyanpoz[[i]][gyanpoz_szurt[[i]]])

szavak <- lapply(my.corpus.l, "[", , "V1")

#szavak a szűrés után + kontextus
fn_gyan_prev2 <- Map(`[`, szavak, prev_pos(gyanpoz_szurt,2) )
fn_gyan_pev1 <- Map(`[`, szavak, prev_pos(gyanpoz_szurt,1) )
fn_gyan <- Map(`[`, szavak, gyanpoz_szurt )
gyan <- Map(`[`, szavak, next_pos(gyanpoz_szurt,1) )
gyan_next1 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,2) )
gyan_next2 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,3) )
gyan_next3 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,4) )
gyan_next4 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,5) )
gyan_next5 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,6) )
gyan_next6 <- Map(`[`, szavak, next_pos(gyanpoz_szurt,7) )

# gyanánt + kontextus
fn_gyan_df <- lapply(seq(length(fn_gyan)),
                     function(j)
                       lapply(seq(length(fn_gyan[[j]])),
                              function (i) 
                                if(length(fn_gyan[[j]]>0))
                                  tibble(
                                fn_gyan_prev2 = fn_gyan_prev2[[j]][[i]],
                                fn_gyan_prev1 = fn_gyan_prev1[[j]][[i]],
                                fn_gyan = fn_gyan [[j]][[i]],
                                gyanánt = gyan[[j]] [[i]],
                                gyan_next1 = gyan_next1[[j]][[i]],
                                gyan_next2 = gyan_next2[[j]][[i]],
                                gyan_next3 = gyan_next3[[j]][[i]],
                                gyan_next4 = gyan_next4[[j]][[i]],
                                gyan_next5 = gyan_next5[[j]][[i]],
                                gyan_next6 = gyan_next6[[j]][[i]]
                              )
                              else
                                0))

for(i in seq(my.corpus.l)){
  fn_gyan_df [[i]] <- do.call("rbind", fn_gyan_df[[i]])
}
fn_gyan_df <- lapply(fn_gyan_df, setNames, as.character(c(1:10)))

#eredmények kiírása
files.v <- lapply(files.v, str_remove_all, "(.txt|out_)")
names(fn_gyan_df) <- files.v
options(dplyr.print_max = 1e9)
capture.output(fn_gyan_df, file="proba1.txt")


# összes példány összeszámolása
osszgyan <- sapply(fn_gyan_df, nrow)

library(dplyr)
## használt függvények
intersect_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(intersect,lapply(list(x, y),"[[",i)))
}

union_lists <- function (x,y) {
  lapply(seq(length(my.corpus.l)),
         function(i) Reduce(union,lapply(list(x, y),"[[",i)))
}

grep_list_df <- function (x, y){
  lapply(my.corpus.l, 
         function(df) grep(x, df[,y], ignore.case = T))
}

prev_pos <- function (a){
  lapply(a, function(x) x-1)
}

next_pos <- function (a,b){
  lapply(a, function(x) x+b)
}

# magyarálnc által elemzett szövegek

input.dir <- "YOUR DIR"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in seq(lenght(my.corpus.l))){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

#stopszavak és pozíciói
stop.szeru <- scan("YOUR_DIR/szeru_stop.txt",
                   what="character", sep="\f", quote = "", encoding = "UTF-8")

stop_sz <- paste(stop.szeru, collapse = "|") #csak a scan után, nem dataframe
szeru_hely <- grep_list_df(stop_sz, 1)

# szerű végződésű szavak
szeru_1 <- grep_list_df("[a-zíéáűúőóüö](szerű)$", 1)
szeru_all <- lapply(my.corpus.l, "[", szeru_1, )
szeru_nostop <- lapply(seq(length(my.corpus.l)),
                       function(i) Reduce(setdiff,lapply(list(szeru_1, szeru_hely),"[[",i)))

unlist(lapply(szeru_nostop, length))

###

#mintahozás, először az összeből

szeru_prev8 <- Map(`[`, szavak, prev_pos(szeru_nostop,8) )
szeru_prev7 <- Map(`[`, szavak, prev_pos(szeru_nostop,7) )
szeru_prev6 <- Map(`[`, szavak, prev_pos(szeru_nostop,6) )
szeru_prev5 <- Map(`[`, szavak, prev_pos(szeru_nostop,5) )
szeru_prev4 <- Map(`[`, szavak, prev_pos(szeru_nostop,4) )
szeru_prev3 <- Map(`[`, szavak, prev_pos(szeru_nostop,3) )
szeru_prev2 <- Map(`[`, szavak, prev_pos(szeru_nostop,2) )
szeru_prev1 <- Map(`[`, szavak, prev_pos(szeru_nostop,1) )
szeru <- Map(`[`, szavak, szeru_nostop )
szeru_next1 <- Map(`[`, szavak, next_pos(szeru_nostop,1) )
szeru_next2 <- Map(`[`, szavak, next_pos(szeru_nostop,2) )
szeru_next3 <- Map(`[`, szavak, next_pos(szeru_nostop,3) )
szeru_next4 <- Map(`[`, szavak, next_pos(szeru_nostop,4) )
szeru_next5 <- Map(`[`, szavak, next_pos(szeru_nostop,5) )
szeru_next6 <- Map(`[`, szavak, next_pos(szeru_nostop,6) )
szeru_next7 <- Map(`[`, szavak, next_pos(szeru_nostop,7) )
szeru_next8 <- Map(`[`, szavak, next_pos(szeru_nostop,8) )


szeru_df <- lapply(seq(length(szeru_nostop)),
                   function(j)
                     lapply(seq(length(szeru_nostop[[j]])),
                            function (i) 
                              if (length(szeru_nostop[[j]]) >0)
                                data.frame( #nem tibble az egyszerűbb kiíráshoz
                                  szeru_prev8 = szeru_prev8[[j]][[i]],
                                  szeru_prev7 = szeru_prev7[[j]][[i]],
                                  szeru_prev6 = szeru_prev6[[j]][[i]],
                                  szeru_prev5 = szeru_prev5[[j]][[i]],
                                  szeru_prev4 = szeru_prev4[[j]][[i]],
                                  szeru_prev3 = szeru_prev3[[j]][[i]],
                                  szeru_prev2 = szeru_prev2[[j]][[i]],
                                  szeru_prev1 = szeru_prev1[[j]][[i]],
                                  szeru = szeru[[j]][[i]],
                                  szeru_next1 = szeru_next1[[j]][[i]],
                                  szeru_next2 = szeru_next2[[j]][[i]],
                                  szeru_next3 = szeru_next3[[j]][[i]],
                                  szeru_next4 = szeru_next4[[j]][[i]],
                                  szeru_next5 = szeru_next5[[j]][[i]],
                                  szeru_next6 = szeru_next6[[j]][[i]],
                                  szeru_next7 = szeru_next7[[j]][[i]],
                                  szeru_next8 = szeru_next8[[j]][[i]]
                                )
                            else 
                              0
                     ))

for(i in seq(my.corpus.l)){
  szeru_df [[i]] <- do.call("rbind", szeru_df[[i]])
}


#a fájlok nevéből a szerzőt és címet megtartani
library(stringr)
options(dplyr.print_max = 100)
options(width=10000)
capture.output(szeru_df1, file="eredmeny.txt")

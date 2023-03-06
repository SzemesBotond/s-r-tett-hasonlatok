# az emagyar által elemzett szövegek betöltése táblázatonként
input.dir <- "A szövegekt tartalmazó mappa elérése"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:100){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}

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
library(dplyr)
#megtalalni az "r" változóba azoknak  
#a -val-vel ragos birtokos főneveknak a helyét, 
#amelyeket főnév vagy főnév-melléknév sorrend, illetve 4 szó távolságban névelő előz meg
library(dplyr)
val <- grep_list_df("[a-zíéáűúőóüö]+(ság|ség|elm|alm|és|ás)(ével|ával)$", 1) #-val-vel ragos alakokat tartalmazó szavak pozíciója
birt<- grep_list_df("psor", 4) # a birtokos személyjelet tartalmazó szavak pozíciója
birt_val<- intersect_lists(val, birt) #az előző kettő közös halmaza
fn<- grep_list_df("NOUN", 3) #főnevek pozíciója
mn<- grep_list_df("ADJ", 3) #melléknevek pozíciója
p1 <- intersect_lists(prev_pos(birt_val, 1), fn) #a "birt_val"-t megelőző szavak és a főnevek közös pozíciójának halmaza
p2 <- intersect_lists(prev_pos(birt_val, 1), mn) #a "birt_val"-t megelőző szavak és a melléknevek közös pozíciójának halmaza
p3 <- union_lists(p1, p2) # az előző kettő közös halmaza
nevelo<- grep_list_df("DET", 3) #névelők pozíciójának listája
q2<- intersect_lists(prev_pos(p3, 4), nevelo) #a p3 előtt 4 szóval névelő
q3<- intersect_lists(prev_pos(p3, 3), nevelo) #a p3 előtt 3 szóval névelő
q4<- intersect_lists(prev_pos(p3, 2), nevelo) #a p3 előtt 2 szóval névelő
q5<- intersect_lists(prev_pos(p3, 1), nevelo) #a p3 előtt 1 szóval névelő
r <- union_lists( # a névelővel megelőzött p3 szavak pozíciója növekvő sorrendben
  union_lists(
    union_lists(
      next_pos(q2,4), next_pos(q3,3)),
    next_pos(q4,2)),
  next_pos(q5,5)) 
r <- lapply(lapply(r, sort, decreasing = F), unique)

#a keresett szavak azonosítása a pozíció alapján
lemmák <- lapply(my.corpus.l, "[", , "V2")
szavak <- lapply(my.corpus.l, "[", , "V1")
birtokszo <- Map(`[`, lemmák, r )

#a képző előtti lemmák meghatározása
birtokszo1 <- lapply(birtokszo, unlist, recursive = F)
birtokszo1 <- lapply(birtokszo11,
                     function(x) gsub("([a-zíéáűúőóüö]+)(ság|ség|és|ás)", "\\1", ignore.case=T,as.character(x)))
birtokszo1 <- lapply(birtokszo11,
                     function(x) gsub("(-|–)", "", ignore.case=T,as.character(x)))

#az azonosított szavak kiírása txt fájlként
capture.output(birtokszo1, file=paste("birtokszavak_lemma2.txt",sep=""))

#emagyar elemzés betöltése, melléknevekkel jelölt szavak és pár főnév kiszedése
birtokszo_elemzett <- read.table("Elérési út", header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
birtokszo_elemzett <- split(birtokszo_elemzett, cumsum(birtokszo_elemzett[,1] == ""))

birtokszo_elemzett <- list(birtokszo_elemzett[[1]],birtokszo_elemzett[[2]])

birtokszo_elemzett1 <- lapply(birtokszo_elemzett, 
                              function(df) grep("\\[/Adj]", df[,3], perl = TRUE))
birtokszo_elemzett2 <- lapply(birtokszo_elemzett, 
                              function(df) grep("(nyugalom|izgalom|szerelem|aggodalom|fájdalom|szánalom|léleknyugalom|unalom|vasfegyelem|buzgalom|értelem|félelem|szorgalom|riadalom|ijedelem)", df[,1], perl = TRUE))
birt_elm <- lapply(union_lists(birtokszo_elemzett1,birtokszo_elemzett2), sort)
birt_elm <- prev_pos(birt_elm,1)

#azok a birtokosok pozíciója, amik melléknevek, vagy a szűkített főnévlistán szerepelnek
birtokszo_szuk <- Map(`[`, r, birt_elm )


# a kontextus által jelölt távolságban a szavak sorozata
hasbirt_prev8 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,8) )
hasbirt_prev7 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,7) )
hasbirt_prev6 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,6) )
hasbirt_prev5 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,5) )
hasbirt_prev4 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,4) )
hasbirt_prev3 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,3) )
hasbirt_prev2 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,2) )
hasbirt_prev1 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,1) )
hasbirt <- Map(`[`, szavak, birtokszo_szuk )
hasbirt_next1 <- Map(`[`, szavak, next_pos(birtokszo_szuk,1) )
hasbirt_next2 <- Map(`[`, szavak, next_pos(birtokszo_szuk,2) )
hasbirt_next3 <- Map(`[`, szavak, next_pos(birtokszo_szuk,3) )
hasbirt_next4 <- Map(`[`, szavak, next_pos(birtokszo_szuk,4) )
hasbirt_next5 <- Map(`[`, szavak, next_pos(birtokszo_szuk,5) )
hasbirt_next6 <- Map(`[`, szavak, next_pos(birtokszo_szuk,6) )
hasbirt_next7 <- Map(`[`, szavak, next_pos(birtokszo_szuk,7) )
hasbirt_next8 <- Map(`[`, szavak, next_pos(birtokszo_szuk,8) )


hasbirt_szurt <- lapply(seq(length(birtokszo_szuk)),
                        function(j)
                          lapply(seq(length(birtokszo_szuk[[j]])),
                                 function (i) 
                                   if (length(birtokszo_szuk[[j]]) >0)
                                     data.frame( #nem tibble az egyszerűbb kiíráshoz
                                       hasbirt_prev8 = hasbirt_prev8[[j]][[i]],
                                       hasbirt_prev7 = hasbirt_prev7[[j]][[i]],
                                       hasbirt_prev6 = hasbirt_prev6[[j]][[i]],
                                       hasbirt_prev5 = hasbirt_prev5[[j]][[i]],
                                       hasbirt_prev4 = hasbirt_prev4[[j]][[i]],
                                       hasbirt_prev3 = hasbirt_prev3[[j]][[i]],
                                       hasbirt_prev2 = hasbirt_prev2[[j]][[i]],
                                       hasbirt_prev1 = hasbirt_prev1[[j]][[i]],
                                       hasbirt = hasbirt[[j]][[i]],
                                       hasbirt_next1 = hasbirt_next1[[j]][[i]],
                                       hasbirt_next2 = hasbirt_next2[[j]][[i]],
                                       hasbirt_next3 = hasbirt_next3[[j]][[i]],
                                       hasbirt_next4 = hasbirt_next4[[j]][[i]],
                                       hasbirt_next5 = hasbirt_next5[[j]][[i]],
                                       hasbirt_next6 = hasbirt_next6[[j]][[i]],
                                       hasbirt_next7 = hasbirt_next7[[j]][[i]],
                                       hasbirt_next8 = hasbirt_next8[[j]][[i]]
                                     )
                                 else 
                                   0
                          ))

for(i in seq(my.corpus.l)){
  hasbirt_szurt [[i]] <- do.call("rbind", hasbirt_szurt[[i]])
}

#bizonyos szavak kiszedése, ami prototipikusan nem ez a konstrukcióban
kivetel <- "(szembe(|n)|visszaél|helyettesít|összevet|versenyez| küzd| vetek|ellenkezik|ellentétben|megeléged|hidal|rendelkez|kapcsolat(os|ba)| tisztába| együtt | rendelkez| összefügg|összetévesz|összeegyeztet| arány|ruház)"

hasbirt_szurt1 <- lapply(seq(length(my.corpus.l)), function (i)
  if (hasbirt_szurt[[i]][1,1]!=0 )
    hasbirt_szurt[[i]] %>% filter(across(everything(), ~ !str_detect(., kivetel))))


#a fájlok nevéből a szerzőt és címet megtartani
library(stringr)
files.v <- lapply(files.v, str_remove_all, "(.txt|out_)")
names(hasbirt_szurt1) <- files.v
options(dplyr.print_max = 100)
options(width=10000)
capture.output(hasbirt_szurt1, file="eredmeny.txt")


#Extra szavak, amelyek prototipikusan a konstrukcióban

#megtalalni az "roz" változóba a felsorolt főneveknak a helyét, 
#amelyeket főnév vagy főnév-melléknév, illetve 4 szó távolságban névelő előz meg
library(dplyr)
szo <- grep_list_df("türelmével|szolidaritásával|önzésével|fölényével|lendületével|lelkesedésével|éhségével|duzzogásával", 1)
poz1<- intersect_lists(prev_pos(szo, 1), fn)
poz2<- intersect_lists(prev_pos(szo, 1), mn)
poz3 <- union_lists(poz1, poz2)

qoz2[[i]] <- intersect(poz3[[i]]-3, nevelo [[i]]) 
qoz3[[i]] <- intersect(poz3[[i]]-2, nevelo [[i]])
qoz4[[i]] <- intersect(poz3[[i]]-1, nevelo [[i]])
qoz5[[i]] <- intersect(poz3[[i]]-4, nevelo [[i]])

qoz1<- intersect_lists(prev_pos(poz3, 4), nevelo)
qoz2<- intersect_lists(prev_pos(poz3, 3), nevelo)
qoz3<- intersect_lists(prev_pos(poz3, 2), nevelo)
qoz4<- intersect_lists(prev_pos(poz3, 1), nevelo)
roz<- union_lists(union_lists(union_lists(
  next_pos(qoz1, 5), next_pos(qoz2, 4)), 
  next_pos(qoz3,3)),
  next_pos(qoz4, 2))
roz <- lapply(lapply(roz, sort, decreasing = F), unique)

# a kontextus által jelölt távolságban a szavak sorozata
hasbirt_e_prev8 <- Map(`[`, szavak, prev_pos(birtokszo_szuk,8) )
hasbirt_e_prev7 <- Map(`[`, szavak, prev_pos(roz,7) )
hasbirt_e_prev6 <- Map(`[`, szavak, prev_pos(roz,6) )
hasbirt_e_prev5 <- Map(`[`, szavak, prev_pos(roz,5) )
hasbirt_e_prev4 <- Map(`[`, szavak, prev_pos(roz,4) )
hasbirt_e_prev3 <- Map(`[`, szavak, prev_pos(roz,3) )
hasbirt_e_prev2 <- Map(`[`, szavak, prev_pos(roz,2) )
hasbirt_e_prev1 <- Map(`[`, szavak, prev_pos(roz,1) )
hasbirt_e <- Map(`[`, szavak, roz )
hasbirt_e_next1 <- Map(`[`, szavak, next_pos(roz,1) )
hasbirt_e_next2 <- Map(`[`, szavak, next_pos(roz,2) )
hasbirt_e_next3 <- Map(`[`, szavak, next_pos(roz,3) )
hasbirt_e_next4 <- Map(`[`, szavak, next_pos(roz,4) )
hasbirt_e_next5 <- Map(`[`, szavak, next_pos(roz,5) )
hasbirt_e_next6 <- Map(`[`, szavak, next_pos(roz,6) )
hasbirt_e_next7 <- Map(`[`, szavak, next_pos(roz,7) )
hasbirt_e_next8 <- Map(`[`, szavak, next_pos(roz,8) )


hasbirt_e_szurt <- lapply(seq(length(roz)),
                          function(j)
                            lapply(seq(length(roz[[j]])),
                                   function (i) 
                                     if (length(roz[[j]]) >0)
                                       data.frame( #nem tibble az egyszerűbb kiíráshoz
                                         hasbirt_prev8 = hasbirt_e_prev8[[j]][[i]],
                                         hasbirt_prev7 = hasbirt_e_prev7[[j]][[i]],
                                         hasbirt_prev6 = hasbirt_e_prev6[[j]][[i]],
                                         hasbirt_prev5 = hasbirt_e_prev5[[j]][[i]],
                                         hasbirt_prev4 = hasbirt_e_prev4[[j]][[i]],
                                         hasbirt_prev3 = hasbirt_e_prev3[[j]][[i]],
                                         hasbirt_prev2 = hasbirt_e_prev2[[j]][[i]],
                                         hasbirt_prev1 = hasbirt_e_prev1[[j]][[i]],
                                         hasbirt = hasbirt_e[[j]][[i]],
                                         hasbirt_next1 = hasbirt_e_next1[[j]][[i]],
                                         hasbirt_next2 = hasbirt_e_next2[[j]][[i]],
                                         hasbirt_next3 = hasbirt_e_next3[[j]][[i]],
                                         hasbirt_next4 = hasbirt_e_next4[[j]][[i]],
                                         hasbirt_next5 = hasbirt_e_next5[[j]][[i]],
                                         hasbirt_next6 = hasbirt_e_next6[[j]][[i]],
                                         hasbirt_next7 = hasbirt_e_next7[[j]][[i]],
                                         hasbirt_next8 = hasbirt_e_next8[[j]][[i]]
                                       )
                                   else 
                                     0
                            ))

for(i in seq(my.corpus.l)){
  hasbirt_e_szurt [[i]] <- do.call("rbind", hasbirt_e_szurt[[i]])
}

#bizonyos szavak kiszedése, amiknek a vonazata a -val-vel

hasbirt_e_szurt1 <- lapply(seq(length(my.corpus.l)), function (i)
  if (hasbirt_e_szurt[[i]][1,1]!=0 )
    hasbirt_e_szurt[[i]] %>% filter(across(everything(), ~ !str_detect(., kivetel))))


#a fájlok nevéből a szerzőt és címet megtartani
library(stringr)
options(dplyr.print_max = 100)
options(width=10000)
capture.output(hasbirt_e_szurt1, file="eredmeny.txt")

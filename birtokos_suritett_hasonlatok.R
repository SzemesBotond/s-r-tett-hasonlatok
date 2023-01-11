# az emagyar által elemzett szövegek betöltése táblázatonként
input.dir <- "A szövegekt tartalmazó mappa elérése"
files.v <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:100){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files.v[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
}


#megtalalni az "r" változóba azoknak  
#a -val-vel ragos birtokos főneveknak a helyét, 
#amelyeket főnév vagy főnév-melléknév sorrend, illetve 4 szó távolságban névelő előz meg
library(dplyr)
val <- list() #-val-vel ragos alakokat tartalmazó szavak pozíciója
birt<- list() # a birtokos személyjelet tartalmazó szavak pozíciója
birt_val<- list() #az előző kettő közös halmaza
fn<- list() #főnevek pozíciója
mn<- list() #melléknevek pozíciója
p1<- list() #a "birt_val"-t megelőző szavak és a főnevek közös pozíciójának halmaza
p2<- list() #a "birt_val"-t megelőző szavak és a melléknevek közös pozíciójának halmaza
p3 <- list() # az előző kettő közös halmaza
nevelo<- list() #névelők pozíciójának listája
q2<- list() #a p3 előtt 4 szóval névelő
q3<- list() #a p3 előtt 3 szóval névelő
q4<- list() #a p3 előtt 2 szóval névelő
q5<- list() #a p3 előtt 1 szóval névelő
r<- list() # a névelővel megelőzött p3 szavak pozíciója növekvő sorrendben
#a listák feltöltése értékekkel:
for(i in 1:length(my.corpus.l)){
  val [[i]] <- grep("[a-zíéáűúőóüö]+(ság|ség|elm|alm|és|ás)(ével|ával)$", my.corpus.l[[i]]$V1, ignore.case=T)
  birt[[i]] <- grep("psor", my.corpus.l[[i]]$V4, ignore.case = T)
  birt_val [[i]] <- intersect(val[[i]], birt[[i]])
  fn [[i]] <- grep("NOUN", my.corpus.l[[i]]$V3, ignore.case = T)
  mn [[i]] <- grep("ADJ", my.corpus.l[[i]]$V3, ignore.case = T)
  p1[[i]] <- intersect(birt_val [[i]]-1, fn[[i]])
  p2[[i]] <- intersect(birt_val [[i]]-1, mn[[i]])
  p3[[i]] <- c(p1[[i]], p2[[i]])
  nevelo [[i]]<- grep("DET", my.corpus.l[[i]]$V3)
  q2[[i]] <- intersect(p3[[i]]-4, nevelo [[i]]) 
  q3[[i]] <- intersect(p3[[i]]-3, nevelo [[i]])
  q4[[i]] <- intersect(p3[[i]]-2, nevelo [[i]])
  q5[[i]] <- intersect(p3[[i]]-1, nevelo [[i]])
  r[[i]]<- c(q2[[i]]+4,q3[[i]]+3, q4[[i]]+2,q5[[i]]+5)
  r[[i]] <- unique(sort(r[[i]], decreasing = F))
}
#a keresett szavak azonosítása a pozíció alapján
birtokszo <- list()
for(i in 1:100){
  birtokszo[[i]] <- my.corpus.l[[i]][r[[i]],2]
}
#a képző előtti lemmák meghatározása
birtokszo1<- list()
for(i in 1:length(birtokszo)){
  birtokszo1[[i]] <- unlist(birtokszo[[i]], recursive = F)
  birtokszo1 [[i]] <- gsub("([a-zíéáűúőóüö]+)(ság|ség|és|ás)", "\\1", birtokszo1[[i]], ignore.case=T)
  birtokszo1 [[i]] <- gsub("(-|–)", "", birtokszo1[[i]], ignore.case=T)
}
#az azonosított szavak kiírása txt fájlként
capture.output(birtokszo1, file=paste("birtokszavak_lemma2.txt",sep=""))

#emagyar elemzés betöltése, melléknevekkel jelölt szavak és pár főnév kiszedése
birtokszo_elemzett <- read.table("Elérési út", header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = FALSE)
birtokszo_elemzett <- split(birtokszo_elemzett, cumsum(birtokszo_elemzett[,1] == ""))
birtokszo_elemzett1 <- list() 
birtokszo_elemzett2 <- list()
birt_elm <- list()
for(i in 1:(length(birtokszo_elemzett))){
  birtokszo_elemzett1[[i]] <-  grep("\\[/Adj]", birtokszo_elemzett[[i]]$V3, perl = TRUE)
  birtokszo_elemzett2[[i]] <-  grep("(nyugalom|izgalom|szerelem|aggodalom|fájdalom|szánalom|léleknyugalom|unalom|vasfegyelem|buzgalom|értelem|félelem|szorgalom|riadalom|ijedelem)", birtokszo_elemzett[[i]]$V1, perl = TRUE)
  birt_elm[[i]] <- sort(c(birtokszo_elemzett1[[i]],birtokszo_elemzett2[[i]]))
  birt_elm[[i]] <- birt_elm[[i]]-1
}

#azok a birtokosok pozíciója, amik melléknevek, vagy a szűkített főnévlistán szerepelnek
birtokszo_szuk <- list()
for(i in 1:(length(birtokszo_elemzett))){
  birtokszo_szuk[[i]] <-  r[[i]][birt_elm[[i]]]}
#a kostrukció kontextusának pozíciója
r2 <- list()
r1 <- list()
for(i in 1:length(r)){
  r1[[i]]<-birtokszo_szuk[[i]]-10
  r2[[i]]<-birtokszo_szuk[[i]]+10
}
# a kontextus által jelölt távolságban a szavak sorozata
hasbirt_szavak <- list()
for(i in 1:100){
  hasbirt_szavak [[i]] <- my.corpus.l[[i]]$V1
}
hasbirt_szurt <- hasbirt_szavak
for(i in 1:length(hasbirt_szavak)){
  hasbirt_szurt[[i]] <- list(hasbirt_szavak[[i]])
  for(j in 1:length(r1[[i]])){
    if (length(r1[[i]]) >0){
      hasbirt_szurt[[i]][[j]] <- hasbirt_szavak[[i]][r1[[i]][[j]]:r2[[i]][[j]]]}
    else{
      hasbirt_szurt[[i]] <- 0  
    }
  }}

#bizonyos szavak kiszedése, ami prototipikusan nem ez a konstrukcióban
hasbirt_szurt1 <- hasbirt_szurt
for(i in 1:length(hasbirt_szurt)){
  for(j in 1:length(hasbirt_szurt[[i]])){
    if(class(hasbirt_szurt[[i]])== "list"){    
      hasbirt_szurt1[[i]][[j]] <- hasbirt_szurt[[i]][j][!grepl("(szembe(|n)|visszaél|helyettesít|összevet|versenyez| küzd| vetek|ellenkezik|ellentétben|megeléged|hidal|rendelkez|kapcsolat(os|ba)| tisztába| együtt | rendelkez| összefügg|összetévesz|összeegyeztet| arány|ruház)", hasbirt_szurt[[i]][j])]
    }
  }}

# a szavak listáját mondatokká  alakítani és unlistelni: itt lehet összeszámolni, hogy melyikben mennyi van
birtokhas1   <- hasbirt_szurt1
for(i in 1:length(hasbirt_szurt)){
  for(j in 1:length(hasbirt_szurt1[[i]])){
    if(class(hasbirt_szurt1[[i]])== "list"){    
      birtokhas1[[i]][[j]] <- paste(unlist(hasbirt_szurt1[[i]][j]), collapse = " " )}
  }}

#a fájlok nevéből a szerzőt és címet megtartani
library(stringr)
for (i in 1:length(files.v)) {
  files.v[[i]] <- str_remove_all(files.v[[i]], ".txt")
  files.v[[i]] <- str_remove_all(files.v[[i]], "out_")
}
names(birtokhas1) <- files.v

#az eredmények kiírása
capture.output(birtokhas1,file=paste("Elérési út"))



#Extra szavak, amelyek prototipikusan a konstrukcióban

#megtalalni az "roz" változóba a felsorolt főneveknak a helyét, 
#amelyeket főnév vagy főnév-melléknév, illetve 4 szó távolságban névelő előz meg
library(dplyr)
szo <- list()
fn<- list()
mn<- list()
poz1<- list()
poz2<- list()
poz3 <- list()
nevelo<- list()
qoz2<- list()
qoz3<- list()
qoz4<- list()
qoz5<- list()
qoz6<- list()
qoz7<- list()
roz<- list()
for(i in 1:length(my.corpus.l)){
  szo [[i]] <- grep("türelmével|szolidaritásával|önzésével|fölényével|lendületével|lelkesedésével|éhségével|duzzogásával", my.corpus.l[[i]]$V1, ignore.case=T)
  fn [[i]] <- grep("NOUN", my.corpus.l[[i]]$V3, ignore.case = T)
  mn [[i]] <- grep("ADJ", my.corpus.l[[i]]$V3, ignore.case = T)
  poz1[[i]] <- intersect(szo [[i]]-1, fn[[i]])
  poz2[[i]] <- intersect(szo [[i]]-1, mn[[i]])
  poz3[[i]] <- c(poz1[[i]], poz2[[i]])
  nevelo [[i]]<- grep("DET", my.corpus.l[[i]]$V3)
  qoz2[[i]] <- intersect(poz3[[i]]-3, nevelo [[i]]) 
  qoz3[[i]] <- intersect(poz3[[i]]-2, nevelo [[i]])
  qoz4[[i]] <- intersect(poz3[[i]]-1, nevelo [[i]])
  qoz5[[i]] <- intersect(poz3[[i]]-4, nevelo [[i]])
  roz[[i]]<- c(qoz2[[i]]+4,qoz3[[i]]+3, qoz4[[i]]+2,qoz5[[i]]+5)
  roz[[i]] <- unique(sort(roz[[i]], decreasing = F))
}
#a kostrukció kontextusának pozíciója
roz2 <- list()
roz1 <- list()
for(i in 1:length(r)){
  roz1[[i]]<-roz[[i]]-10
  roz2[[i]]<-roz[[i]]+10
}
#csak szavak
hasbirt_szavak <- list()
for(i in 1:100){
  hasbirt_szavak [[i]] <- my.corpus.l[[i]]$V1
}
hasbirt_szurt_extra <- hasbirt_szavak
for(i in 1:length(hasbirt_szavak)){
  hasbirt_szurt_extra[[i]] <- list(hasbirt_szavak[[i]])
  for(j in 1:length(roz1[[i]])){
    if (length(roz1[[i]]) >0){
      hasbirt_szurt_extra[[i]][[j]] <- hasbirt_szavak[[i]][roz1[[i]][[j]]:roz2[[i]][[j]]]}
    else{
      hasbirt_szurt_extra[[i]] <- 0  
    }
  }}

#azok az esetek kiszűrése, amelyben igék -val-vel vonzattal
hasbirt_szurt_extra1 <- hasbirt_szurt_extra
for(i in 1:length(hasbirt_szurt_extra)){
  for(j in 1:length(hasbirt_szurt_extra[[i]])){
    if(class(hasbirt_szurt_extra[[i]])== "list"){    
      hasbirt_szurt_extra1[[i]][[j]] <- hasbirt_szurt_extra[[i]][j][!grepl("(szembe(|n)|visszaél|helyettesít|összevet|versenyez| küzd| vetek|ellenkezik|ellentétben|megeléged|hidal|rendelkez|kapcsolat(os|ba)| tisztába| együtt | rendelkez| összefügg|összetévesz|összeegyeztet| arány|ruház)", hasbirt_szurt_extra[[i]][j])]
    }
  }}

# a szavak listáját mondatokká  alakítani és unlistelni: itt lehet összeszámolni, h melyikben mennyi van
birtokhas_extra1   <- hasbirt_szurt_extra1
for(i in 1:length(hasbirt_szurt_extra)){
  for(j in 1:length(hasbirt_szurt_extra1[[i]])){
    if(class(hasbirt_szurt_extra1[[i]])== "list"){    
      birtokhas_extra1[[i]][[j]] <- paste(unlist(hasbirt_szurt_extra1[[i]][j]), collapse = " " )}
  }}

#a fájlok nevéből a szerzőt és címet megtartani, majd txt fájba kírni
names(birtokhas_extra1) <- files.v
capture.output(birtokhas_extra1,file=paste("Elérési út"))

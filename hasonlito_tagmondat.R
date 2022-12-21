#mondatokra bontott szöveg a "token_sent2" változóban.
#(előállítható a "mondathossz" github repozitórium alapján)

#kezelni azoka az eseteket, amikor "akár...akár", "nem mint" vagy régies "a mint" szerkezetek
hasmin <- list()
for (i in 1: length(token_sent2)){
  hasmin[[i]] <- str_remove_all(token_sent2[[i]], "((akár|Akár) (.*?)akár )|( a mint | nem mint )")
}

#hasonlító tagmondatkpacoslat regex
hasonlito <- "(^|([,;:\\-\\–] ))(([a-zíéáűúőóüö]+ ){0,1})(mint |mintha |akár |akárha |akárcsak |minthogyha)"


ossz <- list ()
darab <- list ()
arany <- list()
for (i in 1:length(hasmin)) {
  ossz[[i]] <- gregexpr(hasonlito, hasmin[[i]], perl = TRUE, ignore.case =TRUE)
  ossz [[i]] <- unlist(ossz[[i]])
  ossz [[i]] <- ossz[[i]][which(ossz[[i]] != -1)]
  darab [[i]] <- length(ossz[[i]])
}

unlist(darab)

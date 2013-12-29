normalizzaslang <-
function(testo){
	
	testo <- gsub(" (#?zz+|#?u+ff[aif]+?|#?r+o+n+f+|#uff|ronf) "," EMOTEZZZ ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" (#?sii+|#si+|#?yes+|#?sìì+) "," EMOTESIII ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?noo+|#no+|#?nuu+) "," EMOTENOOO ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?ahh+) "," EMOTEAHHH ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" (#?ehh+) "," EMOTEEHHH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?o?h?o+h[oh]+) "," EMOTEOHOH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEIHIH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?i?h?i+h[ih]+) "," EMOTEUHUH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?a?h?ah[^h][ah]+) "," EMOTEAHAH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?e?h?eh[^h][eh]+) "," EMOTEEHEH ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?azz+) "," EMOTEAZZ ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" (#?[dv]aii+|#[dv]ai|forzaa+) "," EMOTEDAIII ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?cazz[oi]+) "," cazzo ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?cazzat[a]+) "," cazzata ",testo, perl=TRUE, ignore.case=TRUE)  
	testo <- gsub(" (#?merd[a]+) "," merda ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?aaa+) "," EMOTEAAA ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" (#?ooo+) "," EMOTEOOO ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" (#?eee+) "," EMOTEEEE ",testo, perl=TRUE, ignore.case=TRUE)		
	testo <- gsub(" (#?l+o+l+|#?r+o+f+t+l+) "," EMOTELOL ",testo, perl=TRUE, ignore.case=TRUE)		
	testo <- gsub(" (#?aiutoo+|#?sos|#?help+) "," EMOTESOS ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" (#ba+sta+|ba+staa+) "," EMOTEBASTA ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" #?([uw]+[ao]+[uw]+) "," EMOTEWOW ",testo, perl=TRUE, ignore.case=TRUE)		
	
	testo <- gsub(" (#?grande[e]+|#?grandi[i]+) "," grandeee ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" #?(grand(issim)[aeoi]+) "," grandissimo ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" #?(brav[aeoi]+) "," bravo ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" #?(brav(issim)[aeoi]+) "," bravissimo ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" #?(bell[aeoi]+) "," bello ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" #?(mit[t]ic([aio]|he)+) "," mitico ",testo, perl=TRUE, ignore.case=TRUE)			
	testo <- gsub(" #?(graziee+) "," grazieee ",testo, perl=TRUE, ignore.case=TRUE)	
	testo <- gsub(" #?(stronz[oaie]+) "," stronzooo ",testo, perl=TRUE, ignore.case=TRUE)	
		
	# ALTRO		
	testo <- gsub(" perch[éeè] "," perchè ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" x(ch|k)[éeè] "," perchè ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" nn "," non ",testo, perl=TRUE, ignore.case=TRUE)
	testo <- gsub(" nun "," non ",testo, perl=TRUE, ignore.case=TRUE)	
		testo		
	}

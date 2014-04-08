.toeliminate<- function(some_txt){

##link e tag
some_txt = gsub("http://\\w*.\\w*/\\w*{1,}", " ", some_txt)		#link http immagini
some_txt = gsub("https://\\w*.\\w*/\\w*{1,}", " ", some_txt)	
some_txt = gsub("/\\w+", " ", some_txt)							#link
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", some_txt)	#tag
some_txt = gsub("RT  @", " @", some_txt)						#ancora tag
some_txt = gsub("(MT|via)((?:\\b\\W*@\\w+)+)", " ", some_txt)	#tag
some_txt = gsub("MT @", " @", some_txt)							#ancora tag
some_txt = gsub("@\\w+", " ", some_txt)							#tag
some_txt = gsub("#\\w+", " ", some_txt)							#hashtag
some_txt = gsub("www.", " ", some_txt)							#link2  

reg = "/(http|https):\\/\\/[a-z0-9]+([\\-\\.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})?(\\/[^\\s]*)/ix"	#espressione regolare per togliere i link
some_txt = gsub(reg, " ", some_txt)							#link2

some_txt = gsub("^\\s+|\\s+$", " ", some_txt)					#p rete

some_txt = gsub("[[:digit:]]", " ", some_txt)					#numeri
some_txt = gsub("[ \t]{2,}", " ", some_txt)						#tab
some_txt = gsub(" null", " ", some_txt)							#tolga le parole null
some_txt = gsub("[[:punct:]]", " ", some_txt)					#punteggiatura

some_txt = gsub("\\W", " ", some_txt)							#solo caratteri alfanumerici
some_txt = gsub("   ", " ", some_txt)							#tolgo gli spazi tripli
some_txt = gsub("  ", " ", some_txt)							#tolgo gli spazi doppi

some_txt = gsub(" amp ", "  ", some_txt, fixed=T)	
some_txt = gsub(" amp$", "  ", some_txt)	
some_txt = gsub("^amp", "  ", some_txt)	
some_txt = gsub(" rt ", "  ", some_txt, fixed=T)	
some_txt = gsub(" tp ", "  ", some_txt, fixed=T)	
some_txt = gsub(" gt ", "  ", some_txt, fixed=T)	
some_txt = gsub(" yg ", "  ", some_txt, fixed=T)	
some_txt = gsub("^rt ", "  ", some_txt)	
some_txt = gsub("^tp ", "  ", some_txt)	
some_txt = gsub("^gt ", "  ", some_txt)	
some_txt = gsub("^yg ", "  ", some_txt)	
some_txt = gsub(" rt$", "  ", some_txt)	
some_txt = gsub(" tp$", "  ", some_txt)	
some_txt = gsub(" gt$", "  ", some_txt)	
some_txt = gsub(" yg$", "  ", some_txt)	
some_txt = gsub(" httpuhhappy co ", "  ", some_txt)	


}

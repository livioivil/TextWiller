normalizzacaratteri <- function(testo){
	# fonte: http://www.utf8-chartable.de/
	#        http://www.utf8-chartable.de/unicode-utf8-table.pl?start=4736&number=128&utf8=string-literal&unicodeinhtml=dec
	
# 	# test: http://www.utf8-chartable.de/unicode-utf8-table.pl
# 	testo <- gsub("\xc3\xa0" ,"ââ", testo, fixed=TRUE, useBytes=FALSE)
# 	testo <- gsub("\xc3\xa8" ,"â®", testo, fixed=TRUE, useBytes=FALSE)
# 	testo <- gsub("\xc3\xa9" ,"â©", testo, fixed=TRUE, useBytes=FALSE)
# 	testo <- gsub("\xc3\xac" ,"â¨", testo, fixed=TRUE, useBytes=FALSE)
# 	testo <- gsub("\xc3\xad" ,"i", testo, fixed=TRUE, useBytes=FALSE)	
# 	testo <- gsub("\xc3\xb2" ,"ââ", testo, fixed=TRUE, useBytes=FALSE) 	
# 	testo <- gsub("\xc3\xba" ,"âÏ", testo, fixed=TRUE, useBytes=FALSE)  

	
	testo <- gsub("\001" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\002" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\003" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\004" ," ", testo, fixed=TRUE, useBytes=FALSE)			
	testo <- gsub("\005" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\006" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\007" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\008" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\009" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\010" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\011" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\012" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\013" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\014" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\015" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\016" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\017" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\018" ," ", testo, fixed=TRUE, useBytes=FALSE)
#	testo <- gsub("\019" ," ", testo, fixed=TRUE, useBytes=FALSE)																
	testo <- gsub("\020" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\021" ,"!", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\022" ,'"', testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\023" ,' ', testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\027" ,"'", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\030" ,"'", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\031" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\032" ,"'", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\033" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\034" ,"'", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\035" ,"'", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\036" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\037" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\x80" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x81" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x82" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x83" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x84" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x85" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x86" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x87" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x88" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x89" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x8a" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x8b" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x8c" ," ", testo, fixed=TRUE, useBytes=FALSE)			
	testo <- gsub("\x8d" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x8e" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x8f" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\x90" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x91" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x92" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x93" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x94" ," ", testo, fixed=TRUE, useBytes=FALSE)
 	testo <- gsub("\x95" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x96" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x97" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\x98" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x99" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x9a" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x9b" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\x9c" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\x9d" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x9e" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\x9f" ," ", testo, fixed=TRUE, useBytes=FALSE)							
	testo <- gsub("â¨\xa0" ," ", testo, fixed=TRUE, useBytes=FALSE) # strange	
	testo <- gsub("\xa0" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xa1" ,"!", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xa2" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xa3" ,"L", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xa4" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xa5" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xa6" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xa7" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xa8" ,'"', testo, fixed=TRUE, useBytes=FALSE) # modified					
	testo <- gsub("\xa9" ,"C", testo, fixed=TRUE, useBytes=FALSE) # modified			
	testo <- gsub("\xaa" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xab" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xac" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xad" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xae" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xaf" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xb0" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xb1" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xb2" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xb3" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xb4" ,"'", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xb5" ,"u", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xb6" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xb7" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xb8" ,",", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xb9" ,"1", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xba" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xbb" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xbc" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xbd" ," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xbe" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xbf" ,"?", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xc0" ,"A", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xc1" ,"A", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xc2" ,"ââ", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xc3" ,"ò", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xc4" ,"a", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xc5" ,"A", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xc6" ," ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xc7" ,"âÃ", testo, fixed=TRUE, useBytes=FALSE)		
	testo <- gsub("\xc8" ,"â®", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xc9" ,"â©", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xca" ,"E", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xcb" ,"E", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xcc" ,"â¨", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xcd" ,"I", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xce" ," ", testo, fixed=TRUE, useBytes=FALSE) 	
	testo <- gsub("\xcf" ," ", testo, fixed=TRUE, useBytes=FALSE) 		
	testo <- gsub("\xe0" ,"ââ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xe1" ,"a", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xe2" ,"a", testo, fixed=TRUE, useBytes=FALSE) # modified 	 
	testo <- gsub("\xe3" ,"a", testo, fixed=TRUE, useBytes=FALSE) # modified 
	testo <- gsub("\xe4" ,"a", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xe5" ,"a", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xe6" ,"ae", testo, fixed=TRUE, useBytes=FALSE) # modified			
	testo <- gsub("\xe7" ,"âÃ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xe8" ,"â®", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xe9" ,"â©", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xea" ,"e", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xeb" ,"e", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xec" ,"â¨", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xed" ,"i", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xee" ,"i", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xef" ,"i", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xd0" ,"D", testo, fixed=TRUE, useBytes=FALSE) # modified			
	testo <- gsub("\xd1" ,"N", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xd2" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xd3" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xd4" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xd5" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xd6" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xd7" ,"x", testo, fixed=TRUE, useBytes=FALSE) # modified		
	testo <- gsub("\xd8" ,"O", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xd9" ,"U", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xda" ,"U", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xdb" ," ", testo, fixed=TRUE, useBytes=FALSE) 
	testo <- gsub("\xdc" ," ", testo, fixed=TRUE, useBytes=FALSE) 			
	testo <- gsub("\xdd" ,"Y", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xde" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified			
	testo <- gsub("\xdf" ," ", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xf0" ," ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xf1" ,"n", testo, fixed=TRUE, useBytes=FALSE)			
	testo <- gsub("\xf2" ,"ââ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xf3" ,"o", testo, fixed=TRUE, useBytes=FALSE) # modified
	testo <- gsub("\xf4" ,"o", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xf5" ,"o", testo, fixed=TRUE, useBytes=FALSE) # modified 	
	testo <- gsub("\xf6" ,"o", testo, fixed=TRUE, useBytes=FALSE) # modified	
	testo <- gsub("\xf8" ,"o", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xf9" ,"âÏ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xfa" ,"âÏ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\xfb" ,"âÏ", testo, fixed=TRUE, useBytes=FALSE)	
	testo <- gsub("\xfc" ,"u", testo, fixed=TRUE, useBytes=FALSE) # modified 
	testo <- gsub("\xfd" ,"y", testo, fixed=TRUE, useBytes=FALSE) # modified 
	testo <- gsub("\xfe" ,"y", testo, fixed=TRUE, useBytes=FALSE) # modified 		
#	testo <- gsub("(\U3e35393c ? \U3e35393c)" ," ", testo, fixed=TRUE, useBytes=FALSE)			
  
	testo <- gsub("\n"," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\r"," ", testo, fixed=TRUE, useBytes=FALSE)
	testo <- gsub("\t"," ", testo, fixed=TRUE, useBytes=FALSE)
  testo
}

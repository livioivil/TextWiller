normalizzaemote <-
function(testo,perl=TRUE){
	# EMOTEGOOD :) :-) :] :-] =) =] => :> ^^ ^_^ ^-^ ^o^ : ) (: :'D
testo <- gsub("\\:\\)+|\\:\\-\\)+|\\:\\]+|\\:\\-\\]+|\\=\\)+|\\=\\]+|\\=\\>|\\:\\>|\\^\\^|\\^\\_+\\^|\\^\\-\\^|\\^\\o\\^|\\:[[:blank:]]\\)+|[[:blank:]]\\([[:blank:]]?\\:|\\:\\'D+",' EMOTEGOOD ', testo, perl=perl)	

#testo <- gsub(":\\-?[\\)\\]]+",' EMOTEGOOD ', testo, perl=TRUE)	


	# EMOTEGOOD :d :D :-d :-D =d =D 8d 8D :') 
testo <- gsub("\\:d+|\\:D+|\\:\\-d+|\\:\\-D+|\\=d+|\\=D+|8d+|8D+|\\:\\'+\\)+|v\\.v",' EMOTEGOOD ', testo, perl=perl)
	# EMOTELOVE <3 ❤ ♥
testo <- gsub("\\<3+|❤|♥",' EMOTELOVE ', testo, perl=perl)        
              # EMOTEBAD :( :-( :[ :-[ =[ =( : ( ):
testo <- gsub("\\:\\(+|\\:\\-\\(+|\\:\\[+|\\:\\-\\[+|\\=\\[+|\\=\\(+|\\:[[:blank:]]\\(|[[:blank:]]\\([[:blank:]]?\\:",' EMOTEBAD ', testo, perl=perl)
	# EMOTEBAD :'( :-[ :* 
testo <- gsub("\\:\\'+\\(+|\\:\\'\\[|\\:\\*+|\\:\\-[\\|]",' EMOTEBAD ', testo, perl=perl)
	# EMOTEBAD :| :/ =/ :x :-|
testo <- gsub("\\:\\|\\:/+|\\=/+|\\:x",' EMOTEBAD ', testo, perl=perl)
	# EMOTEBAD #_# X_X x_x X.X x.x >.< >_< >.> >_>
testo <- gsub("\\#\\_+\\#|X\\_+X|x\\_+x|X\\.X|x\\.x|>\\.<|>\\_+<|>\\_+>|>\\.>",' EMOTEBAD ', testo, perl=perl)
	# EMOTEWINK ;) ;-) ;] ;-] ;> ;d ;D ;o
testo <- gsub("\\;\\)+|\\;\\-\\)+|\\;\\]|\\;\\-\\]|\\;\\>|;d+|;D+|;o",' EMOTEWINK ', testo, perl=perl)
	# EMOTESHOCK O.o o.o O.O o.O O_o o_o O_O o_O :OO =O -.- u.u u.ù ù.u u_u çç ç_ç t_t ù_ù ù.ù :oo 0_0 =_= ._. òò ò_ò *u* -_- ùù -,- :s --' .-. '-'
testo <- gsub("O\\.o|o\\.o|O\\.O|o\\.O|O\\_+o|o\\_+o|O\\_+O|o\\_+O|\\:OO+|\\=O+|\\-\\.\\-|u\\.u|u\\.ù|ù\\.u|u\\_+u|çç|ç_+ç|t_+t|ù\\_+ù|ù\\.ù|\\:oo+|0\\_+0|\\=\\_+\\=|\\.\\_+\\.|òò|ò\\_+ò|\\*u+\\*|\\-\\_+\\-|ùù|\\-\\,\\-|\\-\\-\\'|\\.\\-\\.|\\'\\-\\'",' EMOTESHOCK ', testo, perl=perl)
	# EMOTEAMAZE *_* *-* *o* *.* 
testo <- gsub("\\*\\_+\\*|\\*\\-\\*|\\*\\o\\*|\\*\\.\\*",' EMOTEAMAZE ', testo, perl=perl)
	# EMOTEJOKE :P :p =P =p XD xD xd d:
testo <- gsub("\\:P+[^e]|\\:p+[^e]|\\=P+|\\=p+|XD+|xD+|xd+|[[:blank:]]d\\:",' EMOTEAMAZE ', testo, perl=perl)

	# NEW VERSION
#testo <- gsub("([:=8]([- '])?[])Dd>]+)|(\\^[-_o]?\\^)","EMOTEGOOD", testo)
#testo <- gsub("([:=]([- '])?[(|/x*[])|([>Xx#][._][>Xx<#])|(\\):)","EMOTEBAD", testo)
#testo <- gsub(";-?[])>Ddo]","EMOTEWINK", testo)
#testo <- gsub("([-oO0òtuùç=][._]+[-oO0òtuùç=])|(\\.[-_]+\\.)|(\\b[=oO0:][oO0]+\\b)|(:[sS]+\\b)|('-')|(\\*u+\\*)|(-,-)|(--')|([çòù]{2})","EMOTESHOCK", testo)
#testo <- gsub("\\*[-._o]\\*","EMOTEAMAZE", testo)
#testo <- gsub("([:=]-?[pP]+)|(\\b[xX][dD]+\\b)|(\\bd:\\b)","EMOTEJOKE", testo)


	# AGGIUNTA ConfrontoSky
testo <- gsub("#0_o"," EMOTESHOCK ",testo, perl=perl)

testo <- gsub("[[:blank:]]+"," ",testo, perl=perl)
testo
	}

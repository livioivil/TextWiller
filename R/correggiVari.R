.correggiVari<-function(some_txt){
##some_txt = "ahahahahahahahahahah"
some_txt = gsub("(ha[ha]+)|(ah[ah]+)", "haha", some_txt) #abbrevio gli ha+ e hah+ per? deve essere utilizzata molte volte

### consultata la pagina http://www.treccani.it/enciclopedia/doppie-lettere-prontuario_%28Enciclopedia_dell%27Italiano%29/
### deduco che mantengo le vocali doppie -ii e -oo
### esempio di prova: some_txt="ariiaaaaaaaaaaaaaaa cheeeeeeeeeeeeeeeeeeeee voogliiiiiioooooooooooooooo iiiiiiiiiiiiiiiiooooooooooo eeeeeeeeeeeeee"

some_txt = gsub("a+", "a", some_txt)		# lascio una a
some_txt = gsub("e+", "e", some_txt)		#lascio una e
some_txt = gsub("iii+", "i", some_txt)		#lascio solo una i se ce ne sono pi? di due
some_txt = gsub("ooo+", "o", some_txt)		#tolgo se ho pi? di due oo vicine e ne lascio solo una
some_txt = gsub("u+", "u", some_txt)		#lascio solo una u

some_txt <- gsub("&quot;",'"', some_txt)

some_txt <- gsub("&apos;","'", some_txt)
some_txt <- gsub("&lt;","<", some_txt)
some_txt <- gsub(" amp "," ", some_txt, fixed=T) #problema quando tolgo &

#altro
some_txt <- gsub(" perch[?e?] "," perch? ",some_txt, perl=perl, ignore.case=TRUE)
some_txt <- gsub(" x(ch|k)[?e?] "," perch? ",some_txt, perl=perl, ignore.case=TRUE)
some_txt <- gsub(" nn "," non ",some_txt, perl=perl, ignore.case=TRUE)
some_txt <- gsub(" nun "," non ",some_txt, perl=perl, ignore.case=TRUE)
some_txr <- gsub(" #?([uw]+[ao]+[uw]+) "," EMOTEWOW ",some_txt, perl=perl, ignore.case=TRUE)

}

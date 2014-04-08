.finale<- function(x) {
  
x<-gsub("  $", "", x)#doppi spazi bianchifinali  
x<-gsub("^  ", "", x)#doppi spazi bianchi iniziali
x<-gsub(" $", "", x)#spazi bianchi finali
x<-gsub("^ ", "", x)#spazi bianchi iniziali
x<-gsub("^\\w?$", " ", x)

x = gsub("   ", " ", x)  						#tolgo gli spazi tripli
x = gsub("  ", " ", x)							#tolgo gli spazi doppi


return(x)
}
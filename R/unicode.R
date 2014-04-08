
.clean.unicode <- function(some_txt){
#emoticon da tavola unicode
some_txt <- gsub("<U+201C>",'"', some_txt)
some_txt <- gsub("<U+201D>",'"', some_txt)

some_txt <- gsub("<U+00E8>",'?', some_txt)
some_txt <- gsub("<U+00E9>",'?', some_txt)
some_txt <- gsub("<U+00F9>",'?', some_txt)
some_txt <- gsub("<U+00E0>",'?', some_txt)
some_txt <- gsub("<U+00EC>",'?', some_txt)
some_txt <- gsub("<U+00E7>",'?', some_txt)
some_txt <- gsub("<U+2665>",' EMOTELOVE ', some_txt)
some_txt <- gsub("<U+2794>",' ', some_txt)

some_txt<- gsub("<U+2639>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+263A>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+263B>",' EMOTEGOOD ', some_txt)

some_txt<- gsub("<U+2661>",' EMOTELOVE ', some_txt)
some_txt<- gsub("<U+2665>",' EMOTELOVE ', some_txt)
some_txt<- gsub("<U+2764>",' EMOTELOVE ', some_txt)
some_txt<- gsub("<U+1F499>",' EMOTELOVE ', some_txt)

some_txt<- gsub("<U+1F600>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F601>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F602>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F603>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F604>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F605>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F606>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F607>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F608>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F609>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60A>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60B>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60C>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60D>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60E>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F60F>",' EMOTEGOOD ', some_txt)
some_txt<- gsub("<U+1F610>",'  ', some_txt)	#neutral face
some_txt<- gsub("<U+1F611>",'  ', some_txt)	#neutral face
return(some_txt)
}

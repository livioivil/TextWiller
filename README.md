#TextWiller

Collection of text mining utilities, specially devoted to the italian language.

* * *

## Set up

To **install** this github version type (in R):

    #if devtools is not installed yet: 
    # install.packages("devtools") 
    library(devtools)
    install_github("livioivil/TextWiller")


* * *

## Some examples



```r
library(TextWiller)

### normalize texts
normalizzaTesti(c('ciao bella!','www.associazionerospo.org','noooo, che grandeeeeee!!!!!','mitticooo', 'mai possibile?!?!'))
```

```
## [1] "ciao bello"         "wwwurlwww"          "emotenooo grandeee"
## [4] "mitico"             "mai possibile"     
## attr(,"counts")
##      Conteggi.\\? Conteggi.\\! Conteggi.@ Conteggi.# Conteggi.(€|euro)
## [1,]            0            1          0          0                 0
## [2,]            0            0          0          0                 0
## [3,]            0            5          0          0                 0
## [4,]            0            0          0          0                 0
## [5,]            2            2          0          0                 0
##      Conteggi.(\\$|dollar) Conteggi.SUPPRESSEDTEXT
## [1,]                     0                       0
## [2,]                     0                       0
## [3,]                     0                       0
## [4,]                     0                       0
## [5,]                     0                       0
```

```r
# get the sentiment of a document
sentiment(c("ciao bella!","farabutto!","fofi sei figo!"))
```

```
## ciao bello  farabutto  fofi figo 
##          1         -1          1
```

```r
# Classify users' gender by (italian) names
classificaUtenti(c('livio','alessandra','andrea'))
```

```
##      livio alessandra     andrea 
##     "masc"     "femm"     "masc"
```

```r
# and classify location
data(vocabolarioLuoghi)
classificaUtenti(c('Bosa','Pordenone, Italy','Milan'),vocabolarioLuoghi)
```

```
##             bosa pordenone, italy            milan 
##          "Isole"       "Nord-est"     "Nord-ovest"
```

```r
# find re-twitt (RT) by evaluation of texts similarity (and replace texts so that they become equals):
data(TWsperimentazioneanimale)
RTHound(TWsperimentazioneanimale[1:10,"text"], S = 3, L = 1, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=3)
```

```
## 
##  There will be  2  sliding windows:
## Window # 1
## Window # 2
## Window # 3
##  3  most frequent RTs:
##  (fr 8) Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi.
##  (fr 1) Caterina Simonsen, #animalari e #libertà (Potrebbe essere il nome di un nuovo partito! XD ) #iostoconcaterina http://t.co/Hlz94hfI57
##  (fr 1) @orianoPER: http://t.co/RD5vyvA1Gw dr.ssa S. Penco-Ricercatrice-Premio Nazionale 2013 per la #Ricerca- #vivisezione #sperimentazione #an…
```

```
##                                                                                                                                           1 
## "@orianoPER: http://t.co/RD5vyvA1Gw dr.ssa S. Penco-Ricercatrice-Premio Nazionale 2013 per la #Ricerca- #vivisezione #sperimentazione #an…" 
##                                                                                                                                           2 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           3 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           4 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           5 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           6 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           7 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           8 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                           9 
##     "Hanno augurato la morte a Caterina, la 25enne intubata a favore della sperimentazione animale. E non la auguravano a Bersani? Illusi." 
##                                                                                                                                          10 
##      "Caterina Simonsen, #animalari e #libertà (Potrebbe essere il nome di un nuovo partito! XD ) #iostoconcaterina http://t.co/Hlz94hfI57"
```

```r
#extract short urls and get the long ones
## Not run: urls=urlExtract("Influenza Vaccination | ONS - Oncology Nursing Society http://t.co/924sRKGBU9 See All http://t.co/dbtPJRMl00")

#extract users:
patternExtract(c("@luca @paolo: buon giorno!", "@matteo: a te!"), pattern="@\\w+")
```

```
##   id pattern
## 1  1   @luca
## 2  1  @paolo
## 3  2 @matteo
```


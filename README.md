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
# find re-twitts (RT) by texts similarity:
RTHound(TWsperimentazioneanimale[1:10,"text"], S = 3, L = 1, 
                 hclust.dist = 100, hclust.method = "complete",
                 showTopN=3)
```

```
## Error in iconv(testo, to = "UTF-8"): object 'TWsperimentazioneanimale' not found
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


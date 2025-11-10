

expect_equal(as.vector(sentiment("ciao bella!")==1), TRUE)

expect_equal(all(normalizzaTesti(c("ciao bella!","www.associazionerospo.org"))==c("ciao bello","wwwurlwww")), TRUE)
  
             
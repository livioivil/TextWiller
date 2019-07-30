---
title: 'TextWiller: Collection of functions for text mining, specially devoted to the Italian language'
tags:
  - R package
  - text mining
  - sentiment analysis
  - text normalization
  - users gender and location classification
authors:
 - name: Dario Solari
   orcid: 0000-0003-2901-2155
   affiliation: 1
 - name: Andrea Sciandra
   orcid: 0000-0001-5621-5463
   affiliation: 2
 - name: Livio Finos
   orcid: 0000-0003-3181-8078
   affiliation: 3
affiliations:
 - name: Bee Viva srl
   index: 1
 - name: Department of Communication and Economics, University of Modena and Reggio Emilia
   index: 2
 - name: Department of Developmental Psychology and Socialisation, University of Padova
   index: 3
date: 29 July 2019
bibliography: paper.bib
---

# Summary

``TextWiller`` is the development version of a R package that collects some text mining utilities intended for the Italian language. It's available at https://github.com/livioivil/TextWiller. The aim of ``TextWiller`` is to help to deal with the pre-processing of a corpus and it also provides some functions about word classification and polarity. 

This software is one of the few text mining R packages in Italian language. The main differences compared to other popular packages like ``tm`` is that TextWiller allows sentiment analysis; it includes classification tools for Italian cities and names; and it can help social media researchers with some specific functions for the data extracted from a social networking site via APIs. In particular, ``TextWiller`` allows to normalize (@miner2012practical, @bolasco2013analisi) Italian text, i.e. transforming a corpus in a canonical form, useful for text mining. Normalization includes several functions for removing punctuation, stopwords and for managing:

- upper-lower cases;
- plurals;
- URLs and emoticons recognition;
- some slang expressions (which are brought back to the correct form in Italian).

Specifically, other relevant ``TextWiller`` functions allow to:

- get the sentiment (@wilson2005recognizing, @ceron2014social) of each document in a corpus, based on an internal lexicon or a custom one; 
- classify users' gender by (Italian) names; 
- classify Italian cities into 5 macro-areas (North East, North West, Centre, South, Islands); 
- find re-tweets (``RTHound`` function; @ferraccioli2014topic) by evaluation of texts similarity (and replace texts so that they become equals) through hierarchical clustering on Levenshtein distance (dissimilarity) matrix; 
- extract short URLs and get the long ones; 
- extract users communication pattern, with the ``patternExtract`` function, based on the '@' sign followed by a username, as in several social media platforms and forums it's used to denote a reply.

``TextWiller`` is designed to be used by researchers (mainly statisticians and social scientists) and by students in courses on text mining (it has already been used in several Bachelor and Master's degree theses).

# References

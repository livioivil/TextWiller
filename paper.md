---
title: 'TextWiller: Collection of functions for text mining, specially devoted to the italian language'
tags:
  - R package
  - text mining
  - sentiment analysis
  - text normalization
  - users gender and location classification
authors:
 - name: Dario Solari
   orcid: 0000-0001-5621-5463
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
- name: STAR.Lab - Socio Territorial Analysis and Research, University of Padova
   index: 2
 - name: Department of Developmental Psychology and Socialisation, University of Padova
   index: 3
date: 18 October 2018
bibliography: paper.bib
---

# Summary

TextWiller is the development version of a R package that collect some text mining utilities, specially devoted to the Italian language. It's available at https://github.com/livioivil/TextWiller.

TextWiller allows to:
- normalize (@miner2012practical, @bolasco2013analisi) Italian text (stopwords, lower case, punctuation, plurarls, html, emoticons, slang, etc.);
- get the sentiment (@wilson2005recognizing, @ceron2014social) of a document, based on an internal lexicon or a custom one;
- classify users' gender by (Italian) names;
- classify Italian cities into 5 macro-areas (North East, North West, Centre, South, Islands);
- find re-tweet (@ferraccioli2014topic) by evaluation of texts similarity (and replace texts so that they become equals);
- extract short urls and get the long ones;
- extract users communication pattern.

# References

# BanxicoSentiment
Applying Text Mining algorithms to determine the sentiment in the minutes of Banco de México's monetary policy meetings.

The analysis consists of the application of the financial lexicon developed by Tim Loughran y Bill McDonald to the minutes of each meeting of Banco de México's Governing Board that took place since may 2018. These minutes have been translated to English by the staff of Banco de Mexico, so I was able to use Loughran and McDonald lexicon in its original language. 
The analysis assumes the user already has the pdf minutes in its local system, as well as the list of positive and negative words from Loughran-McDonald lexicon.
Preprocessing consists of extracting the sections of the minutes that are part of the discussion held by the members of Banxico's Governing Body. I decided not to eliminate stopwords nor to use word stemming, as the lexicon already contains the inflexions the authors considered significant. I conducted the analysis over the whole discussion section of the minutes, but also on a subsection level (the discussion section was divided into subsections starting in february 2020).
Processing consists of creating a corpus, and searching for matches of positive and negative words, and calculating a simple score equal to the difference of the count of positive words minus the count of negative words, and dividing this result by the total words (positive, neutral and negative) in the section under analysis. The results are presented in charts.


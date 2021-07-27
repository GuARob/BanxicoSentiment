# BanxicoSentiment
Applying Text Mining algorithms to determine the sentiment in the minutes of Banco de México's monetary policy meetings.

The analysis consists of the application of the financial lexicon developed by Tim Loughran y Bill McDonald to the minutes of each meeting of Banco de México's Governing Board that took place since may 2018. These minutes have been translated to English by the staff of Banco de Mexico, so I was able to use Loughran and McDonald lexicon in its original language. 
The analysis assumes the user has downloaded and installed the required packages, the user has the pdf minutes in its local system, as well as the list of positive and negative words from Loughran-McDonald lexicon. The folder MinutesEng contains the pdf minutes from may 2018 to june 2021, the lists of positive and negative words, and a file names Decisiones.csv which contains additional information about the meetings (for instance the date it was held on, attendants, votes).

Preprocessing consists of extracting the sections of the minutes that are part of the discussion held by the members of Banxico's Governing Body. I decided not to eliminate stopwords nor to use word stemming, as the lexicon already contains the inflections the authors considered significant. I conducted the analysis over the whole discussion section of the minutes, but also on a subsection level (the discussion section was divided into subsections starting in february 2020).
Processing consists of creating a corpus, and searching for matches of positive and negative words, and calculating a simple score equal to the difference of the count of positive words minus the count of negative words, and dividing this result by the total words (positive, neutral and negative) in the section (or subsection) under analysis. The results are presented in charts.


(1) Tim Loughran and Bill McDonald, 2011, When is a Liability not a Liability?  Textual Analysis, Dictionaries, and 10-Ks, Journal of Finance, 66:1, 35-65. (Available at SSRN: http://ssrn.com/abstract=1331573.)

(2) Benoit K, Watanabe K, Wang H, Nulty P, Obeng A, Müller S, Matsuo A (2018). “quanteda: An R package for the quantitative analysis of textual data.” Journal of Open Source Software, 3(30), 774. doi: 10.21105/joss.00774, https://quanteda.io. 

(3) Silge J, Robinson D (2016). “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” JOSS, 1(3). doi: 10.21105/joss.00037, http://dx.doi.org/10.21105/joss.00037. 

(4) Wickham et al., (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686, https://doi.org/10.21105/joss.01686

(5) Thomas J. Leeper (2018). tabulizer: Bindings for Tabula PDF Table Extractor Library. R package

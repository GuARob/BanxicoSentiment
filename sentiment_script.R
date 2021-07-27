

#Loading required libraries
#It is assumed the user already has the packages installed


library(tidyverse)
library(quanteda)
library("tabulizer")
library(tidytext)
library(lubridate)



#Reading the pdf Minutes

setwd("MinutasEng")
archivos <- list.files(pattern = "pdf$")
archivos <- archivos[order(archivos,decreasing=FALSE)]


#Creating an empty tibble
doc_tibble <- tibble(
  meeting = integer(),
  complete_text = character(),
  rationale_text = character(),
  decision_text = character(),
  vote_text = character(),
  dissident_text = character(),
  externo_text = character(),
  econmex_text = character(),
  infmex_text = character(),
  finance_text = character(),
  monetary_text = character()
)


#The text of each pdf Minute file will be read and preprocessed
#The Annex section will be discarded from the analysis (it does not contain opinions from the members
#of the Governing Board of Banco de Mexico)
#The whole text of the Minute starting with the "Analysis and Rationale behind the governing board's voting" section
#will be saved in the doc_tibble
#Additionally, the minute text will be divided into its different sections and subsections (when applicable)
#and the parts will be saved in the doc_tibble


for (i in 1:length(archivos)){
  archivo <- archivos[i]
  fechaReunion <- as.integer(str_remove(archivo,".pdf"))
  texto <- extract_text(archivo, encoding='UTF-8')
  secciones <- str_split(texto, pattern = "\r\n \r\nANNEX \r\n \r\n")
  secciones <- unlist(secciones)
  secciones <- str_split(secciones[1], pattern = "(?=2.\\ ?ANALYSIS AND RATIONALE BEHIND)")
  secciones <- unlist(secciones)
  paginas <- str_split(secciones[2], pattern = "\r\n\\d+ \r\n \r\n")
  paginas <- unlist(paginas)
  numpags <- as.integer(length(paginas))
  lineas <- str_split(paginas,pattern = "\r\n")
  lineas <- unlist(lineas)
  #at this point lineas represent a one-paragraph text with no line breaks
  if (!is.na(lineas)){
    n <- length(lineas)
    lineas <- str_c(lineas[1:n], sep = " ", collapse = " ")
    forewarning <- paste0("FOREWARNING This document is provided for the reader’s convenience only. ",
                          "The translation from the official Spanish version was made by Banco de México’s staff. ",
                          "Discrepancies may arise between the original document in Spanish and its English translation. ",
                          "For this reason, the original Spanish version is the only official document. ", separate = "")
    #forewarning text is eliminated
    lineas <- str_replace(lineas, pattern = forewarning, "")
    #multiple spaces are replaced by a single space
    lineas <- str_replace_all(lineas,regex("[ ]{2,}")," ")
    
    #The different sections and subsections are extracted when applicable -some of them are not mandatory-.
    rationale <- str_extract(lineas, pattern = 
                                  "2.\\ ?ANALYSIS AND RATIONALE BEHIND(.*)(?=3.\\ ?MONETARY POLICY DECISION)")
    rationale <- unlist(rationale)

    
    decision <- str_extract(lineas, pattern = 
                               "3.\\ ?MONETARY POLICY DECISION(.*)(?=4.\\ ?VOTING)")
    decision <- unlist(decision)
    
    
    if (str_detect(lineas,"(5.\\ ?(DISSENTING)? OPINIONS)|(5.\\ ?VIEWPOINTS)|(5.\\ ?DISSENTING VOTE)")) {
      votes <- str_extract(lineas, pattern = 
                                "4.\\ ?VOTING(.*)(?=(5.\\ ?(DISSENTING)? OPINIONS)|(5.\\ ?VIEWPOINTS)|(5.\\ ?DISSENTING VOTE))")
      votes <- unlist(votes)
   
      dis_votes <- str_extract(lineas, pattern = 
                             "((5.\\ ?(DISSENTING)? OPINIONS)|(5.\\ ?VIEWPOINTS)|(5.\\ ?DISSENTING VOTE))(.*)")
      dis_votes <- unlist(dis_votes)
    

    } else {
      votes <- str_extract(lineas, pattern = 
                             "4.\\ ?VOTING(.*)")
      votes <- unlist(votes)
      
      dis_votes <- ""
      
    }
    
    #The subsections of the "Analysis and Rationale..." sections began to appear in the first minute of 2020
    if(fechaReunion > 20191219){    
      ent_ext <- str_extract(lineas, pattern = 
                               " International (E|e)nvironment (.*)(?= Economic (A|a)ctivity in Mexico)")
      ent_ext <- unlist(ent_ext)
          
      act_mex <- str_extract(lineas, pattern = 
                               " Economic (A|a)ctivity in Mexico (.*)(?= Inflation in Mexico)")
      act_mex <- unlist(act_mex)
      
      inf_mex <- str_extract(lineas, pattern = 
                               " Inflation in Mexico (.*)(?= Macrofinancial (E|e)nvironment )")
      inf_mex <- unlist(inf_mex)
      
      macro_env <- str_extract(lineas, pattern = 
                               " Macrofinancial (E|e)nvironment (.*)(?= Monetary (P|p)olicy )")
      macro_env <- unlist(macro_env)
  
      mon_pol <- str_extract(lineas, pattern = 
                               " Monetary (P|p)olicy (.*)(?=3.\\ ?MONETARY POLICY DECISION )")
      mon_pol <- unlist(mon_pol)
    } else {
      ent_ext <- ""
      act_mex <- ""
      inf_mex <- ""
      macro_env <- ""
      mon_pol <- ""
    }

    #doc_tibble gets populated
    z <- tibble(meeting = fechaReunion, complete_text = lineas, rationale_text = rationale, decision_text = decision,
                vote_text = votes, dissident_text = dis_votes, externo_text = ent_ext, econmex_text = act_mex,
                infmex_text = inf_mex, finance_text = macro_env, monetary_text = mon_pol)
    doc_tibble <- bind_rows(doc_tibble, z)
    
  }
}


#Optional
View(doc_tibble)
write.csv(doc_tibble,file="doc_tibble.csv",row.names=FALSE)


#Reading the lists of positive and negative words from Loughran and McDonald financial lexicon
#and creating a dictionary

positivos <- read_csv("Positive.csv", col_names = FALSE)
positivos <- tolower(pull(positivos,"X1"))

negativos <- read_csv("Negative.csv", col_names = FALSE)
negativos <- tolower(pull(negativos,"X1"))

polaridad <- dictionary(list(positivo = positivos,
                         negativo= negativos))

###The Decisiones.csv file contains information regarding the governing board meeting,
###such as the date it was held on, the outcome (rate hike,no change, rate cut), members of the board who attended,
###and their vote (if publicly available)

decisiones <- read_csv("Decisiones.csv", col_names = TRUE)
decisiones$Fecha_Anuncio <- dmy(decisiones$Fecha_Anuncio)


###res_tibble will contain the score of the "Analysis and Rationale..." section and its subsections, which are 
###considered to be topics under analysis (variable: "temas")

temas <- c("rationale_text", "externo_text", "econmex_text", "infmex_text", "finance_text", "monetary_text")
res_tibble <- as.character(doc_tibble$meeting)
res_tibble <- tibble(meeting = res_tibble)

###Each topic is considered to be a corpus, and tokenized
###We did not eliminate stopwords, we did not apply stemming to the words as the lexicon already
###contains inflections of words the authors considered significant
###After the positive and negative words are searched for, a text frequency table is created, which
###is later used to calculate the sentiment score

for (tema in temas){
  textos <- doc_tibble %>%
            select(meeting, text = tema)
  textos$meeting <- as.character(textos$meeting)
  banxico_corpus <- corpus(textos)
  banxico_corpus <- tolower(banxico_corpus)

  toks <- tokens(banxico_corpus, remove_punct=TRUE, remove_separators=TRUE, remove_symbols=TRUE, remove_numbers=TRUE)
  sentiment_toks <- dfm(tokens_lookup(toks, polaridad, valuetype = "glob", verbose = TRUE, nomatch = "Neutral"))
  tstat_freq <- textstat_frequency(sentiment_toks, groups = "meeting", n = 3)

  titulos <- c("meeting", paste0(tema,"_negativo", separate=""), paste0(tema,"_neutral", separate=""),
               paste0(tema,"_positivo", separate=""), paste0(tema,"_score", separate=""))

  #we need to use mutate command to arrange the text frequency table to calculate the sentiment score   
  resultado_tema <- as_tibble(tstat_freq) %>% 
                select(meeting = group, feature, frequency) %>% 
                spread(key = feature, value = frequency) %>%
                mutate(score = (positivo - negativo) / (positivo + negativo + neutral))
  
  names(resultado_tema) <- titulos

  res_tibble <- res_tibble %>%
                left_join(resultado_tema, by = "meeting")

}


#res_tibble contains the scores by each topic, this tibble and
#decisiones are joined to create a tibble with the results and additional data from the meetings

resultados <- res_tibble %>%
  mutate(Fecha_Anuncio = ymd(meeting)) %>%
  left_join(decisiones, by = "Fecha_Anuncio") %>%
  mutate(Resultado = 
           case_when (
             Cambio > 0 ~ "Alza en Tasa",
             Cambio == 0 ~ "Tasa sin Cambio",
             Cambio < 0 ~ "Baja en Tasa"
           ))




##Creating the chart about the overall sentiment tone in the "Analysis and Rationale..." section##
lbls <- paste0(month.abb[month(resultados$Fecha_Anuncio)], " ", lubridate::year(resultados$Fecha_Anuncio))
brks <- resultados$Fecha_Anuncio
bb <- c(3, 4, 5) 
ll <- c("3","4","5")

setwd("..")
chart_one <- resultados %>%
  ggplot(aes(x=Fecha_Anuncio, y=rationale_text_score, fill=Resultado, color=Resultado, size=Votos_Favor)) +
  geom_point(shape=21, stroke=1) +
  labs(title="Tono del Análisis y Motivación de los Votos de los miembros de la Junta de Gobierno en las Minutas de Banco de México", 
       subtitle="Proporción de Palabras Positivas menos Palabras Negativas sobre el Total de Palabras", 
       caption="www.linkedin.com/in/rguzald", 
       y="Proporción",
       x="Fecha de la Reunión") +  
  scale_x_date(labels = lbls, 
               breaks = brks) +  
  scale_y_continuous(limits=c(-0.03, 0)) +
  scale_color_manual(values = c("orange","blue","black")) +
  scale_fill_manual(values = c("orange","blue","white")) +
  scale_size_continuous(name = "Votos a Favor",
                        breaks = bb,
                        limits = c(3, 5),
                        labels = ll,
                        range = c(4, 10)) +
  theme(legend.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())

#Chart is saved
file <- "chart_Tono_General.png"
ggsave(path = "Charts", filename = file, plot = chart_one, device = "png", width=11.5, height=7.67,dpi=300)
unlink(file)



##Creating the chart with the scores by subsection of the "Analysis and Rationale..." section##
res_temas <- resultados %>%
                filter(year(Fecha_Anuncio) >= 2020) %>%
                select(Fecha_Anuncio,externo_text_score, econmex_text_score,
                       infmex_text_score, finance_text_score, monetary_text_score) %>%
                gather(key = "Tema", value = "Score",-Fecha_Anuncio)


lbls_lineas <- c("Actividad Económica en México", "Entorno Externo", "Entorno Macrofinanciero", "Inflación en México",
          "Política Monetaria")

chart_two <- res_temas %>%
  ggplot(aes(x=Fecha_Anuncio, y=Score, color=Tema)) +
  geom_line(size=2) +
  labs(title="Tono por Tema del Análisis y Motivación de los Votos de los miembros de la Junta de Gobierno en las Minutas de Banco de México", 
       subtitle="Proporción de Palabras Positivas menos Palabras Negativas sobre el Total de Palabras de cada Tema", 
       caption="www.linkedin.com/in/rguzald", 
       y="Proporción",
       x="Fecha de la Reunión") +  
  scale_x_date(labels = lbls, 
               breaks = brks) +  
  scale_y_continuous(limits=c(-0.05, 0.05)) +
  scale_color_manual(values = c("orange","blue","green", "red", "purple"),
                     labels = lbls_lineas) +
  theme(legend.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())


#Chart is saved
file <- "chart_Tono_Temas.png"
ggsave(path = "Charts", filename = file, plot = chart_two, device = "png", width=11.5, height=7.67,dpi=300)
unlink(file)

rm(list=ls())











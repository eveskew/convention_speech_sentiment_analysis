# An R project performing sentiment analysis on Republican and Democratic
# National Convention speeches
# 08.09.2016

# Speeches obtained from The American Presidency Project:
# http://www.presidency.ucsb.edu/nomination.php

# Many ideas based on this blog post: 
# http://juliasilge.com/blog/If-I-Loved-NLP-Less/


library(dplyr)
library(tokenizers)
library(stringr)
library(readr)
library(syuzhet)
library(ggplot2)
library(lme4)

#==============================================================================


# Define functions used in downstream analysis


# Take a raw speech as a text file and import it into R, returning a 
# character vector tokenized by word
process_speech <- function(rawspeech) {
  
  read_file(rawspeech) %>%
    tokenize_words() %>%
    unlist()
}


# This function modified from Julia Silge
# Take text and process the sentiment using a given method
process_sentiment <- function(rawtext, mymethod) {
  
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 100)) %>% 
    summarize(text = str_c(x, collapse = " "))
  
  mySentiment <- 
    data.frame(cbind(linenumber = chunkedtext$linenumber, 
                     sentiment = get_sentiment(chunkedtext$text, 
                                               method = mymethod)))
}


# This function modified from Julia Silge
# Chunk the text
chunk_text <- function(rawtext) {
  
  chunkedtext <- data_frame(x = rawtext) %>% 
    group_by(linenumber = ceiling(row_number() / 100)) %>% 
    summarize(text = str_c(x, collapse = " "))
}


# This function modified from Julia Silge
# Plot the text sentiment
plot_sentiment <- function(mySentiment) {
  
  color.vec <- ifelse(mySentiment$sentiment >= 0, 
                      "forestgreen", "firebrick3")
  
  g <- ggplot(data = mySentiment, aes(x = linenumber, y = sentiment)) +
    labs(y = "Sentiment", size = 2) +
    geom_bar(stat = "identity", fill = color.vec) + 
    # geom_segment(data = myAnnotate, aes(x = x, y = y1, xend = x, yend = y2),
    # arrow = arrow(length = unit(0.04, "npc")), 
    # inherit.aes = FALSE) +
    theme_minimal() +
    scale_x_discrete(expand=c(0.02,0)) +
    coord_cartesian(ylim = c(-15, 15)) +
    # theme(plot.caption=element_text(size=8)) +
    theme(axis.text.y=element_text(margin=margin(r=-10))) +
    theme(axis.title.x=element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    theme(axis.text.x=element_blank())
}

#==============================================================================


# Analyze convention speechs from 1980-2016

# For every speech, I process the speech into R from .txt files, chunk the text
# into 100 word chunks, and perform the sentiment analysis using three
# different sentiment methods ('bing', 'afinn', and 'nrc')
# Sentiment plots using the 'bing' method are saved to the 'plots' directory


# RNC 1980
reagan_80_speech <- process_speech("speeches/reagan_1980.txt")
reagan_80.chunked <- chunk_text(reagan_80_speech)
reagan_80.sentiment.bing <- process_sentiment(reagan_80_speech, "bing")
reagan_80.sentiment.afinn <- process_sentiment(reagan_80_speech, "afinn")
reagan_80.sentiment.nrc <- process_sentiment(reagan_80_speech, "nrc")

png(filename = "plots/reagan_80.png", width = 600, height = 500)
p.reagan_80 <- plot_sentiment(reagan_80.sentiment.bing)
p.reagan_80 +
  labs(title = "Sentiment in Reagan's 1980 RNC Speech ('bing' Method)")
dev.off()

mean(reagan_80.sentiment.bing$sentiment)


# DNC 1980
carter_80_speech <- process_speech("speeches/carter_1980.txt")
carter_80.chunked <- chunk_text(carter_80_speech)
carter_80.sentiment.bing <- process_sentiment(carter_80_speech, "bing")
carter_80.sentiment.afinn <- process_sentiment(carter_80_speech, "afinn")
carter_80.sentiment.nrc <- process_sentiment(carter_80_speech, "nrc")

png(filename = "plots/carter_80.png", width = 600, height = 500)
p.carter_80 <- plot_sentiment(carter_80.sentiment.bing)
p.carter_80 +
  labs(title = "Sentiment in Carter's 1980 DNC Speech ('bing' Method)")
dev.off()

mean(carter_80.sentiment.bing$sentiment)


# RNC 1984
reagan_84_speech <- process_speech("speeches/reagan_1984.txt")
reagan_84.chunked <- chunk_text(reagan_84_speech)
reagan_84.sentiment.bing <- process_sentiment(reagan_84_speech, "bing")
reagan_84.sentiment.afinn <- process_sentiment(reagan_84_speech, "afinn")
reagan_84.sentiment.nrc <- process_sentiment(reagan_84_speech, "nrc")

png(filename = "plots/reagan_84.png", width = 600, height = 500)
p.reagan_84 <- plot_sentiment(reagan_84.sentiment.bing)
p.reagan_84 +
  labs(title = "Sentiment in Reagan's 1984 RNC Speech ('bing' Method)")
dev.off()

mean(reagan_84.sentiment.bing$sentiment)


# DNC 1984
mondale_84_speech <- process_speech("speeches/mondale_1984.txt")
mondale_84.chunked <- chunk_text(mondale_84_speech)
mondale_84.sentiment.bing <- process_sentiment(mondale_84_speech, "bing")
mondale_84.sentiment.afinn <- process_sentiment(mondale_84_speech, "afinn")
mondale_84.sentiment.nrc <- process_sentiment(mondale_84_speech, "nrc")

png(filename = "plots/mondale_84.png", width = 600, height = 500)
p.mondale_84 <- plot_sentiment(mondale_84.sentiment.bing)
p.mondale_84 +
  labs(title = "Sentiment in Mondale's 1984 DNC Speech ('bing' Method)")
dev.off()

mean(mondale_84.sentiment.bing$sentiment)


# RNC 1988
bush_88_speech <- process_speech("speeches/bush_1988.txt")
bush_88.chunked <- chunk_text(bush_88_speech)
bush_88.sentiment.bing <- process_sentiment(bush_88_speech, "bing")
bush_88.sentiment.afinn <- process_sentiment(bush_88_speech, "afinn")
bush_88.sentiment.nrc <- process_sentiment(bush_88_speech, "nrc")

png(filename = "plots/bush_88.png", width = 600, height = 500)
p.bush_88 <- plot_sentiment(bush_88.sentiment.bing)
p.bush_88 +
  labs(title = "Sentiment in Bush's 1988 RNC Speech ('bing' Method)")
dev.off()

mean(bush_88.sentiment.bing$sentiment)


# DNC 1988
dukakis_88_speech <- process_speech("speeches/dukakis_1988.txt")
dukakis_88.chunked <- chunk_text(dukakis_88_speech)
dukakis_88.sentiment.bing <- process_sentiment(dukakis_88_speech, "bing")
dukakis_88.sentiment.afinn <- process_sentiment(dukakis_88_speech, "afinn")
dukakis_88.sentiment.nrc <- process_sentiment(dukakis_88_speech, "nrc")

png(filename = "plots/dukakis_88.png", width = 600, height = 500)
p.dukakis_88 <- plot_sentiment(dukakis_88.sentiment.bing)
p.dukakis_88 +
  labs(title = "Sentiment in Dukakis's 1988 DNC Speech ('bing' Method)")
dev.off()

mean(dukakis_88.sentiment.bing$sentiment)


# RNC 1992
bush_92_speech <- process_speech("speeches/bush_1992.txt")
bush_92.chunked <- chunk_text(bush_92_speech)
bush_92.sentiment.bing <- process_sentiment(bush_92_speech, "bing")
bush_92.sentiment.afinn <- process_sentiment(bush_92_speech, "afinn")
bush_92.sentiment.nrc <- process_sentiment(bush_92_speech, "nrc")

png(filename = "plots/bush_92.png", width = 600, height = 500)
p.bush_92 <- plot_sentiment(bush_92.sentiment.bing)
p.bush_92 +
  labs(title = "Sentiment in Bush's 1992 RNC Speech ('bing' Method)")
dev.off()

mean(bush_92.sentiment.bing$sentiment)


# DNC 1992
clinton_92_speech <- process_speech("speeches/clinton_1992.txt")
clinton_92.chunked <- chunk_text(clinton_92_speech)
clinton_92.sentiment.bing <- process_sentiment(clinton_92_speech, "bing")
clinton_92.sentiment.afinn <- process_sentiment(clinton_92_speech, "afinn")
clinton_92.sentiment.nrc <- process_sentiment(clinton_92_speech, "nrc")

png(filename = "plots/clinton_92.png", width = 600, height = 500)
p.clinton_92 <- plot_sentiment(clinton_92.sentiment.bing)
p.clinton_92 +
  labs(title = "Sentiment in Clinton's 1992 DNC Speech ('bing' Method)")
dev.off()

mean(clinton_92.sentiment.bing$sentiment)


# RNC 1996
dole_96_speech <- process_speech("speeches/dole_1996.txt")
dole_96.chunked <- chunk_text(dole_96_speech)
dole_96.sentiment.bing <- process_sentiment(dole_96_speech, "bing")
dole_96.sentiment.afinn <- process_sentiment(dole_96_speech, "afinn")
dole_96.sentiment.nrc <- process_sentiment(dole_96_speech, "nrc")

png(filename = "plots/dole_96.png", width = 600, height = 500)
p.dole_96 <- plot_sentiment(dole_96.sentiment.bing)
p.dole_96 +
  labs(title = "Sentiment in Dole's 1996 RNC Speech ('bing' Method)")
dev.off()

mean(dole_96.sentiment.bing$sentiment)


# DNC 1996
clinton_96_speech <- process_speech("speeches/clinton_1996.txt")
clinton_96.chunked <- chunk_text(clinton_96_speech)
clinton_96.sentiment.bing <- process_sentiment(clinton_96_speech, "bing")
clinton_96.sentiment.afinn <- process_sentiment(clinton_96_speech, "afinn")
clinton_96.sentiment.nrc <- process_sentiment(clinton_96_speech, "nrc")

png(filename = "plots/clinton_96.png", width = 600, height = 500)
p.clinton_96 <- plot_sentiment(clinton_96.sentiment.bing)
p.clinton_96 +
  labs(title = "Sentiment in Clinton's 1996 DNC Speech ('bing' Method)")
dev.off()

mean(clinton_96.sentiment.bing$sentiment)


# RNC 2000
bush_00_speech <- process_speech("speeches/bush_2000.txt")
bush_00.chunked <- chunk_text(bush_00_speech)
bush_00.sentiment.bing <- process_sentiment(bush_00_speech, "bing")
bush_00.sentiment.afinn <- process_sentiment(bush_00_speech, "afinn")
bush_00.sentiment.nrc <- process_sentiment(bush_00_speech, "nrc")

png(filename = "plots/bush_00.png", width = 600, height = 500)
p.bush_00 <- plot_sentiment(bush_00.sentiment.bing)
p.bush_00 +
  labs(title = "Sentiment in Bush's 2000 RNC Speech ('bing' Method)")
dev.off()

mean(bush_00.sentiment.bing$sentiment)


# DNC 2000
gore_00_speech <- process_speech("speeches/gore_2000.txt")
gore_00.chunked <- chunk_text(gore_00_speech)
gore_00.sentiment.bing <- process_sentiment(gore_00_speech, "bing")
gore_00.sentiment.afinn <- process_sentiment(gore_00_speech, "afinn")
gore_00.sentiment.nrc <- process_sentiment(gore_00_speech, "nrc")

png(filename = "plots/gore_00.png", width = 600, height = 500)
p.gore_00 <- plot_sentiment(gore_00.sentiment.bing)
p.gore_00 +
  labs(title = "Sentiment in Gore's 2000 DNC Speech ('bing' Method)")
dev.off()

mean(gore_00.sentiment.bing$sentiment)


# RNC 2004
bush_04_speech <- process_speech("speeches/bush_2004.txt")
bush_04.chunked <- chunk_text(bush_04_speech)
bush_04.sentiment.bing <- process_sentiment(bush_04_speech, "bing")
bush_04.sentiment.afinn <- process_sentiment(bush_04_speech, "afinn")
bush_04.sentiment.nrc <- process_sentiment(bush_04_speech, "nrc")

png(filename = "plots/bush_04.png", width = 600, height = 500)
p.bush_04 <- plot_sentiment(bush_04.sentiment.bing)
p.bush_04 +
  labs(title = "Sentiment in Bush's 2004 RNC Speech ('bing' Method)")
dev.off()

mean(bush_04.sentiment.bing$sentiment)


# DNC 2004
kerry_04_speech <- process_speech("speeches/kerry_2004.txt")
kerry_04.chunked <- chunk_text(kerry_04_speech)
kerry_04.sentiment.bing <- process_sentiment(kerry_04_speech, "bing")
kerry_04.sentiment.afinn <- process_sentiment(kerry_04_speech, "afinn")
kerry_04.sentiment.nrc <- process_sentiment(kerry_04_speech, "nrc")

png(filename = "plots/kerry_04.png", width = 600, height = 500)
p.kerry_04 <- plot_sentiment(kerry_04.sentiment.bing)
p.kerry_04 +
  labs(title = "Sentiment in Kerry's 2004 DNC Speech ('bing' Method)")
dev.off()

mean(kerry_04.sentiment.bing$sentiment)


# RNC 2008
mccain_08_speech <- process_speech("speeches/mccain_2008.txt")
mccain_08.chunked <- chunk_text(mccain_08_speech)
mccain_08.sentiment.bing <- process_sentiment(mccain_08_speech, "bing")
mccain_08.sentiment.afinn <- process_sentiment(mccain_08_speech, "afinn")
mccain_08.sentiment.nrc <- process_sentiment(mccain_08_speech, "nrc")

png(filename = "plots/mccain_08.png", width = 600, height = 500)
p.mccain_08 <- plot_sentiment(mccain_08.sentiment.bing)
p.mccain_08 +
  labs(title = "Sentiment in McCain's 2008 RNC Speech ('bing' Method)")
dev.off()

mean(mccain_08.sentiment.bing$sentiment)


# DNC 2008
obama_08_speech <- process_speech("speeches/obama_2008.txt")
obama_08.chunked <- chunk_text(obama_08_speech)
obama_08.sentiment.bing <- process_sentiment(obama_08_speech, "bing")
obama_08.sentiment.afinn <- process_sentiment(obama_08_speech, "afinn")
obama_08.sentiment.nrc <- process_sentiment(obama_08_speech, "nrc")

png(filename = "plots/obama_08.png", width = 600, height = 500)
p.obama_08 <- plot_sentiment(obama_08.sentiment.bing)
p.obama_08 +
  labs(title = "Sentiment in Obama's 2008 DNC Speech ('bing' Method)")
dev.off()

mean(obama_08.sentiment.bing$sentiment)


# RNC 2012
romney_12_speech <- process_speech("speeches/romney_2012.txt")
romney_12.chunked <- chunk_text(romney_12_speech)
romney_12.sentiment.bing <- process_sentiment(romney_12_speech, "bing")
romney_12.sentiment.afinn <- process_sentiment(romney_12_speech, "afinn")
romney_12.sentiment.nrc <- process_sentiment(romney_12_speech, "nrc")

png(filename = "plots/romney_12.png", width = 600, height = 500)
p.romney_12 <- plot_sentiment(romney_12.sentiment.bing)
p.romney_12 +
  labs(title = "Sentiment in Romney's 2012 RNC Speech ('bing' Method)")
dev.off()

mean(romney_12.sentiment.bing$sentiment)


# DNC 2012
obama_12_speech <- process_speech("speeches/obama_2012.txt")
obama_12.chunked <- chunk_text(obama_12_speech)
obama_12.sentiment.bing <- process_sentiment(obama_12_speech, "bing")
obama_12.sentiment.afinn <- process_sentiment(obama_12_speech, "afinn")
obama_12.sentiment.nrc <- process_sentiment(obama_12_speech, "nrc")

png(filename = "plots/obama_12.png", width = 600, height = 500)
p.obama_12 <- plot_sentiment(obama_12.sentiment.bing)
p.obama_12 +
  labs(title = "Sentiment in Obama's 2012 DNC Speech ('bing' Method)")
dev.off()

mean(obama_12.sentiment.bing$sentiment)


# RNC 2016
trump_16_speech <- process_speech("speeches/trump_2016.txt")
trump_16.chunked <- chunk_text(trump_16_speech)
trump_16.sentiment.bing <- process_sentiment(trump_16_speech, "bing")
trump_16.sentiment.afinn <- process_sentiment(trump_16_speech, "afinn")
trump_16.sentiment.nrc <- process_sentiment(trump_16_speech, "nrc")

png(filename = "plots/trump_16.png", width = 600, height = 500)
p.trump_16 <- plot_sentiment(trump_16.sentiment.bing)
myAnnotate <- 
  data.frame(x = c(11, 46), y = rep(14, 2), 
             label = c('"death, destruction, terrorism, and weakness"', 
                       "Mentions children, wife, father"), 
             y1 = rep(13.5, 2), y2 = c(0.2, 10.2))
p.trump_16 +
  labs(title = "Sentiment in Trump's 2016 RNC Speech ('bing' Method)") +
  geom_label(data = myAnnotate, aes(x, y, label = label), hjust = 0.5, 
             label.size = 0, size = 3.2, color="#2b2b2b", 
             inherit.aes = FALSE) +
  geom_segment(data = myAnnotate, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.04, "npc")), inherit.aes = FALSE)
dev.off()

mean(trump_16.sentiment.bing$sentiment)


# DNC 2016
clinton_h_16_speech <- process_speech("speeches/clinton_h_2016.txt")
clinton_h_16.chunked <- chunk_text(clinton_h_16_speech)
clinton_h_16.sentiment.bing <- process_sentiment(clinton_h_16_speech, "bing")
clinton_h_16.sentiment.afinn <- process_sentiment(clinton_h_16_speech, "afinn")
clinton_h_16.sentiment.nrc <- process_sentiment(clinton_h_16_speech, "nrc")

png(filename = "plots/clinton_h_16.png", width = 600, height = 500)
myAnnotate <- 
  data.frame(x = c(11, 15.5), y = c(14, 12), 
             label = c("Refuting Trump RNC claims", 
                       '"love trumps hate"'), 
             y1 = c(13.5, 11.5), y2 = c(0.2, 9.2))
p.clinton_h_16 <- plot_sentiment(clinton_h_16.sentiment.bing)
p.clinton_h_16 +
  labs(title = "Sentiment in Clinton's 2016 DNC Speech ('bing' Method)") +
  geom_label(data = myAnnotate, aes(x, y, label = label), hjust = 0.5, 
             label.size = 0, size = 3.2, color="#2b2b2b", 
             inherit.aes = FALSE) +
  geom_segment(data = myAnnotate, aes(x = c(11, 14), y = y1, 
                                      xend = c(11, 14), yend = y2),
               arrow = arrow(length = unit(0.04, "npc")), inherit.aes = FALSE)
dev.off()

mean(clinton_h_16.sentiment.bing$sentiment) 

#==============================================================================


# Set up mean speech sentiment dataframes using the three different 
# sentiment metrics


d.bing <- data.frame(
  rep(c("Republican", "Democrat"), 10),
  rep(c("red", "blue"), 10),
  rep(c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016), each = 2),
  c(mean(reagan_80.sentiment.bing$sentiment), 
    mean(carter_80.sentiment.bing$sentiment), 
    mean(reagan_84.sentiment.bing$sentiment), 
    mean(mondale_84.sentiment.bing$sentiment), 
    mean(bush_88.sentiment.bing$sentiment), 
    mean(dukakis_88.sentiment.bing$sentiment), 
    mean(bush_92.sentiment.bing$sentiment), 
    mean(clinton_92.sentiment.bing$sentiment), 
    mean(dole_96.sentiment.bing$sentiment), 
    mean(clinton_96.sentiment.bing$sentiment), 
    mean(bush_00.sentiment.bing$sentiment), 
    mean(gore_00.sentiment.bing$sentiment), 
    mean(bush_04.sentiment.bing$sentiment), 
    mean(kerry_04.sentiment.bing$sentiment), 
    mean(mccain_08.sentiment.bing$sentiment), 
    mean(obama_08.sentiment.bing$sentiment), 
    mean(romney_12.sentiment.bing$sentiment), 
    mean(obama_12.sentiment.bing$sentiment),
    mean(trump_16.sentiment.bing$sentiment), 
    mean(clinton_h_16.sentiment.bing$sentiment)),
  c(0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1),
  c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, NA, NA))
colnames(d.bing) <- 
  c("Party", "Color", "Year", "Sentiment", "Incumbent", "Winner")


d.afinn <- data.frame(
  rep(c("Republican", "Democrat"), 10),
  rep(c("red", "blue"), 10),
  rep(c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016), each = 2),
  c(mean(reagan_80.sentiment.afinn$sentiment), 
    mean(carter_80.sentiment.afinn$sentiment), 
    mean(reagan_84.sentiment.afinn$sentiment), 
    mean(mondale_84.sentiment.afinn$sentiment), 
    mean(bush_88.sentiment.afinn$sentiment), 
    mean(dukakis_88.sentiment.afinn$sentiment), 
    mean(bush_92.sentiment.afinn$sentiment), 
    mean(clinton_92.sentiment.afinn$sentiment), 
    mean(dole_96.sentiment.afinn$sentiment), 
    mean(clinton_96.sentiment.afinn$sentiment), 
    mean(bush_00.sentiment.afinn$sentiment), 
    mean(gore_00.sentiment.afinn$sentiment), 
    mean(bush_04.sentiment.afinn$sentiment), 
    mean(kerry_04.sentiment.afinn$sentiment), 
    mean(mccain_08.sentiment.afinn$sentiment), 
    mean(obama_08.sentiment.afinn$sentiment), 
    mean(romney_12.sentiment.afinn$sentiment), 
    mean(obama_12.sentiment.afinn$sentiment),
    mean(trump_16.sentiment.afinn$sentiment), 
    mean(clinton_h_16.sentiment.afinn$sentiment)),
  c(0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1),
  c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, NA, NA))
colnames(d.afinn) <- 
  c("Party", "Color", "Year", "Sentiment", "Incumbent", "Winner")


d.nrc <- data.frame(
  rep(c("Republican", "Democrat"), 10),
  rep(c("red", "blue"), 10),
  rep(c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016), each = 2),
  c(mean(reagan_80.sentiment.nrc$sentiment), 
    mean(carter_80.sentiment.nrc$sentiment), 
    mean(reagan_84.sentiment.nrc$sentiment), 
    mean(mondale_84.sentiment.nrc$sentiment), 
    mean(bush_88.sentiment.nrc$sentiment), 
    mean(dukakis_88.sentiment.nrc$sentiment), 
    mean(bush_92.sentiment.nrc$sentiment), 
    mean(clinton_92.sentiment.nrc$sentiment), 
    mean(dole_96.sentiment.nrc$sentiment), 
    mean(clinton_96.sentiment.nrc$sentiment), 
    mean(bush_00.sentiment.nrc$sentiment), 
    mean(gore_00.sentiment.nrc$sentiment), 
    mean(bush_04.sentiment.nrc$sentiment), 
    mean(kerry_04.sentiment.nrc$sentiment), 
    mean(mccain_08.sentiment.nrc$sentiment), 
    mean(obama_08.sentiment.nrc$sentiment), 
    mean(romney_12.sentiment.nrc$sentiment), 
    mean(obama_12.sentiment.nrc$sentiment),
    mean(trump_16.sentiment.nrc$sentiment), 
    mean(clinton_h_16.sentiment.nrc$sentiment)),
  c(0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1),
  c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, NA, NA))
colnames(d.nrc) <- 
  c("Party", "Color", "Year", "Sentiment", "Incumbent", "Winner")

#==============================================================================


# Plotting


# Mean speech sentiment over time ('bing' method)
png(filename = "plots/speeches_regression_bing.png", 
    width = 600, height = 500)
myAnnotate <- 
  data.frame(x = c(1988, 2012, 2012, 2015.5), y = c(3.35, 0.83, 3.6, 0.38), 
             label = c("M. Dukakis", "B. Obama", "M. Romney", "D. Trump"), 
             y1 = rep(13.5, 2), y2 = c(0.2, 8.2))
ggplot(data = d.bing, aes(x = Year, y = Sentiment)) +
  geom_point(color = d.bing$Color, size = 4) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  ggtitle("Sentiment of RNC and DNC Speeches ('bing' Method)") +
  ylab("Mean Sentiment of the Convention Speech") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 4)) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 4)) +
  geom_label(data = myAnnotate, aes(x, y, label = label), hjust = 0.5, 
             label.size = 0, size = 3.5, color="#2b2b2b", 
             inherit.aes = FALSE)
dev.off()


# Mean speech sentiment over time ('afinn' method)
png(filename = "plots/speeches_regression_afinn.png", 
    width = 600, height = 500)
myAnnotate <- 
  data.frame(x = c(1984, 1988, 2012, 2015.5), y = c(1.5, 7.26, 6.3, 0), 
             label = c("W. Mondale", "M. Dukakis", "M. Romney", "D. Trump"), 
             y1 = rep(13.5, 2), y2 = c(0.2, 8.2))
ggplot(data = d.afinn, aes(x = Year, y = Sentiment)) +
  geom_point(color = d.afinn$Color, size = 4) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  ggtitle("Sentiment of RNC and DNC Speeches ('afinn' Method)") +
  ylab("Mean Sentiment of the Convention Speech") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 4)) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 8)) +
  geom_label(data = myAnnotate, aes(x, y, label = label), hjust = 0.5, 
             label.size = 0, size = 3.5, color="#2b2b2b", 
             inherit.aes = FALSE)
dev.off()


# Mean speech sentiment over time ('nrc' method)
png(filename = "plots/speeches_regression_nrc.png", 
    width = 600, height = 500)
myAnnotate <- 
  data.frame(x = c(1988, 1992, 2004, 2008), y = c(5.29, 0.7, 5.57, 1.84), 
             label = c("M. Dukakis", "G. H. W. Bush", 
                       "G. W. Bush", "B. Obama"), 
             y1 = rep(13.5, 2), y2 = c(0.2, 8.2))
ggplot(data = d.nrc, aes(x = Year, y = Sentiment)) +
  geom_point(color = d.nrc$Color, size = 4) +
  geom_smooth(method = "lm", se = F, color = "black", size = 0.5) +
  ggtitle("Sentiment of RNC and DNC Speeches ('nrc' Method)") +
  ylab("Mean Sentiment of the Convention Speech") +
  scale_x_continuous(breaks = seq(from = 1980, to = 2016, by = 4)) +
  theme_bw() +
  coord_cartesian(ylim = c(0, 6)) +
  geom_label(data = myAnnotate, aes(x, y, label = label), hjust = 0.5, 
             label.size = 0, size = 3.5, color="#2b2b2b", 
             inherit.aes = FALSE)
dev.off()


# November winner vs. mean speech sentiment
png(filename = "plots/winner_v_sentiment.png", width = 600, height = 500)
ggplot(data = d.bing, aes(x = Sentiment, y = Winner, group = Year)) +
  geom_point(color = d.bing$Color, size = 4) +
  geom_line(size = 0.5, linetype = 3) +
  ylab("Winner in November?") +
  xlab("Mean Sentiment of the Convention Speech") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme_bw() +
  coord_cartesian(xlim = c(0.5, 3.5))
dev.off()

# A GLMM addressing the same question
glmer(Winner ~ Sentiment + (1|Year), data = d.bing, family = binomial)


# Filter to only the challenging parties
filter(d.bing, Incumbent == 0) %>%
  ggplot(aes(x = Sentiment, y = Winner)) +
  geom_point(size = 4) +
  ylab("Winner in November?") +
  xlab("Mean Sentiment of the Convention Speech") +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme_bw() +
  coord_cartesian(xlim = c(0.5, 3.5))


# Mean speech sentiment vs. incumbent status
png(filename = "plots/sentiment_v_incumbent.png", width = 600, height = 500)
ggplot(data = d.bing, aes(x = Incumbent, y = Sentiment, group = Year)) +
  geom_point(color = d.bing$Color, size = 4) +
  geom_line(size = 0.5, linetype = 3) +
  ylab("Mean Sentiment of the Convention Speech") +
  xlab("Candidate from the Incumbent Party?") +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.25, 1.25), ylim = c(0.5, 3.5))
dev.off()

# A LMM addressing the same question
lmer(Sentiment ~ Incumbent + (1|Year), data = d.bing)
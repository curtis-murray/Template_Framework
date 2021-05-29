# sentiment_analysis -------------------------------------------------
#
# Author: Curtis Murray
# Date:	 
# Description:
#   Adapted from Sentiment Analysis on r/Covid19Positive
#   
#   (1) 
#
#
# TODO:
#
# Log:
# Libs -----------------------------------------------------------
library(tidyverse)
library(tidytext)
library(broom)
library(data.table)
library(lubridate)
max_day <- 14

nrc <- get_sentiments("nrc")
colour_pallete <- read_csv("../COVID/data/colour_pallete.csv")

named_colours <- colour_pallete %>% 
	spread(sentiment, colour)

clean <- read_csv("data/clean_posts.csv") %>% 
	mutate(Date = as_date(Date))%>%
	mutate(Year = Date %>% year %>% as.character) %>%
	mutate(Month = Date %>% month %>% as.character) %>%
	mutate(Date = floor_date(Date, "months"))

sent_nrc_template <- clean %>% 
	mutate(sentiment = list(c(nrc %>% group_by(sentiment) %>% summarise() %>% pull(sentiment)))) %>% 
	unnest() %>% 
	mutate(p = 0)

sent_clean <- clean %>% 
	unnest_tokens(word, Content) %>% 
	inner_join(nrc, by = "word") %>% 
	group_by(Post_ID, Year, Month, Date, sentiment) %>% 
	summarise(count = n()) %>% 
	group_by(Post_ID, Date) %>% 
	mutate(p = count/sum(count)) %>% 
	ungroup() %>% 
	bind_rows(sent_nrc_template) %>% 
	group_by(Post_ID, Year, Month, Date, sentiment) %>% 
	summarise(p = sum(p)) %>% 
	ungroup() %>% 
	drop_na()

sent_clean_path <- paste("data/sent_clean.csv")
write_csv(sent_clean, sent_clean_path)

# p_sent_nrc <- sent_clean %>% 
# 	ggplot() + 
# 	theme_minimal() + 
# 	#geom_jitter(aes(Day, p, color = sentiment), alpha = 0.05) + 
# 	geom_smooth(aes(Day, p, color = sentiment),  
# 							method = "loess") +
# 	theme(legend.position = "none") + 
# 	facet_wrap(~sentiment,
# 						 ncol = 3) +
# 	labs(x = "Day", y = "Sentiment proportion") +
# 	ylim(c(0,0.5)) +
# 	xlim(c(1,max_day))

# p_sent_nrc <- sent_clean %>% 
# 	left_join(colour_pallete) %>% 
# 	mutate(colour = str_replace(colour, "#", "")) %>% 
# 	arrange(sentiment) %>% 
# 	#filter(Day >= 1, Day <= 14) %>% 
# 	ggplot() + 
# 	theme_minimal() + 
# 	#geom_jitter(aes(Day, p, color = sentiment), alpha = 0.05) + 
# 	geom_smooth(aes(Day, p, color = colour, linetype = sentiment),  
# 							method = "loess", se = F) +
# 	geom_text(data = group_by(., colour) %>% 
# 							do(augment(loess(y~x, .))) %>% 
# 							filter(x == max(p)),
# 						aes(x, .fitted), nudge_x = 5)
# theme(legend.position = "bottom") + 
# 	labs(x = "Day", y = "Sentiment proportion") + 
# 	scale_x_continuous(limits = c(1,14), breaks = 1:14, labels = 1:14) +
# 	labs(color = "", linetype = "") + 
# 	#scale_color_discrete(labels = colour_pallete %>% pull(sentiment)) + 
# 	scale_color_manual(values = colour_pallete %>% pull(colour)) + 
# 	theme(legend.key.width=unit(2,"cm"))

clean %>% 
	unnest_tokens(word, Content) %>% 
	inner_join(nrc, by = "word") %>% 
	group_by(Post_ID, sentiment) %>% 
	summarise(count = n()) %>% 
	group_by(Post_ID) %>% 
	mutate(p = count/sum(count)) %>% 
	bind_rows(sent_nrc_template %>% select(sentiment, Post_ID,p)) %>% 
	group_by(Post_ID, sentiment) %>% 
	summarise(p = sum(p)) %>% 
	group_by(Post_ID) %>% 
	filter(sum(p) > 0) %>% 
	group_by(sentiment) %>% 
	summarise(p = mean(p))

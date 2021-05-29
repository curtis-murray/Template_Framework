library(tidyverse)
library(wordcloud)
library(ggwordcloud)
library(tidytext)
library(quantreg)
library(ggforce)
library(scales)
library(cowplot)
library(lubridate)
# Loading Data----------------------------------------------------

clean_posts <- read_csv("data/clean_posts.csv")

tibble(date = today()) %>% 
	write_csv("shiny/data/data_date.csv")

fnames <- 
	c(
		"data/clean_posts.csv",
	)

file.copy(fnames, "shiny/data", overwrite = T)


# Auto LOESS -----------------------
aicc.loess <- function(fit) {
	stopifnot(inherits(fit, 'loess'))
	# parameters
	n <- fit$n
	trace <- fit$trace.hat
	sigma2 <- sum(resid(fit) ^ 2) / (n - 1)
	return(log(sigma2) + 1 + (2 * (trace + 1)) / (n - trace - 2))
}

topic_level <- "0"

# max level will be the highest level created at most 100 seconds before latest variable
max_level <- tibble(path = list.files("data/Topic_Model", full.names = T,recursive = F,include.dirs = F)) %>% 
	mutate(time = map(
		path, ~file.info(.x)$ctime
	)) %>% 
	unnest(time) %>% 
	filter(str_detect(path,"level_")) %>% 
	mutate(level = str_extract(path, "\\d")) %>% 
	mutate(path = str_remove(path, "\\d")) %>% 
	group_by(path) %>% 
	mutate(maxtime = max(time)) %>% 
	mutate(dif = time-maxtime) %>% 
	filter(dif > -100) %>%
	ungroup() %>% 
	filter(level == max(level)) %>% 
	group_by(level) %>% 
	summarise()

write_csv(max_level, "data/Topic_Model/max_level.csv")

tail(file.info("data/Topic_Model/")$ctime)

for(topic_level in as.character(0:max_level$level)){

p_w_tw_path <- paste("data/Topic_Model/", "p_w_tw_level_",topic_level, ".csv", sep = "")
p_tw_d_path <- paste("data/Topic_Model/", "p_tw_d_level_", topic_level, ".csv", sep = "")
words_all_path <- paste("data/Topic_Model/","words_all.csv", sep = "")
docs_all_path <- paste("data/Topic_Model/","documents_all.csv", sep = "")

p_w_tw <- read_csv(p_w_tw_path) %>%
	select(word_ID = X1, everything()) %>%
	mutate(word_ID = word_ID + 1)

p_tw_d <- read_csv(p_tw_d_path) %>%
	select(topic = X1, everything()) %>%
	mutate(topic = topic + 1)

words_all <- read_csv(words_all_path) %>% 
	mutate(word_ID = X1+1) %>% 
	select(word_ID, word = `0`)

docs_all <- read_csv(docs_all_path) %>% 
	mutate(doc_ID = X1+1) %>% 
	select(doc_ID, doc = `0`)

tidy_topics <- p_w_tw %>% 
	gather("topic", "p", -word_ID) %>% 
	mutate(topic = as.numeric(topic) + 1) %>% 
	filter(p > 0) %>% 
	full_join(words_all, by = "word_ID")

n_topics <- tidy_topics %>% pull(topic) %>% unique() %>% length()

tidy_topic_docs <- p_tw_d %>% 
	gather("doc_ID", "p", -1) %>% 
	mutate(doc_ID = as.numeric(doc_ID) + 1) %>% 
	full_join(docs_all, by = "doc_ID") %>% 
	left_join(clean_posts, by = c("doc" = "Post_ID")) %>% 
	select(topic, doc_ID, p, doc, Sub, Date)

tidy_topics_path <- paste("data/Topic_Model/Clean/tidy_topics_", topic_level, ".csv",sep = "")
write_csv(tidy_topics, tidy_topics_path)

tidy_topic_docs_path <- paste("data/Topic_Model/Clean/tidy_topics_docs", topic_level, ".csv",sep = "")
write_csv(tidy_topic_docs, tidy_topic_docs_path)

ttdp_small <- paste("~/Dropbox/PhD/shiny/data/tidy_topics_docs", topic_level, ".csv",sep = "")
ttp_small <- paste("~/Dropbox/PhD/shiny/data/tidy_topics_", topic_level, ".csv",sep = "")

tidy_topic_docs %>%
	select(topic, p, Sub, Date) %>%
	mutate(Date = floor_date(Date, unit = "day")) %>% 
	group_by(topic, Sub, Date) %>%
	summarise(p = mean(p), count = n()) %>%
	write_csv(ttdp_small)

tidy_topics %>% 
	select(topic, p, word) %>% 
	write_csv(ttp_small)

}

do_auto_loess <- function(my_df){
	
	fit <- loess(
		p ~ as.numeric(Date),
		data = my_df
	) 
	
	span=c(.1, .9)
	stopifnot(inherits(fit, 'loess'), length(span) == 2)
	# loss function in form to be used by optimize
	f <- function(span){
		aicc.loess(update(fit, span=span))
	}
	# find minimum span
	min_span <- optimize(f, span)$minimum
	# find best loess according to loss function
	fit <- update(fit, span = min_span)
	
	tibble(fit = fit$fitted) %>% 
		return()
}

make_reddit_sub_plots <- function(track_subs = c("scrape_subs_")){
	
	
	topic_from_word <- function(my_word){
		tidy_topics %>%
			filter(word == my_word) %>% 
			pull(topic) %>% 
			return()
	}
	
	plot_list <- tidy_topic_docs %>% 
		filter(topic == topic_from_word(my_word)) %>% 
		mutate(Sub = ifelse(Sub %in% track_subs, Sub, "Other")) %>% 
		group_by(topic, Sub) %>% 
		nest() %>% 
		mutate(
			fit = map(
				data,
				do_auto_loess
			)
		) %>% 
		unnest(data, fit) %>% 
		ungroup() %>% 
		group_by(topic) %>% 
		nest() %>% 
		mutate(
			density_plot = map(
				data,
				function(data){
					data %>% 
						drop_na() %>% 
						ggplot() +
						theme_minimal() + 
						geom_line(aes(x = as_datetime(Date), y = fit, color = Sub)) +
						theme(legend.position = "none") + 
						labs(x = "Date", y = "Topic density") +
						labs(title = paste("Topic: ", topic)) + 
						#	coord_cartesian(ylim = c(0,NA), expand = F) + 
						scale_x_datetime(date_breaks = "months", date_labels = "%b") + 
						ggrepel::geom_label_repel(data = data %>%
																				group_by(Sub) %>% 
																				filter(Date == max(Date)) %>% 
																				group_by(Sub, fit, Date) %>% 
																				summarise(),
																			aes(x = Date, y = fit, label = Sub, colour = Sub),
																			nudge_x = 6) +
						lims(y = c(0,NA))
					
				}
				
			)
		) %>% 
		left_join(tidy_topics %>% 
								group_by(topic) %>% 
								nest() %>% 
								select(topic, topic_data = data),
							by = "topic") %>% 
		mutate(
			word_cloud = 
				map(
					topic_data,
					function(data){
						data %>% 
							top_n(50,p) %>% 
							mutate(p = p/max(p)) %>% 
							ungroup() %>% 
							ggplot(
								aes(
									label = word, size = p,
									color = p
								)
							) +
							geom_text_wordcloud_area(rm_outside = T) +
							#scale_size_area(max_size = ) +
							theme_minimal() +
							labs(title = paste("Topic: ", topic))
					}
				)
		) %>% 
		mutate(
			plots = 
				map2(
					word_cloud, density_plot,
					function(p1,p2){
						list(p1,p2) %>% 
							return()
					}
				)
		) %>% 
		select(topic, plots) %>% 
		unnest(plots) %>% 
		pull(plots) 
	
	all_p <- plot_grid(plotlist = plot_list, ncol = 2)
	
	return(all_p)
	
}




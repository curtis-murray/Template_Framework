library(lubridate)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(highcharter)
library(htmltools)
library(htmlwidgets)

paths <- list.files(path = "data/Scrape/", pattern="*.csv")
#paths <- list.files(path = "data/Scrape/", pattern="*.csv")


Posts <- tibble(path = paths) %>% 
	mutate(Sub = str_replace(paths, ".csv", "")) %>% 
	mutate(path = paste("data/Scrape/", path, sep = "")) %>% 
	#mutate(path = paste("data/Scrape/", path, sep = "")) %>% 
	group_by(Sub) %>% 
	mutate(posts = map("data", ~read_csv(path))) %>% 
	unnest() %>% 
	select(-path) %>% 
	ungroup()

more_stop_words <- c("im", "didnt", "shouldnt", "cant", "wont", "amp",
										 "https", "http", "x200b", "www.reddit.com",
										 "utm_name", "ios_app", "utm_medium")


clean_posts <- Posts %>% 
	group_by_all() %>% 
	summarise() %>% 
	ungroup() %>% 
	filter(!is.na(Content)) %>% 
	mutate(Content = str_replace_all(Content, "’", "'")) %>%
	mutate(Content = str_replace_all(Content, "\\.\\.\\.", " ")) %>% 
	filter(!is.na(Content)) %>% 
	select(Sub,Post_ID = `Post ID`, Title, Author, Date = `Publish Date`, Flair, Content, parent_id, link_id, Score) %>% 
	mutate(Post_type = ifelse(is.na(parent_id), "Post", "Response")) %>% 
	map_at(c("link_id", "parent_id"), ~str_remove(.x, "t[1-9]_")) %>% 
	as_tibble() %>% 
	group_by(link_id) %>% 
	nest() %>% 
	mutate(data =
				 	map(data, function(data){
				 		
				 		OP <- data %>% 
				 			filter(Post_type == "Post") %>% 
				 			pull(Author) %>% 
				 			unique() # Aged posts are duplicated if multiple ages appear in the same post
				 		
				 		if(is_empty(OP)){
				 			return()
				 		}
				 	
				 		if(length(OP) > 1){ # Make sure that there are not two original author's, although there won't be.
				 			print(data) 
				 		}
				 		
				 data %>% 
				 	mutate(Original_Author = (Author == OP))
				 	})
	) %>% 
	unnest(data, keep_empty = TRUE) %>% 
	group_by_all() %>% 
	summarise() %>% 
	ungroup() %>% 
	unnest_tokens(word, Content) %>% 
	anti_join(stop_words) %>% 
	filter(!(word %in% more_stop_words)) %>% 
	group_by(word) %>% 
  mutate(count = n()) %>% 
	filter(n() >= 5) %>% 
	ungroup() %>% 
	group_by(Post_ID) %>% 
	filter(n() >= 10) %>%
	#group_by(Sub, Post_ID, Title, Author, Date, Flair) %>% 
	group_by(Sub, Post_ID, Title, Author, Date, Flair, parent_id, link_id) %>% 
	summarise(Content = paste(word, collapse = " ")) %>% 
	ungroup() %>% 
	group_by(Sub, Post_ID, Title, Author, Date, Flair, Content) %>% 
	summarise() %>% 
	ungroup()

write_csv(clean_posts, "data/clean_posts.csv")




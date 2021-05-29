# ----------------------------------------------------------------
# Name:
#
# Author: Curtis Murray
# Date:	 25 Aug 2020
#
# Description:
# 
# ----------------------------------------------------------------
# Libs
library(tidyverse)
library(ggraph)
library(igraph)
library(visNetwork)
library(tidytext)
library(RColorBrewer)
library(tidygraph)
library(networkD3)
library(visNetwork)
library(htmlwidgets)
library(ape)
# ----------------------------------------------------------------
# Loading Data

# ----------------------------------------------------------------
# Functions

# ----------------------------------------------------------------

clean_posts <- read_csv("data/clean_posts.csv")

hSBM_input <- read_csv("data/clean_posts.csv")

word_probs <- hSBM_input %>% 
	unnest_tokens(word, Content) %>% 
	group_by(word) %>% 
	summarise(count = n()) %>% 
	ungroup() %>% 
	mutate(prop = count/sum(count))

keep_words <- hSBM_input %>% unnest_tokens(word, Content) %>% 
	group_by(word) %>% 
	summarise(count = n()) %>% 
	top_n(600, count) %>% 
	arrange(-count) %>% 
	pull(word)

tidy_topics_all <- list.files(path = "data/Topic_Model/Clean/", pattern = "tidy_topics_\\d", full.names = T, include.dirs=T, all.files =T) %>% 
	as_tibble() %>% 
	mutate(posts = map(value, read_csv)) %>% 
	mutate(level = str_extract(value, "\\d")) %>% 
	select(posts, level) 

tidy_topic_docs_all <- list.files(path = "data/Topic_Model/Clean", pattern = "tidy_topics_docs", full.names = T, include.dirs=T, all.files =T) %>% 
	as_tibble() %>% 
	mutate(posts = map(value, read_csv)) %>% 
	mutate(level = str_extract(value, "\\d")) %>% 
	select(posts, level)

topic_probs <- tidy_topic_docs_all %>% 
	unnest(posts) %>% 
	group_by(topic, level) %>% 
	summarise(p = mean(p)) %>% 
	mutate(topic = paste("L",level, "_", topic, sep = "")) %>% 
	select(-level)

words_wide <- tidy_topics_all %>% 
	unnest(posts) %>%
	filter(word %in% keep_words) %>% 
	pivot_wider(names_from = level, values_from = topic, names_prefix = "L") %>% 
	group_by(word, word_ID) %>% 
	summarise_at(c("p", "L0", "L1", "L2", "L3"), ~max(.x, na.rm = t)) %>% 
	mutate(
		L3 = paste("L3_", L3, sep = ""),
		L2 = paste("L2_", L2, sep = ""),
		L1 = paste("L1_", L1, sep = ""),
		L0 = paste("L0_", L0, sep = "")
	) %>% 
	ungroup()

l4_3 <- words_wide %>%
	group_by(L3) %>%
	summarise() %>%
	mutate(from = "L4_") %>%
	rename(to = L3)

l3_2 <- words_wide %>%
	group_by(L2, L3) %>%
	summarise() %>%
	rename(from = L3, to = L2) %>%
	ungroup()


l2_1 <- words_wide %>% 
	group_by(L1, L2) %>% 
	summarise() %>% 
	rename(from = L2, to = L1) %>% 
	ungroup() 

l1_0 <- words_wide %>% 
	group_by(L0, L1) %>% 
	summarise() %>% 
	rename(from = L1, to = L0) %>% 
	ungroup() 

l0_w <- words_wide %>% 
	group_by(word, L0) %>% 
	summarise() %>% 
	rename(from = L0, to = word) %>% 
	ungroup()

edges <- l4_3 %>% 
	bind_rows(l3_2) %>% 
	bind_rows(l2_1) %>% 
	bind_rows(l1_0) %>% 
	bind_rows(l0_w)

nodes <- tibble(
	label = unique(c(edges$to, edges$from))
) %>% 
	mutate(id = 1:n()) %>% 
	select(id, label) %>% 
	mutate(level = as.numeric(str_extract(str_extract(label,"L\\d_"), "\\d"))) %>% 
	mutate(level = ifelse(is.na(level), -1, level)) %>%
	left_join(tibble(level = (-1):2, color = brewer.pal(n = 4, name = "Dark2"))) %>% 
	left_join(words_wide %>% select(word, p), by = c("label" = "word")) %>% 
	mutate(p = ifelse(is.na(p), 1, p)) %>% 
	rename(size = p) %>% 
	left_join(word_probs, by = c("label" = "word"))

edges_id <- edges %>% 
	inner_join(nodes %>% select(-level), by = c("from" = "label")) %>% 
	inner_join(nodes %>% select(-level), by = c("to" = "label")) %>% 
	select(from = id.x, to = id.y)

nodes_named <- nodes %>% 
	left_join(topic_probs, by = c("label" = "topic")) %>% 
	select(id, label, level, color, size, p, prop) %>% 
	mutate(size = ifelse(str_detect(label, "L\\d_"), p, prop)) %>% 
	select(-p) %>% 
	mutate(label = ifelse(str_detect(label, "L\\d_"), "", label)) %>% 
	mutate(font.size = ifelse(label == "", 0, size^(1/.7))) %>% 
	mutate(font.size = font.size/max(font.size)*1000) %>% 
	mutate(size = ifelse(label == "", 0, size)) %>% 
	mutate(node_key = id) %>% 
	mutate(size = size * 10000)

edges_id_full <- edges_id %>% 
	inner_join(nodes_named %>% select(id, label), by = c("to" = "id")) %>% 
	mutate(edge.width = ifelse(label == "", 0, 1))

root <- edges_id_full$from[1]
	


# visNetwork(nodes = nodes_named, edges = edges_id, height = "2000px", width = "100%") %>% 
#   #visNodes(color = "white") %>% 
#   visEdges(hidden = TRUE)


my_graph <- graph_from_data_frame(edges_id_full %>% 
																		filter(from != root) %>% 
																		select(-label), 
																	directed = TRUE, 
																	vertices = nodes_named %>% 
																		filter(id != root) %>% 
																		mutate(font.size = font.size) %>% 
																		mutate(size = size) %>% 
																		mutate(font.alpha = 0.5))


saveWidget(visIgraph(my_graph, 
										 layout="layout_as_tree", 
										 circular = T, 
										 root=edges_id_full$from[2], 
										 idToLabel = F) %>% 
					 	visOptions(highlightNearest = list(enabled = T, hover = T), 
					 						 nodesIdSelection = T,height = 1440), 
					 file = "Figures/topic_network.html")

coords <- layout_as_tree(my_graph, circular = F, root = edges_id_full$from[1])

plot(my_graph, 
		 layout = coords, 
		 idToLabel = F)

visIgraph(my_graph, idToLabel = F, type = "full") %>% 
	visIgraphLayout(layout = "layout.norm", layoutMatrix = cbind(-coords[,2], coords[,1]), type = "full") %>% 
	visPhysics()

visNetwork(nodes_named, edges_id, width = "100%") %>% 
	visHierarchicalLayout(direction = "LR", levelSeparation = 500)
#plot(my_graph, layout = layout_as_tree(my_graph, circular = TRUE, root = edges_id_full$from[1]))

visNetwork(nodes = nodes_named, edges = edges_id_full %>% select(-label) , width = "100%", height = "2000px")

visIgraph(my_graph, 
					layout="layout_as_tree", 
					circular = TRUE, 
					root=edges_id_full$from[1], 
					idToLabel = F
)

visNetwork(nodes = tmp$nodes, edges = tmp$edges, height = "500px")


my_node_id <- nodes_named %>% 
	filter(label == " legal ") %>% 
	pull(id)

parent_node_id <- edges_id %>% 
	filter(to %in% my_node_id) %>% 
	pull(from)

parents_node_ids <- edges_id %>% 
	filter(to %in% parents_node_ids) %>% 
	pull(from)

edges_id %>% 
	filter(
		from %in% parent_node_id
	) %>% 
	left_join(nodes_named, by = c("to" = "id"))


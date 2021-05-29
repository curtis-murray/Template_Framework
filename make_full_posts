analyse_tm: py_topic_models
	R CMD BATCH R/analyse_topics_clean.R
py_topic_models: clean_posts
	jupyter nbconvert --to=python Python/hSBM.ipynb
	python Python/hSBM.py
clean_posts: scrape_from_reddit
	R CMD BATCH R/topic_model.R
scrape_from_reddit:
	jupyter nbconvert --to=python Python/scrape_posts_replies.ipynb
	python Python/scrape_posts_replies.py

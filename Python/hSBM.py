#!/usr/bin/env python
# coding: utf-8

# # Implementing Network approach to topic modelling
# ## Gerlach

# In[ ]:


import matplotlib


# In[ ]:


from platform import python_version

print(python_version())


# In[ ]:


# Gerlach
#%load_ext autoreload
#%autoreload 2

import os
import pylab as plt
#%matplotlib inline  

from sbmtm import sbmtm
import graph_tool.all as gt


# In[ ]:


import pandas as pd
import numpy as np
import re
from itertools import chain


# In[ ]:


data = pd.read_csv("data/clean_posts.csv")


# In[ ]:


#texts = data["Text"].values.tolist()
#titles = data["Day"].values.tolist()

#texts = data["word"].values.tolist()
#titles = data["ID"].values.tolist()

texts = data["Content"].values.tolist()
titles = data["Post_ID"].values.tolist()


# In[ ]:


texts = [str(c).split() for c in texts]


# In[ ]:


i_doc = 0
texts


# In[ ]:


for i in range(10):
    print(i)
    model = sbmtm()

    ## we have to create the word-document network from the corpus
    model.make_graph(texts,documents=titles)

    ## we can also skip the previous step by saving/loading a graph
    # model.save_graph(filename = 'graph.xml.gz')
    # model.load_graph(filename = 'graph.xml.gz')

    ## fit the model
    #gt.seed_rng(32) ## seed for graph-tool's random number generator --> same results
    model.fit()
    topics = model.topics(l=0,n=10)
    
    if len(topics) > 1:
        break


# In[ ]:

for level in range(0,model.L+1):

    group_results = model.get_groups(l = level)

    p_tw_w = group_results['p_tw_w']
    p_td_d = group_results['p_td_d']
    p_w_tw = group_results['p_w_tw']
    p_tw_d = group_results['p_tw_d']
    p_td_tw = model.group_to_group_mixture(l = level)

    pd.DataFrame.to_csv(pd.DataFrame(p_tw_w), "".join(["data/Topic_Model/p_tw_w","_level_", str(level), ".csv"]))
    pd.DataFrame.to_csv(pd.DataFrame(p_td_d), "".join(["data/Topic_Model/p_td_d","_level_",str(level), ".csv"]))
    pd.DataFrame.to_csv(pd.DataFrame(p_w_tw), "".join(["data/Topic_Model/p_w_tw","_level_", str(level), ".csv"]))
    pd.DataFrame.to_csv(pd.DataFrame(p_tw_d), "".join(["data/Topic_Model/p_tw_d", "_level_",str(level), ".csv"]))
    pd.DataFrame.to_csv(pd.DataFrame(p_td_tw), "".join(["data/Topic_Model/p_td_tw", "_level_",str(level), ".csv"]))

pd.DataFrame.to_csv(pd.DataFrame(model.words), "".join(["data/Topic_Model/words_all_", ".csv"]))
pd.DataFrame.to_csv(pd.DataFrame(model.documents), "".join(["data/Topic_Model/documents_all_", ".csv"]))



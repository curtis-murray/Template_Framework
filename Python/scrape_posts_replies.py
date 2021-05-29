#!/usr/bin/env python
# coding: utf-8

# In[6]:


import pandas as pd
import requests
import json
import csv
import time
import datetime
import emoji
import pinyin
import math
import ciso8601
import os


# In[7]:


from platform import python_version

print(python_version())


# Some functions from https://medium.com/@RareLoot/using-pushshifts-api-to-extract-reddit-submissions-fb517b286563 that we adapt to get what we want from /r/COVID19Positive

# In[13]:


def scrape_sub(after, before, sub):

    def getPushshiftData(after, before, sub):
        url = 'https://api.pushshift.io/reddit/search/submission/?size=500&after='+str(after)+'&before='+str(before)+'&subreddit='+str(sub)
        # Get up to current time
        #url = 'https://api.pushshift.io/reddit/search/submission/?size=500&after='+str(after)+'&subreddit='+str(sub)
        print(url)
        
        # Sometimes get JSONDecdeError so this just sleeps and trys again
        while 1:
            try:
                r = requests.get(url)
                data = json.loads(r.text)
                return data['data']

            except:
                print("Retrying")
                time.sleep(1)
                
    def getPushshiftDataReplies(parent_link_id):
        url = 'https://api.pushshift.io/reddit/search?&limit=10000&link_id='+str(parent_link_id)
        # Get up to current time
        #url = 'https://api.pushshift.io/reddit/search/submission/?size=500&after='+str(after)+'&subreddit='+str(sub)
        #print(url)

        # Sometimes get JSONDecdeError so this just sleeps and trys again
        while 1:
            try:
                r = requests.get(url)
                data = json.loads(r.text)
                return data['data']

            except:
                print("Retrying")
                time.sleep(1)
                
    def collectComments(parent_link_ids, sub):
        
            #print(parent_link_ids)

            #for parent_link_id in parent_link_ids:
        parent_link_id = parent_link_ids
        #print(parent_link_id)
        data = getPushshiftDataReplies(parent_link_id)
        
        subData = list()
        for subm in data:
            #print(subm)
            #subData = list() #list to store data points Not sure why this was here
            title = 'NA'
            url = 'NA'
            flair = 'NA'
            author = subm['author']
            sub_id = subm['id']
            score = subm['score']
            selftext = subm['body']
            created = datetime.datetime.fromtimestamp(subm['created_utc']) #1520561700.0
            numComms = 'NA'
            permalink = subm['permalink']
            link_id = subm['link_id']
            parent_id = subm['parent_id']

            subData.append((sub_id,title,url,author,score,created,numComms,permalink,flair,selftext,link_id,parent_id))
            subStats[sub_id] = subData
            #print(subData)
        return(subData)
                    
    def collectSubData(subm):
        subData = list() #list to store data points
        title = subm['title']
        url = subm['url']
        if 'link_flair_text' in subm:
            flair = subm['link_flair_text']
        else:
            flair = 'NA'
        author = subm['author']
        sub_id = subm['id']
        score = subm['score']
        if 'selftext' in subm:
            selftext = subm['selftext']
        else:
            selftext = 'NA'
        created = datetime.datetime.fromtimestamp(subm['created_utc']) #1520561700.0
        numComms = subm['num_comments']
        permalink = subm['permalink']
        link_id = subm['id']
        parent_id = 'NA'

        subData.append((sub_id,title,url,author,score,created,numComms,permalink,flair,selftext,link_id,parent_id))
        
        if numComms > 0:
            #print(numComms)
            subData.append(collectComments(link_id, sub_id))
        
        subStats[sub_id] = subData


        
    subCount = 0
    subStats = {}
    
    data = getPushshiftData(after, before, sub)
    # Will run until all posts have been gathered 
    # from the 'after' date up until before date

    while len(data) > 0:
        time.sleep(0.4)
        for submission in data:
            collectSubData(submission)
            subCount+=1
        # Calls getPushshiftData() with the created date of the last submission
        print(len(data))
        print(str(datetime.datetime.fromtimestamp(data[-1]['created_utc'])))
        after = data[-1]['created_utc']
        data = getPushshiftData(after, before, sub)
        
    print(str(len(subStats)) + " submissions have added to list")
    #print("1st entry is:")
    #print(list(subStats.values())[0][0][1] + " created: " + str(list(subStats.values())[0][0][5]))
    #print("Last entry is:")
    #print(list(subStats.values())[-1][0][1] + " created: " + str(list(subStats.values())[-1][0][5]))
    
    def updateSubs_file(sub):
        upload_count = 0
        location = "data/Scrape/"
        #location = "data/Scrape/"
        f_name = location + sub + '.csv'
        exists_yn = os.path.exists(f_name)
        if not(exists_yn):
            df2 = pd.DataFrame(
                {"Post ID": [],
                "Title": [],
                "Url": [],
                 "Author": [],
                 "Score": [],
                 "Publish Date": [],
                 "Total No. of Comments": [],
                 "Permalink": [],
                 "Flair": [],
                 "Content": [],
                 "link_id": [],
                 "parent_id": [],
                })
            df2.to_csv(f_name, sep=',', encoding='utf-8', index=False)
        with open(f_name, 'a+', newline='', encoding='utf-8') as file: 
            a = csv.writer(file, delimiter=',')
            for sub in subStats:
                a.writerow(subStats[sub][0])
                upload_count+=1

            print(str(upload_count) + " submissions have been uploaded")
        
    updateSubs_file(sub)


# In[10]:


start_date_str = "20190101"
start_date = math.floor(time.mktime(ciso8601.parse_datetime(start_date_str).timetuple()))

before = math.floor(time.time())
subs = pd.read_csv("subs",header=None).values.tolist()[0]



# In[ ]:


# Make scrape_info to tell if sub has been scraped and up to where
scrape_info_path = "data/Scrape/Scrape_Info/scrape_info.csv"

if not(os.path.exists(scrape_info_path)):
    scrape_info = pd.DataFrame(
        {"Sub": [],
         "Path": [],
         "Start": [],
         "End": []}
    )
else:
    scrape_info = pd.read_csv(scrape_info_path)
    
# Add new subs to scrape_info
for sub in subs:
    if not(any(sub == scrape_info['Sub'])):
        scrape_info = scrape_info.append(
            pd.DataFrame(
                {"Sub": [sub],
                "Path": ["data/Scrape/" + sub +  ".csv"],
                #"Path": ["data/Scrape" + sub + ".csv"],
                "Start": [start_date],
                "End": [start_date]}
            ),
            ignore_index = True
        )


# In[ ]:


for i in range(len(scrape_info)):
    scrape_sub(int(scrape_info['End'][i]), int(before), scrape_info['Sub'][i])
    scrape_info.at[i,"End"] = before
scrape_info.to_csv(scrape_info_path, index = False)


# In[ ]:





# In[ ]:




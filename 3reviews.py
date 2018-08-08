#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Oct 24 13:57:41 2017

@author: peerchristensen

jan 2015 - oct 2017
"""
import requests
import os
from bs4 import BeautifulSoup
import re
import pandas as pd

os.chdir("/Users/peerchristensen/Desktop")

urls=[]
for i in range(1,201):
        urls.append('https://dk.trustpilot.com/review/www.3.dk?page=%d' % (i))
        
times=[]
names=[]
reviews=[]

for url in urls:
    r=requests.get(url)
    soup = BeautifulSoup(r.content)
    times.append(soup.find_all("time",{"class":"ndate"}))
    names.append(soup.find_all("a",{"class":"user-review-name-link"}))
    reviews.append(soup.find_all("div",{"class":"review-body"}))

names=[item for sublist in names for item in sublist]
reviews=[item for sublist in reviews for item in sublist]
times=[item for sublist in times for item in sublist]

#clean
times2=[]
for i in times:
    name=re.findall(r'span title="(.*?).000+',str(i))
    times2.append(name)
times2=[item for sublist in times2 for item in sublist]
times3=[]
for i in times2:
  if i not in times3:
    times3.append(i)


names2=[]
for i in names:
    name=i.text
    names2.append(name.strip("\n").lstrip().rstrip())
    
reviews2=[]
for i in reviews:
    review=i
    reviews2.append(review.text.strip("\n").lstrip().rstrip())        
       
data = pd.DataFrame(
    {#'Time': times3,
     'Name': names2,
     'Review': reviews2
    })

#data=data[data.Time.str.contains("Mar") == False]

data.to_csv("3reviews.csv", sep=',', encoding='utf-8')



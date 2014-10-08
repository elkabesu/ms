#!/usr/bin/python

import json
import urllib2
import time
import re
from socket import error as SocketError

def enter_site():
	site = "http://api.nytimes.com/svc/search/v2/articlesearch.json?fq=section_name:" + section + "&fl=headline,snippet&sort=newest&page=" + str(page) + "&api-key=" + APIKEY

	# enter the site
	req = urllib2.Request(site)
	return urllib2.urlopen(req)

def retrieve_info(loaded):

	# enter the many levels of the json file
	for j in loaded['response']['docs']:
		headline = j['headline']['main']
		snippet = j['snippet']

	return write_to_variable(headline, snippet)


def write_to_variable(headline, snippet):

	# write to a variable the titles and bodies of articles
	section_file = " "
	if headline != None:
		section_file = " " + section_file + headline + " "
	if snippet != None:
		section_file = " " + section_file + snippet + " "
	section_file = " " + section_file + " \n"

	return section_file

def normalize(s):

	# remove unicode
    s=filter(lambda c: ord(c) < 128, s)

    # remove non-word characters
    return re.sub(r'[^A-Za-z\s]',' ',s.lower())
    
def save_to_file(section, section_file):

	# save to a file specific to the section
	filename = section + ".txt"
	myfile = open(filename, 'w')
	myfile.write(section_file)
	myfile.close()

APIKEY =  "482bec15a48e52b666e12eb281b1855f:19:69538550"
sections = ["arts", "business", "obituaries", "sports", "world"]
pages = 100

for section in sections:
	for page in range(1, pages):
		try:
			url = enter_site()

			# turn the file into json format
			loaded = json.load(url)

			# limit bombardment of server
			time.sleep(1)

			section_file = retrieve_info(loaded)
			section_file = normalize(section_file)

			save_to_file(section, section_file)

		except SocketError as e:
			time.sleep(10)

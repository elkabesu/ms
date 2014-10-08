#!/usr/bin/python

import random
import math
import re

def get_all_lines():
	myfile_set = set()

	# open and read a specific section file
	filename = section + ".txt"
	myfile = open(filename, 'r')

	# traverse every line to then traverse every word
	myfile_all_lines = myfile.readlines()
	for line in myfile_all_lines:
		myfile_set.add(line)

	return myfile_set, myfile_all_lines

def train_and_test(myfile_set):

	# split words 50/50 to train & test
	random_list = random.sample(myfile_set, len(myfile_set) / 2)
	training_articles = set(random_list)
	test_articles = myfile_set - training_articles

	return training_articles, test_articles

def unique_training_words(training_articles):
	training_set = set()
	training_list = list(training_articles)

	for line in training_list:
		thisline = line.split(" ")
		for word in thisline:
			training_set.add(word)

	return training_set

def unique_test_words(test_articles):
	test_set = set()
	test_list = list(test_articles)

	for line in test_list:
		thisline = line.split(" ")
		for word in thisline:
			test_set.add(word)

	return test_set

def normalize(s):

	# remove unicode
    s=filter(lambda c: ord(c) < 128, s)

    # remove non-word characters
    return re.sub(r'[^A-Za-z\s]',' ',s.lower())

def get_stopwords():
	stopwords_set = set()

	# open and read stopwords file
	stopwords = open('stopwords.txt', 'r')
	stopwords_lines = stopwords.readlines()
	stopwords_lines = normalize(str(stopwords_lines))

	# traverse every word in stopwords
	for word in stopwords_lines:
		stopwords_set.add(word)

	return stopwords_set

def count_occurences(myfile_all_lines, training_set, temp_dict):

	# traverse each article and count if a word exists
	for line in myfile_all_lines:
		thisline = line.split(" ")
		thisline = set(thisline)
		thisline = list(thisline)
		for word in thisline:
			if word in training_set:
				temp_dict[word] += 1

	return temp_dict

def Wjc(training_set, temp_dict_p, temp_dict):
	# find Wjc
	for word in training_set:
		temp_dict_p[word] = float(temp_dict[word]) / len(training_set)
		if 1 - temp_dict_p[word] != 0:
			temp_dict_p[word] = temp_dict_p[word] / (1 - temp_dict_p[word])
		if temp_dict_p[word] > 0:
			temp_dict_p[word] = math.log(temp_dict_p[word])
		# print temp_dict_p[word]

	return temp_dict_p

def summation_log(training_set, temp_dict_p):
	fc_log_sum = 0

	# find summation of log(1 - THETAjc) of each section
	for word in training_set:
		temp = 1 - temp_dict_p[word]
		if temp > 0:
			fc_log_sum += math.log(temp)
	first_constant.append(fc_log_sum)

	return first_constant

# lists
sections = ["arts", "business", "obituaries", "sports", "world"]
num_of_articles = []
first_constant = []
w0c = []

list_training_dict = {}
list_training_dict_p = {}
list_test_dict = {}

total_num_of_articles = 0
total = 0

for section in sections:
	myfile_set, myfile_all_lines = get_all_lines()

	training_articles, test_articles = train_and_test(myfile_set)

	training_set = unique_training_words(training_articles)
	test_set = unique_test_words(test_articles)

	stopwords_set = get_stopwords()

	# remove stopwords from articles
	training_set = training_set - stopwords_set
	test_set = test_set - stopwords_set

	# create dicts with key = word & value = 0
	temp_dict = dict.fromkeys(training_set, 0)
	temp_dict_p = dict.fromkeys(training_set, 0)

	temp_dict = count_occurences(myfile_all_lines, training_set, temp_dict)
	temp_dict_p = Wjc(training_set, temp_dict_p, temp_dict)

	first_constant = summation_log(training_set, temp_dict_p)

	# maintain list of word dicts for each section
	list_training_dict[section] = temp_dict
	list_training_dict_p[section] = temp_dict_p

	# count number of articles per section
	articles = len(myfile_all_lines)
	num_of_articles.append(articles)
	total_num_of_articles += articles

	test_dict = dict.fromkeys(test_set, 0)
	for word in test_set:
		# print word
		if word in training_set:
			test_dict[word] += list_training_dict_p[section][word]

	list_test_dict[section] = test_dict

# find W0c
for x, y in zip(num_of_articles, first_constant):
	temp = math.log(float(x) / total_num_of_articles)
	temp += y
	w0c.append(temp)

#print list_test_dict["arts"]['van']

section_num = 0
for section in sections:

	for word in test_set:
		if word in list_test_dict[section]:
			list_test_dict[section][word] += w0c[section_num]
			if word in list_training_dict_p[section]:
				list_test_dict[section][word] += list_training_dict_p[section][word]

	section_num += 1

'''
for section in sections:
	temp = list_test_dict[section]
	print max(temp, key = temp.get)
'''

# print w0c
# print list_test_dict["arts"]

# print number of occurences of every word
# print list_training_dict

# print probabilities of every word
# print list_training_dict_p



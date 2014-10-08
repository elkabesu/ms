#!/usr/bin/python

import csv

Sex = ['male', 'female']
Races = dict(white = "A" , black = "B", native_american = "C", asian = "D", native_hawaiian = "E", other = "F", two_or_more = "G", hispanic = "H", white_not_HL = "I")
Ages = ['0', '5', '10', '15', '18', '20', '21', '22', '25', '30', '35', '40', '45', '50', '55', '60', '62', '65', '67', '70', '75', '80', '85']

d = {}

for r, r_name in Races.items():
	d[r]={}
	for s in Sex:
		d[r][s]={}
		for i, a in enumerate(Ages):
			if s =='male':
				num=i+3
			else:
				num=i+27
			d[r][s][a] = 'H9'+r_name+ "%03d" % num

census_string = ""

Counties = ["Kings County", "Queens County", "Richmond County", "New York County", "Bronx County"]

resultfile = open("output.csv", "wb")
wr = csv.writer(resultfile)

wr.writerow(["County", "Tract", "Block", "Race", "Sex", "Age", "Count"])
sum_races = 0

with open('../../Documents/block.csv', 'r') as filename:
	a = csv.DictReader(filename)
	for linenum, row in enumerate(a):
		if row["COUNTY"] in Counties:
			tract = row["TRACTA"]
			block = row["BLOCKA"]
			county = row["COUNTY"]

			for race in d:
				for sex in d[race]:
					for age in d[race][sex]:
						race_code = d[race][sex][age]
						sum_races += int(row[race_code])
						wr.writerow([county, tract,block,race,sex,age,row[race_code]])
			wr.writerow([county,tract,block,"NA","NA","NA",row["H76001"]])
		#if linenum % 1000 == 0:
		#	print linenum, str(sum_races)

resultfile.close()

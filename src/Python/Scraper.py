# scraps web content
import os, requests, json, csv, time

N = 10000000
n = 2000
start = time.perf_counter()

for i in range(1, n+1, 1):
	link = str("https://fantasy.premierleague.com/api/entry/") + str(i) + str("/history/")

	# get text on web
	html_text = requests.get(link).text
	data = json.loads(html_text)

	# extract level 1 key values or iterate
	
	# points, total_points, rank, rank_sort, overall_rank, bank, value, event_transfers, event_transfers_cost,
	# points_on_bench
	# current = data["current"]

	# season_name, total_points, rank
	past = data["past"]

	# name, time, event
	# chips = data["chips"]

	# gameweek data
	# write to .csv file
	# if(os.path.isdir("gameweek") == False):
		#os.mkdir("gameweek")

	## check if file exists and insert header
	#currentFilePath = str(os.getcwd()) + str("\\gameweek\\current.csv")
	#if(os.path.isfile(currentFilePath) == False):
		#with open(currentFilePath, newline = "", mode = "a") as csvfile:
			#try:
				#colnames = current[0].keys()
			#except:
				#print(i)

			#colnames = current[0].keys()
			#writer = csv.DictWriter(csvfile, fieldnames = colnames)
			#writer.writeheader()
			#csvfile.close()
	
	## append data
	#with open(currentFilePath, newline = "", mode = "a") as csvfile:
		#try:
			#colnames = current[0].keys()
		#except:
			#print(i)

		#colnames = current[0].keys()
		#writer = csv.DictWriter(csvfile, fieldnames = colnames)
		
		#for item in current:
			#writer.writerow(item)

		#csvfile.close()

	# past season data
	# write to .csv file
	if(os.path.isdir("past") == False):
		os.mkdir("past")
	
	with open(str(os.getcwd()) + str("\\past\\") + str(i) + str(".csv"), newline = "", mode = "w") as csvfile:
		try:
			colnames = past[0].keys()
		except:
			print(i)
		
		writer = csv.DictWriter(csvfile, fieldnames = colnames)
		writer.writeheader()

		for item in past:
			writer.writerow(item)

		csvfile.close()

	# chip data
	# write to .csv file
	# if(os.path.isdir("chips") == False):
	# 	os.mkdir("chips")
	
	# with open(str(os.getcwd()) + str("\\chips\\") + str(i) + str(".csv"),newline = "", mode = "w") as csvfile:
	# 	try:
	#		colnames = chips[0].keys()
	#	except:
	#		print(i)

	#	writer = csv.DictWriter(csvfile, fieldnames = colnames)
	#	writer.writeheader()

	#	for item in chips:
	#		writer.writerow(item)

	#	csvfile.close()

end = time.perf_counter()
print(f'Finished in {end-start} second(s)')  
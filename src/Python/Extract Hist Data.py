# scraps web content
import os, requests, json, csv, time

# set this value to starting index
n = 1000000

# set to last index value
N = 2*n

start = time.perf_counter()

for i in range(n+1, N+1, 1):
	link = str("https://fantasy.premierleague.com/api/entry/") + str(i) + str("/history/")

	# get text on web
	html_text = requests.get(link).text
	try:
		data = json.loads(html_text)
	except:
		print("No data found")
	
	past = data["past"]
	
	if(os.path.isdir("past") == False):
		os.mkdir("past")
	
	# change file name each time (optional)
	with open(str(os.getcwd()) + str("\\past\\") + "past1" + str(".csv"), newline = "", mode = "a") as csvfile:
		try:
			colnames = past[0].keys()
		except:
			print(i)
		
		writer = csv.DictWriter(csvfile, fieldnames = colnames)
		# this next for loop is very crucial
		# only access this code to write column headers
		if(i == (n+1)):
			writer.writeheader()

		for item in past:
			writer.writerow(item)

		csvfile.close()
		
end = time.perf_counter()
print(f'Finished in {end-start} second(s)')

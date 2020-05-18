from bs4 import BeautifulSoup
import requests
import csv

source = requests.get('http://coreyms.com').text
#print(source)
soup = BeautifulSoup(source, 'lxml')

csv_file = open('scaper.csv', 'w')

csv_writer = csv.writer(csv_file)
csv_writer.writerow(['headlines', 'Summary', 'YT Link'])


#print(soup.prettify())

#article = soup.find('article')
#print(article.prettify())

# Grabbing headline
#headline = article.h2.a.text
#print(headline)

# Grabbing summary post
#summary = article.find('div', class_='entry-content').p.text
#print(summary)

# Grab link to the video
#vid_link = article.find('iframe', class_='youtube-player')['src']
#print(vid_link) 

#vid_id = vid_link.split('/')[4]
#print(vid_id)
#vid_id = vid_id.split('?')[0]
#print(vid_id)

#yt_link = f'http://youtube.com/watch?v={vid_id}'
#print(yt_link)

###############################
# LOOP OVER AND DO IT FOR ALL ARTICLES USING SAME LOGIC
###############################


for article in soup.find_all('article'):

	headline = article.h2.a.text
	print(headline)

	summary = article.find('div', class_='entry-content').p.text
	print(summary)


	try:
		vid_link = article.find('iframe', class_='youtube-player')['src']
		vid_id = vid_link.split('/')[4]
		vid_id = vid_id.split('?')[0]
		yt_link = f'http://youtube.com/watch?v={vid_id}'
	except Exception as e:
		yt_link = None

	print(yt_link)

	print()

	csv_writer.writerow([headline, summary, yt_link])

csv_file.close()
# Save the parsed data to a file
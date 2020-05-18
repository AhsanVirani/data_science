from bs4 import BeautifulSoup
import requests

with open('simple.html') as html_file:
	soup = BeautifulSoup(html_file, 'lxml')


match = soup.title.text
print(match)

match = soup.div
print(match)

match = soup.find('div', class_='footer')

# Gets the first article info
article = soup.find('div', class_='article')
print(article)
#Getting headline
headline = article.h2.a.text
print(headline)
# Get summary of article 1
summary = article.p.text
print(summary)


##########
## Using the same logic we could iterate through all the stuff
##########
for article in soup.find_all('div', class_='article'):
	headline = article.h2.a.text
	print(headline)

	summary = article.p.text
	print(summary)

	print()

	




import scrapy

class FirmSpider(scrapy.Spider):
  name = "firmspider"
  start_urls = ["https://en.wikipedia.org/wiki/List_of_venture_capital_firms"]

  def parse(self, response):
    for row in response.css('div.mw-parser-output > table.wikitable > tbody > tr > th'):
        text = row.css('a::text').extract_first()
        if text and text.strip():
            print(text.strip())

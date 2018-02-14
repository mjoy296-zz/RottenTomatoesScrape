from scrapy import Spider, Request
from RTScrape.items import RtscrapeItem
from RTScrape.items import RTDetailedItem
from scrapy.spidermiddlewares.httperror import HttpError
from twisted.internet.error import DNSLookupError
from twisted.internet.error import TimeoutError, TCPTimedOutError
import itertools


class RTSpider(Spider):
    name = "rt1_spider"
    allowed_urls = ["https://www.rottentomatoes.com/"]
    start_urls = ["https://www.rottentomatoes.com/top/bestofrt/?year=1950"]
    global box_office
    global runtime

    def parse(self, response):
        links = ["https://www.rottentomatoes.com/top/bestofrt/?year=" + str(x) for x in range(1950, 2019)]
        for url in links:
            yield Request(url, callback = self.parse_detail)

    def parse_detail(self, response):
        top = response.xpath('//section[@id= "top_movies_main"]/div/table')
        titles = top.xpath('./*/td[3]/a/text()').extract()
        titles = [title.strip() for title in titles]
        t_links = top.xpath('./*/td[3]/a/@href').extract()
        ratings  = top.xpath('./*/td[2]/span/span[2]/text()').extract()
        ranks = top.xpath('./*/td[1]/text()').extract()
        no_reviews = top.xpath('./*/td[4]/text()').extract()
        Z = [titles, t_links, ratings, ranks, no_reviews]
        Z = list(zip(*Z))
        t_links2 = t_links.copy()
        for titles, t_links, ratings, ranks, no_reviews in Z:
            item = RtscrapeItem()
            item['titles'] = titles
            item['ratings'] = ratings
            item["ranks"] = ranks
            item['no_reviews'] = no_reviews
            item['t_links'] = t_links

            yield item

        t_link = ["https://www.rottentomatoes.com" + x for x in t_links2]
        t_link = [x.replace('-', '_') for x in t_link]
        for t_url in t_link:
            yield Request(t_url, callback = self.parse_page, errback = self.errback)


    def parse_page(self, response):
        titles2 = response.xpath('//div[@id = "mainColumn"]/h1/text()').extract_first()
        year =  response.xpath('//div[@id = "mainColumn"]/h1/span/text()').extract()[0].strip()
        aud = response.xpath('//div[@id="scorePanel"]/div[2]')
        a_score = aud.xpath('./div[1]/a/div/div[2]/div[1]/span/text()').extract()
        a_count = aud.xpath('./div[2]/div[2]/text()').extract()
        c_score = response.xpath('//a[@id = "tomato_meter_link"]/span/span[1]/text()').extract()[0].strip()
        c_count = response.xpath('//div[@id = "scoreStats"]/div[3]/span[2]/text()').extract()[0].strip()
        info = response.xpath('//div[@class="panel-body content_body"]/ul')
        mp_rating = info.xpath('./li[1]/div[2]/text()').extract()[0].strip()
        genre = info.xpath('./li[2]/div[2]/a/text()').extract()
        date = info.xpath('./li[5]/div[2]/time/text()').extract_first()

        item2 =  RTDetailedItem()
        for x in info.xpath('//li'):
            if x.xpath('./div[1]/text()').extract_first() == "Runtime: ":
                runtime = x.xpath('./div[2]/time/text()').extract_first()
                item2["runtime"] = runtime

            elif x.xpath('./div[1]/text()').extract_first() == "Box Office: ":
                box_office = x.xpath('./div[2]/text()').extract_first()
                item2["box_office"] = box_office
            else:
                continue

        box = response.xpath('//section[@class = "panel panel-rt panel-box "]/div')
        actor1 = box.xpath('./div/div[1]/div/a/span/text()').extract()
        actor2 = box.xpath('./div/div[2]/div/a/span/text()').extract()
        actor3 = box.xpath('./div/div[3]/div/a/span/text()').extract_first()


        item2['titles2']= titles2
        item2['year'] = year
        item2["a_score"] = a_score
        item2["c_score"] = c_score
        item2["mp_rating"] = mp_rating
        item2["genre"] = genre
        item2["c_count"] = c_count
        item2["a_count"] = a_count
        item2["actor1"] = actor1
        item2["actor2"] = actor2
        item2["actor3"] = actor3
        item2["date"] = date

        yield item2


    def errback(self, failure):
        self.logger.error(repr(failure))
        if failure.check(HttpError):
            response = failure.value.response
            self.logger.error('HttpError on %s', response.t_url)
        elif failure.check(DNSLookupError):
            request = failure.request
            self.logger.error('DNSLookupError on %s', request.t_url)
        elif failure.check(TimeoutError, TCPTimedOutError):
            request = failure.request
            self.logger.error('TimeoutError on %s', request.t_url)

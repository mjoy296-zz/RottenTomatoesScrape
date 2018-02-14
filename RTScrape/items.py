# -*- coding: utf-8 -*-

# Define here the models for your scraped items
#
# See documentation in:
# http://doc.scrapy.org/en/latest/topics/items.html
import scrapy
from scrapy import Item, Field

class RtscrapeItem(scrapy.Item):
    titles = scrapy.Field()
    ranks = scrapy.Field()
    no_reviews = scrapy.Field()
    t_links = scrapy.Field()
    ratings = scrapy.Field()


class RTDetailedItem(scrapy.Item):
    titles2 = scrapy.Field()
    year = scrapy.Field()
    c_score = scrapy.Field()
    c_count = scrapy.Field()
    a_score = scrapy.Field()
    runtime = scrapy.Field()
    a_count = scrapy.Field()
    date = scrapy.Field()
    box_office = scrapy.Field()
    actor1 = scrapy.Field()
    actor2 = scrapy.Field()
    actor3 = scrapy.Field()
    mp_rating = scrapy.Field()
    genre = scrapy.Field()

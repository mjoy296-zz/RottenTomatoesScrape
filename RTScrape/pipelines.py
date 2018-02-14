# -*- coding: utf-8 -*-

# Define your item pipelines here
#
# Don't forget to add your pipeline to the ITEM_PIPELINES setting
# See: http://doc.scrapy.org/en/latest/topics/item-pipeline.html
from scrapy.exceptions import DropItem
from RTScrape.items import RtscrapeItem
from RTScrape.items import RTDetailedItem
from scrapy.exporters import CsvItemExporter


class ValidItemPipeline(object):
    def process_item(self, item, spider):
        if not all(item.values()):
            raise DropItem("Missing values!")
        else:
            return item


class WriteItemPipeline100(object):
    def __init__(self):
         self.filename = "Top100to50.csv"
         self.filename2 = "Detailedto50.csv"

    def open_spider(self, spider):
        self.csvfile = open(self.filename, 'wb')
        self.exporter = CsvItemExporter(self.csvfile)
        self.csvfile2 = open(self.filename2, 'wb')
        self.exporter2 = CsvItemExporter(self.csvfile2)
        self.exporter.start_exporting()
        self.exporter2.start_exporting()

    def close_spider(self, spider):
        self.exporter.finish_exporting()
        self.csvfile.close()
        self.exporter2.finish_exporting()
        self.csvfile2.close()

    def process_item(self, item, spider):
        if  isinstance(item, RtscrapeItem):
            self.exporter.export_item(item)
        else:
            self.exporter2.export_item(item)
        return item


# class WriteItemDetailed(object):
#     def __init__(self):
#          self.filename = "DetailedPages.csv"
#
#     def open_spider(self, spider):
#         self.csvfile = open(self.filename, 'wb')
#         self.exporter = CsvItemExporter(self.csvfile)
#         self.exporter.start_exporting()
#
#     def close_spider(self, spider):
#         self.exporter.finish_exporting()
#         self.csvfile.close()
#
#     def process_item(self, item, spider):
#         self.exporter.export_item(item)
#         return item

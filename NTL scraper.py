# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 12:06:25 2021

@author: F0064WK
"""

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import concurrent.futures
import time

# Chrome driver is installed
path = "C:\Program Files (x86)\chromedriver.exe"

def Scrapper(year, month, tile, username, password, stray_lights_correction = True):
    with webdriver.Chrome(path) as driver:
        driver.get("https://eogdata.mines.edu/nighttime_light/monthly/v10/")
        #driver.close()
        year = str(year)
        month = "{:02}".format(month)
        
        #Tile info on the EOG website
        if tile == 1:
            tilename = "75N180W"
        elif tile == 2:
            tilename = "75N060W"
        elif tile == 3:
            tilename = "75N060E"
        elif tile == 4:
            tilename = "00N180W"
        elif tile == 5:
            tilename = "00N060W"
        else:
            tilename = "00N060E"
            
        
        link_year = driver.find_element_by_link_text(year + "/")
        link_year.click()
        
        link_month = driver.find_element_by_link_text(year + month + "/")
        link_month.click()
        
        if stray_lights_correction:
            link_correction = driver.find_element_by_link_text("vcmslcfg/")
        else:
            link_correction = driver.find_element_by_link_text("vcmcfg/")
        link_correction.click()
        
        
        link_tile = driver.find_element_by_partial_link_text(tilename)
        link_tile.click()
        
        search_un = driver.find_element_by_id("username")
        search_un.send_keys(username)
        
        search_pw = driver.find_element_by_id("password")
        search_pw.send_keys(password)
        
        search_pw.send_keys(Keys.RETURN)
        
        
        driver.quit()
    
def unzip(filename):
    pass

def org(files):
    pass

def main(year,  month_start, month_end, tile, username, password, stray_lights_correction = True, num_threads = 4):
    #Use threads to scrap multiple simulatneously:
    with concurrent.futures.ThreadPoolExecutor(max_workers = 4) as executor:
        for i in range(month_start-1, month_end + 1):
            executor.submit(Scrapper, year, i+1, tile, username, password) 
    



            
    
import csv
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
import time
from selenium.webdriver.common.keys import Keys

main_url = 'https://www.ncbi.nlm.nih.gov/sra/?term=TXID1%5BOrganism%5D'
csv_name = 'table_exp_code_name.csv'
def scrape_page(driver):
    with open(csv_name, 'a', newline='', encoding='utf-8') as csvfile:
        csvwriter = csv.writer(csvfile)
        page_html = driver.page_source
        soup = BeautifulSoup(page_html, 'html.parser')
        elements_with_link = soup.select('div.rprt')

        for element in elements_with_link:
            link_inf_a = element.find('a')
            link_name = link_inf_a.get_text()
            link_code = link_inf_a['href'][5:-6]
            csvwriter.writerow([link_code, link_name])
def main():
    driver = webdriver.Chrome()
    try:
        driver.get(main_url)
        button_panel = driver.find_elements(By.ID, 'EntrezSystem2.PEntrez.Sra.Sra_ResultsPanel.Sra_DisplayBar.Display')
        button_panel[1].click()
        time.sleep(1)
        button_200 = driver.find_element(By.ID, 'ps200')
        button_200.click()
        time.sleep(2)
        line_count = 0
        with open(csv_name, 'r', newline='', encoding='utf-8') as csvfile:
            for i in csvfile:
                line_count += 1
        print(line_count)
        pageno = driver.find_element(By.ID, 'pageno')
        pageno.clear()
        pageno.send_keys((line_count // 200)+1)
        pageno.send_keys(Keys.RETURN)
        time.sleep(1)
        tmp_pag=(line_count // 200) + 1
        while True:
            start_time = time.time()
            scrape_page(driver)
            try:
                pageno = driver.find_element(By.ID, 'pageno')
                pageno.clear()
                pageno.send_keys(tmp_pag)
                pageno.send_keys(Keys.RETURN)
                tmp_pag=tmp_pag+1
            except:
                  try:
                        driver.back()
                        pageno = driver.find_element(By.ID, 'pageno')
                        time.sleep(1)
                        pageno.clear()
                        time.sleep(1)
                        pageno.send_keys(tmp_pag)
                        time.sleep(1)
                        pageno.send_keys(Keys.RETURN)
                        time.sleep(1)
                        tmp_pag = tmp_pag + 1
                  except: return 0
            end_time = time.time()
            elapsed_time = end_time - start_time
            print(elapsed_time)
    except:
        return 0
while True:
    main()

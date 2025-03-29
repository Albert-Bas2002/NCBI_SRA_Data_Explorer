import aiohttp
import asyncio
from bs4 import BeautifulSoup
import re
import pandas as pd
import csv
import time


csv_filename = 'SRA_Descriptive_Information.csv'# place for save

async def parse_site(session, experiment_code):
    experiment_code=experiment_code[0]
    name = None
    design = None
    const_prot = None

    flag = 'Successfully'
    html = None
    try:
        async with session.get('https://www.ncbi.nlm.nih.gov/sra/' + experiment_code) as response:
            if response.status == 200:
                html = await response.text()
    except:
        flag = 'Error'

    if html:
        soup = BeautifulSoup(html, 'html.parser')
        try:
            try:
                p_data = soup.find('p', class_='details expand e-hidden')
                b_element = p_data.find('b')
                if b_element:
                    text = b_element.text
                    colon_position = text.find(':')
                    substring_after_colon = text[colon_position + 1:].strip()
                    name=substring_after_colon
            except:pass
            sra_full_data = soup.find_all('div', class_='sra-full-data')
            for block_data in sra_full_data:
                block_data_list = list(block_data.stripped_strings)
                len_block = len(block_data_list)

                if block_data_list[0] == 'Design:':
                    design = block_data_list[1]

               
                if block_data_list[0] == 'Library:':
                    for i, item in enumerate(block_data_list):
                        if i < len_block - 1:
                            if item == "Construction protocol:":
                                const_prot = block_data_list[i + 1]

        except:
            pass
       
    main_list = [experiment_code, name,    design,
                  const_prot ,flag]

    return main_list


async def process_links(links):
    async with aiohttp.ClientSession() as session:
        tasks = [parse_site(session, link) for link in links]
        return await asyncio.gather(*tasks)

end = 1 #We choose the number of pages to be parsed at a time

df = pd.read_csv('Experiments_SRA.csv') #The links are taken from here
start = 0
while True:
    with open(csv_filename, 'a', newline='', encoding='utf-8') as csvfile:
        csvwriter = csv.writer(csvfile)
        start_time = time.time()
        loop = asyncio.get_event_loop()
        results = loop.run_until_complete(process_links(df[start:end+start].values))

        for result in results:
            csvwriter.writerow(result)

        end_time = time.time()
        execution_time = end_time - start_time
        start=start+end
        print(start)
        print(f"Время выполнения запросов: {execution_time} секунд")


import pandas as pd
import csv
from selenium.webdriver.common.keys import Keys
import time
from joblib import dump, load

list_countries_centername=load('list_countries_centername.joblib')
list_countries_centername_elements= [sublist for sublist in list_countries_centername if sublist[1] != 'None']
file_path = 'Chat_gpt_text.txt'
list_tmp=[]
with open(file_path, 'r', encoding='utf-8') as file:
    for line in file:
        index = line.find('- ')
        if index != -1:
            result = line[index + 2:].strip()
            list_tmp.append(result)
list_countries= [sublist[0] for sublist in list_countries_centername if sublist[1]=='None']
main_list_countries_centername = list(zip(list_countries, list_tmp))
main_list_countries_centername.extend(list_countries_centername_elements)
file_path = '../CenterName_Country.csv'  # Путь к файлу CSV

# Запись в CSV файл
with open(file_path, 'w', newline='', encoding='utf-8') as csv_file:
    csv_writer = csv.writer(csv_file)

    # Запись заголовков
    csv_writer.writerow(['CenterName', 'Country'])

    # Запись данных
    csv_writer.writerows(main_list_countries_centername)
df=pd.read_csv('../CenterName_Country.csv')
df['Country'] = df['Country'].replace('Taiwan', 'China')
df['Country'] = df['Country'].replace('USA', 'United States')
df['Country'] = df['Country'].replace('United States of America', 'United States')
df['Country'] = df['Country'].replace('UK', 'United Kingdom')
df['Country'] = df['Country'].replace('United Kingdom (NHS Lothian)', 'United Kingdom')
df['Country'] = df['Country'].replace('Netherlands/United Kingdom', 'Netherlands')
df['Country'] = df['Country'].replace('United Kingdom/Netherlands', 'Netherlands')
df['Country'] = df['Country'].replace('Unknown (text contains question marks)', None)
df['Country'] = df['Country'].replace('Unknown', None)
df['Country'] = df['Country'].replace('Poland/Denmark' , 'Poland')
df['Country'] = df['Country'].replace( 'Czechoslovakia' , 'Czech Republic')
df['Country'] = df['Country'].replace('Not specified', None)
df['Country'] = df['Country'].replace('Unspecified', None)
df['Country'] = df['Country'].replace('Unknown (text contains question marks)', None)
df['Country'] = df['Country'].replace('International' , None)
df['Country'] = df['Country'].replace( 'International (CIP - International Potato Center)' , None)
df['Country'] = df['Country'].replace( 'International (International Treaty on Plant Genetic Resources for Food and Agriculture)', None)
df['Country'] = df['Country'].replace('International (No specific country)', None)
df['Country'] = df['Country'].replace( 'International (Howard Hughes Medical Institute)', None)
df['Country'] = df['Country'].replace( 'Europe (specific country not provided)' , 'Europe')
df['Country'].replace({None: 'Unknown', 'nan': 'Unknown'}, inplace=True)
unique_values = df['Country'].unique()
counts = df['Country'].value_counts()
df.to_csv('CenterName_Country.csv', index=False)

df1=pd.read_csv('Run_CenterName.csv')
df2=pd.read_csv('CenterName_Country.csv')
result_df = pd.merge(df1, df2, on='CenterName', how='inner')
result_df = result_df.drop_duplicates(subset='Run', keep='last')
result_df.to_csv('Run_CenterName_Country.csv',index=False)





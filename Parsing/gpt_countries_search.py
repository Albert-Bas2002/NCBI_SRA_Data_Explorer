import joblib
import pandas as pd
import pyperclip


gpt_find_countries_list = joblib.load('list_countries_centername.joblib')
gpt_find_countries= [sublist[0] for sublist in gpt_find_countries_list if sublist[1]=='None']

def concat(sentences,j):
    my_sentences=''
    for i in range(len(sentences)):
        my_sentences = my_sentences+" "+str(i+1)+"-'"+sentences[i]+"'"
    pyperclip.copy("identify each country, your answer should be 'index - country', if not sure, write 'index - None'                    "+ my_sentences)
    a = input(j)
j=0

for i in range(0, len(gpt_find_countries),145):
    batch = gpt_find_countries[i:i + 145]
    j=j+1
    concat(batch,j)

#Copy the chat response, edit if necessary, save to a file



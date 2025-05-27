import pandas as pd
from bs4 import BeautifulSoup
import re
import pandas as pd
import csv
import time
import requests
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
from joblib import dump, load

from joblib import dump, load
pattern = re.compile(r'\/\/(.*?)\/')

#for firts
df = pd.read_csv('Run_CenterName.csv',usecols=['CenterName'])
df=df.drop_duplicates()
df.notna
print(df.shape[0])
list_countries_centername=[]
list_for_error_search=[] #Either try again, or use gpt.
list_all_centers_name=df['CenterName'].tolist()



url='https://www.bing.com/search?q='
country_domains = {
    ".au": "Australia",
    ".at": "Austria",
    ".az": "Azerbaijan",
    ".al": "Albania",
    ".dz": "Algeria",
    ".ai": "Anguilla",
    ".ao": "Angola",
    ".ad": "Andorra",
    ".ag": "Antigua and Barbuda",
    ".ar": "Argentina",
    ".am": "Armenia",
    ".aw": "Aruba",
    ".af": "Afghanistan",
    ".bs": "Bahamas",
    ".bd": "Bangladesh",
    ".bb": "Barbados",
    ".bh": "Bahrain",
    ".by": "Belarus",
    ".bz": "Belize",
    ".be": "Belgium",
    ".bj": "Benin",
    ".bm": "Bermuda",
    ".bg": "Bulgaria",
    ".bo": "Bolivia",
    ".ba": "Bosnia and Herzegovina",
    ".bw": "Botswana",
    ".br": "Brazil",
    ".bn": "Brunei Darussalam",
    ".bf": "Burkina Faso",
    ".bi": "Burundi",
    ".bt": "Bhutan",
    ".vu": "Vanuatu",
    ".va": "Vatican City State",
    ".uk": "United Kingdom",
    ".gb": "Great Britain",
    ".hu": "Hungary",
    ".ve": "Venezuela",
    ".vg": "Virgin Islands",
    ".vi": "Virgin Islands",
    ".tl": "Timor-Leste",
    ".vn": "Vietnam",
    ".ga": "Gabon",
    ".ht": "Haiti",
    ".gy": "Guyana",
    ".gm": "Gambia",
    ".gh": "Ghana",
    ".gp": "Guadeloupe",
    ".gt": "Guatemala",
    ".gf": "French Guiana",
    ".gn": "Guinea",
    ".gq": "Equatorial Guinea",
    ".gw": "Guinea-Bissau",
    ".de": "Germany",
    ".gi": "Gibraltar",
    ".hn": "Honduras",
    ".hk": "Hong Kong",
    ".gd": "Grenada",
    ".gl": "Greenland",
    ".gr": "Greece",
    ".ge": "Georgia",
    ".gu": "Territory of Guam",
    ".dk": "Denmark",
    ".dj": "Djibouti",
    ".dm": "Dominica",
    ".do": "Dominican Republic",
    ".eg": "Egypt",
    ".zm": "Zambia",
    ".eh": "Western Sahara",
    ".zw": "Zimbabwe",
    ".il": "Israel",
    ".in": "India",
    ".id": "Indonesia",
    ".jo": "Jordan",
    ".iq": "Iraq",
    ".ir": "Iran",
    ".ie": "Ireland",
    ".is": "Iceland",
    ".es": "Spain",
    ".it": "Italy",
    ".ye": "Yemen",
    ".cv": "Cape Verde",
    ".kz": "Kazakhstan",
    ".kh": "Cambodia",
    ".cm": "Cameroon",
    ".ca": "Canada",
    ".qa": "Qatar",
    ".ke": "Kenya",
    ".cy": "Cyprus",
    ".kg": "Kyrgyzstan",
    ".ki": "Kiribati",
    ".cn": "China",
    ".co": "Colombia",
    ".km": "Comoros",
    ".cg": "Republic of Congo",
    ".cd": "Democratic Republic of Congo",
    ".kp": "North Korea",
    ".kr": "South Korea",
    ".cr": "Costa Rica",
    ".ci": "Cote d'Ivoire",
    ".cu": "Cuba",
    ".kw": "Kuwait",
    ".la": "Laos",
    ".lv": "Latvia",
    ".ls": "Lesotho",
    ".lr": "Liberia",
    ".lb": "Lebanon",
    ".ly": "Libya",
    ".lt": "Lithuania",
    ".li": "Liechtenstein",
    ".lu": "Luxembourg",
    ".mu": "Mauritius",
    ".mr": "Mauritania",
    ".mg": "Madagascar",
    ".mo": "Macao",
    ".mk": "Macedonia",
    ".mw": "Malawi",
    ".my": "Malaysia",
    ".ml": "Mali",
    ".mv": "Maldives",
    ".mt": "Malta",
    ".ma": "Morocco",
    ".mq": "Martinique",
    ".mh": "Marshall Islands",
    ".mx": "Mexico",
    ".fm": "Micronesia",
    ".mz": "Mozambique",
    ".md": "Moldova",
    ".mc": "Monaco",
    ".mn": "Mongolia",
    ".ms": "Montserrat",
    ".mm": "Myanmar",
    ".na": "Namibia",
    ".nr": "Nauru",
    ".np": "Nepal",
    ".ne": "Niger",
    ".ng": "Nigeria",
    ".an": "Netherlands Antilles",
    ".cw": "Curacao",
    ".nl": "Netherlands",
    ".ni": "Nicaragua",
    ".nu": "Niue",
    ".nz": "New Zealand",
    ".nc": "New Caledonia",
    ".no": "Norway",
    ".ae": "United Arab Emirates",
    ".om": "Oman",
    ".im": "Isle of Man",
    ".nf": "Norfolk Island",
    ".sh": "Saint Helena",
    ".ky": "Cayman Islands",
    ".ck": "Cook Islands",
    ".pn": "Pitcairn Islands",
    ".pm": "Saint Pierre and Miquelon",
    ".tc": "Turks and Caicos Islands",
    ".sj": "Svalbard and Jan Mayen",
    ".pk": "Pakistan",
    ".pw": "Palau",
    ".ps": "Palestine",
    ".pa": "Panama",
    ".pg": "Papua New Guinea",
    ".py": "Paraguay",
    ".pe": "Peru",
    ".pf": "French Polynesia",
    ".pl": "Poland",
    ".pt": "Portugal",
    ".pr": "Puerto Rico",
    ".re": "Reunion",
    ".ru": "Russia",
    ".рф": "Russian Federation",
    ".rw": "Rwanda",
    ".ro": "Romania",
    ".sv": "El Salvador",
    ".ws": "Samoa",
    ".as": "American Samoa",
    ".sm": "San Marino",
    ".st": "Sao Tome and Principe",
    ".sa": "Saudi Arabia",
    ".sz": "Swaziland",
    ".mp": "Northern Mariana Islands",
    ".sc": "Seychelles",
    ".sn": "Senegal",
    ".vc": "Saint Vincent and the Grenadines",
    ".kn": "Saint Kitts and Nevis",
    ".lc": "Saint Lucia",
    ".rs": "Serbia",
    ".sg": "Singapore",
    ".sy": "Syria",
    ".sk": "Slovakia",
    ".si": "Slovenia",
    ".us": "United States of America",
    ".sb": "Solomon Islands",
    ".so": "Somalia",
    ".sd": "Sudan",
    ".sr": "Suriname",
    ".sl": "Sierra Leone",
    ".tj": "Tajikistan",
    ".th": "Thailand",
    ".tz": "Tanzania",
    ".tg": "Togo",
    ".tk": "Tokelau",
    ".to": "Tonga",
    ".tt": "Trinidad and Tobago",
    ".tv": "Tuvalu",
    ".tn": "Tunisia",
    ".tm": "Turkmenistan",
    ".tr": "Turkey",
    ".ug": "Uganda",
    ".uz": "Uzbekistan",
    ".ua": "Ukraine",
    ".укр": "Ukraine",
    ".wf": "Wallis and Futuna",
    ".uy": "Uruguay",
    ".fo": "Faroe Islands",
    ".fj": "Fiji",
    ".ph": "Philippines",
    ".fi": "Finland",
    ".fk": "Falkland Islands",
    ".fr": "France",
    ".hr": "Croatia",
    ".cf": "Central African Republic",
    ".td": "Chad",
    ".me": "Montenegro",
    ".cz": "Czech Republic",
    ".cl": "Chile",
    ".ch": "Switzerland",
    ".se": "Sweden",
    ".lk": "Sri Lanka",
    ".ec": "Ecuador",
    ".er": "Eritrea",
    ".ee": "Estonia",
    ".et": "Ethiopia",
    ".za": "South Africa",
    ".jm": "Jamaica",
    ".jp": "Japan"
}
count=len(list_countries_centername)
driver = webdriver.Chrome()
for center in list_all_centers_name[:100]:
    center=str(center)

    url_center=url+' "'+center.replace(" ", "+").replace("(", "").replace(")", "").replace("'", "")+'"'
    print(url_center)
    list_b_text=[]
    try:
        driver.get(url_center)
        page_html = driver.page_source
        soup = BeautifulSoup(page_html, 'html.parser')
        elements_a = soup.find_all('div', class_="b_attribution")
        for element_a in elements_a:
            element_b = element_a.find('cite')
            list_b_text.append(element_b.text)
        elements_a = soup.find_all('div', class_="b_attribution b_nav")
        for element_a in elements_a:
            element_b = element_a.find('cite')
            list_b_text.append(element_b.text)

    except:
        list_for_error_search.append(center)
        dump(list_for_error_search, '../list_for_error.joblib')

    if len(list_b_text)>0:
        inf_list=[]
        list_b_text=list_b_text[:3]
        for inf_text in list_b_text:
            matches = pattern.findall(inf_text)
            if matches:
                inf = matches[0]
                inf=inf[-3:]
            else:
                inf = inf_text[-3:]
            inf_list.append(inf)
        country_dom=None
        flag_country = False
        flag_edu = False
        inf_list=inf_list[::-1]
        print(inf_list)
        for inf in inf_list:
            if inf in country_domains:
                country_dom=inf
                flag_country=True
            

        if flag_country:
            list_countries_centername.append([center,country_domains[country_dom]])

        else:
            list_countries_centername.append([center,'None'])


        print(list_countries_centername[count])

        count=count+1
        print(count)
        dump(list_countries_centername, 'list_countries_centername.joblib')




    else:#Either try again, or use gpt.
        list_for_error_search.append(center)
        dump(list_for_error_search, 'list_for_error_search.joblib')





 
import pandas as pd
import math

def funct_str(str_number):
    if isinstance(str_number, float):
        return
    original_string = str_number.replace('"', '')
    if '.' in original_string or ',' in original_string :
        original_string=original_string.replace(',', '').replace('.', '')
        if 'M' in original_string:
            original_string = original_string.replace('M', '')
            int_number = int(original_string)
            int_number=int_number*100000

        elif 'G' in original_string:
            original_string = original_string.replace('G', '')
            int_number = int(original_string)
            int_number = int_number * 100000000
        else:
            int_number = int(original_string)

    else:
        if 'M' in original_string:
            original_string = original_string.replace('M', '')
            int_number = int(original_string)
            int_number = int_number * 1000000

        elif 'G' in original_string:
            original_string = original_string.replace('G', '')
            int_number = int(original_string)
            int_number = int_number * 1000000000
        else:
            int_number = int(original_string)
    return  int(int_number)

def funct_str_size(str_number):
    if isinstance(str_number, float):
        return
    original_string = str_number.replace('"', '')
    if '.' in original_string or ',' in original_string :
        original_string=original_string.replace(',', '').replace('.', '')
        if 'Mb' in original_string:
            original_string = original_string.replace('Mb', '')
            int_number = int(original_string)
            int_number = int_number * 1024*1024/10
        elif 'Gb' in original_string:
            original_string = original_string.replace('Gb', '')
            int_number = int(original_string)
            int_number = int_number * 1024* 1024*1024/10
        elif 'b' in original_string:
            original_string = original_string.replace('b', '')
            int_number = int(original_string)
            int_number = int_number
        elif 'Kb' in original_string:
            original_string = original_string.replace('Kb', '')

            int_number = int(original_string)
            int_number = int_number * 1024/10
    else:
        if 'Mb' in original_string:
            original_string = original_string.replace('Mb', '')
            int_number = int(original_string)
            int_number = int_number * 1024 * 1024
        elif 'Gb' in original_string:
            original_string = original_string.replace('Gb', '')
            int_number = int(original_string)
            int_number = int_number * 1024 * 1024 * 1024
        elif 'b' in original_string:
            original_string = original_string.replace('b', '')
            int_number = int(original_string)
            int_number = int_number
        elif 'Kb' in original_string:
            original_string = original_string.replace('Kb', '')

            int_number = int(original_string)
            int_number = int_number * 1024

    return int(int_number)

def b_to_mb(num):
    num=num/(1024*1024)
    return num

df = pd.read_csv('tableSRA.csv')
#df['num_spots'] = df['num_spots'].apply(funct_str)
#df['num_bases'] = df['num_bases'].apply(funct_str)
#df['size_inf'] = df['size_inf'].apply(funct_str_size)

df['num_spots'] = pd.to_numeric(df['num_spots'], errors='coerce').fillna(0).apply(lambda x: math.floor(x) if not math.isnan(x) else 0)
df['num_bases'] = pd.to_numeric(df['num_bases'], errors='coerce').fillna(0).apply(lambda x: math.floor(x) if not math.isnan(x) else 0)
df['size_inf'] = pd.to_numeric(df['size_inf'], errors='coerce').fillna(0).apply(b_to_mb).apply(lambda x: math.floor(x) if not math.isnan(x) else 0)

df.to_csv('tableSRA.csv', index=False)



#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#==============================================================================
# Suzanne Sigalla
# Projet de MASTER 2
# Statistiques et Machine Learning 
# Université Paris Sud -- EDF
# 2018/2019
#==============================================================================

#==============================================================================
# ##### Packages #####
#==============================================================================

import requests
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import unicodedata

#==============================================================================
# ##### WEBSCRAPPING #####
#==============================================================================


#==============================================================================
# Importing "regions", the list of the main city of each region in France, and the corresponding INSEE code
# (available on wikipedia)
regions = pd.read_excel("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data préfecture/liste_reg.xlsx", 
                             sep = ',', 
                             encoding = 'unicode_escape', 
                             index_col=0)
#==============================================================================


#==============================================================================
# HTML PARSER
class HTMLTableParser:
       
        def parse_url(self, url):
            response = requests.get(url)
            soup = BeautifulSoup(response.text, 'lxml')
            return [(table['id'],self.parse_html_table(table))\
                    for table in soup.find_all('table')]  
    
        def parse_html_table(self, table):
            n_columns = 0
            n_rows=0
            column_names = []
    
            # Find number of rows and columns
            # we also find the column titles if we can
            for row in table.find_all('tr'):
                
                # Determine the number of rows in the table
                td_tags = row.find_all('td')
                if len(td_tags) > 0:
                    n_rows+=1
                    if n_columns == 0:
                        # Set the number of columns for our table
                        n_columns = len(td_tags)
                        
                # Handle column names if we find them
                th_tags = row.find_all('th') 
                if len(th_tags) > 0 and len(column_names) == 0:
                    for th in th_tags:
                        column_names.append(th.get_text())
    
            # Safeguard on Column Titles
            if len(column_names) > 0 and len(column_names) != n_columns:
                raise Exception("Column titles do not match the number of columns")
    
            columns = column_names if len(column_names) > 0 else range(0,n_columns)
            df = pd.DataFrame(columns = columns,
                              index= range(0,n_rows))
            row_marker = 0
            for row in table.find_all('tr'):
                column_marker = 0
                columns = row.find_all('td')
                for column in columns:
                    df.iat[row_marker,column_marker] = column.get_text()
                    column_marker += 1
                if len(columns) > 0:
                    row_marker += 1
                    
            # Convert to float if possible
            for col in df:
                try:
                    df[col] = df[col].astype(float)
                except ValueError:
                    pass
            
            return df
#==============================================================================





#==============================================================================
# TEXT NORMALIZATION

def remove_accents(input_str):
    nfkd_form = unicodedata.normalize('NFKD', input_str)
    return u"".join([c for c in nfkd_form if not unicodedata.combining(c)])

def normalize_caseless(text):
    return (remove_accents(unicodedata.normalize("NFKD", text.casefold())))

def keep_first_word(text):
    return text.split()[0]

def keep_main_stations(df, col, reference):
    """   
    we wish to keep the values of the col n°col of the dataframe df
    which also are in reference   
    """
    df.query('col in reference')
#==============================================================================


#==============================================================================
# OUR STATIONS


stations = [["paris-montsouris",7156],["reims-champagne",7070],
            ["abbeville",7005],["rouen-boos",7037], 
            ["orleans-bricy",7249],["caen-carpiquet",7027],
            ["dijon-longvic",7280],["lille-lesquin",7015],
            ["metz-frescaty",7090],["strasbourg-entzheim",7190],
            ["besancon-thise",7288],["nantes-atlantique",7222],
            ["rennes-st-jacques",7130],["poitiers-biard",7335],
            ["bordeaux-merignac",7510],["toulouse-blagnac",7630],
            ["limoges-bellegarde",7434],["lyon-bron",7480],
            ["clermont-ferrand-aulnat",7460],["montpellier-frejorgues",7643],
            ["marseille-marignane-marseille-provence",7650],["bastia-poretta",7790]]

# Cities only
villes = [item[0] for item in stations] 
# INSEE number or each region
ind_regions = regions.index.values
# Corresponding INSEE region-number of each city
villes_to_reg = dict(zip(villes, ind_regions)) 
mois_ep={1:'janvier', 2:'fevrier', 3:'mars', 4:'avril', 5:'mai', 6:'juin', 7:'juillet', 8:'aout', 9:'septembre', 10:'octobre', 11:'novembre', 12:'decembre'}
rev_mois_ep = dict(zip(mois_ep.values(),mois_ep.keys()))
#==============================================================================






#==============================================================================
# OUR FUNCTIONS

""" an intermediary function """
def remove(s):
    if s == '' :
        return(None)
    elif s[0]==".":
        return(s[2:])
    else :
        return(remove(s[1:]))

def scrap_weather_one_city_year_month(station, annee, mois):
    """"
    scrap weather data for one station, one year, one month
    station is a list of the shape [city, number] 
    annee is a year (integer)
    mois is a month (in letter, string)
    scrap_weather_one_city_year_month returns a dataframe
    
    """"
    # ---- extracting the right url --- #
    url_base = "https://www.infoclimat.fr/climatologie-mensuelle/"
    url = url_base + '0' + str(station[1]) + '/' + mois + '/' + str(annee) + '/' + station[0] + '.html'
    hp = HTMLTableParser()
    table = hp.parse_url(url)[0][1]
    # --- keeping the columns and rows we are interested in --- #
    table.columns = ["Jour","TMin","TMax","Precip","Ensoleillement","Rafale","Neige","Temps obs"]
    table = table.drop(columns=["Precip","Ensoleillement","Rafale","Neige","Temps obs"]) 
    table.drop(table.tail(2).index,inplace=True)
    # --- shaping the data --- #
    table["TMin"] = table["TMin"].apply(remove)
    table["TMax"] = table["TMax"].apply(remove) 
    # --- removing "°C" --- #
    table["TMin"] = table["TMin"].apply(lambda x : x[:-2])
    table["TMax"] = table["TMax"].apply(lambda x : x[:-2])
    # --- creating date column --- # 
    table["Jour"] = range(1,len(table)+1)
    table["Jour"] = table["Jour"].apply(str)
    table["Jour"] = str(annee) + "/" + str(rev_mois_ep[mois]) + "/" + table["Jour"] 
    # --- this is it ! --- #
    return(table)
    

def scrap_weather_by_city(station, debut, fin):
    """ scrap weather data for one station, from year "debut" to year "fin" 
    station is a list of the shape [city, number] 
    debut is a year (integer)
    fin is a year (integer)
    scrap_weather_by_city returns a dataframe
    """
    # --- array of all the years we wish to collect the data for --- # 
    annees_ep = np.arange(debut, fin+1)
    table = pd.DataFrame(columns = ["Jour","TMin","TMax"])
    # --- for each year, we simply apply the previous "scrap_weather_one_city_year_month" and we aggregate everything --- #
    for annee in annees_ep :
        for i in range(1,13):
            table = pd.concat([table, scrap_weather_one_city_year_month(station, annee, mois_ep[i])])
    return(table)

def scrap_weather(stations, debut, fin):
    """ scrap weather data for a list of stations, from year "debut" to year "fin" 
    stations is a list of stations, each station being of the shape [city, number] 
    debut is a year (integer)
    fin is a year (integer)
    scrap_weather returns a dictionary where the keys are the stations name (the cities) and the values are dataframes
    
    """
    villes = [item[0] for item in stations]
    dict_result = {x: pd.DataFrame(columns = ["Jour","TMin","TMax"]) for x in villes}
    for station in stations :
        print(station[0])
        dict_result[station[0]] = scrap_weather_by_city(station, debut, fin)
    return(dict_result)   
#==============================================================================


        

#==============================================================================
# APPLICATION 


# Weather for every city, from 1984 to 2018
dict_weather = scrap_weather(stations, 1984, 2018) 


# Creating a name to save each table
noms_tables = ["df_" + str(villes_to_reg[x]) for x in villes]
dict_villes = dict(zip(villes, noms_tables))

# Saving out data
def save_to_csv(dict_weather, dict_villes, villes):
    for x in villes : 
        df = dict_weather[x]
        file_name = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/code Python/" + dict_villes[x]
        df.to_csv(file_name, sep=",", encoding = 'utf-8', index= False)


save_to_csv(dict_weather, dict_villes, villes)
#==============================================================================























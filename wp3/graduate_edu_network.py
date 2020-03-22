# -*- coding: utf-8 -*-

# =============================================================================
# 1. Libraries
# =============================================================================

import pandas as pd
import numpy as np
import os
import time
import networkx as nx
import matplotlib.pyplot as plt
pd.options.mode.chained_assignment = None 
import requests

# Set working directory
os.chdir('C:/Country/Russia/Data/SEASHELL/SEABYTE/Databases/Tertiary')

# =============================================================================
# 2. Create edgelist
# =============================================================================

# 2013-2016 data
# Get names of .csv files in working directory
files_in_dir = os.listdir('migration_data_2013_2016/')
# Drop г.Байконур
files_in_dir.remove('г.Байконур.csv')

# Combine seperate csv files into edgelist dataframe
edgelist_df = pd.DataFrame()
for i in range(len(files_in_dir)):
    
    # Exract region name
    temp_file = files_in_dir[i]
    temp_region_name = temp_file.split('.csv')[0]
    
    # Load migration data from one region
    temp_df = pd.read_csv('migration_data_2013_2016/' + temp_file)
    
    # Drop г.Байконур and foreign countries
    temp_df = temp_df[temp_df['ID'].apply(
        lambda region_id: region_id not in [55, 1000, 1001, 1002, 1003, 1004, 1005,
                                            1006, 1007, 1008, 1009, 1010, 1011, 1012,
                                            1013, 1014])]
    # Create column with out region name
    temp_df['out_region'] = temp_region_name
    # Remove Out region from the dataframe
    temp_df = temp_df[temp_df['Название'] != temp_region_name]
    
    edgelist_df = edgelist_df.append(temp_df)
    print(i)
    
# Remove edges with less than 30 graduates
edgelist_df = edgelist_df[edgelist_df['Число уехавших в регион выпускников (чел.)'] >= 30]

# Create short version of dataframe
# Graduates number was chosen as an edge characteristic, but you can define it based on other variables.
edgelist_df = edgelist_df.loc[:,['out_region',
                                 'Название',
                                 'Число уехавших в регион выпускников (чел.)']]
edgelist_df.columns = ['out_region', 'in_region', 'graduates_number']

# Save to excel
edgelist_df.to_excel("migration_network_edgelist_df.xlsx")


# =============================================================================
# 3. Create migration graph in networkx
# =============================================================================

# Save directed network as txt edgelist
outF = open("migration_network_edgelist.txt", "w", encoding="utf-8")
for i in range(len(edgelist_df)):
  # write line to output file
  outF.write(
      edgelist_df.iloc[i]['out_region'] + ',' + edgelist_df.iloc[i]['in_region'] + ',' + str(edgelist_df.iloc[i]['graduates_number']))
  outF.write("\n")
outF.close()

# Create graph object
G = nx.read_weighted_edgelist("migration_network_edgelist.txt",
                              delimiter=',', create_using=nx.DiGraph())
print(nx.info(G))

# Add labels
labels = {}
for i, label in enumerate(list(G.nodes)):
    labels[i] = label

# Basic centrality statistics
regions_network_stats = pd.DataFrame(
    {'region':list(nx.in_degree_centrality(G).keys()),
     'in_degree':list(nx.in_degree_centrality(G).values()),
     'out_degree':list(nx.out_degree_centrality(G).values()),
     'betweenness':list(nx.betweenness_centrality(G).values()),
     'closeness':list(nx.closeness_centrality(G).values())})

# Save to excel
regions_network_stats.to_excel('regions_network_stats.xlsx')

# =============================================================================
# 4. Parse coordinates of regions. ONLY FOR PLOTTING ON THE MAP
# =============================================================================

api_key = 'AIzaSyAu62ZUHNgrys8fm98CdZB7gT97moR75us'
region_names = np.unique(edgelist_df['in_region'])
region_coords = []
for region_name in region_names:

    response = requests.get('https://maps.googleapis.com/maps/api/geocode/json?address=' + 
                            region_name + '&key=' + api_key)
    region_coords.append(response.json()['results'][0]['geometry']['location'])
    print(region_name)
    time.sleep(1)

region_coords = pd.DataFrame(region_coords)    

# Create edgelist with coordinates
region_long_dict = dict(zip(region_names, region_coords['lng']))
region_lat_dict = dict(zip(region_names, region_coords['lat']))
coordinates_edgelist_df = pd.DataFrame(
    {'out_region_long':edgelist_df['out_region'].replace(region_long_dict),
     'in_region_long':edgelist_df['in_region'].replace(region_long_dict),
     'out_region_lat':edgelist_df['out_region'].replace(region_lat_dict),
     'in_region_lat':edgelist_df['in_region'].replace(region_lat_dict)})

# Save to excel
coordinates_edgelist_df.to_excel('migration_network_coordinates_edgelist.xlsx')

# Add coordinates to regions dataframe
regions_network_stats['long'] = regions_network_stats['region'].replace(region_long_dict)
regions_network_stats['lat'] = regions_network_stats['region'].replace(region_lat_dict)

# Save to excel
regions_network_stats.to_excel('regions_network_stats.xlsx')



















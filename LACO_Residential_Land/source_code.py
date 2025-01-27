# -*- coding: utf-8 -*-
"""
Created on Wed Oct  2 20:49:19 2024

This code downloads a zip file of parcel data from
Los Angeles County. It dissolves the parcels by 'Use Type'
and then creates a map.

@author: coquito77
"""

import geopandas as gpd
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime as dt
import os
import requests, zipfile, io
from pathlib import Path
import logging

#get current working directory
cwd = os.getcwd()

logger = logging.getLogger(__name__)

logger.setLevel("INFO")
logger

formatter = logging.Formatter("{levelname} - {message}", style="{")
console_handler = logging.StreamHandler()
console_handler.setFormatter(formatter)
logger.addHandler(console_handler)

logger.info("%s: %s", "The Directory is", cwd)

#create data directory if not exist
if not os.path.exists(os.path.join(cwd, "data/")):
    os.makedirs(os.path.join(cwd, "data/"))

pd.set_option('display.max_columns', None) # this is to view all columns for validation

my_file = Path(os.path.join(cwd, "data/LACounty_Parcels.gdb")) # set the location of data file

# create variable to measure
use_type_land = "Residential"

# check if unziped file exits in data folder if not download it and unzip in folder
if not my_file.exists():
    
    # download the zip file of LA County parcels and unzip it
    r = requests.get("https://apps.gis.lacounty.gov/hubfiles/LACounty_Parcels.zip")
    z = zipfile.ZipFile(io.BytesIO(r.content))
    z.extractall(os.path.join(cwd, "data/"))

# create function to read map data
def map_reader(file1):
    df1 = gpd.read_file(file1)

    return df1

LA_parcels = map_reader(my_file)

LA_parcels.info() # get file information

# get map projection

crs = LA_parcels.crs

print(crs)

# read the data, then disolve the polygons based on the Use Type colum
startTime = dt.now() # start timer

LA_parcels['geometry'] = LA_parcels['geometry'].make_valid()

dissolved_gdf = LA_parcels.dissolve(by='UseType')

dissolved_gdf = dissolved_gdf.reset_index()

logger.info("%s: %s %s %s", "Done processing data", dt.now(), "done in", dt.now()-startTime)

# plot the map

fig = plt.figure(figsize=(7, 12))
ax = plt.gca()
dissolved_gdf[dissolved_gdf.UseType == use_type_land].plot(ax=ax, fc='r', ec='r', lw=0.5, alpha=0.8)
dissolved_gdf[dissolved_gdf.UseType != use_type_land].plot(ax=ax, fc='w', ec='k', lw=0.1, alpha=0.6)
ax.set_title('Los Angeles County Parcels that are ' + use_type_land)
plt.tight_layout()
plt.show()

# save plots directory if not exist, create folder if none exists
if not os.path.exists(os.path.join(cwd, "output/")):
    os.makedirs(os.path.join(cwd, "output/"))

fig.savefig(os.path.join(cwd, "output/LACO_Parcels_Disolved.png"), dpi=fig.dpi)

# Calculate total area in the County in square miles
LA_parcels['area'] = LA_parcels.geometry.area

# Calculate the total area in the County
total_area = LA_parcels['area'].sum()

logger.info("%s: %s %s", "Total area of County is", total_area/2.788e+7, "square miles.")

# Filter the data to only include the specified land type

filtered_data = LA_parcels[LA_parcels["UseType"] == use_type_land]

# Calculate the total area and convert the area to square miles by dividing by 2.788e+7
total_target_area = filtered_data.geometry.area.sum()

logger.info("%s %s %s %s %s", "Total area of", 
            use_type_land, "is:", total_target_area/2.788e+7, "square miles.")



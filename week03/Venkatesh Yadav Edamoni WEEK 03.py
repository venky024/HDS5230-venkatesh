import pandas as pd
import numpy as np
import time

df = pd.read_excel("clinics.xls")

def haversine(lat1, lon1, lat2, lon2):
    """Calculate distance between two points in miles"""
    MILES = 3959
    lat1, lon1, lat2, lon2 = map(np.deg2rad, [lat1, lon1, lat2, lon2])
    dlat = lat2 - lat1 
    dlon = lon2 - lon1 
    a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
    c = 2 * np.arcsin(np.sqrt(a)) 
    return MILES * c

ref_lat = df.iloc[0]['locLat']
ref_long = df.iloc[0]['locLong']

# Method 1: Simple for loop
def method_loop():
    start_time = time.time()
    distances = []
    for i in range(len(df)):
        d = haversine(ref_lat, ref_long, 
                     df.iloc[i]['locLat'], 
                     df.iloc[i]['locLong'])
        distances.append(d)
    end_time = time.time()
    return distances, end_time - start_time

# Method 2: Using pandas apply
def method_apply():
    start_time = time.time()
    distances = df.apply(lambda row: haversine(ref_lat, ref_long,
                                             row['locLat'],
                                             row['locLong']), axis=1)
    end_time = time.time()
    return distances, end_time - start_time

# Method 3: Vectorized approach
def method_vectorized():
    start_time = time.time()
    distances = haversine(ref_lat, ref_long,
                         df['locLat'].values,
                         df['locLong'].values)
    end_time = time.time()
    return distances, end_time - start_time

# For loop method
distances_loop, time_loop = method_loop()
print(f"For loop time: {time_loop:.4f} seconds")

# Apply method
distances_apply, time_apply = method_apply()
print(f"Apply method time: {time_apply:.4f} seconds")

# Vectorized method
distances_vector, time_vector = method_vectorized()
print(f"Vectorized time: {time_vector:.4f} seconds")

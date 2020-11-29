"""BeerCoin dia 4"""

from tsp_solver.greedy import solve_tsp
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

df = pd.read_excel("Working_data.xlsx")
d1 = df[df["D1"]==1]

# Para el cluster 1
ciudades = [i for i in range(d1.shape[0])]
arcos = [(i,j) for i in ciudades for j in ciudades if i!=j]

coord_x = d1["lon"].tolist()
coord_y = d1["lat"].tolist()

distancia = {(i,j):np.hypot(coord_x[i]-coord_x[j], coord_y[i]-coord_y[j]) for i,j in arcos}

a = np.zeros((len(distancia),len(distancia)), float)
a.shape

for i in range(d1.shape[0]):
    for j in range(d1.shape[0]):
        if i==j: continue
        a[i][j] = distancia[i,j]

path = solve_tsp(a)

"""Solo hay que sumar la distancia de la variable path"""


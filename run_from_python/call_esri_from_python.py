import pandas as pd
import numpy as np

import time
import subprocess

def run_ESRI(path_name, nodelist, edgelist):
    # nodelist: pd.DataFrame with cols: firm id, nace
    # edgelist: supplier, buyer
    # firm_ids must be consecutive and from 0!!

    s = time.gmtime()
    t0ESRI = time.time()
    print(f'starting ESRI calc at {time.strftime("%Y-%m-%d %H:%M:%S", s)}')

    nodelist.to_csv(path_name+'_nodelist.csv', index=False)
    edgelist.to_csv(path_name+'_edgelist.csv', index=False)
    
    path_ESRI_script = "~/bin/ESRI/ESRI_script_version.R"

    path_nodelist = path_name+'_nodelist.csv'
    path_edgelist = path_name+'_edgelist.csv'

    result = subprocess.check_output(f"Rscript {path_ESRI_script} {path_nodelist} {path_edgelist} {path_name}", shell=True,
                                     encoding='UTF-8')
    with open(f'{path_name}_output.log', 'w') as ofs:
        ofs.write(result)

    ESRI = np.array(result.split('### ESRI VECTOR ###')[-1].split(' ')).astype(float)

    s = time.gmtime()
    t1ESRI = time.time()
    print(f'finished ESRI calc at {time.strftime("%Y-%m-%d %H:%M:%S", s)} after {t1ESRI-t0ESRI:.1}s')

    return ESRI


### For testing:
import networkx as nx
import matplotlib.pyplot as plt

# make toy NW
nodelist = pd.DataFrame({"firm_id":np.arange(6), "nace":[1,1,10,11,46, 46]})
edgelist = pd.DataFrame({'supplier':[0,1,2,3,3], 'buyer':[2,2,3,4,5]})

# visualize it
G = nx.from_pandas_edgelist(edgelist, source='supplier', target='buyer', create_using=nx.DiGraph)
nx.draw(G)
plt.show()

# run ESRI
run_ESRI('./bla/', nodelist, edgelist)


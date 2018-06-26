 # -*- coding: utf-8 -*-
"""
this script defines which compound in list 1 corresponds with a molecule from list 2
"""
import rdkit
import pandas as pd
import os
from rdkit import Chem, DataStructs
from rdkit.Chem import AllChem


os.chdir('mydiretory')


compounds_list1=pd.read_csv('01_compounds_list1.csv')
compounds_list2=pd.read_csv('01_compounds_list2.csv')

#%%

def make_fingerprint(mol):
    return AllChem.GetMorganFingerprintAsBitVect(mol,2,nBits=1024)
               
#%% get fingerprint list1

fps_list1=[]

for rec in compounds_list1.iterrows():
    try:
        a=[rec[1][0],make_fingerprint(Chem.MolFromInchi(rec[1][2]))]
        fps_list1.append(a)
        print(rec[1][0])
    except Exception:
        continue 

#%%get fingerprint list2     

fps_list2=[]

for rec in compounds_list2.iterrows():
    try:
        a=[rec[1][0],make_fingerprint(Chem.MolFromInchi(rec[1][9]))]
        fps_lincs.append(a)
        print(rec[1][0])
    except Exception:
        continue 
    
#%% calculate similarity

matches=[]
for rec_2 in enumerate(fps_list2):#rec_q in enumerate(fps_lincs):
    best_similarity=0
    best_names=[]
    for rec_1 in enumerate(fps_list1):#rec_db in enumerate(fps_dundee):
        similarity=DataStructs.FingerprintSimilarity(
                                                     rec_2[1][1], rec_1[1][1], DataStructs.TanimotoSimilarity
                                                     )
        #print(qi,di)
        if similarity > best_similarity:
            best_similarity = similarity
            best_names =[rec_1[1][0]]
        elif similarity==best_similarity:
            best_names.append([rec_1[1][0]])
    print(rec_2[0])
    for m in best_names:
        matches.append([rec_2[1][0],m,best_similarity])

        
#%%

matches_df=pd.DataFrame(matches)
matches_df.columns=['query','match','similarity']

matches_df.to_csv('csv_export_similarities_List1toList2.csv', index=False)
























    
    
    
    

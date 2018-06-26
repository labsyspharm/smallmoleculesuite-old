import cmapPy as cm
import pandas as pd
import numpy as np
import scipy.spatial as scisp
import scipy.stats as scists
import pickle
import re

# load the drugs to test
drug_map = pd.read_csv('../INPUT/map_broadcmpd_to_lincscmpd.csv')
drugs = [(d,[b for i,b in enumerate(drug_map.broad_id) if drug_map.hms_name[i] is d]) \
    for d in drug_map.hms_name.drop_duplicates().values]
print drugs[:4]

# load the L1000 annotations
filename = '../DATA/LINCS_1/GSE92742_Broad_LINCS_Level5_COMPZ.MODZ_n476251x12328.gctx'
col_meta = cm.parse_gctx.get_column_metadata(filename)

def spearman(x):
    r = scists.spearmanr(x).correlation
    if np.isscalar(r):
        return r
    else:
        return r[0,1:]


all_dist = []
for iD1 in range(len(drugs)):

    for iD2 in range(iD1+1,len(drugs)):

        print((iD1,iD2,len(drugs)))
        c1 = [d for s in [[i for i in col_meta.index if j in i] for j in drugs[iD1][1]] for d in s]
        c2 = [d for s in [[i for i in col_meta.index if j in i] for j in drugs[iD2][1]] for d in s]

        # select the right rows in the gctx file
        l1k1 = cm.parse_gctx.parse(filename, cid = c1)
        l1k2 = cm.parse_gctx.parse(filename, cid = c2)

        # find the condition
        cond1 = [re.findall('\d_([\w_\.]*):[\w-]*:([\d\.]*)', i)[0] for i in l1k1.col_metadata_df.index]
        cond2 = [re.findall('\d_([\w_\.]*):[\w-]*:([\d\.]*)', i)[0] for i in l1k2.col_metadata_df.index]

        # match the conditions
        matched = [(i,[k for k,l in enumerate(cond2) if l == j],j) \
            for i,j in enumerate(cond1) if j in cond2]

        # calculate the correlation distance
        corrdist = np.asarray([1-spearman(np.hstack((l1k1.data_df.iloc[:,[m[0]]].values,
            l1k2.data_df.iloc[:,m[1]].values))).mean() for m in matched])
        cosinedist = np.asarray([  np.asarray([scisp.distance.cosine(l1k1.data_df.iloc[:,m[0]].values,
            l1k2.data_df.iloc[:,mi].values.T) for mi in m[1]]).mean() for m in matched])
        all_dist.append((drugs[iD1], drugs[iD2], corrdist, cosinedist))

    print('saving')
    pickle.dump(all_dist, open('all_dist.pkl','wb'))


f = open('/Users/mah47/Dropbox (HMS)/LINCS drug targets/DATA/summary_dist.tsv','w')
f.write('LINCS_name_1\tBRD_ids_1\tLINCS_name_2\tBRD_ids_2\t25-pct spearman\t25-pct cosine')
for i in all_dist:
    f.write('\n%s\t%s\t%s\t%s\t%.3f\t%.3f' % (i[0][0],' '.join(i[0][1]),i[1][0],' '.join(i[1][1]), \
        np.percentile(i[2],25),np.percentile(i[3],25)))
f.close()

f = open('/Users/mah47/Dropbox (HMS)/LINCS drug targets/DATA/all_dist.tsv','w')
f.write('LINCS_name_1\tBRD_ids_1\tLINCS_name_2\tBRD_ids_2\tspearman\tcosine')
for i in all_dist:
    f.write('\n%s\t%s\t%s\t%s\t%s\t%s' % (i[0][0],' '.join(i[0][1]),i[1][0],' '.join(i[1][1]), \
        np.array_str(i[2], max_line_width=np.inf)[2:-1], np.array_str(i[3], max_line_width=np.inf)[2:-1]))
f.close()

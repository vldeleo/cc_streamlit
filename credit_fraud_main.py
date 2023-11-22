import streamlit as sl 
import pandas as pd
import pyarrow.parquet as pq
from matplotlib import pyplot as plt # should maybe use plotly instead
#from credit_fraud_predictions import predict
import datetime
import numpy as np
import warnings
warnings.filterwarnings('ignore') # so warnings aren't plotted in streamlit app?

sl.set_page_config(page_title="Fraud Metrics", layout="wide", page_icon=":chart_with_upwards_trend:")
sl.title("Credit Fraud Alert: a DS4A project output")
#sl.header("a DS4A project output")
sl.text("Credit fraud causes major financial losses each year. What features of transactions can help us identify fraud?")
#sl.markdown("> caption here") #should probably use sl.caption instead.
# other options: json, latex, etc or can make any one of those with sl.write()


#if we want to upload data interactively, use the below line
#cc_data=sl.file_uploader('Upload a file?')



# maybe skip this when I've figured out the model?
#parquet_file = pq.ParquetFile('cc_data_subset.parquet')
parquet_file=pd.read_parquet('cc_data_subset.parquet')
#for batch in parquet_file.iter_batches():
#    print("RecordBatch")
#    batch_df = batch.to_pandas()
#    print("batch_df:", batch_df)

# subset to date range:
batch_df=parquet_file
batch_df["date"] = pd.to_datetime(batch_df["date"])


# interactive options
date1=sl.sidebar.date_input("Start Date")
date2=sl.sidebar.date_input("End Date")

# use this input to subset to correct date range
batch_df2=batch_df[~batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2))]
batch_df2['AlertFraud']=0 # default?

Topt=sl.sidebar.radio("Fraud Probability Threshhold", options=("95%", "90%", "75%", "66%", "50%")) # a sidebar... where I'll eventually want my input categories like "False Positive Rate"
#sl.sidebar.slider("Type 1 Error Threshhold", max_value=float(1.0), min_value=float(0.0), step=float(0.1))
if Topt=="95%":
    batch_df2.loc[batch_df2['probFraud'] >= 0.95, 'AlertFraud'] = 1
elif Topt=="90%":
    batch_df2.loc[batch_df2['probFraud'] >= 0.90, 'AlertFraud'] = 1
elif Topt=="75%":
    batch_df2.loc[batch_df2['probFraud'] >= 0.75, 'AlertFraud'] = 1
elif Topt=="66%":
    batch_df2.loc[batch_df2['probFraud'] >= 0.66, 'AlertFraud'] = 1
elif Topt=="50%":
    batch_df2.loc[batch_df2['probFraud'] >= 0.50, 'AlertFraud'] = 1



# other important calculations 
TotalTrans=batch_df.shape[0]
TotalTrans1=batch_df2.shape[0]
FraudTrans=batch_df[(batch_df['Is Fraud?']=="Yes")].shape[0]
FraudTrans1=batch_df2[(batch_df['Is Fraud?']=="Yes")].shape[0]


col0, col1, col2, col3 = sl.columns(4)

with col0:
    sl.metric(label="Alerts", value=batch_df2['AlertFraud'].sum())
with col1:
    sl.metric(label="Fraudulent Transactions", value = FraudTrans1) #need units, but equivalent to a big number box , delta = "0"
with col2:
    sl.metric(label="\% Total Transactions", value = round(FraudTrans1/TotalTrans1*100,2)) #need units, but equivalent to a big number box , delta = "0"
with col3:
    sl.metric(label="Fraudulent Losses", value = '%s%s' % ("$",round(batch_df.loc[batch_df['Is Fraud?']=="Yes"].Amount.sum(),0))) # eventually, could add , delta = "???"


col4, col5, col6 = sl.columns(3)

fig0=plt.figure()
plt.scatter(batch_df['Amount'], batch_df['Is Fraud?'])
plt.xlabel('Transaction Amount ($)')
plt.ylabel('Fraud?')
#sl.write(fig0)
fig1=plt.figure() # have to create figure in which the plot will be formed
   #plt.style.use() #get style from github??
plt.hist(batch_df['Total Debt'])
plt.xlabel('Total Debt ($)')
plt.title('Distribution of Credit Card Debt')
    #sl.write(fig1)


with col4:
    fig2=plt.figure()
    frauds=batch_df.groupby(batch_df.date.dt.month)['Is Fraud?'].sum()
    alltrans=batch_df.groupby(batch_df.date.dt.month).agg('count')
    #FraudTrans1=batch_df[~batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2)) & (batch_df['Is Fraud?']=="Yes")].shape[0]

    #plt.plot(batch_df.loc[batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2)), 'date'], batch_df.loc[batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2)), 'Is Fraud?']) # the verbose way of doing it
    plt.plot(batch_df2['date'], batch_df2['Is Fraud?'])
    plt.xlabel('Date')
    plt.ylabel('Fraudulent Transactions')
    plt.title('Fraud and Total Transactions through Time')
    sl.write(fig2)

with col5:
    fig3=plt.figure()
    TruePos=(batch_df2[(batch_df2['FraudBin'] == 1) & (batch_df2['AlertFraud'] == 1)].shape[0])
    TrueNeg=(batch_df2[(batch_df2['FraudBin'] == 0) & (batch_df2['AlertFraud'] == 0)].shape[0])
    FalsePos=(batch_df2[(batch_df2['FraudBin'] == 0) & (batch_df2['AlertFraud'] == 1)].shape[0])
    FalseNeg=(batch_df2[(batch_df2['FraudBin'] == 1) & (batch_df2['AlertFraud'] == 0)].shape[0])

    plt.pie(np.array([TruePos, TrueNeg, FalsePos, FalseNeg]))
    plt.title("Type I and II Error")
    sl.write(fig3)

with col6:
    #accuracy, precision, true positive/false negative 
    #batch_df2['week']=datetime.date(batch_df2['date']).isocalendar().week
    fig4=plt.figure()
    #fig4, ax1 = plt.subplots()
    #ax1.set_xlabel('Date') 
    #ax1.set_ylabel('True Positives', color = 'red') 
    batch_df2[['Month', 'Amount']].groupby('Month').mean().plot(type="bar")
    
    #plt.bar(batch_df2['Month'], batch_df2[['Amount']].groupby('Month').sum())
    
    #ax1.plot(batch_df2['date'], batch_df2[(batch_df2['FraudBin'] == 1) & (batch_df2['AlertFraud'] == 1)].sum(), color = 'red') 
    #ax1.tick_params(axis ='y', labelcolor = 'red') 
    # twin axes so I can plot a different Y
    #ax2 = ax1.twinx() 
    #ax2.set_ylabel('Y2-axis', color = 'blue') 
    #ax2.plot(x, data_2, color = 'blue') 
    #ax2.tick_params(axis ='y', labelcolor = 'blue') 
    #plt.title("True Positives/False Negatives")
    #plt.xlabel('Date')
    sl.write(fig4)

sl.subheader("Transaction Anomalies")
from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.datasets import make_blobs
from sklearn.preprocessing import StandardScaler # these might be unnecessary for my toy model
from sklearn.preprocessing import MinMaxScaler
from sklearn import datasets

from sklearn.cluster import DBSCAN
import numpy as np
'''
scaler = MinMaxScaler()
#X=scaler.fit_transform(batch_df2.iloc[: , [0:13,15:62,64]]) # I can't transform factors!
X=batch_df2.iloc[: , 0:13] # 15:62, 64

#min should be 2 * number of dimensions
clustering = DBSCAN(eps=3, min_samples=126).fit(X)
clustering.labels_
'''
'''
X, y_true = make_blobs(n_samples=1000, centers=4,
                       cluster_std=0.50, random_state=0)
db = DBSCAN(eps=0.3, min_samples=10).fit(X)
core_samples_mask = np.zeros_like(db.labels_, dtype=bool)
core_samples_mask[db.core_sample_indices_] = True
labels = db.labels_
 
# Number of clusters in labels, ignoring noise if present.
n_clusters_ = len(set(labels)) - (1 if -1 in labels else 0)
 
# Plot result
# Black used for noise 
unique_labels = set(labels)
colors = ['y', 'b', 'g', 'r']
print(colors)
for k, col in zip(unique_labels, colors):
    if k == -1:
        col = 'k'
 
    class_member_mask = (labels == k)
 
    xy = X[class_member_mask & core_samples_mask]
    plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
             markeredgecolor='k',
             markersize=6)
 
    xy = X[class_member_mask & ~core_samples_mask]
    plt.plot(xy[:, 0], xy[:, 1], 'o', markerfacecolor=col,
             markeredgecolor='k',
             markersize=6)
 
plt.title('number of clusters: %d' % n_clusters_)
plt.show()
sc = metrics.silhouette_score(X, labels)
print("Silhouette Coefficient:%0.2f" % sc)
ari = adjusted_rand_score(y_true, labels)
print("Adjusted Rand Index: %0.2f" % ari)
'''

#alternative method
#scaler = MinMaxScaler() 
#num2 = scaler.fit_transform(batch_df2.iloc[: , [0:13,15:62,64]])
num2 = batch_df2.iloc[: , 0:13] #15:62,64

outlier_detection = DBSCAN(
 eps = .2, 
 metric='gower', 
 min_samples = 28, #2xdimensions
 n_jobs = -1)
#clusters = outlier_detection.fit_predict(num2)
clusters = DBSCAN(eps=3, min_samples=28).fit(num2)

sl.write(clusters.labels_)

'''
from matplotlib import cm
cmap = cm.get_cmap('Set1')
plt.scatter(x='Amount', y ='Income', c=clusters, cmap=cmap,
 colorbar = False)
'''

col7, col8, col9= sl.columns(3)


sl.subheader("Customer Profiles")

sl.subheader("Anomaly Clustering")
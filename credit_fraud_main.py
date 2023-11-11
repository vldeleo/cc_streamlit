import streamlit as sl 
import pandas as pd
import pyarrow.parquet as pq
from matplotlib import pyplot as plt # should maybe use plotly instead
#from credit_fraud_predictions import predict
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

#with col6:
#accuracy, precision, true positive/false negative 


sl.subheader("Transaction Anomalies")
col7, col8, col9= sl.columns(3)


sl.subheader("Customer Profiles")

sl.subheader("Anomaly Clustering")
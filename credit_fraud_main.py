import streamlit as sl 
import pandas as pd
import pyarrow.parquet as pq
from matplotlib import pyplot as plt # should maybe use plotly instead
#from credit_fraud_predictions import predict

import warnings
warnings.filterwarnings('ignore') # so warnings aren't plotted in streamlit app?

sl.set_page_config(page_title="Fraud Metrics", layout="wide", page_icon=":chart_with_upwards_trend:")
sl.title("Credit Fraud Alert: a DS4A project output")
#sl.header("a DS4A project output")
sl.text("Credit fraud causes major financial losses each year. What features of transactions can help us identify fraud?")
#sl.markdown("> caption here") #should probably use sl.caption instead.
# other options: json, latex, etc or can make any one of those with sl.write()
code="""
print("hi")
"""
#sl.code(code, language = "python") # this just shows as code

#if we want to upload data interactively, use the below line
#cc_data=sl.file_uploader('Upload a file?')



# maybe skip this when I've figured out the model?
parquet_file = pq.ParquetFile('credit_card_data_da.parquet')
for batch in parquet_file.iter_batches():
    print("RecordBatch")
    batch_df = batch.to_pandas()
    print("batch_df:", batch_df)

batch_df["date"] = pd.to_datetime(batch_df["date"])
#date = batch_df["date"]

# interactive options
date1=sl.sidebar.date_input("Start Date")
date2=sl.sidebar.date_input("End Date")

sl.sidebar.radio("Accuracy Threshhold", options=("95%", "90%", "75%", "66%")) # a sidebar... where I'll eventually want my input categories like "False Positive Rate"
sl.sidebar.slider("Type 1 Error Threshhold", max_value=float(1.0), min_value=float(0.0), step=float(0.1))


#df[~df['values'].between('2017-03-02', '2017-03-05', inclusive=False)]

TotalTrans=batch_df.shape[0]
TotalTrans1=batch_df[~batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2))].shape[0]
FraudTrans=batch_df[(batch_df['Is Fraud?']=="Yes")].shape[0]
FraudTrans1=batch_df[~batch_df['date'].between(pd.to_datetime(date1), pd.to_datetime(date2)) & (batch_df['Is Fraud?']=="Yes")].shape[0]


col1, col2, col3 = sl.columns(3)

with col1:
    sl.metric(label="Fraudulent Transactions", value = FraudTrans1) #need units, but equivalent to a big number box , delta = "0"
with col2:
    sl.metric(label="\% Total Transactions", value = round(FraudTrans1/TotalTrans1*100,2)) #need units, but equivalent to a big number box , delta = "0"
with col3:
    sl.metric(label="Fraudulent Losses", value = '%s%s' % ("$",round(batch_df.loc[batch_df['Is Fraud?']=="Yes"].Amount.sum(),0))) # eventually, could add , delta = "???"


sl.columns(1)
#sl.data.frame(table) #differs from table because this becomes sortable!
#if opt=="95%":
#    modelOutput=
#elif opt=="90%":

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
sl.write(fig1)

fig2=plt.figure()
frauds=batch_df.groupby(batch_df.date.dt.month)['Is Fraud?'].sum()
alltrans=batch_df.groupby(batch_df.date.dt.month).agg('count')

plt.plot(batch_df['date'], batch_df['Is Fraud?'])
#plt.plot(batch_df.date.dt.month, frauds )
#plt.plot(alltrans, frauds)
plt.xlabel('Monthly Transactions')
plt.ylabel('Monthly Fraudulent Transactions')
plt.title('Fraud and Total Transactions through Time')
sl.write(fig2)

#sl.write(batch_df.shape[0]) # number of rows. [1] would give number of columns
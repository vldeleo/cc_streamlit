import sklearn as sk
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
import pandas as pd
import joblib
import numpy as np

# import the data
cc_fraud=pd.read_parquet("C:/Users/torie/Documents/Python Scripts/Streamlit/credit_card_data_da.parquet") # too big!
cc_fraud.sample(frac=1, random_state=1031)

# checked nulls in R: one column I'm interested in is the zipcode of merchant. I wonder if I can interpolate?
# another column I'd like to fix, if time, is the fact that user != id

# does merchant zipcode match buy zipcode?
cc_fraud['GeoDif']=(cc_fraud['Zipcode']==cc_fraud['Zip']).astype(int) # gives a 1 if they are the same, a 0 if not


#parquet_file = pq.ParquetFile('C:/Users/torie/Documents/Python Scripts/Streamlit/credit_card_data_da.parquet')
#for batch in parquet_file.iter_batches():
#    print("RecordBatch")
#    batch_df = batch.to_pandas()
#    print("batch_df:", batch_df)


# selecting columns to use for independent and dependent variables
X = cc_fraud[['Amount', 'GeoDif']] #'Use Chip', 'Merchant Name', 'Errors?' ... cannot use categorical variables in random forest
    # eventually I want to add more/all features tbh
y = cc_fraud[['Is Fraud?']]

# split data into train and test sets
    # 70% training and 30% test, because that's standard
X_train, X_test, y_train, y_test = train_test_split(
X, y, test_size=0.3, random_state=1031, stratify=y)

# create an instance of the random forest classifier (eventually my density based model?)

clf = RandomForestClassifier(n_estimators=100)

# train the classifier on the training data
clf.fit(X_train, y_train.values.ravel())

# predict on the test set
y_pred = clf.predict(X_test)

# calculate accuracy
accuracy = accuracy_score(y_test, y_pred)
print(f'Accuracy: {accuracy}') # Accuracy: 0.9987??? that's not possible
#print(y_pred.head())

# save model 
# save the model to disk
joblib.dump(clf, 'rf_fraud_model.sav')

y_prob = clf.predict_proba(X_test)
#cc_pred=X_test.join(y_test, y_pred, y_prob)
X_test['y_actual']=y_pred
X_test['y_probs']=y_prob
#cc_pred=np.column_stack((X_test,y_test,y_pred, y_prob))
#cc_pred = np.hstack([X_test, y_test, y_pred, y_prob])
np.savetxt("RF_prob_preds.csv", X_test, delimiter=",")
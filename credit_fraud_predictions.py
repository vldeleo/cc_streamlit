# making a function that will use our fitted fraud model to return values via streamlit app

import joblib
def predict(data):
    clf=joblib.load('rf_fraud_model.sav')
return clf.predict(data)
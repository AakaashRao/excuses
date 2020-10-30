from nltk.corpus import stopwords
import pandas as pd
import pdb
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVR
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn import metrics
from sklearn.metrics import confusion_matrix
import json

cross_validate = True

with open('code/table-notes.json') as f:
	notes = json.load(f)

def main():
	data = pd.read_stata('data/working/exp1.dta')
	data = data.dropna(subset=['openendedmotives'])
	data = data[data['openendedmotives']!=""]
	data['condition'] = data['excuse'].replace({1:'Excuse', 0:'No excuse'})

	samples = []
	for score in ['cultural_score', 'gullibility_score']:

		if score == 'cultural_score':
			training = data[data['bias']==1]
		elif score == 'gullibility_score':
			training = data[data['gullibility']==1]
			
		training = training[(training[score]>=0) & (training[score]<=100)]
		training[score] = (training[score]-training[score].mean())/training[score].std()

		tfidfconverter = TfidfVectorizer(min_df=10, max_df=0.5, 
			stop_words=stopwords.words('english'), ngram_range=(1,2))

		Xtrain = tfidfconverter.fit_transform(training['openendedmotives']).toarray()
		ytrain = training[score]

		svr = SVR()

		if cross_validate:
			crv = GridSearchCV(svr, {'kernel':['rbf'], 'C':[1,3,5,7,9]})
			crv.fit(Xtrain, ytrain)
			print(crv.best_estimator_.C)
			svr = crv.best_estimator_

		svr.fit(Xtrain, ytrain)

		X = tfidfconverter.transform(data['openendedmotives']).toarray()
		y = data[score]

		y_prob_predicted = svr.predict(X)
		
		data[score+'_predicted'] = y_prob_predicted

	data = data[['responseid','cultural_score_predicted','gullibility_score_predicted']]

	data.to_stata('data/working/exp1-svr-scores.dta')

main()

'''

'''

from nltk.corpus import stopwords
import pandas as pd
import pdb
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn import metrics
from sklearn.metrics import confusion_matrix
import json

with open('code/table-notes.json') as f:
	notes = json.load(f)

note='test'
cross_validate = False
def main(data_path, data_path2, exp):
	data = pd.read_stata(data_path)
	data2 = pd.read_stata(data_path2)
	data = data.append(data2).reset_index(drop=True)
	#data = data[(data['condition']=='excuse') | (data['condition']=='noexcuse')]
	#data['excuse'] = pd.to_numeric(data['condition']=='excuse')
	data = data.dropna(subset=['purpose'])
	data = data[data['purpose']!=""]

	print('Excuse vs. no excuse')
	matrices = []
	matrices.append(confusion_matrices(data, ['excuse','noexcuse'], exp))
	if exp=='1':
		consolidated = [matrices[0][0], \
						'\\caption{Experiment 2: Condition prediction confusion matrix}',
						'\\label{t:1-conditionprediction}'] +\
					   matrices[0][3:6] +\
					   [r'& \textbf{Predicted Excuse} & \textbf{Predicted Excuse} \\'] +\
					   [r'\midrule'] +\
					   matrices[0][9:15] +\
					   [r'\begin{tablenotes} \footnotesize',
					   r'\item \textit{Notes:} ' + notes['t1-conditionprediction'], r'\end{tablenotes}',
					   r'\end{threeparttable}', r'\end{table}']
		with open('output/tables/t1-conditionprediction.tex', 'w') as f:
			f.writelines([l+'\n' for l in consolidated])

	if exp=='2':
		print('Control vs. no excuse')
		matrices.append(confusion_matrices(data, ['control','noexcuse'], exp))
		print('Control vs. excuse')
		matrices.append(confusion_matrices(data, ['control','excuse'], exp))
		intermediate = [r'\midrule',\
						r'& \textbf{Predicted Excuse} & \textbf{Predicted Excuse} \\',
						r'\midrule']
		consolidated = [matrices[0][0], \
						'\\caption{Experiment 1: Condition prediction confusion matrices}',
						'\\label{t:2-conditionprediction}'] +\
					   matrices[0][3:6] +\
					   [r'\multicolumn{3}{l}{\textbf{Panel A}: \textit{Excuse vs. No Excuse}} \\'] +\
					   intermediate +\
					   matrices[0][9:13] +\
					   [r'\midrule', r'\midrule'] +\
					   [r'\multicolumn{3}{l}{\textbf{Panel B}: \textit{Control vs. No Excuse}} \\'] +\
					   intermediate +\
					   matrices[1][9:13] +\
					   [r'\midrule', r'\midrule'] +\
					   [r'\multicolumn{3}{l}{\textbf{Panel C}: \textit{Control vs. Excuse}} \\'] +\
					   intermediate +\
					   matrices[2][9:13] +\
					   [r'\midrule', r'\bottomrule', r'\end{tabular}', \
					   r'\begin{tablenotes} \footnotesize',
					   r'\item \textit{Notes:} ' + notes['t2-conditionprediction'], r'\end{tablenotes}',
					   r'\end{threeparttable}', r'\end{table}']
		with open('output/tables/t2-conditionprediction.tex', 'w') as f:
			f.writelines([l+'\n' for l in consolidated])

def confusion_matrices(data, labels, exp):
	data = data[(data['condition']==labels[0]) | (data['condition']==labels[1])]

	tfidfconverter = TfidfVectorizer(min_df=10, max_df=0.5, 
		stop_words=stopwords.words('english'), ngram_range=(1,2))

	X = tfidfconverter.fit_transform(data['purpose']).toarray()
	y = data['condition']
	X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)
	#classifier = RandomForestClassifier(n_estimators=100, random_state=0)
	classifier = SVC()
	if cross_validate:
		clf = GridSearchCV(classifier, {'kernel':['rbf'], 'C':[0.25, 0.5, 1, 1.5]})
		clf.fit(X, y)
		print(clf.best_estimator_.C)
		classifier = clf.best_estimator_
	classifier.fit(X_train, y_train)
	y_test_predicted = classifier.predict(X_test)
	accuracy = classifier.score(X_test, y_test)
	print(accuracy)
	matrix = confusion_matrix(y_test, y_test_predicted, labels=labels)
	latex = make_latex(matrix, labels = labels, accuracy = accuracy, exp=exp)
	with open('output/tables/t{}-conditionprediction-{}-{}.tex'.format(exp, labels[0], labels[1]), 'w') as f:
		f.writelines([l+'\n' for l in latex])
	return latex

def make_latex(matrix, labels, accuracy, exp):
	accuracy = '{:.4f}'.format(round(accuracy, 4))
	label_dict = {'control': 'Control', 'noexcuse': 'No Excuse', 'excuse': 'Excuse'}
	labels_f = [label_dict[l] for l in labels]
	lines = [r'\begin{table} \centering', \
		'\\caption{{Confusion matrix: {} vs. {} }}'.format(labels_f[0], labels_f[1]),\
		'\\label{{t:{}-conditionprediction-{}-{} }}'.format(exp, labels[0], labels[1]),
		r'\begin{threeparttable}',
		r'\begin{tabular}{@{\extracolsep{4pt}}r|cc}', \
		r'\toprule', \
		'& \\textbf{Predicted} & \\textbf{Predicted} \\\\',
		'& \\textbf{{ {} }} & \\textbf{{ {} }} \\\\'.format(labels_f[0], labels_f[1]),
		r'\midrule', \
		'\\textbf{{True {} }} & {} & {} \\\\'.format(labels_f[0], matrix[0,0], matrix[0,1]), \
		'\\textbf{{True {} }} & {} & {} \\\\'.format(labels_f[1], matrix[1,0], matrix[1,1]), \
		r'\midrule', \
		'\\multicolumn{{3}}{{r}}{{\\small \\textit{{Overall accuracy: }} {} }} \\\\'.format(accuracy),\
		r'\bottomrule', \
		r'\end{tabular}', \
		r'\end{threeparttable}',\
		r'\end{table}']
	return lines

main('data/raw/exp1-pilot-deidentified.dta' , 'data/raw/exp1-mainexp-deidentified.dta', '1')
main('data/raw/exp2-pilot-deidentified.dta', 'data/raw/exp2-mainexp-deidentified.dta', '2')


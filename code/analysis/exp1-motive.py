'''
This script appends two columns to the deidentified dataset: bias_word and
gullibility_word. bias_word and gullibility_word take value 1 if the 
openendedmotives response contains a bias stem or a gullibility stem, 
respectively.
'''

from nltk.stem import SnowballStemmer
import pandas as pd
import pdb
import json

stemmer = SnowballStemmer(language='english')

# Replace with deidentified main exp data
data_path = 'data/raw/exp1-pilot-deidentified.dta' 
data_path2 = 'data/raw/exp1-mainexp-deidentified.dta' 

# Replace with main exp output path
output_path = 'data/working/exp1-motives.csv' 

def _custom_stem(word):
	'''
	NLTK's Snowball Stemmer leaves "racist" and "racism" unchanged. We 
	modify the stemmer to stem both terms to "racis". 
	'''

	if 'racis' in word:
		return 'racis'
	else:
		return stemmer.stem(word)

def _get_synonyms():
	'''
	Synonyms drawn from www.thesaurus.com on 2019-02-17. All "most relevant" synonyms 
	(and only "most relevant" synonyms), as marked on the webpage, are included. We
	stem all synonyms so that we capture different parts of speech.
	'''

	with open('data/raw/synonyms.json', 'r') as f:
  		synonyms = json.load(f)
	
	stemmed_synonyms = {}
	for dimension in ['bias','gullibility']:
		stemmed_synonyms[dimension] = {}
		for base_word, synonym_list in synonyms[dimension].items():
			base_word_stemmed = _custom_stem(base_word) if base_word != 'racist' else 'racis'
			synonyms_stemmed = [_custom_stem(synonym) if synonym != 'racist' else 'racis' for synonym in synonym_list]
			stemmed_synonyms[dimension][base_word_stemmed] = synonyms_stemmed
	return stemmed_synonyms

def main():
	synonyms = _get_synonyms()

	bias_words = set([word for word in synonyms['bias'].keys()])
	bias_words_extended = set([word for wordlist in synonyms['bias'].values() \
								for word in wordlist]) | bias_words
	gullibility_words = set([word for word in synonyms['gullibility'].keys()])
	gullibility_words_extended = set([word for wordlist in synonyms['gullibility'].values() \
										for word in wordlist]) | gullibility_words

	data = pd.read_stata(data_path)
	data2 = pd.read_stata(data_path2)
	data = data.append(data2).reset_index(drop=True)

	data['bias_word'] = data['openendedmotives'].str.contains('|'.join(bias_words_extended), case=False)
	data['gullibility_word'] = data['openendedmotives'].str.contains('|'.join(gullibility_words_extended), case=False)

	word_cols = ['bias_word', 'gullibility_word']
	data = data[['responseid'] + word_cols]
	for col in word_cols:
		data[col] = data[col].astype(int)

	data.to_csv(output_path, index=False)

main()

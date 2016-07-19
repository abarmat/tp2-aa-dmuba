import pprint
import re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

from sklearn import metrics, cross_validation
from sklearn.base import BaseEstimator, TransformerMixin
from sklearn.feature_extraction import DictVectorizer
from sklearn.feature_extraction.text import CountVectorizer, TfidfTransformer
from sklearn.naive_bayes import MultinomialNB
from sklearn.pipeline import Pipeline, FeatureUnion
from sklearn.preprocessing import Imputer
from sklearn.svm import LinearSVC, SVC
from sklearn.grid_search import GridSearchCV

from nltk.stem.snowball import SpanishStemmer
from nltk.corpus import stopwords

import xgboost


def merge_dicts(*dict_args):
    '''
    Given any number of dicts, shallow copy and merge into a new dict,
    precedence goes to key value pairs in latter dicts.
    '''
    result = {}
    for dictionary in dict_args:
        result.update(dictionary)
    return result


def dict_list(l, i):
    return [e[i] for e in l]


class MultiItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, keys):
        self.keys = keys

    def fit(self, x, y=None):
        return self

    def transform(self, data_dict):
        l = []
        for k in self.keys:
            l.append([{k: e} for idx, e in data_dict[k].iteritems()])
        return [merge_dicts(*dict_list(l, i)) for i in range(len(l[0]))]


class ItemSelector(BaseEstimator, TransformerMixin):
    def __init__(self, key):
        self.key = key

    def fit(self, x, y=None):
        return self

    def transform(self, data_dict):
        return data_dict[self.key]


def read_file(filename):
    d = pd.read_csv(filename, sep=';')
    return d.ix[:,:-1], d['Clase']


def save_prediction(filename, data):
    with open(filename, 'w') as f:
        for item in data:
            proba = map(lambda e: str(round(e, 4)), item[1])
            f.write(';'.join([str(item[0])] + proba) + '\n')


def parse_doc(title, body):
    title = '' if title is np.nan else title
    body = '' if body is np.nan else body
    doc = (title + ' ' + body).strip()
    return doc.decode('latin-1')


def parse_latlon(text):
    if pd.isnull(text):
        return (0, 0)
    return map(lambda e: round(e, 3), map(abs, map(float, text.split(','))))


def process(X, y):
    # Create body attr
    X_body = [parse_doc(e[0], e[1]) for e in X[['tit', 'des']].values.tolist()]
    X['body'] = pd.Series(X_body, index=X.index)

    # Y
    y = np.array(y)

    return (X, y)


def show_feature_importance(clf):
    fn = clf.named_steps['vec'].get_feature_names()
    fn = np.asarray(fn)

    for class_id in range(5):
        top = np.argsort(clf.named_steps['clf'].coef_[class_id])[-10:]
        print('[{}]'.format(class_id + 1))
        for idx, feature in enumerate(fn[top]):
            print('+ {} : {}'.format(feature, top[idx]))


def plot_confusion_matrix(cm, classes, title='Confusion matrix', cmap=plt.cm.Blues):
    plt.imshow(cm, interpolation='nearest', cmap=cmap)
    plt.title(title)
    plt.colorbar()
    tick_marks = np.arange(len(classes))
    plt.xticks(tick_marks, classes, rotation=45)
    plt.yticks(tick_marks, classes)
    plt.tight_layout()
    plt.ylabel('True label')
    plt.xlabel('Predicted label')


class CustomTokenizer(object):
    def __init__(self, stemming=False):
        self.stemmer = SpanishStemmer()
        self.stemming = stemming

    def __call__(self, doc):
        pattern = re.compile(r"(?u)\b[a-zA-Z][a-zA-Z]+\b")
        tokens = pattern.findall(doc)
        if self.stemming:
            tokens = [self.stemmer.stem(token) for token in tokens]
        return tokens


def main():
    # Ensure consistency of tests
    rng = np.random.RandomState(100)

    # Options
    opt_cross_validate = True
    opt_show_report = True
    opt_save_prediction = True
    opt_show_feature_importance = False
    opt_grid_search = False
    opt_plot_confusion_matrix = False

    # Read data
    X_train, y_train = read_file('tp2-work-train-final.csv')
    X_test, y_test = read_file('tp2-clasificacion-V2.csv')

    # Preprocess
    X_train, y_train = process(X_train, y_train)
    X_test, y_test = process(X_test, y_test)

    txt_fields = 'body'
    num_fields = ['lat', 'lon']

    # Classification pipeline
    vectorizer = CountVectorizer(
        ngram_range=(1,4),
        strip_accents='ascii',
        lowercase=True,
        stop_words=stopwords.words('spanish') + ['argentina', 'id', 'mls'],
        tokenizer=CustomTokenizer(stemming=True)
    )
    clf = Pipeline([
        ('union', FeatureUnion(transformer_list=[
            # ('num', Pipeline([
            #     ('sel', MultiItemSelector(num_fields)),
            #     ('dic', DictVectorizer(sparse=False))
            # ])),
            ('txt', Pipeline([
                ('sel', ItemSelector(txt_fields)),
                ('vec', vectorizer)
            ]))
        ])),
        # ('clf', LinearSVC(C=0.01)),
        # ('clf', MultinomialNB(alpha=0.5))
        ('clf', SVC(probability=True, kernel='linear', C=0.01))
    ])

    # Cross-validate
    if opt_cross_validate:
        k_fold = cross_validation.KFold(n=len(X_train), n_folds=10, random_state=rng)
        results = cross_validation.cross_val_score(
            clf, X_train, y_train, cv=k_fold, n_jobs=-1
        )
        print(results)
        print(sum(results)/len(results))

    # Grid search
    if opt_grid_search:
        param_grid = {
            'clf__C': [0.01],
            'union__txt__vec__lowercase': [True, False],
            'union__txt__vec__ngram_range': [(1,2), (1,3), (1,4), (2,4)]
        }
        grid = GridSearchCV(clf, param_grid, n_jobs=4, verbose=3)
        grid.fit(X_train, y_train)
        print(grid.best_params_)

    # Fit
    clf = clf.fit(X_train, y_train)

    # Test
    y_pred = clf.predict(X_test)

    if opt_show_report:
        # Model performance
        accuracy = metrics.accuracy_score(y_test, y_pred)
        print(accuracy)

        # Confusion Matrix
        cm = metrics.confusion_matrix(y_test, y_pred)
        print(cm)
        if opt_plot_confusion_matrix:
            plt.figure()
            plot_confusion_matrix(cm, range(1,6))
            plt.show()
        
        # Report
        report = metrics.classification_report(y_test, y_pred)
        print(report)

    if opt_show_feature_importance:
        show_feature_importance(clf)

    if opt_save_prediction:
        save_prediction('svm-pred-test.csv', zip(y_pred, clf.predict_proba(X_test)))


if __name__ == '__main__':
    main()

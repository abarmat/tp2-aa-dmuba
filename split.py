import pandas as pd
import string
import sys

from sklearn.cross_validation import train_test_split


def save_data_file(filename, data):
    with open(filename, "w") as f:
        f.write(data)


def remove_file_ext(filename):
    return '.'.join(filename.split('.')[:-1])


def main():
    # Args
    input_filename = sys.argv[1]
    test_size = 0.2
    remove_header = True

    noext_filename = remove_file_ext(input_filename)

    with open(input_filename, "r") as f:
        data = map(string.strip, f.readlines())

        # Header
        header = data[0]
        if remove_header:
            data = data[1:]

        # Split
        train_set, test_set = train_test_split(data, test_size=test_size)

        # Save files
        d = pd.read_csv(input_filename, sep=';')
        save_data_file(noext_filename + '.train.csv', '\n'.join([';'.join(list(d.columns))] + train_set))
        save_data_file(noext_filename + '.test.csv',  '\n'.join([';'.join(list(d.columns))] + test_set))


if __name__ == '__main__':
    main()

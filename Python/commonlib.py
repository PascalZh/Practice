import os
import urllib.request
import csv


class TaskProgress(object):
    '''
    This class is used to save task progress in a file and read from it.
    the file is named '.TaskProgress.tpr'
    the file will be looked like as following:

    TASK1_COMPLETED
    TASK2_COMPLETED
    '''
    __slots__ = ('flags', )

    def __init__(self):
        self.__update_flags()

    def __update_flags(self):
        if os.path.exists('.TaskProgress.tpr'):
            with open('.TaskProgress.tpr', 'r') as f:
                self.flags = [s.strip() for s in f.readlines()]
        else:
            self.flags = []

    def check(self, str_):
        if str_ not in self.flags:
            self.flags.append(str_)
            with open('.TaskProgress.tpr', 'a') as f:
                f.writelines([str_ + '\n'])

    def ischecked(self, str_):
        self.__update_flags()
        return str_ in self.flags

    def list(self):
        return self.flags


def open_url(url):
    return urllib.request.urlopen(url)


def read_csv(path):
    '''read csv in a unix dialect, return a 2d array(a table) as a list'''
    ret = []
    with open(path, 'r') as f:
        csv_reader = csv.reader(f, dialect='unix')
        for row in csv_reader:
            ret.append(row)
    return ret


def write_csv(path, table):
    '''write csv in a unix dialect'''
    with open(path, 'w') as f:
        csv_writer = csv.writer(f, dialect='unix')
        csv_writer.writerows(table)


def print_lst(_iterable):
    '''print _iterable (such as a list) with \n append to every
    element'''
    for i in _iterable:
        print(i)

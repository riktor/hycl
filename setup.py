# -*- coding: utf-8 -*-
import sys
import os
from os import path
from setuptools import find_packages, setup


here = os.path.dirname(os.path.abspath(__file__))
version = next((line.split('=')[1].strip().replace("'", '')
                for line in open(os.path.join(here,
                                              'hycl',
                                              '__init__.py'))
                if line.startswith('__version__ = ')),
               '0.0.dev0')

setup(
    name='hycl',
    version=version,
    description='hycl : common-lisp-like functions and macros for hylang',

    url='https://github.com/niitsuma/hycl/',
    author='Riku Togashi',
    author_email='riktor1221@gmail.com',
    license='MIT License',
    classifiers=[
	'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3.6',
    ],
    keywords='hy lisp common-lisp',
    install_requires=['hy>=0.15.0'],
    packages=['hycl'],
    package_data={'hycl': ['*.hy'],},
    test_suite='nose.collector',
    tests_require=['nose'],
    platforms='any',
)

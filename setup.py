# -*- coding: utf-8 -*-
import sys
import os
from os import path
from setuptools import setup

if sys.version_info < (2, 7):
    sys.exit('hycl requires Python 2.7 or higher')

ROOT_DIR = path.abspath(path.dirname(__file__))
sys.path.insert(0, ROOT_DIR)

LONG_DESCRIPTION = open(path.join(ROOT_DIR, 'README.rst')).read()
# version
here = os.path.dirname(os.path.abspath(__file__))
version = next((line.split('=')[1].strip().replace("'", '')
                for line in open(os.path.join(here,
                                              'hycl',
                                              '__init__.py'))
                if line.startswith('__version__ = ')),
               '0.0.dev0')

HYSRC = ['**.hy']


setup(
    name='hycl',
    version=version,
    description='hycl : common-lisp-like functions and macros for hylang',
    long_description=LONG_DESCRIPTION,
    url='https://github.com/riktor/hycl/',
    author='Riku Togashi',
    author_email='riktor1221@gmail.com',
    license='BSD-New',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
		'License :: OSI Approved :: MIT License',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
    ],
    keywords='hy lisp common-lisp',
    install_requires=[
        'hy>=0.13.0',
    ],
    packages=['hycl'],
    package_data={'hycl': HYSRC},
    platforms='any',
)

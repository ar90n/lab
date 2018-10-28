"""Minimal setup file """

from setuptools import setup, find_packages

setup(
    name='python_lifegame',
    version='0.1.0',
    license='proprietary',
    description='python life game',
    author='Masahiro Wada',
    author_email='argon.argon.argon@gmail.com',
    url='https://github.com/ar90n/lab/sandbox/python-lifegame',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},

    install_requires=[],
    entry_points={
        'console_scripts': [
            'lifegame = lifegame.cli:main',
        ]
    },
)

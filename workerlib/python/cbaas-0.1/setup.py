import os.path
from setuptools import setup, find_packages

def current_path(file_name):
    return os.abspath(os.path.join(__file__, os.path.pardir, file_name))

setup(
    name = 'cbaas',
    version = '0.1',
    include_package_data = True,
    packages=['cbaas'],
)


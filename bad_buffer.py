import os

import os.path
from django.shortcuts import get_object_or_404
from importmagic.index import SymbolIndex


config_path = os.path.join(os.getcwd(), 'index.json')
index = SymbolIndex.deserialize(config_path)


get_object_or_404('n')
render('nada')

config_path = os.path.join(os.getcwd(), 'index.json')
index = SymbolIndex.deserialize(config_path)


get_object_or_404('n')

render('nada')

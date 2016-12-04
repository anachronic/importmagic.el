import os
import sys
import threading
from collections import deque

import importmagic
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

# We'll follow a very passive approach. I'm not really familiar with
# neither EPC or cl-lib. So please, don't expect to see a lot of
# quality in this code

index = None


# Since the transformation from Emacs Lisp to Python causes strings to
# be lists of separate characters, we need a function that can provide
# a regular string, which is this one.
def _stringify(input_param):
    return ''.join(input_param)


# Construct the symbol index specified by the paths given. As the
# names suggest, these paths correspond to sys path and user_path. We
# still have to figure out if sys.path and user_path default values
# are ok.
def _build_index(sys_path=sys.path, user_path=None):
    # since index is a global variable, need the global keyword. I did
    # not know this
    # http://stackoverflow.com/questions/423379/using-global-variables-in-a-function-other-than-the-one-that-created-them
    global index
    try:
        paths = []

        if user_path is not None:
            if isinstance(user_path, list):
                paths = paths + user_path
            else:
                paths.append(user_path)

        if isinstance(sys_path, list):
            paths = paths + sys_path
        else:
            paths.append(sys_path)

        index = importmagic.SymbolIndex()
        index.build_index(paths=paths)
    except:
        print('Failed to build index')
        sys.exit(-1)


# Launch a thread that builds the index.
def build_index(sys_path=sys.path, user_path=None):
    thread = threading.Thread(target=_build_index, args=(user_path, sys_path))
    thread.daemon = True
    thread.start()


# Returns a list of every unresolved symbol in source.
@server.register_function
def get_unresolved_symbols(*source):
    source = _stringify(source)

    scope = importmagic.Scope.from_source(source)
    unres, unref = scope.find_unresolved_and_unreferenced_symbols()

    return list(unres)


# Returns a list of candidates that can import the queried symbol. The
# returned list is ordered by score, meaning that the first element is
# more likely to be appropriate.
@server.register_function
def get_candidates_for_symbol(*symbol):
    symbol = _stringify(symbol)

    candidates = deque([])
    for score, module, variable in index.symbol_scores(symbol):
        if variable is None:
            fmt = 'import {}'.format(str(module))
        else:
            fmt = 'from {} import {}'.format(str(module), str(variable))

        candidates.append(fmt)

    return list(candidates)


# Takes a list where the firest element is the source file as a string
# (assuming the call is from elisp) and the second element is the
# chosen import statement.
@server.register_function
def get_import_statement(*source_and_import):
    source = _stringify(source_and_import[0])
    import_statement = _stringify(source_and_import[1])

    imports = importmagic.importer.Imports(index, source)

    if import_statement.startswith('import '):
        module = import_statement[7:]
        imports.add_import(module)
    else:
        separator = import_statement.find(' import ')
        module = import_statement[5:separator]

        if separator >= 0:
            imports.add_import_from(import_statement[5:separator],
                                    import_statement[(separator + 8):])

    start, end, new_statement = imports.get_update()

    return [start, end, new_statement]


# Adds the specified path to symbol index.
@server.register_function
def add_path_to_index(*path):
    path = _stringify(path)

    global index
    if index is None:
        return "Index not ready. Hang on a second."

    index.index_path(path)
    return 0


@server.register_function
def add_directory_to_index(*path):
    path = _stringify(path)

    everything = os.listdir(path)
    files = []
    dirs = [d for d in everything if os.path.isdir(os.path.join(path, d))]

    for something in everything:
        if os.path.isfile(os.path.join(path, something)):
            if something.endswith('.py') and not something.startswith(
                    '__init__'):
                files.append(os.path.join(path, something))

    for file in files:
        index.index_path(file)

    # Not sure about this one.
    for dir in dirs:
        index.index_path(dir)

    return 0


build_index()
server.print_port()
server.serve_forever()

import sys
import threading
import importmagic
from collections import deque
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

# We'll follow a very passive approach. I'm not really familiar with
# neither EPC or cl-lib. So please, don't expect to see a lot of
# quality in this code

index = None


def _stringify(input_param):
    return ''.join(input_param)


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


def build_index(user_path, sys_path):
    thread = threading.Thread(
        target=_build_index, daemon=True, args=(user_path, sys_path))
    thread.start()


@server.register_function
def get_unresolved_symbols(*filepath):
    path = _stringify(filepath)

    with open(path, 'r') as f:
        source = f.read()
        scope = importmagic.Scope.from_source(source)
        unres, unref = scope.find_unresolved_and_unreferenced_symbols()

    return list(unres)


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
    filepath = _stringify(source_and_import[0])
    import_statement = _stringify(source_and_import[1])

    with open(filepath, 'r') as f:
        source = f.read()

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


@server.register_function
def echo(*arg):
    # Should return arg + every letter in the alphabet
    rval = []
    for e in arg:
        thestr = _stringify(e)
        rval.append([thestr + ' como estas', thestr + 'que tal'])
    return rval


server.print_port()
_build_index()
server.serve_forever()

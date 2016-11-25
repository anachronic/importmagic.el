import sys
import threading
import importmagic
from epc.server import EPCServer
from string import ascii_lowercase

server = EPCServer(('localhost', 0))

# We'll follow a very passive approach. I'm not really familiar with
# neither EPC or cl-lib. So please, don't expect to see a lot of
# quality in this code

index = None


def _stringify(input_param):
    return ''.join(input_param)


def _build_index(sys_path=sys.path, user_path=None):
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

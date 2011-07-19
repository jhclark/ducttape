from ductutil import *

class Versioner(object):
    def prepare():
        pass

# just use the version already installed on the local system
# do our best to track what happened, but make no guarantees
class Installed(Versioner):
    pass

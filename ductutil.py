import subprocess
import os

def isPidRunning(pid):
    return os.path.exists('/proc/{0}'.format(pid))

def writeFile(filename, line):
    f = open(filename, 'w')
    f.write(line + '\n')
    f.close()

def writeKVFile(filename, myDict):
    f = open(filename, 'w')
    for key, value in myDict.iteritems():
        key = str(key)
        value = str(value)
        if '\t' in key: raise Exception('key contains illegal tab character: {0}'.format(key))
        if '\t' in value: raise Exception('value contains illegal tab character: {0}'.format(value))
        f.write('{0}\t{1}\n'.format(key, value))
    f.close()

def loadKVFile(filename):
    f = open(filename)
    myDict = dict()
    for line in f:
        (key, value) = line.strip().split('\t')
        myDict[key] = value
    f.close()
    return myDict

# Since this isn't available until Python 2.7, just reimplement it
def check_output(*args, **kwargs):
    proc = subprocess.Popen(stdout=subprocess.PIPE, stderr=subprocess.PIPE, *args, **kwargs)
    (stdout, stderr) = proc.communicate()
    if proc.returncode == 0:
        return (stdout, stderr)
    else:
        raise subprocess.CalledProcessError('Process failed with error code {0}'.format(proc.returncode))

def get_ducttape_dir():
    scriptDir = os.path.dirname(os.path.realpath(__file__))
    return scriptDir


#!/usr/bin/env python
import sys
import os
import os.path
import inspect
import subprocess
import datetime
import socket
import time
import shutil

import submitters
from ductutil import *

scriptDir = get_ducttape_dir()

def showHeader():
    print >>sys.stderr, "DuctTape V1.0 alpha (c) 2011 by Jonathan Clark"
    print >>sys.stderr
showHeader()

class Path(object):
    def __init__(self, path):
        self.absPath = path
    def __str__(self):
        return self.absPath

class FuturePath(Path):
    pass

class Tool(object):
    def __init__(self, workflow, script, name, realization, submitter):
        self.script = script
        self.name = name 
        self.realization = realization
        self.submitter = submitter

        # baseDir is already guaranteed to be an absolute path
        self.workDir = '{0}/{1}/{2}'.format(workflow.baseDir, name, '-'.join(realization))
        self.stdoutFile = '{0}/ducttape.job.stdout'.format(self.workDir)
        self.stderrFile = '{0}/ducttape.job.stderr'.format(self.workDir)

        self.env = dict()
        
        # Do some PATH trickery to make sure libducttape.bash can be found
        externalPath = os.environ.get("PATH")
        self.env['PATH'] = '{0}:{1}'.format(externalPath, scriptDir)
        libPath = '{0}/libducttape.bash'.format(scriptDir)
        if not os.path.exists(libPath):
            raise Exception('Could not find required file {0}'.format(libPath))
        # TODO: Parse bash script to determine what inputs/outputs/parameters are legal

    def inputs(self, **kwargs):
        for key, path in kwargs.iteritems():
            scriptVar = 'I_{0}'.format(key)
            self.env[scriptVar] = path.absPath

#            if type(path) != type(FuturePath):
#                if not sys.path.exists(path):
#                    self.error('Input path does not exist: '+path)

    def params(self, **kwargs):
        for key, value in kwargs.iteritems():
            scriptVar = 'P_{0}'.format(key)
            self.env[scriptVar] = str(value)

    def output(self, outname):
        scriptVar = 'O_{0}'.format(outname)
        value = self.workDir + '/' + outname
        self.env[scriptVar] = value
        return FuturePath(value)

    # TODO: Rework this to automatically read from some central location
    def set_path(self, name, path):
        scriptVar = 'T_{0}'.format(name)
        self.env[scriptVar] = path

    def full_name(self):
        return '{0}-{1}'.format(self.name, '-'.join(self.realization))

    def run(self):
        startFile = '{0}/ducttape.START'.format(self.workDir)
        endFile = '{0}/ducttape.COMPLETED'.format(self.workDir)

        pid = os.getpid()
        host = socket.gethostname()

        if os.path.exists(endFile):
            print >>sys.stderr, 'Step already completed according to {0}'.format(endFile)
            return
        elif os.path.exists(startFile):
            startInfo = loadKVFile(startFile)
            oldHost = startInfo['Hostname']
            oldPid = startInfo['pid']
            if oldHost == host:
                if isPidRunning(oldPid):
                    print >>sys.stderr, 'Another process is already running this step. Waiting for completion...'
                    while isPidRunning(oldPid) and not os.path.exists(endFile):
                        time.sleep(15)
                    if not os.path.exists(endFile):
                        raise Exception('Competing process died and failed to complete {0}'.format(self.get_full_name()))
                    else:
                        print >>sys.stderr, 'Another process completed {0}'.format(self.get_full_name())
                else:
                    print >>sys.stderr, 'Another process on this host previously attempted this step, but failed. Retrying...'
                    shutil.rmtree(self.workDir)
            else:
                raise Exception('Step is already in progress according to {0}, but it was started from {1} on pid {2}. Try rerunning me on the same host so that I can tell if the process is still running.'.format(startFile, oldHost, oldPid))

        # Create directories
        if not os.path.exists(self.workDir):
            os.makedirs(self.workDir)
        
        writeKVFile(startFile, {'SubmitTime': datetime.datetime.now(),
                                'Hostname': host,
                                'pid': pid})
        try:
            print 'Running {0}'.format(self.name)
            self.submitter.run(self)
            print 'Completed {0}'.format(self.name)
            writeFile(endFile, str(datetime.datetime.now()))
        except subprocess.CalledProcessError as e:
            print "Error while running", self.name, ":", str(e)
            raise Exception('Died while running {0}'.format(self.name))

class Workflow(object):

    # Recommended usage: Workflow(baseDir='/my/base/directory')
    def __init__(self, baseDir):
        self.errors = []
        self.graph = []
        self.baseDir = os.path.abspath(baseDir)
        if not os.path.exists(self.baseDir):
            os.makedirs(self.baseDir)
            #raise Exception('Workflow base directory not found: {0}'.format(self.baseDir))

    def error(self, msg):
        # Get line number and source file name of workflow
        f = inspect.stack()[2][0]
        callStr = ' (%s:%d)'%(f.f_code.co_filename, f.f_lineno)

        MAX_ERRORS = 50
        self.errors.append(msg + callStr)
        if len(self.errors) > MAX_ERRORS:
            self.showErrors()
            sys.exit(1)

    def showErrors(self):
        for error in self.errors:
            print 'ERROR:', error

    # Use a file from the filesystem
    def file(self, filename, **kwargs):
        absPath = os.path.abspath(filename)
        if not os.path.exists(absPath):
            self.error('File not found: {0}'.format(absPath))
        return Path(absPath)
        # TODO: Errors for unrecognized kwargs

    # Recommended usage: tool('script/path', name='MyName')
    def tool(self, script, name, submitter=submitters.Local()):
        # TODO: Use environ vars to determine tools dir
        realization = ['btecZhEn', 'btecGlc1']
        script = os.path.abspath(script)
        if not os.path.exists(script):
            self.error('Tool script not found: {0}'.format(script))
        t = Tool(self, script, name, realization, submitter)
        self.graph.append(t)
        return t
 
    def run(self):

        if len(self.errors) > 0:
            self.showErrors()
            sys.exit(1)

        # TODO: Topological sort
        args = sys.argv[1:]
        try:
            for tool in self.graph:
                tool.run()
        except Exception as e:
            raise
            #print "ERROR:", e[0]
            #sys.exit(1)
            
    

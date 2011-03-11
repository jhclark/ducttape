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

def getStatusScript(runScript):
    toks = runScript.split('.')
    toks[-1:-1] = ['status']
    statusScript = '.'.join(toks)
    return statusScript

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
        self.statusScript = getStatusScript(script)
        self.reports = dict()
        self.name = name 
        self.realization = realization
        self.submitter = submitter
        self.workflow = workflow

        # baseDir is already guaranteed to be an absolute path
        self.workDir = '{0}/{1}/{2}'.format(workflow.baseDir, name, '-'.join(realization))
        self.stdoutFile = '{0}/ducttape.job.stdout'.format(self.workDir)
        self.stderrFile = '{0}/ducttape.job.stderr'.format(self.workDir)
        self.startFile = '{0}/ducttape.START'.format(self.workDir)
        self.endFile = '{0}/ducttape.COMPLETED'.format(self.workDir)

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

    def outputs(self, *outnames):
        paths = []
        for outname in outnames:
            scriptVar = 'O_{0}'.format(outname)
            value = self.workDir + '/' + outname
            self.env[scriptVar] = value
            paths.append(FuturePath(value))
        return paths

    def report(self, group, script):
        absScript = os.path.abspath(self.workflow.toolsDir+'/'+script)
        if not os.path.exists(absScript):
            self.workflow._error('Report script not found at {0} for report group {1}: {2}'
                       .format(self.full_name(), group, absScript))
        self.reports[group] = absScript

    # TODO: Rework this to automatically read from some central location
    def set_path(self, name, path):
        scriptVar = 'T_{0}'.format(name)
        self.env[scriptVar] = path

    def full_name(self):
        return '{0}-{1}'.format(self.name, '-'.join(self.realization))

    # Usage: use kwargs
    def _report(self, group):
        # Only report after things have finished running
        if group in self.reports and os.path.exists(self.endFile):
            reportScript = self.reports[group]
            try:
                # TODO: Cache reports?
                # TODO: Make tools like METEOR X-Ray trivial to include in reports?
                (stdout, stderr) = check_output(reportScript, shell=True, cwd=self.workDir)
                for line in stdout.split('\n'):
                    fields = line.split('\t')
                    yield fields
            except:
                raise

    # Usage: use kwargs
    def _status(self, verbose):
        print '=== {0} ==='.format(self.full_name())

        if os.path.exists(self.startFile):
            started = True
            print >>sys.stderr, 'Started at', open(self.startFile).readline(),
        if os.path.exists(self.endFile):
            print >>sys.stderr, 'Finished at', open(self.endFile).readline(),
        if not started:
            return # nothing to show

        if os.path.exists(self.statusScript):
            try:
                if verbose:
                    vFlag = ' -v'
                else:
                    vFlag = ''
                subprocess.check_call(self.statusScript+vFlag, shell=True, stdout=None, stderr=subprocess.STDOUT, cwd=self.workDir)
            except:
                pass

        print >>sys.stderr

    def _run(self):

        pid = os.getpid()
        host = socket.gethostname()

        if os.path.exists(self.endFile):
            print >>sys.stderr, 'Step already completed according to {0}'.format(self.endFile)
            return
        elif os.path.exists(self.startFile):
            startInfo = loadKVFile(self.startFile)
            oldHost = startInfo['Hostname']
            oldPid = startInfo['pid']
            if oldHost == host:
                if isPidRunning(oldPid):
                    print >>sys.stderr, 'Another process is already running this step. Waiting for completion...'
                    while isPidRunning(oldPid) and not os.path.exists(self.endFile):
                        time.sleep(15)
                    if not os.path.exists(self.endFile):
                        raise Exception('Competing process died and failed to complete {0}'.format(self.full_name()))
                    else:
                        print >>sys.stderr, 'Another process completed {0}'.format(self.full_name())
                else:
                    print >>sys.stderr, 'Another process on this host previously attempted this step, but failed. Retrying...'
                    shutil.rmtree(self.workDir)
            else:
                raise Exception('Step is already in progress according to {0}, but it was started from {1} on pid {2}. Try rerunning me on the same host so that I can tell if the process is still running.'.format(self.startFile, oldHost, oldPid))

        # Create directories
        if not os.path.exists(self.workDir):
            os.makedirs(self.workDir)
        
        writeKVFile(self.startFile, {'SubmitTime': datetime.datetime.now(),
                                'Hostname': host,
                                'pid': pid})
        try:
            print 'Running {0}'.format(self.name)
            self.submitter.run(self)
            print 'Completed {0}'.format(self.name)
            writeFile(self.endFile, str(datetime.datetime.now()))
        except subprocess.CalledProcessError as e:
            print "Error while running", self.name, ":", str(e)
            raise Exception('Died while running {0}'.format(self.full_name()))

class Workflow(object):

    # Recommended usage: Workflow(baseDir='/my/base/directory'...)
    def __init__(self, baseDir, toolsDir):
        self.errors = []
        self.graph = []
        self.baseDir = os.path.abspath(baseDir)
        self.toolsDir = os.path.abspath(toolsDir)
        if not os.path.exists(self.baseDir):
            os.makedirs(self.baseDir)
            #raise Exception('Workflow base directory not found: {0}'.format(self.baseDir))

    def _error(self, msg):
        # Get line number and source file name of workflow
        f = inspect.stack()[2][0]
        callStr = ' (%s:%d)'%(f.f_code.co_filename, f.f_lineno)

        MAX_ERRORS = 50
        self.errors.append(msg + callStr)
        if len(self.errors) > MAX_ERRORS:
            self._showErrors()
            sys.exit(1)

    def _showErrors(self):
        for error in self.errors:
            print 'ERROR:', error

    # Use a file from the filesystem
    def file(self, filename, **kwargs):
        absPath = os.path.abspath(filename)
        if not os.path.exists(absPath):
            self._error('File not found: {0}'.format(absPath))
        return Path(absPath)
        # TODO: Errors for unrecognized kwargs

    # Recommended usage: tool('script/path', name='MyName')
    def tool(self, script, name, submitter=submitters.Local()):
        # TODO: Use environ vars to determine tools dir
        realization = ['btecZhEn', 'btecGlc1']
        script = os.path.abspath(self.toolsDir+'/'+script)
        if not os.path.exists(script):
            self._error('Tool script not found: {0}'.format(script))
        t = Tool(self, script, name, realization, submitter)
        self.graph.append(t)
        return t

    def report(self, group):
        """An alternate mode to run in which reported values are given directly to user code. """
        for tool in self.graph:
            if group in tool.reports:
                # NOTE: 3rd item of tuple is itself a generator
                yield (tool, tool._report(group))

    def run(self):

        if len(self.errors) > 0:
            self._showErrors()
            sys.exit(1)

        # TODO: Topological sort
        from optparse import OptionParser
        parser = OptionParser()
        parser.add_option("-f", "--file", dest="filename",
                  help="write report to FILE", metavar="FILE")
        parser.add_option("-q", "--quiet",
                  action="store_false", dest="verbose", default=True,
                  help="don't print status messages to stdout")

        #(options, args) = parser.parse_args()
        args = sys.argv[1:]

        if not len(args) >= 1:
            print >>sys.stderr, 'Usage: [options...] action'
            print >>sys.stderr
            print >>sys.stderr, 'Valid actions: run'
            sys.exit(1)

        action = args[0]

        if action == 'run':
            try:
                for tool in self.graph:
                    tool._run()
            except Exception as e:
                raise
            
        elif action == 'status':
            # TODO: running only/all
            # verbose or not
            v = (len(args) >= 2 and args[1] == '-v')
            try:
                for tool in self.graph:
                    tool._status(verbose=v)
            except Exception as e:
                raise

        elif action == 'report':
            grp = ''
            if len(args) == 3 and args[1] == '--group':
                grp = args[2]

            for (vertex, data) in self.report(group=grp):
                print >>sys.stderr, '=== {0} ==='.format(vertex.full_name())
                for fields in data:
                    print >>sys.stderr, '\t'.join(fields)

        else:
            print >>sys.stderr, 'Unrecognized action: {0}'.format(action)
            sys.exit(1)
            
            
            
    

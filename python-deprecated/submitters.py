import sys
import subprocess
from ductutil import *

class Submitter(object):
    def run(self, vertex): pass

class Local(Submitter):
    """The simplest submitter. It just runs everything on the local shell command line."""

    def run(self, vertex):
        print >>sys.stderr, "Running locally:", vertex.script
        print >>sys.stderr, "Environment:", vertex.env
        subprocess.check_call(vertex.script, shell=True, stdout=None, stderr=subprocess.STDOUT,
                              env=vertex.env, cwd=vertex.workDir)
        # TODO: Write "retry" script on failure, for debugging

class Torque(Submitter):
    """Submitter for the Torque scheduler and resource allocation system (PBS-based)"""
    
    def __init__(self, **kwargs):
        """Set job submission arguments. Both flags and resource requests can be specified as keyword args use Torque(mem='5g', ncpus='16', walltime='00:01:00', q='all.q')"""
        self.submitArgs = kwargs

    def run(self, vertex):
        # Write out a submit script with the environment vars embedded in it
        qsubScript = '{0}/ducttape.qsub.sh'.format(vertex.workDir)
        f = open(qsubScript, 'w')
        print >>f, '#PBS -N {0}'.format(vertex.full_name())
        print >>f, '#PBS -V'
        print >>f, '#PBS -d {0}'.format(vertex.workDir)
        #print >>f, '#PBS -D {0}'.format(vertex.workDir) # this is for chroot
        print >>f, '#PBS -e {0}'.format(vertex.stderrFile)
        print >>f, '#PBS -o {0}'.format(vertex.stdoutFile)

        nonresourceFlags = set(['a', 'A', 'b', 'c', 'f', 'h', 'k', 'm', 'M', 'N', 'p', 'q', 'r', 'S', 'u', 'z'])
        for flag, value in self.submitArgs.iteritems():
            if flag in nonresourceFlags:
                print >>f, '#PBS -{0} {1}'.format(flag, str(value))
            else:
                print >>f, '#PBS -l {0}={1}'.format(flag, str(value))

        # Pass environment variables. Don't use qsub's -v since it is overridden by -V
        for var, value in vertex.env.iteritems():
            print >>f, 'export {0}={1}'.format(var, str(value))

        # Finally, execute the target script and make sure we don't lose any environment variables
        print >>f, 'set -auxeo pipefail'
        print >>f
        print >>f, 'bash -auxeo pipefail {0}'.format(vertex.script)
        f.close()

        qsubCmd = 'qsub {0}'.format(qsubScript)
        try:
            (jobid, stderr) = check_output(qsubCmd, shell=True)
            jobid = jobid.strip()
            # Remove hostname in "25000.blacklight1" if present
            if '.' in jobid:
                jobid = jobid.split('.')[0]
            if not jobid:
                raise Exception('Did not receive job ID when submitting {0}'.format(vertex.full_name()))
        except subprocess.CalledProcessError:
            print >>sys.stderr, 'Failed to submit job {0}'.format(vertex.full_name())
            raise

        # Now use bash to poll for completion
        # TODO: Move this to python instead?
        qsubPollCmd = '{0}/pbs-wait-for-job.sh {1}'.format(get_ducttape_dir(), jobid)
        try:
            subprocess.check_call(qsubPollCmd, shell=True, stdout=None, stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError:
            print >>sys.stderr, 'Detected error while waiting for job to complete {0} with Torque ID {1}'.format(vertex.full_name(), jobid)
            raise

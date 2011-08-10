DuctTape by Example
By Jonathan Clark

1-basics
========

1-hello-world.tape
------------------

 1) Running a single command

 Syntax:
 * This is just a single command that writes output to stdout and stderr
 * Lines that begin with # are comments
 * Comment blocks preceeding task declarations are associated with that task

 Execution:
 * After ducttape has been added to your PATH, this workflow can be executed with:
   $ ducttape basic.tape
 * Artifacts from this workflow will be in the current directory of basic.tape (./)
 * This task will run in the working directory ./hello-world/baseline/1/work
 * stdout will be placed at ./hello-world/baseline/1/stdout.txt
 * To have ducttape write a bash script that runs this step to
      ./hello-world/baseline/commands.sh, use:
   $ ducttape --dry-run --write-scripts basic.tape

```
[hello-world]
  echo hi
  echo >&2 hello
```

2-outputs.tape
--------------

 2) Writing output files

 * Ducttape will assign the paths for the output files
   as something like ./hello-world-2/x and ./hello-world-2/y.txt
 * The environment variables x and y_txt will be set by
   ducttape before calling bash
 * Note that bash disallows variables containing .

```
[hello-world-2] > x y_txt
  echo writing files $x and $y...
  echo hello > $x
  echo world > $y_txt
```

3-inputs.tape
-------------

 3) Reading input files

 * This task takes 2 input files (a and b) and produces no output
 * Ducttape will set the environment variables in $a and $b before invoking bash

```
[hello-world-3] < a=/etc/passwd b=/etc/hosts
  echo "I will be reading files called $a and $b and doing nothing useful with them"
  cat $a>/dev/null
  cat $b>/dev/null
```

4-dependencies.tape
-------------------

 4) Running tasks with dependencies

```

```

 First a step we already know about...

```
[first] > x
  for i in {1..10}; do
    echo $i >> $x
  done

```

 * We use first's output "x" as the input "a" by using the = operator
 * Instead of specifying an absolute path as before, we specify
   it as a dependency using the "$" prefix

```
[and-then] < a=$first/x > x
  cat < $a > $x
```

5-params.tape
-------------

 5) Using parameters

 * Parameters are for variables that aren't files
 * They are listed after inputs and outputs, using a double colon
 * Because we distinguish files from parameters, ducttape can check if input
   files exist before running length commands or submitting jobs to a scheduler

```
[param-step] < in=/etc/passwd > out :: N=5
  echo "$in has $(wc -l < $in) lines"
  echo "The parameter N is $N"
  echo $N > $out

```

 * The distinction between files and parameters also means
   that parameters don't introduce temporal dependencies when
   they are references (like this step)
 * "no-dep" can start running in parallel with "param-step"

```
[no-dep] :: X=$param-step/N
  echo "X=$N"
```

3-hyper
=======

1-hello-hyper.tape
------------------

 Experimentation often requires one to run similar sequences of tasks
 in a variety of configurations. To compactly represent such workflows,
 ducttape provides HyperWorkflows.

 For example, if we want to determine the effect of data size on some task,
 we could run the task several times, using input files of differing sizes,
 as below.

```


```

 This step shows the simplest way to pack (i.e. create a hyperworkflow)
 Parentheses with "fileName=(packName: a=x, b=y)" indicates that we will
 have branches "a" and "b" for file "f" and the name of the Branch Point
 (aka Pack) will be "packName".
 The file "x" will be used for branch a and the file "y"
 will be used when the branch b is active.

 * This task will result in two output directories:
   ./has-branches/baseline/1/work
   ./has-branches/bigger/1/work

 Because a is the first branch, it is considered the Baseline branch.
 Therefore, it is given the special directory name "baseline". This
 will become important later.

 Because there is a task with more than one Branch, we say that
 this workflow has more than one Realization (which can be thought of
 as a single DAG derivation of this HyperDAG).

```
[has_branches] < in=( whichSize: smaller=small.txt bigger=big.txt ) > out
  cat < $in > $out


```

 Since its parent task has several branches (smaller and bigger),
 this task will be run once for each parent branch.

 * This task will also result in two output directories:
   ./parent-has-branches/baseline/1/work
   ./parent-has-branches/bigger/1/work

```
[parent_has_branches] < in=$has_branches/out
  wc -l $in
```


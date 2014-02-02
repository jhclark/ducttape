#!/usr/bin/env ducttape

task a1 > out :: param=(A: 0 1 2) {
}
task a2 < out=@a1[A:1] {
}
task b1 > out :: param=(B: 0 1) {
}
task b2 < out=@b1[B:*] {
}
plan P {
 reach a1 via (A: 0)
 reach b1 via (B: 0)
}

# As of ducttape 0.3, Running ducttape workflow.tape gives the following work plan:

# RUN: /usr0/home/nschneid/./a1/Baseline.baseline (A.0)
# RUN: /usr0/home/nschneid/./a1/A.1 (A.1)
# RUN: /usr0/home/nschneid/./b1/Baseline.baseline (B.0)
# RUN: /usr0/home/nschneid/./b1/B.1 (B.1)

# In other words, because the downstream tasks (a2, b2) have graft dependencies on branches A.1 and B.1, they are followed, even though neither those branches nor the downstream tasks are specified in the plan. Expected results:

# RUN: /usr0/home/nschneid/./a1/Baseline.baseline (A.0)
# RUN: /usr0/home/nschneid/./b1/Baseline.baseline (B.0)
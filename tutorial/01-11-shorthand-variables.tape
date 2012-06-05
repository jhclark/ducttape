# Chapter 1: Basics
# Lesson 11: Shorthand variable references

task hello :: foo=@ {
     echo ${foo}
}

task prev :: a=42 {
     echo ${a}
}

task next :: a=@prev {
     echo ${a}
}

global {
       foo="hello, world"
}

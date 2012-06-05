# Chapter 1: Basics
# Lesson 11: Shorthand variable references

global {
       foo="hello, world"
}

task hello :: foo=@ {
     echo ${foo}
}

task prev :: a=42 {
     echo ${a}
}

task next :: a=@prev {
     echo ${a}
}

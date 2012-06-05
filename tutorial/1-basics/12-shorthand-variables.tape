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

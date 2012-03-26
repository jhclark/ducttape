package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CrossProductTest extends AbstractTest("cross product",Grammar.crossProduct) {
 
  def successCases = Set(
    """(branchPointName: a)""",
    """// some comments
    (branchPointName: a)""",
    """(branchPointName: a b)""",   
    """(greeting: y z)""",
    """(sauce: a1 ketchup wasabi)""",
    "(flags: a b )",
    "(flags: a b)",

    // Bare rvalues  
    """(branchPointName: a b c)""",
    "(a: a1 a4)",
    "(a: a1)",
    "(a: a1 a2 a3 a4)",
    
    // With comments
    """(
      // Here is a comment
    a: a1 a2 a3)""",
    """(
      // Here is a comment
    a: 
    // More comments
    a1 a2 a3)""",
    """(
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 a3)""",    
    """(
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 
    // And more
    a3)""",        
    """(
      // Here is a comment
    a: 
    // More comments
    a1 
    // And more
    // And more
    a2 
    // And more
    a3
    // And more
    // and more
    // and More
    )""",
    """(
      // Here is a comment
    a: 
    // More comments
    a1 // here too!
    // And more
    // And more
    a2 
    // And more
    a3
    // And more
    // and more
    // and More
    )""",    
    """(
      // Here is a comment
    a: 
    // More comments
    a1 // here too!
    // And more
    // And more
    a2 // and here
    // And more
    a3  // and here
    // And more
    // and more
    // and More
    )""",    
    
    // Cross products
    "(a: a1 a2 a3) * (b: b1 b2)",
    "(a: a1) * (b: b1)",
    "(a: a1 a2 a3) * (b: b1 b2) * (c: c_one c_two c_three c_four)",
    "(a: a1 a2 a3) * (b: b1 b2) * (c: c_one c_two c_three c_four) * (d: *)",
    
    // Multi-line
    """{
          (a: a1 a2 a3) 
        * (b: b1 b2)
       }""",
    "{ (a: a1) * (b: b1) }",
    "{ (a: a1 a2 a3) * (b: b1 b2) " +
    "* (c: c_one c_two c_three c_four)" +
    "}",
    """{ // Here I come to save the day
    
    // Mighty mouse
    // is on the way
    (a: a1 a2 a3) // Fighting evil
    * 
    // in the breeze
    (b: b1 b2) * (c: c_one c_two c_three c_four) * 
    
    // Looking for a piece
    (d: *) // of cheese
    }"""   
    
  ) 
  
  def failureCases = Set(
    // Anonymous branch point with bare rvalues
    "(foo)",
    "(1 2)",
    "((k: 1..10) 7 42 (z:100..1000..100))",
    
    // Anonymous branch point (no branch point name)
    "(foo=bar)",
    "(a=1 b=2)",
    
    "",
    " ",
    "NaN",
    "Infinity",
    "-Infinity",
    "+Infinity",
    "3.14e3.14",
    "A-variable_Name__",
    "A_variable_Name__",
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "!@#$%^&*()",
      "1",
      "-1",
      "+1",
      "3.141526535897923384626433832795028841971693993751058209",
      "123456789012345678901234567890123456789012345678901234567890",
      "123456789012345678901234567890123456789012345678901234567890e2147483647",
      "123456789012345678901234567890123456789012345678901234567890e-2147483647",
      "0",
      "0.00",
      "123",
      "-123",
      "1.23E3",
      "1.23E+3",
      "12.3E+7",
      "12.0",
      "12.3",
      "0.00123",
      "-1.23E-12",
      "1234.5E-4",
      "0E+7",
      "-0"     ,
    "10e-2147483648",
    "10e2147483648",
    "123456789012345678901234567890123456789012345678901234567890e123456789012345678901234567890123456789012345678901234567890",
    
    "(a: a1) * (b: b1) }"    
  ) 
  
  def errorCases = Set(
    "(a: a1=g@taskH[i:j])",
    """(branchPointName: a=1)""",
    """(branchPointName: a=1 b=5)""",   
    """(greeting: y="welcome home" z="bugger off")""",
    """(sauce: a1="A1 Sauce" ketchup="Tomato Ketchup" wasabi="wasabi")""",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\" )",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=\"kumbaya\")",
    "(flags: a=\"\"\"-avze 'ssh -o \"SomeOption=Value\"\"\"\" b=kumbaya)",

    // Bare rvalues
    """(branchPointName: 1)""",
    """(branchPointName: 1 5)""",    
    
    // Complex nesting
    "(a: a1=(k: 8..12) a4=7)",
    "(a: a1=$g@taskH[i:j])",
    "(a: a1=(b: f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE) a2=5 a3=(k: 8..12) a4=7)",      
    "(a: a1=(b: c=$d@taskE f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    "(a: a1=(b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    
    // Complex nesting with bare rvalues
    "(a: (k: 8..12) a4=7)",
    "(a: a1=(k: 8..12) 7)",
    "(a: (k: 8..12) 7)",
    "(a: (b: c=d) e=f)",
    "(a: (b: d d2=var d3 d4) e=f)",
    "(a: (b: c=(x: x1=$d@taskE x2=farOut x3=\"\"\"Quoted!\"\"\") f=$g@taskH[i:j]) a2=5 a3=(k: 8..12) a4=7)",
    "(a: b=(b: c=(x: d=$d@taskE farOut \"\"\"Quoted!\"\"\") $g@taskH[i:j]) 5 (k: 8..12) 7)",
    "(a: (b: (x: $d@taskE farOut \"\"\"Quoted!\"\"\") $g@taskH[i:j]) 5 (k: 8..12) 7)",

    // Multi-line
    """{
          (a: a1 a2 a3) 
        * (b: b1 b2)
       """,
    """{ (a: a1 a2 a3) * (b: b1 b2) 
     (c: c_one c_two c_three c_four)
    }""",
    """{ // Here I come to save the day
    
    // Mighty mouse
    // is on the way
    (a: a1 a2 a3) // Fighting evil
    * 
    // in the breeze
    (b: b1 b2) * (c: c_one c_two c_three c_four) * 
    
    // Looking for a piece
    (d: *) // of cheese }"""   
    

//    """(branchPointName: .1..5)""",   
//    """(branchPointName: 1..-.05)""",
//    """(branchPointName: 1.0...5)""",
//    """(branchPointName: -.10e1..10e999)""",
//    """(branchPointName: .9e256..7.7e1024)"""      
  )
  
}
package ducttape.syntax
import ducttape.util.AbstractTest
import ducttape.syntax.GrammarParser.Parser
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaskHeaderTest extends AbstractTest("task header",Grammar.taskHeader) {
 
  def successCases = Set(
//    "[hello_world]",
//    "[hello_world_2] > x y_txt",
//    "[hello_world_3] < a=/etc/passwd b=/etc/hosts",
//    "[first] > x",
//    "[and_then] < a=$x@first > x",
//    "[tokenize] tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
//    "[build_model] < in=$out@tokenize[DataSet:train] > model",
//    "[optimize] < in=$out@tokenize[DataSet:tune] > weights",
//    "[test] < in=$out@tokenize[DataSet:test] > hyps"
    "",      
    "> x y_txt",
    "< a=/etc/passwd b=/etc/hosts",
    "> x",
    "< a=$x@first > x",
    ": tokenizer < in=(DataSet: train=a.txt tune=b.txt test=c.txt) > out",
    "< in=$out@tokenize[DataSet:train] > model",
    "< in=$out@tokenize[DataSet:tune] > weights",
    ": moses tokenizerr giza < in=$out@tokenize[DataSet:test] > hyps",
    """: moses tokenizerr giza
    < in=$out@tokenize[DataSet:test] > hyps""",
    """: moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps""",
    """// Package comments
      : moses tokenizerr giza
    // Do some inputs
    < in=$out@tokenize[DataSet:test] 
    // Here's the result
    > hyps""",
    
    // Empty list  
    "",
    
    // Empty task inputs  
    "< ",
      
    // Sequential branch point
    """< a_1=(branchPointName: 1..5)""",   
    """< bcd=(branchPointName: 1..5.0)""",
    """< _QRS=(branchPointName: 1.0..5.0)""",
    """< t165s_z=(branchPointName: 10e1..10e999)""",
    """< x=(branchPointName: 9.9e256..7.7e1024)""",
    """< y=(branchPointName: 1..6..2)""",   
    """< z=(branchPointName: 1..5.0..0.5)""",
    """< _1=(branchPointName: 1.0..5.0..2)""",
    """< z_=(branchPointName: 10e1..10e999..1)""",
    """< Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    "< in=$variableName@taskName[branchPointName:branchName]",
    "< var=$variableName@taskName[branchPointName:*]",
    "< a=$variableName@taskName[a:b,c:d]",
    "< b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    "< c=$A_variable_Name__@foo",
    "< dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    "< e_1=$abc@def",
      
    // Variable reference
    "< q_687_abt=$A_variable_Name__",
    "< a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "< life_42=$abc",
    
    // Unquoted literal
    "< zip=A_variable_Name__",
    "< a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "< dee=/path/to/something/cool",
    "< doo=abc",
    "< dah=A_variable_Name__",
    "< zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "< a=A-variable_Name__",
    "< dee=A_variable_Name__",
    
    // Quoted literal
    """< aye="This is a quoted string"""",
    """< my='This one uses single quotes '""",
    """< oh=' Escape\tsequences\nare\rallowed! '""",
    "< my=\"Unicode sequences should be fine \u21AF too\"",
    "< what=\'Unicode sequences should be fine \u2231 too\'",
    
    // Triple quoted literal
    "< foo=\"\"\"" + "This is a quoted string" + "\"\"\"",
    "< bar=\"\"\"" + """This is a quoted string with \r lots of \\u garbage! \b in it!""" + "\"\"\"",
    
    // Multiple input items
    "< source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en",

    // With initial comments
    """// These are some cool comments
       < source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
 
    // With multiline comments
   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       < source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
   """// These are some cool comments
      // These are some more
      // I should say something meaningful    
      // Perhaps even profound       
      // Or not       
       < source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
  
    // Sequential branch point
    """> a_1=(branchPointName: 1..5)""",   
    """> bcd=(branchPointName: 1..5.0)""",
    """> _QRS=(branchPointName: 1.0..5.0)""",
    """> t165s_z=(branchPointName: 10e1..10e999)""",
    """> x=(branchPointName: 9.9e256..7.7e1024)""",
    """> y=(branchPointName: 1..6..2)""",   
    """> z=(branchPointName: 1..5.0..0.5)""",
    """> _1=(branchPointName: 1.0..5.0..2)""",
    """> z_=(branchPointName: 10e1..10e999..1)""",
    """> Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    "> in=$variableName@taskName[branchPointName:branchName]",
    "> var=$variableName@taskName[branchPointName:*]",
    "> a=$variableName@taskName[a:b,c:d]",
    "> b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    "> c=$A_variable_Name__@foo",
    "> dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    "> e_1=$abc@def",
      
    // Variable reference
    "> q_687_abt=$A_variable_Name__",
    "> a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "> life_42=$abc",
    
    // Unquoted literal
    "> zip=A_variable_Name__",
    "> a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "> dee=/path/to/something/cool",
    "> doo=abc",
    "> dah=A_variable_Name__",
    "> zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    "> a=A-variable_Name__",
    "> dee=A_variable_Name__",
    
    // Quoted literal
    """> aye="This is a quoted string"""",
    """> my='This one uses single quotes '""",
    """> oh=' Escape\tsequences\nare\rallowed! '""",
    "> my=\"Unicode sequences should be fine \u21AF too\"",
    "> what=\'Unicode sequences should be fine \u2231 too\'",
    
    // Unbound
    "> a",
    "> b_1",
    "> _z",
    
    // Multiple output items
    "> source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en",
    "> source target dev_src dev_tgtn",
    "> source target=/path/to/train.en dev_src dev_tgt=/path/to/dev.en",

    
    // With initial comments
    """// These are some cool comments
       > source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
 
    // With multiline comments
   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       > source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
   """// These are some cool comments
      // These are some more
      // I should say something meaningful       
      // Perhaps even profound       
      // Or not       
       > source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",

    // Empty task params  
    ":: ",      
      
// Variables without dots      
      
    // Sequential branch point
    """:: a_1=(branchPointName: 1..5)""",   
    """:: bcd=(branchPointName: 1..5.0)""",
    """:: _QRS=(branchPointName: 1.0..5.0)""",
    """:: t165s_z=(branchPointName: 10e1..10e999)""",
    """:: x=(branchPointName: 9.9e256..7.7e1024)""",
    """:: y=(branchPointName: 1..6..2)""",   
    """:: z=(branchPointName: 1..5.0..0.5)""",
    """:: _1=(branchPointName: 1.0..5.0..2)""",
    """:: z_=(branchPointName: 10e1..10e999..1)""",
    """:: Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    ":: in=$variableName@taskName[branchPointName:branchName]",
    ":: var=$variableName@taskName[branchPointName:*]",
    ":: a=$variableName@taskName[a:b,c:d]",
    ":: b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    ":: c=$A_variable_Name__@foo",
    ":: dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    ":: e_1=$abc@def",
      
    // Variable reference
    ":: q_687_abt=$A_variable_Name__",
    ":: a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: life_42=$abc",
    
    // Unquoted literal
    ":: zip=A_variable_Name__",
    ":: a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: dee=/path/to/something/cool",
    ":: doo=abc",
    ":: dah=A_variable_Name__",
    ":: zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: a=A-variable_Name__",
    ":: dee=A_variable_Name__",
    
    // Quoted literal
    """:: aye="This is a quoted string"""",
    """:: my='This one uses single quotes '""",
    """:: oh=' Escape\tsequences\nare\rallowed! '""",
    ":: my=\"Unicode sequences should be fine \u21AF too\"",
    ":: what=\'Unicode sequences should be fine \u2231 too\'",   
    
    
// Variables with dots
    
    // Sequential branch point
    """:: .a_1=(branchPointName: 1..5)""",   
    """:: .bcd=(branchPointName: 1..5.0)""",
    """:: ._QRS=(branchPointName: 1.0..5.0)""",
    """:: .t165s_z=(branchPointName: 10e1..10e999)""",
    """:: .x=(branchPointName: 9.9e256..7.7e1024)""",
    """:: .y=(branchPointName: 1..6..2)""",   
    """:: .z=(branchPointName: 1..5.0..0.5)""",
    """:: ._1=(branchPointName: 1.0..5.0..2)""",
    """:: .z_=(branchPointName: 10e1..10e999..1)""",
    """:: .Qz124356797809708970897089780970897=(branchPointName: 9.9e256..7.7e1024..5.4e3)""",      
      
    // Branch graft
    ":: .in=$variableName@taskName[branchPointName:branchName]",
    ":: .var=$variableName@taskName[branchPointName:*]",
    ":: .a=$variableName@taskName[a:b,c:d]",
    ":: .b=$variableName@taskName[a1:b2,c3:*,d4:e5,f6:g7]",
      
    // Task variable reference
    ":: .c=$A_variable_Name__@foo",
    ":: .dDt=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_@barBar",
    ":: .e_1=$abc@def",
      
    // Variable reference
    ":: .q_687_abt=$A_variable_Name__",
    ":: .a1_sauce=$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: .life_42=$abc",
    
    // Unquoted literal
    ":: .zip=A_variable_Name__",
    ":: .a=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: .dee=/path/to/something/cool",
    ":: .doo=abc",
    ":: .dah=A_variable_Name__",
    ":: .zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_",
    ":: .a=A-variable_Name__",
    ":: .dee=A_variable_Name__",
    
    // Quoted literal
    """:: .aye="This is a quoted string"""",
    """:: .my='This one uses single quotes '""",
    """:: .oh=' Escape\tsequences\nare\rallowed! '""",
    ":: .my=\"Unicode sequences should be fine \u21AF too\"",
    ":: .what=\'Unicode sequences should be fine \u2231 too\'",       

    // Multiple param items
    ":: source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en",
    
    // With initial comments
    """// These are some cool comments
       :: source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
 
    // With multiline comments
   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       :: source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
   """// These are some cool comments
      // These are some more
      // I should say something meaningful       
      // Perhaps even profound       
      // Or not       
       :: source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",

       
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
       
       "< a=5 b=10 > c d :: e=7 f=8",
       
       """< a=$z b=10 c=$baz@snarfTask[q:z] d=$f@bar
          > c d 
         :: e=7 f=8""",
       
       """// Task inputs
          < a=/path/to/a b=(7..10) c=$baz@snarfTask[q:z] d=$f@bar
          // Task outputs
          > d e f
          // Task params
          :: g=hello .h=10 i=(d: "hi" "there" "world")""",

       """// Task inputs
          < a=/path/to/a b=(7..10) c=$baz@snarfTask[q:z] d=$f@bar
          < z=7 y=$foo_yo5_
          // Task outputs
          > d e f
          // Task params
          :: g=hello .h=10 i=(d: "hi" "there" "world")""",
          
       """ < in=foo > out :: p=7 < j=$f""",
    
   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       
      // Perhaps even profound
       
      // Or not
       
       < source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",
    
   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       
      // Perhaps even profound
       
      // Or not
       
       > source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en""",

   """// These are some cool comments
      // These are some more
      // I should say something meaningful
       
      // Perhaps even profound
       
      // Or not
       
       :: source=/path/to/train.de target=/path/to/train.en dev_src=/path/to/dev.de dev_tgt=/path/to/dev.en"""
    
  ) 
  
  def failureCases = Set(
    " ",     
    "A-variable_Name__",    
    ":: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_ :",
    ":: ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_     :",
    ":: a",
    ":: b_1",
    ":: _z",
    ":: source target dev_src dev_tgtn",
    ":: source target=/path/to/train.en dev_src dev_tgt=/path/to/dev.en",
    """:: "This is a badly quoted string\"""",
    """:: "This one is, too"it seems"""",
    """:: 'Starting with a single and ending with a double"""",
    """:: "Starting with a double and ending with a single'""",    
    ":: $variableName@taskName[",
    ":: $A-variable_Name__",
    ":: $"      
  ) 
  
  def errorCases = Set(
    ":: daz=A_variable_Name__:",
    ":: zip=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz01234567890_:"
    
  )
  
}
import ducttape.syntax._
import ducttape.util._

object MyParseApp extends Application {

	import ducttape.syntax.AbstractSyntaxTree._
	import ducttape.syntax.Grammar._
	import ducttape.syntax.GrammarParser._
	
	val sampleComment = """# Welcome to make
			# This is a sample

			# Comments


			# blah blah - this line should cause a parse failure

			# Another comment
			""";

	val commentResult: ParseResult[CommentBlock] = parseAll(comments, sampleComment);

	val sampleTaskName = """[myTask]""";
	val taskNameResult: ParseResult[Any] = parseAll(taskName,sampleTaskName);

	val sampleAssignment = """foo=a"""
	val assignmentResult: ParseResult[Spec] = parseAll(assignment,sampleAssignment);
	
	val sampleInputs = """< x1=/tmp/bar y=yow"""
	val inputsResult: ParseResult[Seq[Spec]] = parseAll(taskInputs,sampleInputs)

	val sampleOutputs = """> x1=/tmp/bar y=yow"""
	val outputResult: ParseResult[Seq[Spec]] = parseAll(taskOutputs,sampleOutputs)
	
	val sampleParams = """:: x1=/tmp/bar y=yow"""
	val paramsResult: ParseResult[Seq[Spec]] = parseAll(taskParams,sampleParams)	
	
	val sampleHeader = """[foo] < x1=/tmp/bar y=yow/foo > out=/path/to/^crrazy! :: param=3.1415
""";
	val headerResult: ParseResult[TaskHeader] = parseAll(taskHeader,sampleHeader)
	
	val sampleCommand = """ wc -l $in > $out""";
	val commandResult: ParseResult[String] = parseAll(command,sampleCommand)

	val sampleCommands = """ wc -l $in > $out
 cat < $in > $out
""";
	val commandsResult: ParseResult[Seq[String]] = parseAll(commands,sampleCommands)
	
	val sampleBranch = """( whichSize: smaller=smaller.txt bigger=big.txt )"""
	val branchResult: ParseResult[BranchPointDef] = parseAll(branchPoint,sampleBranch)
	
	val sampleTaskBlock = 
"""

# Hello

# Welcome
[myTask] < input i=( whichSize: smaller=smaller.txt bigger=big.txt ) > output=/path/to/foo v=$var/n w=${wow}/x :: n=5
    cat < $input > $output
""";
	val taskBlockResult: ParseResult[TaskDef] = parseAll(taskBlock,sampleTaskBlock)
	
	val sampleWorkflow = 
"""

# Hello

# Welcome
[myTask] < input i=( whichSize: smaller=smaller.txt bigger=big.txt ) > output=/path/to/foo v=$var/n w=${wow}/x :: n=5
    cat < $input > $output


# More good stuff
[another] > output
	echo "Hello World" > $output
""";
	val workflowResult: ParseResult[WorkflowDefinition] = parseAll(workflow,sampleWorkflow)

	
	val result: WorkflowDefinition = 
			//commentResult;
			//taskNameResult;
			//assignmentResult;
	  		//inputsResult;
	  		//outputResult;
	  		//paramsResult;
			//headerResult;
	  		//commandResult;
			//commandsResult;
	  		//branchResult;
	  		//taskBlockResult;
	  		GrammarParser.read(IO.read(sampleWorkflow, "UTF-8"))

	  println(result)
	  		
//	result match {
//		case Success(result,_) => println(result)
//		case x => println(x)
//		//case Failure(error,inputReader) => println(error)
//	}
//
}

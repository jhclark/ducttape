package ducttape.cli

class Config {
  // TODO: Use Map for color sot that we can remove all of them easily?
  var headerColor = Console.BLUE
  var byColor = Console.BLUE
  var taskColor = Console.CYAN
  var warnColor = Console.BOLD + "\033[1;33;40m\033[1m"
  var errorColor = Console.RED
  var resetColor = Console.RESET

  var modeColor = Console.GREEN

  var errorLineColor = Console.BLUE // file and line number of error
  var errorScriptColor = Console.WHITE // quote from file

  var taskNameColor = Console.CYAN
  var realNameColor = Console.BLUE

  var greenColor = Console.GREEN
  var redColor = Console.RED
  
  // TODO: Enum?
  def clearColors() {
    headerColor = ""
    byColor = ""
    taskColor = ""
    warnColor = ""
    errorColor = ""
    resetColor = ""

    modeColor = ""

    errorLineColor = ""
    errorScriptColor = ""

    taskNameColor = ""
    realNameColor = ""

    greenColor = ""
    redColor = ""
  }
}

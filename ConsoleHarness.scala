/*
Crude Console Execution Harness
Scala script example, Allan Bowhill, 2013.

ConsoleHarness harnesses the execution of a user-supplied Scala console
driver object. The execution harness operates in the current thread. It
blindly writes a specified sequence of line inputs to the harnessed driver,
continually capturing the session's output and error streams. A specified
string to end the console session is then sent to the harnessed driver, and
the recorded contents of the session are returned.

==Example usage (with simple extractor expression):

val Harness(in, out, err) = Harness(Shell.run, "ls -alt ., ps -axwww", "exit")

println(in)
println(out)
println(err)

Explanation:

1. Harness a Scala console driver object named "Shell", running the start method called "run".
2. The harnessed driver is sent the input line "ls -alt"
3. The harnessed driver is sent the input line "ps -auxwww"
4. The Harnessed application is ended by sending "exit"
5. Strings recording stdin, stout and stderr are assigned to local vars "in", "out" and "err".



==Alternate usage (returning a Map([String,String]) object)

val result: Map[String, String] = Harness(ChatApp, " /say Hello there. , /say Bye!"  , "/exit")

println(result("sdtin"))
println(result("stdout"))
println(result("stderr"))

1. Harness a console driver named "ChatApp", running the main method implicitly.
2. The harnessed driver is sent the input line "/say Hello there."
3. The harnessed driver is sent the input line "/say Bye!"
4. The Harnessed application is ended by issuing "/exit"
5. Strings recording stdin, stout, stderr are assigned to a Map with quoted keys "stdin", "stdout", "stderr"

==Consoleharness Arguments In Detail:


ConsoleHarness takes three arguments:

Arg 1:

   The call to start execution of the harnessed object.

   Example:

   ConsoleHarness(Pshell.run, ..., ...)

   For an explicit start method: PShell.run
   For an implicit start (via main or app-extended program): MyConsoleApp

Arg 2:

   A comma-separated string of line-by-line user input. Each comma-seperated field
   represents one line of input sent to the running app. Each field is whitespace
   cleaned, so all leading and trailing whitespace will be removed. Each line of
   input is appended by a newline when sent to the app.

   Example:

   ConsoleHarness(..., myInput, ...)

   If you provide the following for user input:

   val myInput = "my  little pony eats,  other little ponies  , for breakfast.     "

   The following string will be sent to the harnessed console application as a single stream of
   continuous input broken into three lines:

   "my  little pony eats\n
    other little ponies\n
    for breakfast.\n"

    Note: newlines are sent at the end of each line, so there is no need to provide them.


3. Arg 3:

   A string representing user input that ends execution of the harnessed object.

   Example:

   ConsoleHarness(..., ..., "exit\n")

   This is the string that the harnessed application will respond to with a graceful exit.
   If the application always exits unconditionally, you might get lucky. Not tested.
   Otherwise, if no exit sequence exists, there may be a problem with your application.

   This harness is fragile, and not rubust in any sense. It only works if you have an exit
   string the application responds to with a graceful exit.
*/

object ConsoleHarness
   {
   val eol = System.getProperty("line.separator")

   def apply(runapp: => Any, cmds: String, exit: String): Map[String,String] =
      {
      val cmdString = cmds.split(",").map(_.trim).mkString(eol) + eol + exit

      val inS = new java.io.StringReader(cmdString)
      Console.setIn(inS)

      val outS = new java.io.ByteArrayOutputStream
      Console.setOut(outS)

      val errS = new java.io.ByteArrayOutputStream
      Console.setErr (errS)

      runapp

      Map("stdin" -> cmdString, "stdout" -> outS.toString, "stderr" -> errS.toString)
      }

   def unapply(iomap: Map[String,String]): Option[(String, String, String)] =
      {
      Console.setIn(System.in)
      Console.setOut(System.out)
      Console.setErr(System.err)

      if (iomap.size > 0)
         Some(iomap("stdin"), iomap("stdout"), iomap("stderr"))
      else
         None
      }
   }

/* BEGIN example console application: a shell which does pretty much nothing */

case object PShell
   {
   def run =
      {
      val prompt = coding(System.getProperty("file.encoding")) + " "

      var line: String = new String

      while (line != ":exit") // :exit ends this application
         {
         Console.print(prompt.toString)
         line = Console.readLine.toString
         Console.err.println("User Entered: [" + line + "]")
         Console.out.println("No...")
         }
      }

   // if run with: scala -Dfile.encoding=UTF-8 Pshell, you get cool
   // Unicode arrow prompt viewable with: xterm -en UTF-8
   // otherwise, it uses plain carrot prompt

   private def coding(x: Any) = x match
      {
      case "UTF-8" => 0x2794.toChar
      case _ => '>'
      }
   }

/* END example console application */

/* BEGIN sample script usage */

val ConsoleHarness(stdin, stdout, stderr) =
    ConsoleHarness(PShell.run, "My Little Pony,   eats, other little ponies  ,for breakfast.", ":exit\n")

println("Input:\n" + stdin)
println("Output:\n" + stdout)
println("Error:\n" + stderr)

/* END sample script usage */

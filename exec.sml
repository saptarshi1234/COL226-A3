val args = CommandLine.arguments()
val _ = evalFromFile (hd args) 
val _ = OS.Process.exit(OS.Process.success)
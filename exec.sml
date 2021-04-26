val args = CommandLine.arguments()
val verbose = not ( (length (tl args)) > 0 andalso hd(tl(args)) = "-q" )
val _ = evaluateFromFile (hd args, verbose) 
val _ = OS.Process.exit(OS.Process.success)
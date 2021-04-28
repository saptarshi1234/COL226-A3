val args = CommandLine.arguments()
val verbose = not ( (length (tl args)) > 0 andalso hd(tl(args)) = "-q" )
val _ = evaluateFromFile (hd args, verbose) handle Fail(s) => (print(s ^ "\n");[]) 
val _ = OS.Process.exit(OS.Process.success)
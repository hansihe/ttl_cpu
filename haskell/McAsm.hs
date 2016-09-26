import McAsmParser (parseAsm)

import System.Environment (getArgs)

main = do
  [inFileName] <- getArgs
  inData <- readFile inFileName
  print $ parseAsm inData

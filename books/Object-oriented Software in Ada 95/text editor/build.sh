#--   The file io.c is the commented out C code before Raw_io.adb
#--   The current I/O system with Windows 95 can not cope with 
#--     the fast reception of characters
#--   ^C Will Kill the program use ^E to exit
#--   ^D Will cause the program to terminate do not use 

# A compiler sous Win uniquement avec Cygwin

gcc -c io.c                     // Compile C interface (WIN95 env.)
gnatmake ed.adb -largs io.o     // Compile program




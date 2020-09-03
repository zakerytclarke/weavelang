# Build the programming language
cd src && ghc Weave.hs -outputdir build -o weave && mv weave.exe ../weave.exe

# Run a sample program
cd .. && ./weave.exe ./sample_programs/test.wv
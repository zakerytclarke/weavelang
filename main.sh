# Build the programming language
ghc Weave.hs
rm *.o
rm *.hi
# Run a sample program
./Weave ./sample_programs/welcome.wv
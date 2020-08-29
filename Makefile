run: weave
	./weave ./sample_programs/test.wv

weave: src/Weave.hs src/Analyzer.hs src/Runtime.hs src/Parser.hs src/Parselib.hs
	cd src && ghc Weave.hs -outputdir build -o weave && mv weave ../weave 

clean:
	rm -rf src/build && rm weave



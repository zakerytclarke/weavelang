weave: src/Weave.hs src/Parser.hs src/Transform.hs src/Eval.hs
	cd src && ghc Weave.hs -outputdir build -o weave && mv weave ../weave 

run: weave
	./weave ./sample_programs/welcome.wv

clean:
	rm -rf src/build && rm weave



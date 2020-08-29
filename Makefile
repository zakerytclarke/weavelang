run: weave
	./weave ./sample_programs/test.wv

weave: Weave.hs Analyzer.hs Runtime.hs Parser.hs Parselib.hs
	ghc Weave.hs -outputdir build -o weave

clean:
	rm -rf build && rm weave
ghcid:
		ghcid -c"cabal repl hspec" -T":main -f failed-examples --color"
test:
		cabal build hspec
		dist/build/hspec/hspec

gexec:
	ghcid -c"cabal repl doorTimer"



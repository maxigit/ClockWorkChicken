ghcid:
		ghcid -c"cabal repl hspec" -T":main -f failed-examples --color"
test:
		cabal build hspec
		dist/build/hspec/hspec



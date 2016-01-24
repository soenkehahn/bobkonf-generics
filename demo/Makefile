build: demo.cabal client/demo-client.cabal
	stack build --test

start:
	stack exec -- demo

setup: demo.cabal client/demo-client.cabal
	stack setup
	stack build --only-dependencies --haddock --test
	(cd client && stack setup)
	(cd client && stack build --only-dependencies --haddock --test)

demo.cabal: package.yaml
	hpack

client/demo-client.cabal: client/package.yaml
	(cd client && hpack)

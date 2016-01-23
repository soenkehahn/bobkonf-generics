compile: demo.cabal client/demo-client.cabal
	stack build --test --no-run-tests

start:
	stack exec -- demo

setup: demo.cabal client/demo-client.cabal
	stack setup
	stack build --only-dependencies
	(cd client && stack setup)
	(cd client && stack build --only-dependencies)

demo.cabal: package.yaml
	hpack

client/demo-client.cabal: client/package.yaml
	(cd client && hpack)

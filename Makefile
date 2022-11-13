.PHONY: build, test, run

build:
	stack build

test:
	stack test

run:
	stack run

# call:
#
# make message='{"message": "waiting for the summer", "tags": ["random"] }' post-save
post-save:
	curl http://localhost:7070/api/v1/save  -d '$(message)' -v -H "Content-Type: application/json"

# call:
# > make id=0 get-id
get-id:
	curl http://localhost:7070/api/v1/get/message/$(id) -v

# call:
# > make tag=info list-tag
list-tag:
	curl http://localhost:7070/api/v1/list/tag/$(tag) -v

toggle-logs:
	curl http://localhost:7070/api/v1/toggle-logs -d '{}' -v -H "Content-Type: application/json"

# generate dependency tree
# install graphviz (brew, apt) and calligraphy (cabal install)

gen-deps-src:
	cabal build --ghc-options=-fwrite-ide-info
	calligraphy src/**/*.hs -p deps/app-out.png --collapse-modules --no-cluster-modules

gen-deps-app:
	cabal build --ghc-options=-fwrite-ide-info
	calligraphy app/**/*.hs -p deps/app-out.png --collapse-modules --no-cluster-modules


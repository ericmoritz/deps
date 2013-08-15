.PHONY: build configure install

build: configure dist/build/deps/get-deps

configure: dist/setup-config

install:
	runghc Setup.hs install

clean: 
	rm -rf dist

dist/setup-config:
	runghc Setup.hs configure

dist/build/deps/get-deps:
	runghc Setup.hs build

test: clean build
	@make -C test-data/ clean build

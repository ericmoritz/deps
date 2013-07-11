.PHONY: build configure install

build: configure
	runghc Setup.hs build

configure:
	runghc Setup.hs configure

install:
	runghc Setup.hs install

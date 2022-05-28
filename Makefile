PURS_FILES	= $(shell find src/ -type f -name '*.purs')

html/bundle.js: js/index.js $(PURS_FILES)
	spago build
	esbuild --bundle $< --outfile=$@

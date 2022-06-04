PURS_FILES	= $(shell find src/ -type f -name '*.purs')
DAT_FILES	= $(wildcard data/*.dat) $(wildcard data/disks/*.dat)
DAT_JS_FILES	= $(patsubst %, js/%.js, $(DAT_FILES))

html/bundle.js: js/index.js js/base64.js js/files.js $(PURS_FILES)
	spago bundle-module -t output/bundle.js
	esbuild --bundle $< --outfile=$@

js/files.js: $(DAT_FILES)
	@(echo "import * as Base64 from '../js/base64.js';"; \
	echo "export default {"; \
	$(foreach file, $(DAT_FILES), \
		printf "\t'%s': Base64.toArrayBuffer('" $(file) ; \
		base64 $(file) | sed -e 's/$$/\\/' ; \
		printf "')," ; \
		) \
	echo "};"; \
	) > $@


ELM_SOURCE_DIR = src
ELM_SOURCES = \
	$(wildcard $(ELM_SOURCE_DIR)/*.elm) \
	$(wildcard $(ELM_SOURCE_DIR)/*/*.elm)
ELM_OUTPUT = elm.js


.PHONY: all patch clean distclean format

all: $(ELM_OUTPUT) patch

patch: .patched

clean:
	rm -f $(ELM_OUTPUT) .patched

distclean: clean
	rm -rf elm-stuff

format:
	elm-format --yes $(ELM_SOURCES)

$(ELM_OUTPUT): $(ELM_SOURCES)
	elm-make --output $@ $(ELM_SOURCE_DIR)/Main.elm

.patched: $(ELM_OUTPUT)
	patch < remove-flicker.patch
	patch < mouse-isdown.patch
	touch .patched

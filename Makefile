
HTML_FILES := $(patsubst %.Rmd, %.html ,$(wildcard *.Rmd)) \
              $(patsubst %.md, %.html ,$(wildcard *.md))

NODE_MODULES := $(CURDIR)/node_modules
PHANTOMJS := $(NODE_MODULES)/.bin/phantomjs

all: clean html check


html: $(HTML_FILES)

%.html: %.Rmd
	R --slave -e "set.seed(100);rmarkdown::render('$<')"

%.html: %.md
	R --slave -e "set.seed(100);rmarkdown::render('$<')"

$(NODE_MODULES):
	npm install

.PHONY: clean html check
clean:
	$(RM) $(HTML_FILES)

check: $(NODE_MODULES) html
	$(PHANTOMJS) check.js

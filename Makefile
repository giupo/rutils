# Makefile for generating R packages.
	# 2011 Andrew Redd
# 2014 Giuseppe Acito

PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)
 
R_FILES := $(wildcard R/*.[R|r])
SRC_FILES := $(wildcard src/*) $(addprefix src/, $(COPY_SRC))
PKG_FILES := DESCRIPTION NAMESPACE $(R_FILES) $(SRC_FILES)
 

# Aggiunge R ed Rscript come parametri configurabili all'esterno

R_BIN ?= R
RSCRIPT_BIN ?= Rscript

.PHONY: tarball install check clean build NAMESPACE NEWS.md
 
tarball: $(PKG_NAME)_$(PKG_VERSION).tar.gz
$(PKG_NAME)_$(PKG_VERSION).tar.gz: $(PKG_FILES)
	$(R_BIN) CMD build .
 
check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R_BIN) CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
build: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R_BIN) CMD INSTALL --build $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(R_BIN) --vanilla CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz
 
NAMESPACE: $(R_FILES)
	$(RSCRIPT_BIN) -e "devtools::document()"

clean:
	-rm -f $(PKG_NAME)_*.tar.gz
	-rm -r -f $(PKG_NAME).Rcheck
	-rm -r -f man/*
.PHONY: list
list:
	@echo "R files:"
	@echo $(R_FILES)
	@echo "Source files:"
	@echo $(SRC_FILES)
test:
	$(RSCRIPT_BIN) -e 'devtools::test()'
autotest:
	$(RSCRIPT_BIN) autotest.r
so:
	$(RSCRIPT_BIN) --vanilla -e 'devtools::compile_dll()'

coverage:
	$(RSCRIPT_BIN) -e 'covr::package_coverage()'

codecov:
	$(RSCRIPT_BIN) -e 'covr::codecov()'

NEWS.md:
	gitchangelog | grep -v "git-svn-id" > NEWS.md
	git add NEWS.md && git commit -m "updates NEWS.md"

changelog: NEWS.md
	
deps:
	$(RSCRIPT_BIN) -e 'install.packages("R6", repos="https://cran.rstudio.com")'
	$(RSCRIPT_BIN) -e 'devtools::install_cran(c("testthat", "roxygen2", "mockery", "covr"))'

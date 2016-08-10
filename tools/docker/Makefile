TAG	:= iphydf/haskell-hstox
VERSION	:= 8.0.1

SCRIPTS := 				\
	arm/Dockerfile			\
	arm/Dockerfile.fast		\
	arm/build-all.sh		\
	x86/Dockerfile			\
	x86/Dockerfile.fast		\
	x86/build-all.sh		\

all: build scripts
build: build-arm build-x86
scripts: $(SCRIPTS)

build-%: %/Dockerfile $(wildcard */*)
	chmod 0644 patches/*
	chmod 0755 *-scripts/*
	docker build -f $< -t $(TAG):$(VERSION)-$* .

fast-build-%: %/Dockerfile.fast $(wildcard */*)
	chmod 0644 patches/*
	chmod 0755 *-scripts/*
	docker build -f $< -t $(TAG):$(VERSION)-$* .

push-%: build-%
	docker push $(TAG):$(VERSION)-$*

%/Dockerfile: Dockerfile.in %/.gitignore
	sed -e 's/@GHC_TARGET_ARCH@/$*/' $< > $@

%/Dockerfile.fast: %/Dockerfile Makefile
	sed -e '/\$$BEGIN-BUILD-ALL\$$/,/\$$END-BUILD-ALL\$$/d;s/^#@ *//' $< > $@

%/build-all.sh: %/Dockerfile Makefile
	echo '#!/bin/bash' > $@
	sed -n -e '/\$$BEGIN-BUILD-ALL\$$/,/\$$END-BUILD-ALL\$$/{ s/^RUN //; s/^WORKDIR/cd/; /^COPY/d; p; }' $< >> $@

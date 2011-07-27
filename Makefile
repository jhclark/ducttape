

compile: | lib/scalatest-1.6.1.jar bin
	fsc -cp lib/scalatest-1.6.1.jar -d bin scala/*.scala

all: jar doc

jar: compile
	cd bin && zip -qr ../ducttape.jar *

doc scaladoc:
	scaladoc -d scaladoc -cp lib/scalatest-1.6.1.jar scala/*.scala


lib/scalatest-1.6.1.jar:
	./get_deps.sh

bin:
	mkdir -p $@

.PHONY: all compile doc jar scaladoc
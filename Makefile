# Can be overriden by user using environment variable
DUCTTAPE:=$(shell dirname $(realpath $(lastword ${MAKEFILE_LIST})))
SBT:=${DUCTTAPE}/build-support/sbt

DUCTTAPE_LIBDIR:=${DUCTTAPE}/lib

DUCTTAPE_LIBS:=${DUCTTAPE_LIBDIR}/sqlitejdbc-v056.jar:${DUCTTAPE_LIBDIR}/scala-optparse-1.1.jar:${DUCTTAPE_LIBDIR}/commons-lang3-3.1.jar:${DUCTTAPE_LIBDIR}/commons-io-2.2.jar:${DUCTTAPE_LIBDIR}/grizzled-slf4j_2.9.1-1-0.6.8.jar:${DUCTTAPE_LIBDIR}/pegdown-1.1.0.jar:${DUCTTAPE_LIBDIR}/parboiled-core-1.0.2.jar:${DUCTTAPE_LIBDIR}/parboiled-java-1.0.2.jar:${DUCTTAPE_LIBDIR}/webui/servlet-api-3.0.jar:${DUCTTAPE_LIBDIR}/webui/jetty-all-8.0.4.v20111024.jar:${DUCTTAPE_LIBDIR}/test/junit-4.10.jar:${DUCTTAPE_LIBDIR}/test/scalatest-1.7.1.jar


scalac:
	echo >&2 "Building source..."
	mkdir -p ${DUCTTAPE}/bin
	find ${DUCTTAPE}/src/main/scala ${DUCTTAPE}/src/test/scala \
	  | egrep '\.scala$$' \
	  | xargs scalac \
	    -Dscala.timings=true \
	    -unchecked -deprecation -cp ${DUCTTAPE_LIBS}  \
	    -d ${DUCTTAPE}/bin/ \
	  | ${DUCTTAPE}/build-support/color_scalac.awk

sbt:
	${SBT} compile

jar: ${DUCTTAPE}/ducttape.jar

minjar: ${DUCTTAPE}/ducttape.min.jar

${DUCTTAPE}/ducttape.jar:
	echo >&2 "Building JAR..."
	(cd ${DUCTTAPE}/bin; zip -qr ${DUCTTAPE}/ducttape.jar *)

# Make a minimal JAR containing all dependencies,
# but with unnecessary class files removed
${DUCTTAPE}/ducttape.min.jar:
	${SBT} proguard

doc:
	${DUCTTAPE}/doc.sh

scaladoc:
	${DUCTTAPE}/scaladoc.sh

release: jar

test-unit:
	${SBT} test

# Run regression tests using the distribution version
test-regression: dist
	PATH=${DUCTTAPE}/dist/ducttape-current:${PATH} ${DUCTTAPE}/test-regression.sh

clean:
	echo >&2 "TODO"

# SBT likes to keep lots of garbage around
deep-clean:
	rm -rf ~/.ivy2 ~/.m2 ~/.sbt
	sbt clean clean-files


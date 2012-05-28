# Can be overriden by user using environment variable
DUCTTAPE=.
SBT=${DUCTTAPE}/lib/scala/sbt

compile:
	${SBT} compile

# Make a minimal JAR containing all dependencies,
# but with unnecessary class files removed
jar:
	rm ducttape.min.jar
	${SBT} proguard

doc:
	${DUCTTAPE}/doc.sh

scaladoc:
	${DUCTTAPE}/scaladoc.sh

release: jar

test-unit:
	${SBT} test

test-regression: dist
	PATH=${DIST}:${PATH} ${DUCTTAPE}/test-regression.sh

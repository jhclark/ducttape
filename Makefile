# Can be overriden by user using environment variable
DUCTTAPE=.
SBT=${DUCTTAPE}/lib/scala/sbt

clean:
	echo >&2 "TODO"

deep-clean:
	# SBT likes to keep lots of garbage around
	rm -rf ~/.ivy2 ~/.m2 ~/.sbt
	sbt clean clean-files

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

# Run regression tests using the distribution version
test-regression: dist
	PATH=${DUCTTAPE}/dist/ducttape-current:${PATH} ${DUCTTAPE}/test-regression.sh

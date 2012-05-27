# Can be overriden by user using environment variable
DUCTTAPE=.

VERSION=0.2
RELEASE_NAME=ducttape-${VERSION}-bleeding
DIST=${DUCTTAPE}/dist/${RELEASE_NAME}

compile:
	${DUCTTAPE}/build.sh

jar: compile
	echo >&2 "JAR already built." # TODO: make compressed JAR

test: jar
	${DUCTTAPE}/test-unit.sh
	${DUCTTAPE}/test-regression.sh

doc:
	${DUCTTAPE}/doc.sh

# TODO: Throw SBT into git?
dist:
        # Make a minimal JAR containing all dependencies,
        # but with unnecessary class files removed
        rm ducttape.min.jar
        sbt proguard
        cp ducttape.min.jar ${DIST}/

        fgrep -v DEV-ONLY ducttape > ${DIST}/ducttape

        cp Makefile.dist ${DIST}/Makefile
        cp logging.properties ${DIST}/
        cp README.md ${DIST}/
        cp LICENSE.txt ${DIST}/

        cp -r ${DUCTTAPE}/tool-support ${DIST}/
        cp -r ${DUCTTAPE}/examples ${DIST}/
        cp -r ${DUCTTAPE}/syntax/tutorial ${DIST}/
        find ${DIST}/syntax/tutorial -type f | egrep '\.TODO|\.XXX|.DEPRECATED' | xargs rm -rf
        find ${DIST}/syntax/tutorial | fgrep /. | xargs rm -rf

        cd ${DIST}
        tar -cvzf ${RELEASE_NAME}.tgz ${RELEASE_NAME}

scaladoc:
	${DUCTTAPE}/scaladoc.sh

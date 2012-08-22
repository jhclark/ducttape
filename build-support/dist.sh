#!/usr/bin/env bash
set -eo pipefail
shopt -s nullglob
scriptDir=$(dirname $0)

DUCTTAPE=$scriptDir/..
VERSION=0.2
RELEASE_NAME=ducttape-${VERSION}
#RELEASE_NAME=ducttape-${VERSION}-bleeding-$(date '+%Y-%m-%d')
DIST_BASE=${DUCTTAPE}/dist
DIST=${DIST_BASE}/${RELEASE_NAME}

cd $DUCTTAPE

echo "Original JAR stats:"
du -csh lib/*.jar lib/webui/*.jar lib/scala/scala-library-2.9.2.jar

echo "Shrunken JAR stats:"
du -csh ducttape.min.jar

rm -rf ${DIST}
mkdir -p ${DIST}
cp ducttape.min.jar ${DIST}/ducttape.jar

fgrep -v DEV-ONLY ducttape > ${DIST}/ducttape
chmod a+x ${DIST}/ducttape

cp Makefile.dist ${DIST}/Makefile
cp logging.properties ${DIST}/
cp README.md ${DIST}/
cp LICENSE.txt ${DIST}/

mkdir -p ${DIST}/examples
cp -r ${DUCTTAPE}/examples/*.tape ${DIST}/examples

cp -r ${DUCTTAPE}/tool-support ${DIST}/
cp -r ${DUCTTAPE}/builtins ${DIST}/

mkdir -p $DIST/tutorial
tutorialDir=$DUCTTAPE/tutorial
cp $tutorialDir/*.tape \
   $tutorialDir/*.txt \
   $tutorialDir/*.md \
   $tutorialDir/*.pdf \
   $tutorialDir/*conf \
   $tutorialDir/*.sh \
   $DIST/tutorial
    
find ${DIST} -type f | egrep '\.TODO|\.XXX|.DEPRECATED|~' | xargs rm -rf
tar -C ${DIST_BASE} -cvzf ${DIST_BASE}/${RELEASE_NAME}.tgz ${RELEASE_NAME}

# Update symlink for regression testing
cd $DIST_BASE
rm -f ducttape-current
ln -sf ${RELEASE_NAME}/ ducttape-current

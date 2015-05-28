#!/bin/bash
#
# Installs the ninja build system
# Thu May 28 13:21:16 CEST 2015

BASE="ninja"
URL="https://github.com/martine/ninja/archive/master.tar.gz"
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit -1
fi
cd ${QPACKAGE_ROOT}
${QPACKAGE_ROOT}/scripts/install/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/${BASE}.tar.gz
tar -zxf ${BASE}.tar.gz && rm ${BASE}.tar.gz ||exit 1
rm -rf ${BASE}
mv ${BASE}-master ${BASE}
cd ${BASE}
./configure.py --bootstrap


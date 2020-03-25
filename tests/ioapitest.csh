#!/bin/csh -f
#
#   I/O API Test Suite.
#   Version "$Id: ioapitest.csh 146 2020-03-25 18:03:32Z coats $"
#   COPYRIGHT 2020 UNC Institute for the Environment.
#   Distributed under the GNU GENERAL PUBLIC LICENSE version 2
#   See https://www.gnu.org/licenses/old-licenses/gpl-2.0.html for conditions of use
#
#   REVISION  HISTORY:
#       Initial version 2/2020 by Carlie J. Coats, Jr., UNC IE
#
#   USAGE:
#       ioapitest.csh ${BIN} ${BASEDIR}
#

if ( $#argv != 2 ) then
    echo 'USAGE ERROR for I/O API "tests/ioapitest.csh"'
    echo "Incorrect command line with ARGCNT = ${#argv}"
    echo 'USAGE (2 arguments):  "ioapitest.csh ${BASEDIR} ${BIN}'
    exit( 2 )
endif
set bar = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
set base = ${1}
set bin  = ${2}
set foo  = 0

if ( ! -e ${base}/tests ) then
    echo 'USAGE (2 arguments):  "ioapitest.csh ${BIN} ${BASEDIR}'
    echo 'USAGE ERROR for I/O API "tests/ioapitest.csh"'
    echo "BASEDIR = ${BASEDIR} not found"
    echo "You must build both I/O API and M3Tools before running tests"
    echo "See https://cjcoats.github.io/ioapi/AVAIL.html"
    exit( 2 )
endif

if ( ! -e ${base}/${bin} ) then
    echo 'USAGE ERROR for I/O API "tests/ioapitest.csh"'
    echo 'USAGE (2 arguments):  "ioapitest.csh ${BIN} ${BASEDIR}'
    echo "Directory ${BASEDIR}/${BIN} not found"
    echo "You must build both I/O API and M3Tools before running tests"
    echo "See https://cjcoats.github.io/ioapi/AVAIL.html"
    exit( 2 )
endif

echo ' '
echo 'Performing I/O API Tests'
echo "I/O API installation directory BASEDIR=${base}"
echo "I/O API binary build type      BIN    =${bin}"
echo "This test-driver script:  ${base}/tests/ioapitest.csh"
echo ' '
setenv BIN ${bin}

cd ${base}/tests
echo ${bar}
cat tests.txt
echo ' '
echo ${bar}

LOOP1:
    echo "Continue?  (Y/N) [Y]"
    set yn = $<
    if ( ${yn} == 'N' || ${yn} == 'n' ) then
        echo "Test terminated at user request"
        exit( 0 )
    else if ( ${yn} != 'Y' && ${yn} != 'y' && ${yn} != '' ) then
        echo "Did not understand your response:  Use Y or N"
        goto LOOP1
    endif

set m3fake = ${base}/${bin}/m3fake
set m3stat = ${base}/${bin}/m3stat
set latlon = ${base}/${bin}/latlon
set random = ${base}/${bin}/randomstat

if ( ! -e ${m3fake} || ! -e ${m3stat} || ! -e ${latlon} || ! -e ${random} ) then
    echo "M3Tools programs  ${m3fake} ${m3stat}, and/or ${latlon} not found."
    echo "You must build both I/O API and M3Tools before running tests"
    echo "See https://cjcoats.github.io/ioapi/AVAIL.html"
    exit( 2 )
endif

set outdir = /tmp/ioapitest.$$
mkdir -p ${outdir}
if ( ! -e ${outdir} ) then
    echo "Could not create test-directory ${outdir}.  Check system permissions?"
    exit( 2 )
endif

echo "Output files will be placed in ${outdir} and will normally be"
echo "cleaned-up after testing, rather than saved."

LOOP2:
    echo "Save output files (in ${outdir})?  (Y/N) [N]"
    set yn = $<
    if ( ${yn} == 'N' || ${yn} == 'n' || ${yn} == '' ) then
        set savefiles = 'N'
    else if ( ${yn} == 'Y' || ${yn} == 'y' ) then
        set savefiles = 'Y'
    else
        echo "Did not understand your response:  Use Y or N"
        goto LOOP2
    endif


echo ${bar}
cat basic.txt
echo ' '
echo "Recall that the output directory for this test is ${outdir}"
echo ' '
echo ${bar}

LOOP3:
    echo "Perform the BASIC test?  (Y/N) [Y]"
    set yn = $<
    if ( ${yn} == 'N' || ${yn} == 'n' ) then
        goto AFTERBASIC
    else if ( ${yn} != 'Y' && ${yn} != 'y' && ${yn} != '' ) then
        echo "Did not understand your response:  Use Y or N"
        goto LOOP3
    endif

    #####  First, run "latlon", using the standard WRF/ARW spheroid (20)
    #####  for the map-projection definition:

    setenv  GRIDDESC    ${base}/tests/GRIDDESC.tests
    setenv  LL_2D       ${outdir}/LL_2D.NC15_LAM_19x17.nc
    setenv  IOAPI_ISPH  20.0d0

    echo ${bar}
    echo "GRIDDESC FILE to control program latlon"
    cat  ${GRIDDESC}
    echo ${bar}
    echo ' '
    echo 'Testing file creation:  Running "latlon"'
    echo 'This may take a while'
    echo ' '
    echo 'Hit return when ready'
    set yn = $<

    set ui = ${outdir}/latlon.$$
    echo "Yes, continue with the program" >& ${ui}
    echo "LL_2D                         " >> ${ui}
    echo "NONE                          " >> ${ui}
    echo "Yes, use a GRIDDESC file      " >> ${ui}
    echo " NC15_LAM_19x17               " >> ${ui}

    ${latlon} < ${ui}
    set foo = ${status}
    rm ${ui}

    echo ${bar}
    if ( ${foo} != 0 ) then
        echo "### ERROR ${foo} on program latlon"
        goto EXIT
    else
        echo 'File creation test using "latlon" successful'
    endif

    #####  Then, run "m3stat" on the result:

    echo ${bar}

    echo ' '
    echo 'Testing file-read:  Running "m3stat" on output from "latlon"'
    echo ' '
    echo 'Hit return when ready'
    set yn = $<

    setenv  REPORTFILE   ${outdir}/LL_2D.NC15_LAM_19x17.stats

    set ui = ${outdir}/m3stat.$$
    echo "Yes, continue with the program" >& ${ui}
    echo "LL_2D                         " >> ${ui}
    echo "NONE                          " >> ${ui}
    echo "LL_2D                         " >> ${ui}
    echo "LL_2D                         " >> ${ui}
    echo "LL_2D                         " >> ${ui}

    ${m3stat} LL_2D REPORTFILE DEFAULT
    set foo = ${status}
    rm ${ui}

    echo ${bar}
    if ( ${foo} != 0 ) then
        echo "### ERROR ${foo} on program m3stat"
        goto EXIT
    else
        echo 'File read-test using "m3stat" successful'
    endif
    echo ${bar}
    echo "STATISTICS REPORT:"
    echo "-- max, min, mean, and sigma for all variables/time steps in the file"
    cat ${REPORTFILE}
    echo ${bar}

    echo "BASIC TEST successful!"


AFTERBASIC:

echo ${bar}
cat random.txt
echo ' '
echo "Recall that the output directory for this test is ${outdir}"
echo ' '
echo ${bar}

LOOP4:
    echo "Perform the RANDOM ACCESS tests?  (Y/N) [Y]"
    set yn = $<
    if ( ${yn} == 'N' || ${yn} == 'n' ) then
        goto AFTERRANDOM
    else if ( ${yn} != 'Y' && ${yn} != 'y' && ${yn} != '' ) then
        echo "Did not understand your response:  Use Y or N"
        goto LOOP4
    endif

    #####  First, run "m3fake", creating a year-long output file:

    echo ${bar}
    echo ' '
    echo 'Testing file creation:'
    echo 'Using "m3fake" for to create year-long hourly-timestep file.'
    echo 'This may take a while...'
    echo 'This may take a while...'
    echo ' '
    echo 'Hit return when ready'
    set yn = $<

    setenv  GRIDDESC    ${base}/tests/GRIDDESC.tests
    setenv  MYFILE      ${outdir}/TEST_2D.nc
    setenv  MYVBLE      STEP
    set yyyy = `${base}/${bin}/greg2jul TODAY | cut -c 1-4`
    set date = "${yyyy}001"
    @   next = ${yyyy} + 1
    set days = `${base}/${bin}/juldiff ${date} ${next}001`
    @   recs = 24 * ${days} + 1

    set ui = ${outdir}/m3fake.$$
    echo "Yes, continue with the program" >& ${ui}
    echo "2                             " >> ${ui}  # file type from menu:  GRIDDED
    echo "NC15_LAM_19x17                " >> ${ui}  # grid name
    echo "1                             " >> ${ui}  # number of layers
    echo "10000                         " >> ${ui}  # time step
    echo "${date}                       " >> ${ui}  # starting date
    echo "0                             " >> ${ui}  # starting time
    echo "${recs}                       " >> ${ui}  # number of time steps
    echo "1                             " >> ${ui}  # number of variables
    echo "STEP                          " >> ${ui}  # name of first variable
    echo "none                          " >> ${ui}  # units
    echo "Time step number              " >> ${ui}  # description
    echo "1                             " >> ${ui}  # variable-type from menu
    echo "4                             " >> ${ui}  # filler-formula from menu
    echo "Demo test file for I/O API    " >> ${ui}  # file description
    echo "make tests:                   " >> ${ui}  # "
    echo "for use by program randomstat " >> ${ui}  # "
    echo "                              " >> ${ui}  # blank line ends description
    echo "MYFILE                        " >> ${ui}  # output file logical name

    time ${m3fake} < ${ui} >& ${outdir}/m3fake.yeartest.log
    set foo = ${status}
    rm ${ui}

    echo ${bar}
    if ( ${foo} != 0 ) then
        echo "### ERROR ${foo} on program m3fake"
        goto EXIT
    else
        echo 'File creation using "m3fake" successful'
    endif
    echo ${bar}

    #####  Then, run "randomstat", demonstrating random access:

    set xeve = `${base}/${bin}/greg2jul ${yyyy}1224`
    set xmas = `${base}/${bin}/greg2jul ${yyyy}1225`
    
    echo ' '
    echo 'Testing direct access:  "randomstat" for Xmas-eve through Xmas:'
    echo "Christmas Eve is Julian date ${xeve}"
    echo "Christmas Day is Julian date ${xmas}"
    echo ' '
    echo 'Hit return when ready'
    set yn = $<

    set ui = ${outdir}/randomstat.1.$$
    echo "${xeve}                              " >& ${ui}
    echo "${xmas}                              " >> ${ui}

    ${random} < ${ui}
    set foo = ${status}
    rm ${ui}

    echo ${bar}
    if ( ${foo} != 0 ) then
        echo "### ERROR ${foo} on program randomstat XMAS"
        goto EXIT
    else
        echo 'XMAS File analysis using "randomstat" successful'
    endif
    echo ${bar}

    #####  Then, run "randomstat", demonstrating reverse-order access:

    set day1 = `${base}/${bin}/greg2jul ${yyyy}1130`
    set day2 = `${base}/${bin}/greg2jul ${yyyy}1201`

    echo ' '
    echo 'Testing reverse-order analysis with "randomstat" from Dec 1 back to Nov  30:'
    echo "December  1 is Julian date ${day2}"
    echo "November 30 is Julian date ${day1}"
    echo ' '
    echo 'Hit return when ready'
    set yn = $<

    set ui = ${outdir}/randomstat.2.$$
    echo "${day2}                              " >& ${ui}
    echo "${day1}                              " >> ${ui}

    ${random} < ${ui}
    set foo = ${status}
    rm ${ui}

    echo ${bar}
    if ( ${foo} != 0 ) then
        echo "### ERROR ${foo} on program randomstat REVERSE-ORDER"
        goto EXIT
    else
        echo 'REVERSE-ORDER file analysis using "randomstat" successful'
    endif
    echo ${bar}


AFTERRANDOM:

EXIT:

    echo ${bar}
    if ( ${savefiles} == 'N' ) then
        echo "Cleaning up output directory ${outdir}"
        rm -rf  ${outdir}
    else
        echo "Output files are saved in ${outdir}"
    endif
    exit ( ${foo} )

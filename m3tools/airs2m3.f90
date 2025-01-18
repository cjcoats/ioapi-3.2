
PROGRAM AIRS2M3

    !*****************************************************************
    ! Version "$Id: airs2m3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 M3TOOLS.
    ! Copyright (C) 1992-2002 MCNC,
    ! (C) 1995-2002,2005-2013,2021 Carlie J. Coats, Jr.,
    ! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    ! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    ! See file "GPL.txt" for conditions of use.
    !.........................................................................
    !  program body              starts at line  153
    !  logical function rdheader starts at line  607
    !
    !  DESCRIPTION:
    !       This program reads a AMP350 AIRS report and puts it into a
    !       PAVE-able Models-3 I/O API "observations" file, with optional
    !       units conversion and time shift.
    !
    !  PRECONDITIONS REQUIRED:
    !       Time zone cross reference file sorted by state and county
    !       setenv <logical file name> <path name> for all input and
    !       output files.
    !       Time period does not cross leap-year boundaries
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       I/O API
    !       RDHEADER
    !
    !  REVISION  HISTORY:
    !       Adapted 3/2002 by Carlie J. Coats, Jr., MCNC EMC from
    !       program "OBSS2IOAPI"
    !
    !       OBS2IOAPI history:
    !       Saravanan Arunachalam, MCNC EMC, 08/18/99
    !
    !       Restructured on 11/06/01,  Don Olerud, MCNC EMC
    !       The values are converted to ppm, and the time is shifted
    !       from local standard to GMT, plus a user-specified hour shift.
    !       Then the obs are written to an obs NetCDF file.
    !
    !       Modified 11/2003 by CJC: removed redundant re-declarations of
    !       various variables (IREC, MONTH, OLDMON, OLDSPC, OLDYR)
    !
    !       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !       USE M3UTILIO, and related changes.
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !*****************************************************************

    USE M3UTILIO

    IMPLICIT NONE


    !...........   PARAMETERS and their descriptions:

    CHARACTER*1,  PARAMETER::  BAR       = '----------------------'
    CHARACTER*1,  PARAMETER::  QUOTE     = ''''
    CHARACTER*1,  PARAMETER::  COMMA     = ','
    CHARACTER*16, PARAMETER::  PNAME     = 'AIRS2M3'
    INTEGER,      PARAMETER::  IBIG      = 999999999
    REAL,         PARAMETER::  CONVFACO3 = 1.0 / ( 40.89 * 48.0 )
    REAL,         PARAMETER::  CONVFACNO = 1.0 / ( 40.89 * 30.0 )
    REAL,         PARAMETER::  CONVFACNO2= 1.0 / ( 40.89 * 46.0 )
    REAL,         PARAMETER::  CONVFACCO = 1.0 / ( 40.89 * 28.0 )

    CHARACTER*1, PARAMETER :: UFLAG(   5 ) = (/       '$',   ' ',    '%',     '#',     '?'   /)
    REAL,        PARAMETER :: UFACS( 0:5 ) = (/ 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0  /)
    INTEGER,     PARAMETER :: KUNIT(   5 ) = (/         1,     5,      7,       8,     40    /)
    REAL,        PARAMETER :: KFACS( 0:5 ) = (/ 1.0,  1.0, 0.001,    1.0,   0.001,      0.01 /)

    INTEGER, PARAMETER :: CUNIT( 2 ) = (/ 1,   5 /)

    INTEGER, SAVE::     MLENS( 12 ) =                  &
       (/  31,    28,    31,    30,    31,    30,      &
           31,    31,    30,    31,    30,    31   /)

    CHARACTER*5, PARAMETER :: SNAMES( 4 ) = &
             (/ '44201',     & !  Ozone
                '42101',     & !  carbon monoxide
                '42601',     & !  NO
                '42602'      & !  NO_2
                /)

    REAL,         PARAMETER :: SFACS( 0:4 )  = (/   1.0, CONVFACO3, CONVFACCO, CONVFACNO, CONVFACNO2 /)
    CHARACTER*16, PARAMETER :: VNAMES( 0:4 ) = (/ 'O3 ',     'O3 ',     'CO ',     'NO ',     'NO2'  /)


    !...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV      !  unit number for log file
    INTEGER         ADEV      !  unit number for AIRS input file
    INTEGER         TDEV      !  unit number for time zone index file
    INTEGER         SDEV      !  unit number for site index file
    CHARACTER*256   MESG      !  message buffer

    CHARACTER*16    FNAME
    CHARACTER*5     SPC
    CHARACTER*1     STFLAG
    INTEGER         YEAR, IMON
    REAL            CONVFAC
    INTEGER         I, J, K, L, M, N

    INTEGER         JDATE, JTIME
    INTEGER         YDATE, YREC
    INTEGER         ISHIFT    !  date&time shift,  format HHMMSS
    INTEGER         IADDTZ    !  time-zone shift,  format HH
    INTEGER         STDATE    !  starting date,    format YYYYDDD
    INTEGER         STTIME    !  starting time,    format HHMMSS
    INTEGER         ENDATE    !  starting date,    format YYYYDDD
    INTEGER         ENTIME    !  starting time,    format HHMMSS

    INTEGER         ISYEAR, ISMON, ISDAY, ISHR
    INTEGER         IEYEAR, IEMON, IEDAY
    INTEGER         TSTEP,  NRECS, IREC
    INTEGER         STATUS    !  I/O, ALLOCATE status
    LOGICAL         EFLAG

    !!  State-County-Time Zone cross reference table

    INTEGER                     NCTY
    INTEGER,     ALLOCATABLE::  KST( : )
    INTEGER,     ALLOCATABLE::  KCTY( : )
    INTEGER,     ALLOCATABLE::  KTZ( : )

    !!  Observation Site variables:

    INTEGER      IST, ICTY
    INTEGER      IUNIT
    INTEGER      NSTNS
    CHARACTER*16 OBSNAM
    CHARACTER*1  AFLAG( 0:24 )
    CHARACTER*4  COBS( 0:24 )
    INTEGER      DAYS
    REAL         XLON, YLAT, ROBS
    INTEGER,     ALLOCATABLE::  AIRSID( : )
    REAL,        ALLOCATABLE::  LAT( : )
    REAL,        ALLOCATABLE::  LON( : )
    REAL,        ALLOCATABLE::  OBS (: , : )


    !***********************************************************************
    !   begin body of program M3CPLE

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' )                                             &
'Program AIRS2M3 to read an AMP350 AIRS report and reformat the',       &
'data to write an ASCII cross-reference file and an I/O API',           &
'"observations" file.  The AIRS data are hourly averaged, and',         &
'a 00 time flag represents the hour 00-01. A user may wish to',         &
'represent that data segment by the starting hour, the ending ',        &
'hour, or the hour-center for the observational time.  In these',       &
'cases, a shift of 0, 10000, or 3000 should be entered here for',       &
'the "time shift", respectively.  Starting dates, etc. will be',        &
'specified in terms of (4-digit) calendar year, month, and day.',       &
' ',                                                                    &
'THE PROGRAM WILL PROMPT YOU for the logical names of the input',       &
'ASCII AMP350 AIRS data file, the input ASCII time zone index',         &
'file, the date and time period covered by the input data, the',        &
'time shift to be employed, the observational variable names,',         &
'and the logical names of the I/O API output data file and the',        &
'ASCII output station cross reference file',                            &
' ',                                                                    &
'Default responses are indicated in square brackets[LIKE THIS],',       &
'and may be accepted by hitting the RETURN key.',                       &
' ',                                                                    &
'PRECONDITIONS REQUIRED:',                                              &
' ',                                                                    &
'    setenv <name>        <input data file path-name>',                 &
'    setenv <name>        <input TZ   file path-name>',                 &
'    setenv <name>        <output ASCII STNS file path-name>',          &
'    setenv AIRS_NCF_OBS  <output NCF data file path-name>',            &
' ',                                                                    &
'    Input TZ file is sorted by state and county.',                     &
' ',                                                                    &
'See URLs  https://cjcoats.github.io/ioapi/AA.html#tools or',           &
'  https://www.cmascenter.org/ioapi/documentation/all_versions/html/AA.html#tools', &
' ',                                                                    &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013, 2021',            &
'Carlie J. Coats, Jr., (C) 2002-2010 Baron Advanced',                   &
'Meteorological Systems, LLC., and (C) 2014-2018 UNC',                  &
'Institute for the Environment.',                                       &
'Released under Version 2 of the GNU General Public License.',          &
'See enclosed GPL.txt, or URL',                                         &
'https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html',            &
' ',                                                                    &
'Comments and questions are welcome and can be sent to',                &
' ',                                                                    &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                   &
'or',                                                                   &
'    UNC Institute for the Environment',                                &
'    100 Europa Dr., Suite 490',                                        &
'    Campus Box 1105',                                                  &
'    Chapel Hill, NC 27599-1105',                                       &
' ',                                                                    &
'Program version: ',                                                    &
'$Id:: airs2m3.f90 203 2021-10-14 18:02:11Z coats                   $', &
' '

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF


    !!  ---------------------
    !!  Open ASCII input AIRS data, input TimeZone XREF, and
    !!  output station cross-reference files, using logical
    !!  names returned by the user.

    ADEV =  PROMPTFFILE( 'Enter input AIRS input file',      .TRUE., .TRUE., 'AIRS_FILE', PNAME )

    TDEV =  PROMPTFFILE( 'Enter input time zone index file', .TRUE., .TRUE., 'AIRS_TZONE', PNAME )

    SDEV =  PROMPTFFILE( 'Enter output station index file', .FALSE., .TRUE., 'AIRS_INDEX', PNAME )

    !!  Get default responses for prompts from the
    !!  first AIRS-file station-record header:

    IF ( .NOT. RDHEADER( ADEV, -1,                          &
                         SPC, IST, ICTY, IMON, YEAR,        &
                         XLON, YLAT, I, IUNIT ) ) THEN
        MESG = 'Error reading AIRS input header'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF
    REWIND( ADEV )

    ISYEAR = GETNUM( 1900, IBIG, YEAR, 'Enter starting year (YYYY)' )

    IF ( ( MOD( ISYEAR, 400 ) .EQ. 0 ) .OR. ( MOD( ISYEAR, 4   ) .EQ. 0 ) .AND. ( MOD( ISYEAR, 100 ) .NE. 0 ) ) THEN
        MLENS( 2 ) = 29    !  leap year
    END IF

    ISMON  = GETNUM( 1, 12, IMON, 'Enter starting month (MM)' )

    ISDAY  = GETNUM( 1, MLENS( ISMON ), 1, 'Enter starting day (DD)' )

    ISHR   = GETNUM( 0, 23, 0, 'Enter starting hour (HH)' )

    NRECS  = GETNUM( 1, IBIG, 24*MLENS( ISMON ), 'Enter duration (number of input hours)' )

    IADDTZ = GETNUM( -IBIG, IBIG, 0, 'Enter time shift value (H*MMSS).' )

    TSTEP = 10000

    MESG = 'Enter output obs variable name (16 letter max)'
    I    = INDEX1( SPC, 4, SNAMES )
    CALL GETSTR( MESG, VNAMES( I ), OBSNAM )

    STDATE = 1000*ISYEAR + JULIAN( ISYEAR, ISMON, ISDAY )
    STTIME = ISHR*10000
    CALL NEXTIME( STDATE, STTIME, IADDTZ )

    ENDATE = STDATE
    ENTIME = STTIME
    CALL NEXTIME( ENDATE, ENTIME, 10000*NRECS )

    CALL M3MESG( BAR )
    IEYEAR = ENDATE / 1000
    CALL DAYMON( ENDATE, IEMON, IEDAY )
    MESG = 'Processing starts ' // DT2STR( STDATE, STTIME )
    CALL M3MESG( MESG )
    MESG = 'Processing ends   ' // DT2STR( ENDATE, ENTIME )
    CALL M3MESG( MESG )
    CALL M3MESG( BAR )

    !!  ---------------------
    !!  Cycle through the input files to determine how many sites
    !!  and how many cross-reference entries are present.
    !!  We'll look for '1' as the key character indicating
    !!  a new station report has been encountered.

    CALL M3MESG( 'Counting input records...' )
    NSTNS = 0
    L     = 0
11  CONTINUE        !  count-the-stations loop

    READ( ADEV, '( A1 )',IOSTAT=STATUS, END=22 ) STFLAG
    L = L + 1
    IF ( STATUS .NE. 0 ) THEN
        WRITE( MESG, '( A, I9, 2X, A, I9 )' ) 'Error counting AIRS input sites at line', L, 'I/O STATUS=', STATUS
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    ELSE IF ( STFLAG .EQ. '1' ) THEN
        NSTNS = NSTNS + 1
    END IF
    GO TO 11

22  CONTINUE                !  end of count-the-stations loop

    REWIND( ADEV )
    WRITE( MESG, '( A, I9 )' ) 'Total number of sites to be processed:', NSTNS
    CALL M3MESG( MESG )

    NCTY = 0
33  CONTINUE        !  count-the-xrefs loop

    READ( TDEV, '( A1 )',IOSTAT=STATUS, END=44 ) STFLAG
    L = L + 1
    IF ( STATUS .NE. 0 ) THEN
        WRITE( MESG, '( A, I9, 2X, A, I9 )' )       &
            'Error counting TZ XREF file at line', L, 'I/O STATUS=', STATUS
        CALL M3MESG( MESG )
        EFLAG = .TRUE.
    ELSE
        NCTY = NCTY + 1
    END IF
    GO TO 33

44  CONTINUE                !  end of count-the-xrefs loop

    REWIND( TDEV )
    WRITE( MESG, '( A, I9 )' )  'Total number of state/county/TZ entries:', NCTY
    CALL M3MESG( MESG )
    CALL M3MESG( BAR )

    !!  ---------------------
    !!  Create/Open output file:

    P_ALP3D = 0.0D0
    P_BET3D = 0.0D0
    P_GAM3D = 0.0D0
    XCENT3D = 0.0D0
    YCENT3D = 0.0D0
    XORIG3D = 0.0D0
    YORIG3D = 0.0D0
    XCELL3D = 0.0D0
    YCELL3D = 0.0D0
    FTYPE3D = CUSTOM3
    SDATE3D = STDATE
    STIME3D = STTIME
    TSTEP3D = TSTEP
    NCOLS3D = NSTNS
    NROWS3D = 1
    NLAYS3D = 1
    NTHIK3D = 1
    GDTYP3D = LATGRD3
    VGTYP3D = IMISS3
    VGTOP3D = BADVAL3

    N = 1
    VNAME3D(N) = 'STNID'
    UNITS3D(N) = 'none'
    VDESC3D(N) = 'Station ID: ((ISTATE*1000 + ICTY)*10000) + ISITE'
    VTYPE3D(N) = M3INT

    N = N + 1
    VNAME3D(N) = 'LAT'
    UNITS3D(N) = 'degrees'
    VDESC3D(N) = 'Latitude for monitor station'
    VTYPE3D(N) = M3REAL

    N = N + 1
    VNAME3D(N) = 'LON'
    UNITS3D(N) = 'degrees'
    VDESC3D(N) = 'Longitude for monitor station'
    VTYPE3D(N) = M3REAL

    N = N + 1
    VNAME3D(N) = OBSNAM
    UNITS3D(N) = 'ppmv'
    VDESC3D(N) = 'Observed value, or MISSING < -9.0E36'
    VTYPE3D(N) = M3REAL

    NVARS3D = N
    GDNAM3D = 'LATLON'

    FNAME = PROMPTMFILE('Enter I/O API output file logical name',&
                       FSUNKN3, 'AIRS_OBS', PNAME )

    !!  -------------------
    !!  allocate TZ XREF and AIRS STATION arrays:

    ALLOCATE(  KST( NCTY ),             &
              KCTY( NCTY ),             &
               KTZ( NCTY ),             &
               OBS( NSTNS,NRECS ),      &
               LAT( NSTNS ),            &
               LON( NSTNS ),            &
            AIRSID( NSTNS ), STAT = STATUS  )

    IF ( STATUS .NE. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A, I10 )' ) 'Buffer allocation failed:  STAT=', STATUS
        CALL M3MESG(  MESG )
    END IF

    IF ( EFLAG ) THEN
        MESG = 'Fatal setup errors'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    !!  -------------------
    !!  Read the time zone cross-reference file:

    DO  L = 1, NCTY
        READ( TDEV, *, IOSTAT=STATUS ) KST(L), KCTY(L), KTZ(L)
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I9, 2X, A, I9 )' )       &
                'Error reading TZ XREF file at line', L, 'I/O STATUS=', STATUS
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF
    END DO

    IF ( EFLAG ) THEN
        MESG = 'Fatal time-zone setup errors'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    !!  ---------------------
    !!  Initialize obeservations array

    DO J = 1, NRECS
    DO K = 1, NSTNS
        OBS( K,J ) = BADVAL3
    END DO
    END DO

    !!  ---------------------
    !!  Set up record-number offsets so that we always have
    !!  jstep3(jdate,jtime,...) > 0 below.  This is necessary
    !!  because jstep3() returns (-1) when jdate:jtime before
    !!  the indicated time step sequence starts, or is not an
    !!  exact time step within it.

    YDATE = 1000*YEAR + 1
    YREC  = JSTEP3( STDATE, STTIME, YDATE, IADDTZ, TSTEP ) - 1

    !!  -----------------
    !!  Process the input data file (where data is in order by
    !!  station, not by time step):

    CALL M3MESG( BAR )
    CALL M3MESG( 'Processing input data...' )

    DO  M = 1, NSTNS

        IF ( .NOT. RDHEADER( ADEV, SDEV,                    &
                             SPC, IST, ICTY, IMON, YEAR,    &
                             LAT(M), LON(M), AIRSID(M),     &
                             IUNIT ) ) THEN
            WRITE( MESG, '( A, I9 )' ) 'Error reading header for site', M
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            CYCLE
        END IF

        I = INDEX1( SPC, 4, SNAMES )
        IF ( I .LE. 0 ) THEN
            WRITE( MESG, '( 3A, I9 )' )&
                'Unexpected species "', SPC, '" for station', M
            CALL M3MESG( MESG )
        END IF

        J = MAX( FIND1( IUNIT, 5, KUNIT ), 0 )
        IF ( J .EQ. 0 ) THEN
            WRITE( MESG, '( A, I9, 2X, A, I9 )' )&
                'Unexpected units "', IUNIT, 'for station', M
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF

        K = MAX( FIND1( IUNIT, 2, CUNIT ), 0 )
        IF ( K .GT. 0 ) THEN                !  mg/m3 conversion
            CONVFAC = SFACS( I ) * KFACS( J )
        ELSE                                !  pp<volume> conversion
            CONVFAC = KFACS( J )
        END IF

        !!  Look up time zone shift (key=zero acts like a wild-card)

        I     = MAX( FIND2( IST, ICTY, NCTY, KST, KCTY ), 0 )
        IF ( I .EQ. 0 ) THEN
            I = MAX( FIND2( IST,    0, NCTY, KST, KCTY ), 0 )
        ELSE IF ( I .EQ. 0 ) THEN
            I = MAX( FIND2(   0,    0, NCTY, KST, KCTY ), 0 )
        ELSE IF ( I .EQ. 0 ) THEN
            WRITE( MESG, '( A, I9 )' )&
                'No time zone available for STATION ID=', AIRSID(M)
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            CYCLE
        END IF

        ISHIFT = 10000 * KTZ(I) + IADDTZ

        JDATE = 1000*YEAR + JULIAN( YEAR, IMON, 1 )
        JTIME = 10000
        CALL NEXTIME(  JDATE, JTIME, ISHIFT )
        IREC = JSTEP3( JDATE, JTIME, YDATE, IADDTZ, TSTEP ) - YREC

        DO J = 1, MLENS( IMON )

            READ ( ADEV, '( 1X, I2, 2X, 24(A4, A1) )', IOSTAT=STATUS )      &
                DAYS, ( COBS(K), AFLAG(K), K = 0, 23 )

            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '( A, I9, 2X, A, I2, 2X, A, I9 )' )            &
                    'Error reading AIRS file for station', AIRSID(M), 'day', K, 'I/O STATUS=', STATUS
                CALL M3MESG( MESG )
                EFLAG = .TRUE.
                CYCLE
            END IF

            DO K = 0, 23

                IF ( IREC .GE. 1 .AND. IREC .LE. NRECS ) THEN
                    IF ( ( COBS(K)      .NE. '    ' ) .AND.&
                         ( COBS(K)(1:2) .NE.'99'    ) ) THEN
                        ROBS = STR2REAL( COBS(K) )
                        I    = INDEX1( AFLAG(K), 5, UFLAG )
                        OBS( M,IREC ) = CONVFAC*UFACS( I )*ROBS
                    END IF
                END IF

                IREC = IREC + 1

            END DO                          ! end hour loop

        END DO                          ! end day loop

    END DO                          !  end loop on stations

    IF ( EFLAG ) THEN
        MESG = 'Fatal data-input errors'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    CALL M3MESG( 'All the AIRS data have been processed' )
    CALL M3MESG( BAR )

    !!  -----------------
    !!  Write the data to the output file, in time step order
    !!  instead of in station order:

    JDATE = STDATE
    JTIME = STTIME

    DO K = 1, NRECS

        IF ( .NOT.WRITE3( FNAME, 'STNID', JDATE, JTIME, AIRSID) ) THEN
            MESG = 'Error writing variable STNID'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF

        IF ( .NOT.WRITE3( FNAME, 'LAT', JDATE, JTIME, LAT ) ) THEN
            MESG = 'Error writing variable LAT'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF

        IF ( .NOT.WRITE3( FNAME, 'LON', JDATE, JTIME, LON ) ) THEN
            MESG = 'Error writing variable LON'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF

        !!  Write this hour's "slab" from array OBS

        IF ( .NOT.WRITE3( FNAME, OBSNAM, JDATE, JTIME, OBS( 1,K ) ) ) THEN
            MESG = 'Error writing variable' // OBSNAM
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF

        CALL NEXTIME( JDATE, JTIME, TSTEP3D )

    END DO

    CALL M3MESG( BAR )
    CALL M3EXIT( PNAME, 0, 0, 'Normal program completion', 0 )


CONTAINS  !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION RDHEADER( ADEV, SDEV,                      &
                               SPC, IST, ICTY, IMON, YEAR,      &
                               LAT, LON, AIRSID, IUNIT )

        IMPLICIT NONE

        !!  -------------------  Arguments  ------------------------------

        INTEGER         ADEV            !  ASCII AIRS input file
        INTEGER         SDEV            !  ASCII Site-crossref output file
        CHARACTER*5     SPC
        INTEGER         IST, ICTY, IMON, YEAR
        REAL            LAT,  LON
        INTEGER         AIRSID, IUNIT


        !!  -------------------  Parameters  -----------------------------

        REAL, PARAMETER::    CONMIN    = 1.0 /   60.0
        REAL, PARAMETER::    CONSEC    = 1.0 / 3600.0

        !!  -------------------  Local Variables  ------------------------

        CHARACTER*256   MESG
        CHARACTER*80    LINE
        CHARACTER*37    ASTATE, ACITY, ACOUNTY
        CHARACTER*1     ALAT, ALON
        CHARACTER*3     MONTH
        INTEGER         STATUS

        INTEGER         ISITE
        REAL            LATA, LATB, LATC
        REAL            LONA, LONB, LONC

        LOGICAL,     SAVE::     FIRSTIME = .TRUE.

        CHARACTER*3, PARAMETER ::     MONTHS( 12 ) =        &
           (/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',     &
              'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC' /)

        CHARACTER*3, SAVE::     OLDMON
        CHARACTER*5, SAVE::     OLDSPC
        INTEGER    , SAVE::     OLDYR

        !!  -------------------  Function body  --------------------------

        !!  Loop:  read until start of next header

11      CONTINUE
        READ( ADEV, '( A  )', IOSTAT=STATUS ) LINE
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'Error reading AIRS record header; I/O STATUS=',STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        ELSE IF ( LINE( 1:1 ) .NE. '1' ) THEN
            GO TO 11
        END IF

        !!  Read start-of-header flag

        READ( ADEV, '( /, A  )', IOSTAT=STATUS ) LINE
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'Error reading AIRS format header; I/O STATUS=',STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        ELSE IF ( LINE( 2:7 ) .NE. 'AMP350' ) THEN
            MESG = 'Unexpected file format encountered: "' // LINE( 2:7 ) // '" not "AMP350"'
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF      !  if start-of-header error

        READ( ADEV, '( /, 16X, A5, /, 67X, A37, /, 61X, A3, 1X, I4 )', IOSTAT=STATUS ) SPC, ASTATE, MONTH, YEAR
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'Error reading AIRS state/month header; I/O STATUS=',STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        IMON = INDEX1( MONTH, 12, MONTHS )
        IF ( IMON .LT. 0 ) THEN
            WRITE( MESG, '( 3 A )' ) 'Unexpected month "', MONTH, '" found in AIRS header'
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        IF ( FIRSTIME ) THEN
            OLDMON   = MONTH
            OLDSPC   = SPC
            OLDYR    = YEAR
            FIRSTIME = .FALSE.
        ELSE IF ( OLDMON .NE. MONTH .OR.        &
                  OLDSPC .NE. SPC   .OR.        &
                  OLDYR  .NE. YEAR ) THEN
            WRITE( MESG, '( 2 A )' ) 'MONTH:SPECIES:YEAR mismatch in AIRS header'
            CALL M3MESG( MESG )
            MESG = 'Month: "' // MONTH // '" vs "' // OLDMON // '"'
            CALL M3MESG( MESG )
            MESG = 'Species: "' // SPC // '" vs "' // OLDSPC // '"'
            CALL M3MESG( MESG )
            WRITE( MESG, '( A, I4, A, I4, A )' ) 'Year: "', YEAR, '" vs "', OLDYR, '"'
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

104     FORMAT(10X, I2, 1X, I3, 1X, I4, 99X, F3.0, 1X, 2(F2.0, 1X), A1)
105     FORMAT(17X, A37, 66X, F3.0, 1X, 2(F2.0, 1X), A1 )

        READ( ADEV, 104, IOSTAT=STATUS  ) IST, ICTY, ISITE, LATA, LATB, LATC, ALAT
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( A, I9 )' ) 'Error reading AIRS site header; I/O STATUS=', STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        AIRSID = ( ( IST * 1000 + ICTY ) * 10000 ) + ISITE

        READ( ADEV, 105, IOSTAT=STATUS  ) ACOUNTY, LONA, LONB, LONC, ALON
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( 2( A, I9, :, 2X ) )' ) 'Error reading AIRS county header for station', AIRSID, 'I/O STATUS=', STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        LAT = LATA + LATB*CONMIN + LATC*CONSEC
        IF (ALAT.EQ.'S') LAT = -LAT

        LON = LONA + LONB*CONMIN + LONC*CONSEC
        IF (ALON.EQ.'W') LON = -LON

        READ( ADEV, 105, IOSTAT=STATUS ) ACITY
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( 2( A, I9, :, 2X ) )' ) 'Error reading AIRS city header for station', AIRSID, ' I/O STATUS=', STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        !!  Write index/cross-reference report record for this station

106     FORMAT( I9.9, ',', 2X, '''', 3( A, '''', ',', 2X ), F9.5, ',', 2X, F10.5, ')' )
        IF ( SDEV .GE. 0 ) THEN
            WRITE ( SDEV, 106, IOSTAT=STATUS ) AIRSID, TRIM(ACITY), TRIM(ACOUNTY), TRIM(ASTATE), LAT, LON
            IF ( STATUS .NE. 0 ) THEN
                WRITE( MESG, '( 2( A, I9, :, 2X ) )' )      &
                    'Error writing AIRS XREF file for station', AIRSID, 'I/O STATUS=', STATUS
                CALL M3MESG( MESG )
                RDHEADER = .FALSE.
                RETURN
            END IF      !  if I/O status bad
        END IF          !  if sdev >= 0

107     FORMAT (5(/),8X,I3,4(/))
        READ( ADEV, 107, IOSTAT=STATUS ) IUNIT
        IF ( STATUS .NE. 0 ) THEN
            WRITE( MESG, '( 2( A, I9, :, 2X ) )' ) 'Error reading AIRS units header for station', AIRSID, 'I/O STATUS=', STATUS
            CALL M3MESG( MESG )
            RDHEADER = .FALSE.
            RETURN
        END IF

        RDHEADER = .TRUE.
        RETURN

    END FUNCTION RDHEADER


END PROGRAM AIRS2M3

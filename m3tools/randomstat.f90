
PROGRAM RANDOMSTAT

    !!***********************************************************************
    !! Version "$ $Id: randomstat.f90 146 2020-03-25 18:03:32Z coats $"
    !! EDSS/Models-3 I/O API Test Suite.
    !! Copyright (C) 2020 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  60
    !!
    !!  FUNCTION:
    !!      Random-access-statistics  test/demonstration for I/O API
    !!
    !!  REVISION  HISTORY:
    !!      prototype 03/2020 by CJC
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'RANDOMSTAT'
    CHARACTER*72, PARAMETER :: BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         IDEV, ISTAT
    INTEGER         N, V, J, K, L

    INTEGER         JDATE, JTIME, TSTEP
    INTEGER         SDATE, STIME, EDATE, ETIME, TSECS, NRECS
    INTEGER         SECS, STEPS
    INTEGER         NCOLS1      ! number of grid columns
    INTEGER         NROWS1      ! number of grid rows
    INTEGER         NLAYS1      ! number of layers
    INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension
    INTEGER         SDATE1, STIME1, EDATE1, ETIME1

    CHARACTER*16    MYVBLE
    CHARACTER*24    DANDT
    CHARACTER*256   MESG

    REAL, ALLOCATABLE :: RBUF( :,:,: )

    !!***********************************************************************
    !!   begin body of program M3FAKE

    IDEV = INIT3()

    WRITE( *,'( 5X, A )' ) BLANK, BAR, BLANK,                               &
'Program "RANDOMSTAT" to demonstrate random access I/O within the I/O API', &
'This program can be found within the installation "tests" directory.',     &
'It is loosely adapted from "m3tools/m3stat.f" but it simplified',          &
'considerably, and modified to allow time-stepping both forward and',       &
'backward in time.',                                                        &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'     setenv  MYFILE  <path-name>',                                         &
'     setenv  MYVBLE  <variable of type REAL in file ${MYFILE}',            &
'     ${MYFILE} is a time stepped GRIDDED file.',                           &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the starting and ending dates and times',  &
'for the time period to be analyzed.',                                      &
''  ,                                                                       &
'Note that if the ending date&time is BEFORE the starting date&time,',      &
'the analysis will be done in REVERSE chronological order.',                &
'',                                                                         &
'Program copyright (C) 2020 UNC Institute for the Environment.',            &
'Released under Version 2 of the GNU General Public License. See',          &
'enclosed GPL.txt, or URL',                                                 &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                       &
'or',                                                                       &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version: ',                                                        &
'$Id: randomstat.f90 146 2020-03-25 18:03:32Z coats $',&
' '

    !!...............  Open and get description for input data file
    
    IF ( .NOT.OPEN3( 'MYFILE', FSREAD3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Could not open "MYFILE"', 2 )
    ELSE IF ( .NOT.DESC3( 'MYFILE' ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  Could not DESC3( MYFILE )', 2 )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'ERROR:  MYFILE not of type GRIDDED', 2 )
    ELSE
        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        GDTYP1 = GDTYP3D
        P_ALP1 = P_ALP3D
        P_BET1 = P_BET3D
        P_GAM1 = P_GAM3D
        XCENT1 = XCENT3D
        YCENT1 = YCENT3D
        XORIG1 = XORIG3D
        YORIG1 = YORIG3D
        XCELL1 = XCELL3D
        YCELL1 = YCELL3D
        SDATE1 = SDATE3D
        STIME1 = STIME3D
        CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE1,ETIME1 )
    END IF


    !!...............  Get environment

    CALL ENVSTR( 'MYVBLE', 'Name of variable to analyze', 'STEP', MYVBLE, ISTAT )
    V = INDEX1( MYVBLE, NVARS3D, VNAME3D )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "MYVBLE"', 2 )
    ELSE IF ( V .LE. 0 ) THEN
        MESG = 'Variable "' // TRIM( MYVBLE ) // '" not found in "MYFILE"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( VTYPE3D(V) .NE. M3REAL ) THEN
        MESG = 'Variable "' // TRIM( MYVBLE ) // '" not of type REAL'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Allocate input buffer:

    ALLOCATE( RBUF( NCOLS1,NROWS1,NLAYS1 ),  STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10)' ) 'Buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

 
    !!...............  Get time step sequence
    
    SDATE = GETNUM( SDATE1, EDATE1, EDATE1-1, 'Enter starting date for analysis window' )
    EDATE = GETNUM( SDATE1, EDATE1, EDATE1-1, 'Enter   ending date for analysis window' )


    !!...............  Time step loop:
    
    IF ( SDATE .GT. EDATE ) THEN
        JDATE = SDATE
        JTIME = 0
        TSTEP = -TSTEP3D
        NRECS = CURREC( SDATE,235959, EDATE,0,TSTEP3D, K,L )
    ELSE
        JDATE = EDATE
        JTIME = 0
        TSTEP = TSTEP3D
        NRECS = CURREC( EDATE,235959, SDATE,0,TSTEP3D, K,L )        
    END IF
    WRITE( MESG, '( A, I9 )' ) 'Number of time steps:', NRECS
    CALL M3MESG( MESG )
    
    DO N = 1, NRECS
    
        DANDT = DT2STR( JDATE,JTIME )
        WRITE( MESG, '( 3 A, I9.7, I6.6, 2X, 3 A )' )           &
            'Reading "', TRIM( MYVBLE ), '" for', JDATE, JTIME, &
            '(', DANDT, ')'
        CALL M3MESG( MESG )
        IF ( .NOT.READ3( 'MYFILE', MYVBLE, ALLAYS3, JDATE, JTIME, RBUF ) ) THEN
            MESG = 'Error reading variable "' // TRIM( MYVBLE ) // '" from "MYFILE"'
            CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
        END IF
        
        CALL STAT( RBUF )
    
        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO

    CALL M3EXIT( PNAME, 0, 0, 'Program completed successfully', 0 )


CONTAINS

    SUBROUTINE STAT( RBUF )
    
        REAL, INTENT( IN ) :: RBUF( NCOLS1, NROWS1, NLAYS1 )
        
        REAL*8          SUM
        INTEGER         C, R, L
        CHARACTER*256   MESG
        
        SUM = 0.0d0
        DO L = 1, NLAYS1
        DO R = 1, NROWS1
        DO C = 1, NCOLS1
            SUM = SUM + RBUF( C,R,L )
        END DO
        END DO
        END DO

        WRITE( MESG, '( A, 1PE14.6 )' ) 'Mean=', SUM / DBLE( NCOLS1*NROWS1*NLAYS1 )
        CALL M3MESG( MESG )
        CALL M3MESG( BLANK )
        RETURN
        

    END SUBROUTINE STAT

END PROGRAM RANDOMSTAT


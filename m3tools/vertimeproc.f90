
PROGRAM VERTIMEPROC

    !!***************************************************************
    !!  Version "$Id: vertimeproc.f90 1 2017-06-10 18:05:20Z coats $"
    !!  Copyright (c) 2014 UNC Institute for the Environment
    !!  Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !!  See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  Program body starts at line  73
    !!
    !!  DESCRIPTION:  see splash screen
    !!
    !!  PRECONDITIONS:  see splash screen
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  11/2014 by Carlie J. Coats, Jr., UNC IE
    !!      Version    03/2015 by CJC:  call RUNSPEC() for SDATE:STIME:TSTEP:NRECS
    !!***************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!......  PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER ::  PNAME = 'VERTIMEPROC'
    CHARACTER*1,  PARAMETER ::  BLANK = ' '
    CHARACTER*1,  PARAMETER ::  SLASH = '/'
    CHARACTER*80, PARAMETER ::  BAR   = &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!......  LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV, ISTAT
    INTEGER         SDATE, STIME, TSTEP, EDATE, ETIME
    INTEGER         NRECS, NAGGS, JDATE, JTIME, KDATE, KTIME
    INTEGER         TSECS, TSECS1
    INTEGER         V, W, N

    LOGICAL         EFLAG

    REAL            TFAC

    CHARACTER*256   MESG

    !!     name, parameters for input grid

    INTEGER         NCOLS1
    INTEGER         NROWS1
    INTEGER         NLAYS1
    INTEGER         NVARS1
    INTEGER         TSTEP1
    INTEGER         GDTYP1
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension
    LOGICAL         VFLAG1( MXVARS3 )
    CHARACTER*16    VNAME1( MXVARS3 )
    CHARACTER*80    FDESC1( MXVARS3 )

    REAL   ,   ALLOCATABLE :: VBUF( :,:,: )
    REAL*8 ,   ALLOCATABLE :: DTOT( :,: )


    !!--------------------------------------------------------------
    !!   begin body of program VERTIMEPROC

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( LDEV, '( 5X, A )' )  BLANK, BAR,                                     &
'Program VERTIMEPROC to compute vertical-time-period totals for REAL'      ,    &
'variables from a specified 3D GRIDDED input file and specified time-'     ,    &
'aggregation period, and write the result to a 2-D GRIDDED output file.'   ,    &
'Aggregation is TIME-STAMPED ON THE LEFT:  i.e., the output record for'    ,    &
'date&time JDATE:JTIME represents the temporal/vertical total *starting"'  ,    &
'at JDATE:JTIME.'                                                          ,    &
''  ,                                                                           &
'PRECONDITIONS REQUIRED:'                                ,                      &
'   setenv INPFILE    <path name for input file>'        ,                      &
'   setenv OUTFILE    <path name for output file>'  ,                           &
''  ,                                                                           &
'   setenv SDATE      <starting date YYYYDDD>'  ,                               &
'   setenv STIME      <starting time  HHMMSS>'  ,                               &
'   setenv EDATE      <ending   date YYYYDDD>'  ,                               &
'   setenv ETIME      <ending   time  HHMMSS>'  ,                               &
'   setenv TSTEP      <time step      HHMMSS> for the aggregation.'  ,          &
''  ,                                                                           &
'   TSTEP must be an exact multiple of the input-file time step.'  ,            &
''  ,                                                                           &
'   All variables have consistent units of the form <value per unit time>'  ,   &
'   where time is one of:'  ,                                                   &
''  ,                                                                           &
'       "year", "week", "day", "hour", "hr", "min", "s", "sec"'  ,              &
''  ,                                                                           &
'Copyright (C) 2014 UNC Institute for the Environment.'  ,                      &
'Released under Version 2 of the GNU General Public License. See'  ,            &
'enclosed GPL.txt, or URL',                                                     &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
''  ,                                                                           &
'Comments and questions are welcome and can be sent to'  ,                      &
'',                                                                         &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                       &
'or',                                                                       &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version:',                                                             &
'$Id: vertimeproc.f90 1 2017-06-10 18:05:20Z coats $',&
''

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        MESG = 'Program terminated at user request'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open input file:

    IF ( .NOT.OPEN3( 'INPFILE', FSREAD3, PNAME ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not open "INPFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( .NOT.DESC3( 'INPFILE' ) ) THEN
        EFLAG = .TRUE.
        MESG  = 'Could not get description for "INPFILE"'
        CALL M3MESG( MESG )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        EFLAG = .TRUE.
        MESG  = 'File type for "INPFILE" not GRIDDED'
        CALL M3MESG( MESG )
    ELSE

        NCOLS1 = NCOLS3D
        NROWS1 = NROWS3D
        NLAYS1 = NLAYS3D
        NVARS1 = NVARS3D
        TSTEP1 = TSTEP3D
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

        TSECS1 = TIME2SEC( TSTEP3D )
        TFAC   = TIMEFAC( UNITS3D(1), TSECS1 )

        FDESC1( 1:NVARS1 ) =   FDESC3D( 1:NVARS1 )
        VNAME1( 1:NVARS1 ) =   VNAME3D( 1:NVARS1 )
        VFLAG1( 1:NVARS1 ) = ( VTYPE3D( 1:NVARS1 ) .EQ. M3REAL )

    END IF                          !  if  open3( inpfile )


    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Fatal input-file error(s)', 2 )
    END IF


    !!...............  Get run-specifications from environment

    CALL RUNSPEC( 'INPFILE', .TRUE., SDATE, STIME, TSTEP, NRECS )


    !!...............  Allocate work arrays:
    !!  NOTE:  Really, it is bad modularity to allocate scratch arrays for
    !!  CONTAINed SUBROUTINE AGGVAR() here, but it makes the program more
    !!  robust with respect to stacksize limits and error-reporting.

    ALLOCATE( VBUF( NCOLS1,NROWS1,NLAYS1 ),   &
              DTOT( NCOLS1,NROWS1 ),        STAT = ISTAT )
    IF ( ISTAT .NE. 0 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A, I10 )' ) 'Allocation failure for work arrays:  STATUS=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Open output file:

    SDATE3D = SDATE
    STIME3D = STIME
    TSTEP3D = TSTEP

    NLAYS3D = 1

    W = 0
    DO V = 1, NVARS1
        IF ( VFLAG1( V ) ) THEN
            W = W + 1
            VNAME3D( W ) = VNAME3D( V )
            UNITS3D( W ) = UNITFIX( UNITS3D( V ) )
            VDESC3D( W ) = 'Layer&Time aggregated ' // VDESC3D( V )
            VTYPE3D( W ) = VTYPE3D( V )
        END IF
    END DO
    NVARS3D = W

    FDESC3D( 1 )         = 'Layer- / time-aggregated values'
    FDESC3D( 2:MXDESC3 ) = FDESC1( 1:MXDESC3-1 )

    IF ( NVARS3D .EQ. 0 ) THEN
        CALL M3EXIT( PNAME, 0,0, 'No REAL output variables available', 2 )
    ELSE IF ( .NOT.OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open "OUTFILE"', 2 )
    END IF


    !!..........  Process output time step sequence

    CALL M3MESG( BAR )
    JDATE = SDATE
    JTIME = STIME

    DO  N = 1, NRECS

        WRITE( MESG, '( A, I7.7, A, I6.6 )' ) 'Processing  ', JDATE, ':', JTIME
        CALL M3MESG( BLANK )
        CALL M3MESG( MESG )

        DO V = 1, NVARS1

            IF ( VFLAG1( V ) ) THEN

                CALL AGGVAR( 'INPFILE', 'OUTFILE', VNAME1( V ), JDATE, JTIME, TSTEP1, NAGGS, TFAC )

            END IF

        END DO      !!  end loop on variables

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO          !!  end loop on output time steps


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!  Compute per-timestep factor relative to the current units
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    REAL FUNCTION TIMEFAC( UNITS, STEPSECS )

        CHARACTER*16, INTENT(IN   ) :: UNITS
        INTEGER     , INTENT(IN   ) :: STEPSECS

        REAL, PARAMETER :: YR2SEC = 365.0 * 24.0 * 3600.0
        REAL, PARAMETER :: WK2SEC =   7.0 * 24.0 * 3600.0
        REAL, PARAMETER :: DY2SEC =         24.0 * 3600.0
        REAL, PARAMETER :: HR2SEC =                3600.0
        REAL, PARAMETER :: MN2SEC =                  60.0

        CHARACTER*16, PARAMETER :: TNAME( 10 ) =     &
            (/      'YEAR', 'YR  ', 'WEEK', 'WK  ', 'DAY ', 'HOUR', 'HR  ', 'MIN ', 'S   ', 'SEC ' /)

        REAL        , PARAMETER :: TSECS( 0:10 ) =   &
            (/  1.0, YR2SEC, YR2SEC, WK2SEC, WK2SEC, DY2SEC, HR2SEC, HR2SEC, MN2SEC, 1.0   ,  1.0   /)

        INTEGER         L, N
        REAL            FAC
        CHARACTER*16    DENOM

        !!...........  begin body of TIMEFAC()  ........................

        FAC = FLOAT( STEPSECS )

        L = INDEX( UNITS, SLASH )

        IF ( L .GT. 0 ) THEN
            DENOM = ADJUSTL( UNITS( L+1: ) )
            CALL UPCASE( DENOM )
            N   = INDEX1( DENOM, 10, TNAME )
            FAC = FAC / TSECS( N )
        END IF

        TIMEFAC = FAC
        RETURN

    END FUNCTION TIMEFAC


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!  Fix up units for time-aggregated variables
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    CHARACTER*16 FUNCTION UNITFIX( UNITS )

        CHARACTER*16, INTENT(IN   ) :: UNITS

        INTEGER         L

        !!...........  begin body of UNITFIX()  ........................

        L = INDEX( UNITS, SLASH )
        IF ( L .GT. 0 ) THEN
            UNITFIX = UNITS( 1:L-1 )
        ELSE
            UNITFIX = UNITS
        END IF

        RETURN

    END FUNCTION UNITFIX


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!  Aggregate JRECS time steps and NLAYS1 layers of VNAME from FNAME
    !!  into 2-D and write the result to GNAME
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    SUBROUTINE  AGGVAR( FNAME, GNAME, VNAME, JDATE, JTIME, JSTEP, JRECS, TFAC )

        CHARACTER(*), INTENT(IN   ) :: FNAME        !!  input-file name
        CHARACTER(*), INTENT(IN   ) :: GNAME        !!  output-file name
        CHARACTER(*), INTENT(IN   ) :: VNAME        !!  variable-name
        INTEGER     , INTENT(IN   ) :: JDATE        !!  starting date&time for aggregation
        INTEGER     , INTENT(IN   ) :: JTIME
        INTEGER     , INTENT(IN   ) :: JSTEP        !!  input time step
        INTEGER     , INTENT(IN   ) :: JRECS        !!  number of steps per aggregation
        REAL        , INTENT(IN   ) :: TFAC

        INTEGER     C, R, L, N
        INTEGER     IDATE, ITIME
        LOGICAL     EFLAG

        CHARACTER*256   MESG

        !!...........  begin body of AGGVAR()  ........................

        IDATE = JDATE
        ITIME = JTIME
        EFLAG = .FALSE.

        DO N = 1, JRECS

            IF ( .NOT.READ3( FNAME, VNAME, ALLAYS3, IDATE, ITIME, VBUF ) ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I9.7, A, I6.6 )' )   &
                    'Could not read "', TRIM( VNAME ), '" from "', TRIM( FNAME ) , '" for', IDATE, ':', ITIME
                CALL M3MESG( MESG )
                RETURN
            END IF

!$OMP   PARALLEL DO                                                     &
!$OMP&      DEFAULT( NONE ),                                            &
!$OMP&       SHARED( N, NCOLS1, NROWS1, NLAYS1, VBUF, DTOT, TFAC ),     &
!$OMP&      PRIVATE( C, R, L )

            DO R = 1, NROWS1

                IF ( N .EQ. 1 ) THEN
                    DO C = 1, NCOLS1
                        DTOT( C,R ) = 0.0D0
                    END DO
                END IF

                DO L = 1, NLAYS1
                DO C = 1, NCOLS1
                    DTOT( C,R ) = DTOT( C,R ) + VBUF( C,R,L )
                END DO
                END DO

                IF ( N .EQ. JRECS ) THEN
                    DO C = 1, NCOLS1
                        VBUF( C,R,1 ) = TFAC * DTOT( C,R )
                    END DO
                END IF

            END DO

            CALL NEXTIME( IDATE, ITIME, JSTEP )

        END DO

        IF ( .NOT.WRITE3( GNAME, VNAME, JDATE, JTIME, VBUF ) ) THEN
            EFLAG = .TRUE.
            WRITE( MESG, '( 5 A, I9.7, A, I6.6 )' )       &
                'Error writing "', TRIM( VNAME ), '" to "', TRIM( GNAME ) , '" for', JDATE, ':', JTIME
            CALL M3MESG( MESG )
        END IF

        RETURN

    END SUBROUTINE  AGGVAR


END PROGRAM VERTIMEPROC

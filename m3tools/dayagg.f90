
PROGRAM DAYAGG

    !!***********************************************************************
    !! Version "$Id: dayagg.f90 101 2015-01-16 16:52:50Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, 
    !! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
    !! and (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  121
    !!
    !!  DESCRIPTION:
    !!       Program DAYAGG to construct "seasonal standard day" weighted
    !!       averages from a set of input files and write the result to a
    !!       "standard day" output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       All files have the same grid structure and contain all the
    !!       requested output variables on the requested time step structure.
    !!       Requested variables are all of type REAL
    !!       setenv STTIME   <starting time for the "seasonal standard day"
    !!                       in format HHMMSS>
    !!       setenv TSTEP    <time step for the "seasonal standard day"
    !!                       in format HHMMSS>
    !!       setenv RUNLEN   <duration for the "seasonal standard day"
    !!                       in format HHMMSS>
    !!       setenv FILELIST <comma-delimited list of input files>
    !!       setenv WGTSLIST <comma-delimited list of weighting factors
    !!                       for the input-files>
    !!       setenv VBLELIST <comma-delimited list of output variables>
    !!       setenv DATELIST <comma-delimited list of starting dates>
    !!       setenv AGGFILE  <physical (path) name>
    !!       For each of the input files,
    !!               setenv <logical name> <physical (path) name>
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      none
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 12/6/2000 by Carlie J. Coats, Jr., NCSC, adapted
    !!      from "m3tproc'
    !!
    !!      Version   11/2001 by CJC for I/O API Version 2.1
    !!
    !!      Version   11/2005 by CJC:  eliminate unused vbles
    !!
    !!      Version   06/2008 by CJC:  output starting date is
    !!         1 = 1000*YEAR + DAY
    !!
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!
    !!      Version 10/2014 by CJC:  Fix handling of ENV*() calls.
    !!
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source,
    !!      use ENVLIST() generic routines
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME   = 'DAYAGG'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*72, PARAMETER :: BAR   =      &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER     STTIME
    INTEGER     TSTEP
    INTEGER     RUNLEN
    INTEGER     NFILES, NSTEPS, NVARS
    INTEGER     JDATE, JTIME
    LOGICAL     EFLAG

    CHARACTER*16  VNAME( MXVARS3 )
    CHARACTER*16  UNITS( MXVARS3 )
    CHARACTER*80  VDESC( MXVARS3 )

    CHARACTER*16  FNAME( MXFILE3 )
    INTEGER      SDATES( MXFILE3 )
    INTEGER      JDATES( MXFILE3 )
    INTEGER      JTIMES( MXFILE3 )
    REAL        WEIGHTS( MXFILE3 )

    REAL            WTFAC

    INTEGER         ESTAT
    INTEGER         I, C, R, L, V, N, T

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

    REAL, ALLOCATABLE :: ABUF( :, :, : )
    REAL, ALLOCATABLE :: TBUF( :, :, : )

    CHARACTER*256   MESG


    !!***********************************************************************
    !!   begin body of program DAYAGG

    EFLAG = .FALSE.
    I = INIT3()
    WRITE( *,'( 5X, A )' ) BLANK, BAR, BLANK,                               &
'Program DAYAGG to construct a "standard day" file for a set of selected',  &
'variables, by constructing the weighted average of the variables from a',  &
'set of input files, and write the result to a "standard day" output file.',&
'',                                                                         &
'PRECONDITIONS REQUIRED: ',                                                 &
'     setenv STTIME   <starting time (HHMMSS) for the output>',             &
'     setenv TSTEP    <time step (HHMMSS) for the output>',                 &
'     setenv RUNLEN   <duration (HHMMSS) for the output>',                  &
'     setenv AGGFILE  <output-file physical (path) name>',                  &
'     setenv FILELIST <(comma-delimited) list of input file',               &
'                      logical names>',                                     &
'     setenv WGTSLIST <list of weighting factors>',                         &
'     setenv VBLELIST <list of output variables>',                          &
'     setenv DATELIST <list of input-file starting dates>',                 &
'',                                                                         &
' For each of the input files,',                                            &
'     setenv <logical name> <physical name>',                               &
'',                                                                         &
'     All files have the same grid structure and contain all',              &
'     the requested output variables on the requested time',                &
'     step structure.',                                                     &
'',                                                                         &
'     Requested variables are all of type REAL',                            &
'',                                                                         &
'See URL',                                                                  &
'    https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',        &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2015 UNC Institute for the Environment.',                              &
'Released under Version 2 of the GNU General Public License. See',          &
'enclosed "GPL.txt", or URL',                                               &
''  ,                                                                       &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',               &
''  ,                                                                       &
'Comments and questions are welcome and can be sent to'  ,                  &
'',                                                                         &
'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',                        &
'    UNC Institute for the Environment',                                    &
'    100 Europa Dr., Suite 490 Rm 405',                                     &
'    Campus Box 1105',                                                      &
'    Chapel Hill, NC 27599-1105',                                           &
'',                                                                         &
'Program version: ',                                                        &
'$Id: dayagg.f90 101 2015-01-16 16:52:50Z coats $',&
' '

    IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program terminated at user request', 2 )
    END IF

    STTIME = ENVGET( 'STTIME', 'Starting TIME (HHMMSS)', 0, ESTAT )
    IF ( ESTAT .GT. 0 ) THEN
        MESG  = 'Invalid/missing environment variable "STTIME"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    TSTEP = ENVGET( 'TSTEP', 'TIME-STEP (HHMMSS)', 10000, ESTAT )
    IF ( ESTAT .NE. 0 ) THEN
        MESG  = 'Invalid/missing environment variable "TSTEP"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    RUNLEN = ENVGET( 'RUNLEN', 'Run duration (HHMMSS)', 0, ESTAT )
    IF ( ESTAT .NE. 0 ) THEN
        MESG  = 'Invalid/missing environment variable "RUNLEN"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    MESG = 'List of extracted output variables'
    IF ( .NOT. ENVLIST( 'VBLELIST', MESG, MXVARS3, NVARS, VNAME ) ) THEN
        MESG  = 'Bad environment vble "VBLELIST"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    MESG = 'List of input-file logical names'
    IF ( .NOT. ENVLIST( 'FILELIST', MESG, MXFILE3, NFILES, FNAME ) ) THEN
        MESG  = 'Bad environment vble "FILELIST"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    MESG = 'List of input-file starting dates'
    IF ( .NOT. ENVLIST( 'DATELIST', MESG, MXFILE3, N, SDATES ) ) THEN
        MESG  = 'Bad environment vble "DATELIST"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    ELSE IF ( N .NE. NFILES ) THEN
        WRITE ( MESG, '( A, I4, 2X, A, I4 )' )      &
           'Inconsistency:  # of input files:', NFILES, '# of DATES:', N
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    MESG = 'List of input-file weighting factors'
    IF ( .NOT. ENVLIST( 'WGTSLIST', MESG, MXFILE3, N, WEIGHTS ) ) THEN
        MESG  = 'Bad environment vble "WGTSLIST"'
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    ELSE IF ( N .NE. NFILES ) THEN
        WRITE ( MESG, '( A, I4, 2X, A, I4 )' )      &
           'Inconsistency:  # of input files:', NFILES, '# of WEIGHTS:', N
        EFLAG = .TRUE.
        CALL M3MESG( MESG )
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Bad environment', 2 )
    END IF


    !!...............  Open input files

    N = 1
    IF ( .NOT. OPEN3( FNAME( N ), FSREAD3, PNAME ) ) THEN
        MESG = 'Could not open ' // FNAME( N )
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT. DESC3( FNAME( N ) ) ) THEN
        MESG = 'Could not get description for ' // FNAME( N )
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

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
    DO  V = 1, NVARS
        I = INDEX1( VNAME( V ), NVARS3D, VNAME3D )
        IF ( 0 .GE. I ) THEN
           MESG = 'Variable "' // TRIM( VNAME(V) ) // '" not available in file ' // FNAME(N)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( VTYPE3D( I ) .NE. M3REAL ) THEN
            MESG = 'Variable "' // TRIM( VNAME( V ) )  // '" not of type REAL'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        UNITS( V ) = UNITS3D( I )
        VDESC( V ) = VDESC3D( I )

    END DO

    DO  N = 2, NFILES       !  open/check the rest of the input files

        IF ( .NOT. OPEN3( FNAME(N), FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open ' // FNAME(N)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT. DESC3( FNAME(N) ) ) THEN
            MESG = 'Could not get description for ' // FNAME(N)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.FILCHK3( FNAME(N),  GRDDED3, NCOLS1, NROWS1, NLAYS1, NTHIK3D ) ) THEN
            MESG = 'Inconsistent dimensions  for ' // FNAME(N)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.GRDCHK3( FNAME(N),                               &
                                P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1, &
                                XORIG1, YORIG1, XCELL1, YCELL1,         &
                                NLAYS1, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
            MESG = 'Inconsistent coord/grid  for ' // FNAME(N)
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        DO  V = 1, NVARS
            IF ( 0 .GE. INDEX1( VNAME(V), NVARS3D, VNAME3D ) ) THEN
               MESG = 'Variable "' // TRIM( VNAME(V) )  // '" not available in file ' // FNAME(N)
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
        END DO

    END DO          !  end loop opening the rest of the input files


    !!...............  Open the output file

    SDATE3D = 1
    STIME3D = STTIME
    TSTEP3D = TSTEP
    NVARS3D = NVARS

    DO  V = 1, NVARS
        VNAME3D( V ) = VNAME( V )
        UNITS3D( V ) =  UNITS( V )
        VDESC3D( V ) =  VDESC( V )
        VTYPE3D( V ) =  M3REAL
    END DO

    IF ( .NOT. OPEN3( 'AGGFILE', FSUNKN3, PNAME ) ) THEN
        MESG = 'Could not open AGGFILE'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  File-related setup:

    WTFAC = 0.0             !  for normalizing the weights
    DO  N = 1, NFILES
        WTFAC = WTFAC + WEIGHTS( N )
    END DO
    WTFAC  = 1.0 / WTFAC

    WTFAC  = 1.0 / WTFAC
    JDATE = 1
    JTIME = STTIME
    DO  N = 1, NFILES
        WEIGHTS( N ) = WTFAC * WEIGHTS( N )
        JDATES( N )  = SDATES( N )
        JTIMES( N )  = STTIME
    END DO

    NSTEPS = TIME2SEC( RUNLEN ) / TIME2SEC( TSTEP )

    ALLOCATE( ABUF( NCOLS1, NROWS1, NLAYS1 ),       &
              TBUF( NCOLS1, NROWS1, NLAYS1 ), STAT = ESTAT )
    IF ( ESTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10)' )  'Buffer allocation failed:  STAT=', ESTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Process the requested time period and variables:

    DO  T = 1, NSTEPS

        VARLOOP:  DO  V = 1, NVARS

            !!  initialize with first input file:

            N = 1
            IF ( .NOT. READ3( FNAME(N), VNAME(V), ALLAYS3, JDATES(N), JTIMES(N), TBUF ) ) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'ERROR:  read-failure' )
                CYCLE VARLOOP
            END IF

!$OMP       PARALLEL DO                                             &
!$OMP&         DEFAULT( NONE ),                                     &
!$OMP&          SHARED( NLAYS1, NROWS1, NCOLS1, TBUF, WEIGHTS ),    &
!$OMP&         PRIVATE( L, R, C )

            DO  R = 1, NROWS1       !!  transpose R,L order to ensure NLAYS=1 parallelization
            DO  L = 1, NLAYS1
            DO  C = 1, NCOLS1
                TBUF( C,R,L ) = TBUF( C,R,L ) * WEIGHTS( N )
            END DO
            END DO
            END DO

            DO  N = 2, NFILES       !  rest of the input files

                IF ( .NOT. READ3( FNAME(N), VNAME(V), ALLAYS3, JDATES(N), JTIMES(N), ABUF ) ) THEN
                    MESG = 'Could not read "'//TRIM( VNAME(V) )// '" from file ' // FNAME(N)
                    CALL M3EXIT( PNAME, JDATE, JTIME, MESG, 2 )
                END IF

!$OMP           PARALLEL DO                                                 &
!$OMP&             DEFAULT( NONE ),                                         &
!$OMP&              SHARED( NLAYS1, NROWS1, NCOLS1, TBUF, ABUF, WEIGHTS ),  &
!$OMP&             PRIVATE( L, R, C )

                DO  R = 1, NROWS1
                DO  L = 1, NLAYS1
                DO  C = 1, NCOLS1
                    TBUF( C,R,L )  =  TBUF( C,R,L ) + ABUF( C,R,L ) * WEIGHTS( N )
                END DO
                END DO
                END DO

            END DO  !  end loop on the rest of the input files

            IF ( .NOT. WRITE3( 'AGGFILE', VNAME(V), JDATE, JTIME, TBUF )) THEN
                EFLAG = .TRUE.
                CALL M3MESG( 'ERROR:  write-failure' )
                CYCLE VARLOOP
            END IF

        END DO VARLOOP     !  end loop on variables

        CALL NEXTIME( JDATE, JTIME, TSTEP )
        DO  N = 1, NFILES
            CALL NEXTIME( JDATES( N ), JTIMES( N ), TSTEP )
        END DO

    END DO          !  end loop on time steps


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ESTAT = 2
    ELSE
        MESG  = 'Success in program'
        ESTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ESTAT )


END PROGRAM DAYAGG


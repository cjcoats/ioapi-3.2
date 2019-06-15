
PROGRAM  VERTOT

    !!***********************************************************************
    !! Version "$Id: vertot.f90 117 2019-06-15 14:56:29Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, (C) 1992-2002, 2017 Carlie J. Coats, Jr,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems, LLC.,
    !! and (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  107
    !!
    !!  FUNCTION:
    !!       For a user-specified GRIDDED Models-3 file and list of variables
    !!       within it, compute vertical-column totals for each specified
    !!       variable.  Optionally put the output to a user-specified 1-layer
    !!       GRIDDED output file, and  optionally write statistics to the log.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       GETYN, GETNUM, Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 3/1993 by CJC
    !!
    !!      Version   5/1995 by CJC:  command line arguments
    !!
    !!      Version   6/1996 by CJC:  special treatment for NVARS3D=1
    !!
    !!      Version   4/1998 by M Houyoux: Changed exit to use M3EXIT
    !!
    !!      Modified  9/1999 by CJC for enhanced portability
    !!
    !!      Version  11/2001 by CJC for I/O API Version 2.1
    !!
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source
    !!
    !!      Version  09/2017 by CJC for I/O API v3.2:  bug-fix in default RUNLEN;
    !!      format-cleanup
    !!
    !!      Version  09/2017 by CJC for I/O API v3.2:  expand col-row format
    !!      to I3, to handle larger grids.
    !!***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'VERTOT'
    CHARACTER*72, PARAMETER :: BAR   =  &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LUNIT   !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*256   ENVBUF  !  value from command line arguments

    LOGICAL         OUTFLAG ! flag:  generate output file
    LOGICAL         STATOUT ! flag:  generate statistics report
    LOGICAL         EFLAG   ! flag:  error has happened

    CHARACTER*16    INAME   !  logical name of the input file
    CHARACTER*16    ONAME   !  logical name of the output file
    INTEGER         RUNIT    !  report-file unit number

    INTEGER         NCOLS   ! grid dimensions, from INAME header
    INTEGER         NROWS   ! grid dimensions, from INAME header
    INTEGER         NLAYS   ! grid dimensions, from INAME header
    INTEGER         NVARS   !  number of vbles to be totaled, from user
    INTEGER         NVLOW   ! lwoer bound for getnum
    CHARACTER*16    VNAME( MXVARS3 ) !  list of vble names, from user
    CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
    CHARACTER*80    PROMPT  !  scratch buffer for prompt
    INTEGER         SDATE   ! starting date
    INTEGER         STIME   ! starting time
    INTEGER         EDATE   ! ending date
    INTEGER         ETIME   ! ending time
    INTEGER         JDATE   !  starting date, from user
    INTEGER         JTIME   !  starting time, from user
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         TSTEP   !  time step, from INAME header
    INTEGER         NRECS   !  duration in TSTEPs
    INTEGER         I, J    !  scratch variable
    INTEGER         N, V    !  scratch variable
    INTEGER         VMAX    !  string length for names
    INTEGER         UMAX    !  string length for units
    INTEGER         DMAX    !  string length for descriptions
    CHARACTER*256   MESG    !  buffer for m3exit(), etc

    !!.........................................................................
    !!   begin body of program  VERTOT

    LUNIT = INIT3()
    ARGCNT = IARGC()

    WRITE ( LUNIT,'( 5X, A )' ) ' ', BAR, ' ',                              &
'Program VERTOT to compute vertical totals of selected REAL variables',     &
'from a user-specified GRIDDED Models-3 file, with optional generation',    &
'of either a vertically-summed 2-D output file for those variables,',       &
'a statistics report for those variables, or both.',                        &
'',                                                                         &
'USAGE:  vertintegral [INFILE]',                                            &
'',                                                                         &
'PRECONDITIONS REQUIRED:',                                                  &
'    setenv <INFILE>          <path name>',                                 &
'',                                                                         &
'    all the selected variables from ${INFILE} are of type REAL',           &
'',                                                                         &
'THE PROGRAM WILL PROMPT YOU for the starting date&time, time-step,',       &
'and duration (HHMMSS)of the period to be processed, the names of the',     &
'variables to be processed, and the names of the output and statistics',    &
'files.',                                                                   &
' ',                                                                        &
'NOTE:  because it does not deal with layer-thickness, density, etc.,',     &
'this program should be considered much more as a sample program',          &
'illustrating how Models-3 I/O API based programs should be written,',      &
'rather than as a serious computational-tool program in itself.',           &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2002-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2015 UNC Institute for the Environment.',                              &
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
'$Id: vertot.f90 117 2019-06-15 14:56:29Z coats $',&
''

    IF ( ARGCNT .GT. 1 ) THEN

        CALL M3EXIT( PNAME, 0,0, 'usage:  vertot [INFILE]', 2 )

    ELSE IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE', FSREAD3, 'INFILE', PNAME )

    ELSE

        CALL GETARG( 1, ENVBUF )
        INAME = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file ' // INAME
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

    END IF

    IF ( .NOT. DESC3( INAME ) ) THEN
        MESG = 'Could not get description of input file ' // INAME
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    NCOLS = NCOLS3D
    NROWS = NROWS3D
    NLAYS = NLAYS3D
    SDATE = SDATE3D
    STIME = STIME3D
    TSTEP = TSTEP3D
    NRECS = MXREC3D
    CALL LASTTIME( SDATE, STIME, TSTEP, NRECS, EDATE, ETIME )

    !!.......   Get max string-lengths for use in variables-listing:

    VMAX = 0
    UMAX = 0
    DMAX = 0
    DO  I = 1, NVARS3D
        VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
        UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
        DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
    END DO

    CALL M3MESG( '' )
    CALL M3MESG( 'The list of variables in this file is:' )
    WRITE( *,'( I8, ":  ", A )' )                           &
        ( I, VNAME3D( I )( 1:VMAX ) // ' ('  //             &
             UNITS3D( I )( 1:UMAX ) // '): ' //             &
             VDESC3D( I )( 1:DMAX ), I = 1, NVARS3D )

    IF ( NVARS3D .EQ. 1 ) THEN

        NVARS = 1
        VNAME( NVARS ) = VNAME3D( 1 )
        UNITS( NVARS ) = UNITS3D( 1 )
        VDESC( NVARS ) = VDESC3D( 1 )

        IF ( VTYPE3D( 1 ) .NE. M3REAL ) THEN
            MESG = 'Variable "' // TRIM( VNAME3D( 1 ) ) //  &
                   '" not of type REAL; VERTOT processes REAL only'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

    ELSE    !  else nvars3d > 1:

        NVARS  =  0
        I      =  0
        NVLOW  = -1
        PROMPT = 'Enter # for vble (0 to quit, -1 for ALL VBLES)'
        EFLAG  = .FALSE.

111     CONTINUE        !  loop getting variables-list for analysis

            I = GETNUM( NVLOW, NVARS3D, 1 + MOD( I, NVARS3D ), PROMPT )

            IF ( I .EQ. -1 ) THEN
                NVARS = NVARS3D
                DO  V = 1, NVARS3D
                    VNAME( V ) = VNAME3D( V )
                    UNITS( V ) = UNITS3D( V )
                    VDESC( V ) = VDESC3D( V )
                    EFLAG = EFLAG .OR. ( VTYPE3D( V ) .NE. M3REAL )
                END DO
            ELSE IF ( I .GT. 0 )THEN
                PROMPT = 'Enter # for vble (0 to quit)'
                NVLOW  = 0
                IF ( VTYPE3D( I ) .NE. M3REAL ) THEN
                    MESG = 'Variable "' //TRIM( VNAME3D( 1 ) ) //   &
                           '" not of type REAL; please try again'
                    CALL M3MSG2( MESG )
                    GO TO  111
                END IF
                NVARS  = NVARS + 1
                VNAME( NVARS ) = VNAME3D( I )
                UNITS( NVARS ) = UNITS3D( I )
                VDESC( NVARS ) = VDESC3D( I )
                IF ( NVARS .LT. MXVARS3 )  GO TO  111
            END IF

        IF ( NVARS .EQ. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'No variables selected', 2 )
        ELSE IF ( EFLAG ) THEN
            MESG = 'File has INTEGER or DOUBLE variables; VERTOT processes REAL only'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF  !  if nvars=0, or not

    END IF      !  if nvars3d=1, or not


    !!.......   Get mode of operation:

    OUTFLAG = GETYN( 'Do you want an output totals data file?', .TRUE. )
    STATOUT = GETYN( 'Do you want an output  statistics file?', .TRUE. )

    IF ( TSTEP .EQ. 0 ) THEN
        SDATE  = 0
        STIME  = 0
        NRECS = 1
    ELSE
        SDATE  = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting date (YYYYDDD) for run' )
        STIME  = GETNUM(       0,  239999, STIME3D, 'Enter starting time (HHMMSS) for run' )
        N      = CURREC( EDATE, ETIME, SDATE, STIME, TSTEP3D, I, J ) - 1
        RUNLEN = SEC2TIME( N * TIME2SEC( TSTEP3D ) )
        RUNLEN = GETNUM( 0, 999999999, RUNLEN, 'Enter duration (HHMMSS) for run' )
        JDATE  = SDATE
        JTIME  = STIME
        CALL NEXTIME( JDATE, JTIME, RUNLEN )
        NRECS  = CURREC( JDATE, JTIME, SDATE, STIME, TSTEP, EDATE, ETIME )
    END IF          !  time-independent file, or not

    IF ( OUTFLAG ) THEN     !  create output file

        SDATE3D = SDATE
        STIME3D = STIME
        NLAYS3D = 1
        NVARS3D = NVARS
        DO I = 1, NVARS
            VNAME3D( I ) = VNAME( I )
            UNITS3D( I ) = UNITS( I )
            VDESC3D( I ) = VDESC( I )
            VTYPE3D( I ) = M3REAL
        END DO

        ONAME = PROMPTMFILE( 'Enter logical name for OUTPUT FILE', FSUNKN3, 'OUTFILE', PNAME )

    END IF      !  if OUTFLAG

    IF( STATOUT ) THEN

        PROMPT = 'Enter the REPORT FILE logical name (or "NONE" for screen output)'
        RUNIT = PROMPTFFILE( PROMPT, .FALSE., .TRUE., 'REPORT', PNAME )

    ELSE

        RUNIT = -1

    END IF      !  if statout or not

    !!.......   Process this period in the input file:

    IF ( RUNIT .LT. 0 ) RUNIT = LUNIT
    JDATE = SDATE
    JTIME = STIME
    DO  I = 1, NRECS

        CALL VERSTEP( NCOLS, NROWS, NLAYS, NVARS,   &
                      JDATE, JTIME,                 &
                      INAME, VNAME,                 &
                      OUTFLAG, RUNIT, ONAME )

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO         !  end loop on time steps

    CALL M3EXIT( PNAME, 0, 0, 'Program VERTOT completed successfully', 0 )


CONTAINS    !!==========================================================================


    SUBROUTINE  VERSTEP ( NCOLS, NROWS, NLAYS, NVARS,   &
                          JDATE, JTIME,                 &
                          INNAME, VNAME,                &
                          OUTFLAG, RUNIT, OUTNAME )

        IMPLICIT NONE

        !!...........   ARGUMENTS and their descriptions:

        INTEGER     , INTENT(IN   ) :: NCOLS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NROWS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NLAYS   ! grid dimensions, from INNAME header
        INTEGER     , INTENT(IN   ) :: NVARS   ! number of vbles to be totaled
        INTEGER     , INTENT(IN   ) :: JDATE   ! current model date
        INTEGER     , INTENT(IN   ) :: JTIME   ! current model time
        LOGICAL     , INTENT(IN   ) :: OUTFLAG ! flag:  generate output file
        INTEGER     , INTENT(IN   ) :: RUNIT   ! unit number for report-file (or -1: no report)
        CHARACTER*16, INTENT(IN   ) :: VNAME( MXVARS3 ) !  list of vble names
        CHARACTER*16, INTENT(IN   ) :: INNAME  !  logical name of the input file
        CHARACTER*16, INTENT(IN   ) :: OUTNAME !  logical name of the output file


        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         C, R, L, V      !  col, row, level, variable, counters
        INTEGER         MC, MR, ML      !  indexes for maximum
        INTEGER         NC, NR, NL      !  indexes for minimum
        REAL            TOTS( NCOLS, NROWS )
        REAL            GRID( NCOLS, NROWS, NLAYS )
        REAL            T
        REAL            AMAX
        REAL            AMIN
        REAL            ASUM
        REAL            ASSQ
        REAL            DNOM
				
        CHARACTER*120   MESG

        !!***********************************************************************
        !!   begin body of subroutine  VERSTEP

        IF ( RUNIT .GE. 0 ) THEN
            WRITE( RUNIT,92010 ) INNAME, JDATE, JTIME, DT2STR( JDATE, JTIME )
        END IF

        DO  288  V = 1, NVARS

            IF ( .NOT. READ3( INNAME, VNAME( V ), ALLAYS3, JDATE, JTIME, GRID ) ) THEN
                MESG = 'Read failure:  file ' // INNAME // ' variable ' // VNAME( V )
                CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME, MESG, 2 )
                RETURN
            END IF      !  if read3() failed

!$OMP       PARALLEL DO                                         &
!$OMP&          DEFAULT( NONE ),                                &
!$OMP&           SHARED( NCOLS, NROWS, NLAYS, TOTS, GRID ),     &
!$OMP&          PRIVATE( C, R, L )

            DO   R = 1, NROWS       !  initialization:  layer 1

                DO   C = 1, NCOLS
                    TOTS( C,R ) = GRID( C,R,1 )
                END DO

                DO   L = 2, NLAYS       !  all other layers
                DO   C = 1, NCOLS
                    TOTS( C,R ) = TOTS( C,R ) + GRID( C,R,L )
                END DO
                END DO

            END DO

            IF ( RUNIT .GE. 0 ) THEN    !!  Serial code (because of MC, etc...)  !!

                !!...........   Initialization for 3-D GRID stats:

                MC   = 1
                MR   = 1
                ML   = 1
                NC   = 1
                NR   = 1
                NL   = 1
                T    = GRID( 1,1,1 )
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0

                DO  R = 1, NROWS   !  gridded initialization:  layer 1
                DO  C = 1, NCOLS
                    T = GRID( C,R,1 )
                    ASUM = ASUM + T
                    ASSQ = ASSQ + T*T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = C
                        MR   = R
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = C
                        NR   = R
                    END IF
                END DO
                END DO

                DO  L = 2, NLAYS   !  3-D traversal:  all other layers
                DO  R = 1, NROWS
                DO  C = 1, NCOLS
                    T    = GRID( C,R,L )
                    ASUM = ASUM + T
                    ASSQ = ASSQ + T*T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = C
                        MR   = R
                        ML   = L
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = C
                        NR   = R
                        NL   = L
                    END IF
                END DO
                END DO
                END DO

                DNOM = 1.0 / FLOAT( NCOLS * NROWS * NLAYS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0 ) )
                WRITE( RUNIT,92020 )                                    &
                    VNAME( V ) // ' 3-D grid statistics' ,              &
                    'Max   ', AMAX, ' @(c,r,l)=(', MC, MR, ML, ')',     &
                    'Min   ', AMIN, ' @(c,r,l)=(', NC, NR, NL, ')',     &
                    'Mean  ', ASUM,                                     &
                    'Sigma ', ASSQ

                !!...........   Initialization for 2-D TOTS stats:

                MC   = 1
                MR   = 1
                NC   = 1
                NR   = 1
                T    = TOTS( 1,1 )
                AMAX = T
                AMIN = T
                ASUM = 0.0
                ASSQ = 0.0

                DO  R = 1, NROWS       ! traverse 2-D TOTS
                DO  C = 1, NCOLS
                    T    = TOTS( C,R )
                    ASUM = ASUM + T
                    ASSQ = ASSQ + T*T
                    IF ( T .GT. AMAX ) THEN
                        AMAX = T
                        MC   = C
                        MR   = R
                    ELSE IF ( T .LT. AMIN ) THEN
                        AMIN = T
                        NC   = C
                        NR   = R
                    END IF
                END DO
                END DO

                DNOM = 1.0 / FLOAT( NCOLS * NROWS )
                ASUM = DNOM * ASUM
                ASSQ = SQRT( MAX( ASSQ * DNOM - ASUM * ASUM , 0.0 ) )
                WRITE( RUNIT,92030 )                                    &
                    VNAME( V ) // ' 2-D vertical-total statistics' ,    &
                    'Max   ', AMAX, ' @(c,r)=(', MC, MR, ')',           &
                    'Min   ', AMIN, ' @(c,r)=(', NC, NR, ')',           &
                    'Mean  ', ASUM,                                     &
                    'Sigma ', ASSQ

            END IF      !  if runit > 0

            IF ( OUTFLAG ) THEN
                IF ( .NOT. WRITE3( OUTNAME, VNAME( V ), JDATE, JTIME, TOTS ) ) THEN
                    MESG = 'Could not write totals to ' // OUTNAME
                    CALL M3EXIT( 'VERTOT:VERSTEP', JDATE, JTIME, MESG, 2 )
                END IF      !  if write3() failed
            END IF          !  if file output desired

288     CONTINUE        !  end loop on variables V

        RETURN

        !!******************  FORMAT  STATEMENTS   ******************************

        !!...........   Informational (LOG) message formats... 92xxx

92010   FORMAT ( //5X , 'File:  ', A, 5X, 'Date and time:', I7.7, ':', I6.6, A )

92020   FORMAT ( /5X , 'Variable ', A16,                            &
                 2( /5X, A, 1PE12.5, A, I3, ',', I3, ',', I3, A ),  &
                 2( /5X, A, 1PE12.5 ) )

92030   FORMAT ( 5X , 'Variable ', A16,                             &
                 2( /5X, A, 1PE12.5, A, I3, ',', I3, A ),           &
                 2( /5X, A, 1PE12.5 ) )

    END SUBROUTINE VERSTEP


END PROGRAM VERTOT


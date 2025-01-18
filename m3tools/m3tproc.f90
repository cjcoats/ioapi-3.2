
PROGRAM  M3TPROC

    !!***********************************************************************
    !! Version "$Id: m3tproc.f90 206 2021-10-21 15:46:35Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2014-2016 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line 156
    !!
    !!  FUNCTION:
    !!      Sums, give max, or gives average over a specified time period
    !!      for a subset of variables from the input file, and writes the
    !!      processed data to the output file.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!      Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!      consistency with I/O API PARMS3.EXT for name and description lengths.
    !!      File type is CUSTOM, GRIDDED, or BOUNDARY.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!      TIMEAGGS, Models-3 I/O API.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 5/1997 by M Houyoux
    !!
    !!      Version 11/2001 by Carlie J. Coats, Jr., for I/O API Version 2.1
    !!
    !!      Version 11/2002 by Carlie J. Coats, Jr., for I/O API Version 2.2:
    !!      F90 only.
    !!      Does not presume all variables are of type M3REAL.
    !!      Now uses worker routine AGGVAR() instead of multiple entries
    !!      to routine TAGGREG().
    !!
    !!      Version 11/2004 by Carlie J. Coats, Jr., for I/O API Version 3.0:
    !!      now also supports MIN() operation, per-variable selection of
    !!      operation; partial de-Houyouxization
    !!
    !!      Version 12/2004 with changes by Dr. Michael Bane, U.Manchester, UK:
    !!      Fixups to make the Intel v8.1 compiler happy (remove duplicate
    !!      declaration of ARECS; change back from F-90 style to F-77-style
    !!      declarations for MENUITMS and OPNAMES.
    !!
    !!      Version 6/2005 by CJC:  improved/bug-fixed default for NRECS
    !!
    !!      Version  11/2005 by CJC:  eliminate unused vbles
    !!
    !!      Version  7/2006 by CJC:  correct fencepost problem with NRECS
    !!
    !!      Version  2/2007:  Bug-fix for All-Variables case. from
    !!      George Pouliot, US EPA
    !!
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!
    !!      Version 01/2013 by CJC:  use new LASTTIME() to find EDATE:ETIME
    !!
    !!      Version 10/2014 by CJC:  Check status of ENV*() calls.
    !!      PARAMETER menu-arguments.
    !!
    !!      Version  02/2015 by CJC for I/O API v3.2:  F90 free-format source;
    !!      AGGVAR ~~> TIMEAGG;  now CONTAINs  SUBROUTINE TIMEAGG.
    !!      Corrected/improved error-status behavior. Support for M3INT8 variables.
    !!
    !!      Version  06/2016 by CJC:  copy CMAQ metadata, if present
    !!
    !!      Version  03/2020 by CJC:  correct fencepost problem in default EDATE:ETIME
    !!
    !!      Version  06/2021 by CJC:  correct ARECS bug.
    !!***********************************************************************

    USE M3UTILIO
    USE MODATTS3

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    INTEGER, PARAMETER ::  M3SUM = 1
    INTEGER, PARAMETER ::  M3AVG = 2
    INTEGER, PARAMETER ::  M3MAX = 3
    INTEGER, PARAMETER ::  M3MIN = 4

    CHARACTER*48, PARAMETER ::  MENUITMS( M3MIN ) =     &
        (/  'Calculate   sum   over time window',       &
            'Calculate average over time window',       &
            'Determine maximum over time window',       &
            'Determine minimum over time window'   /)

    CHARACTER*8, PARAMETER ::  OPNAMES( M3MIN ) = (/  'SUM', 'BAR', 'MAX', 'MIN'  /)

    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*16, PARAMETER :: PNAME = 'M3TPROC'
    CHARACTER*72, PARAMETER :: BAR   =    &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!...........   LOCAL VARIABLES and their descriptions:

    CHARACTER*16    ANAME   !  scratch buffer for variable names
    CHARACTER*16    IFILE   !  logical name of the  input file
    CHARACTER*16    OFILE   !  logical name of the output file
    CHARACTER*16    INAME( MXVARS3 ) !  list of  input vble names, from user
    CHARACTER*16    ONAME( MXVARS3 ) !  list of output vble names, from user
    CHARACTER*16    UNITS( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESC( MXVARS3 ) !  list of vble descs
    INTEGER         VTYPE( MXVARS3 ) !  list of vble types
    INTEGER         AGGOP( MXVARS3 ) !  aggregation-operation
    CHARACTER*80    ALINE   !  scratch buffer for prompt
    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

    INTEGER         I, N, V !  loop counters (time step #)

    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    INTEGER         EDATE   !  final date
    INTEGER         ETIME   !  final time
    INTEGER         AGLEN   !  aggregation-length (hhmmss)
    INTEGER         INSTEP  !  aggregation-length (hhmmss)
    INTEGER         DMAX    !  string length for descriptions
    INTEGER         IOS     !  I/O status
    INTEGER         JDATE   !  current  input date
    INTEGER         JTIME   !  current  input time
    INTEGER         KDATE   !  scratch  date
    INTEGER         KTIME   !  scratch  time
    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         NVSAV   !  number of variables in input file
    INTEGER         NVARS   !  number of vbles in OFILE
    INTEGER         VSIZE   !  volume of one variable
    INTEGER         SDATE   !  starting  input date, from user
    INTEGER         STIME   !  starting  input time, from user
    INTEGER         NRECS   !  duration, output time steps
    INTEGER         TSTEP   !  input time step, from IFILE header
    INTEGER         TSECS   !  tstep in seconds
    INTEGER         OSTEP   !  output time step, from IFILE header
    INTEGER         UMAX    !  string length for units
    INTEGER         ARECS  !  aggregation-window duration in TSTEPs
    INTEGER         VMAX    !  string length for names
    INTEGER         ITYPE, ISTAT

    LOGICAL         EFLAG, CFLAG
    LOGICAL         NPFLAG  !  iff no prompting for variables

    !!.........................................................................
    !!   begin body of program  M3TPROC

    LOGDEV = INIT3()
    EFLAG  = .FALSE.
    WRITE ( *, '( 5X,  A )' ) BLANK, BAR, BLANK,                            &
'Program M3TPROC to sum, average, or find the maximum values over a',       &
'repeating time period from a selected time window.',                       &
'',                                                                         &
'NOTE that the time period includes both ends:  for example, for a',        &
'file with a 1-hour time-step, a 24-hour aggregation period starting at',   &
'2019001:000000 ends at 2010002:000000, having 24 "fence-rails" and',       &
'25 "fence-posts".',                                                        &
'',                                                                         &
'The time period and starting time window set the start and duration',      &
'of all subsequent time windows. The program inputs and outputs',           &
'Models-3 files.',                                                          &
'',                                                                         &
'You need to have set environment variables for the input and output',      &
'file logical names.  You will be asked to select the time period to be',   &
'copied and the start of the time period to receive the results.',          &
'',                                                                         &
'USAGE:  m3tproc [INFILE OUTFILE]     (and then answer the prompts).',      &
'',                                                                         &
'See URL',                                                                  &
'',                                                                         &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools', &
'',                                                                         &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013 Carlie J. Coats, Jr.', &
'(C) 2003-2010 Baron Advanced Meteorological Systems, LLC., and',           &
'(C) 2014-2016 UNC Institute for the Environment.',                         &
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
'$Id: m3tproc.f90 206 2021-10-21 15:46:35Z coats $',&
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 2 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'usage:  m3tproc [INFILE OUTFILE]', 2 )
    END IF

    IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        IFILE = PROMPTMFILE( 'Enter INPUT FILE name', FSREAD3, 'INFILE', PNAME )

    ELSE        !  argcnt 2

        CALL GETARG( 1, ENVBUF )
        IFILE = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( IFILE, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file "' // TRIM( IFILE ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        CALL GETARG( 2, ENVBUF )
        OFILE = ENVBUF( 1:16 )

    END IF


    IF ( .NOT. DESC3( IFILE ) ) THEN
        MESG = 'Could not get description of input file "' // TRIM( IFILE ) // '"'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( TSTEP3D .EQ. 0 ) THEN
        MESG = 'Input file "' // TRIM( IFILE ) // '" is time-independent: no output written.'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    IF ( ISCMAQ( IFILE ) ) THEN
        CFLAG = ENVYN( 'COPY_META', 'Copy CMAQ metadata to output file?', .TRUE., ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "COPY_META"', 2 )
        ELSE IF ( .NOT.CFLAG ) THEN
            CONTINUE
        ELSE IF ( .NOT.GETCMAQ( IFILE ) ) THEN
            CALL M3EXIT( PNAME, 0, 0, 'Could not get CMAQ metadata for ' // IFILE, 2 )
        END IF
    END IF

    IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
        VSIZE = NCOLS3D * NLAYS3D
    ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
        VSIZE = NCOLS3D * NROWS3D * NLAYS3D
    ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
        VSIZE = 2 * NTHIK3D * NLAYS3D * ( NCOLS3D + NROWS3D + 2 )
    ELSE
        WRITE( MESG, '( 3A, I5 )' )     &
            'Input file "', TRIM( IFILE ), '" has unsupported type', FTYPE3D
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    NVSAV  = NVARS3D
    SDATE  = SDATE3D
    STIME  = STIME3D
    INSTEP = TSTEP3D

    CALL LASTTIME( SDATE3D,STIME3D,TSTEP3D, MXREC3D, EDATE,ETIME )

    !!.......   Get max string-lengths for use in variables-listing:

    VMAX = LEN_TRIM( VNAME3D( 1 ) )
    UMAX = LEN_TRIM( UNITS3D( 1 ) )
    DMAX = LEN_TRIM( VDESC3D( 1 ) )
    DO  V = 1, NVARS3D
        VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( V ) ) )
        UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( V ) ) )
        DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( V ) ) )
    END DO


    !!.......  Determine if all variables are to be used

    NPFLAG = ENVYN( 'M3TPROC_ALLV', 'Process all variables?', .FALSE., IOS )
    IF ( IOS .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "M3TPROC_ALLV"' )
    END IF

    CALL M3MESG( BLANK )
    CALL M3MESG( 'The list of available analysis-types is' )
    WRITE( *,'( 1X, I5,  A )' ) ( I, ':  ' // MENUITMS( I ), I = 1, M3MIN )
    CALL M3MESG( BLANK )
    AGGOP( 1 ) = ENVINT( 'M3TPROC_TYPE', 'Type of analysis', M3MAX, IOS )
    IF ( IOS .GT. 0 ) THEN
        EFLAG = .TRUE.
        CALL M3MESG( 'Bad environment variable "M3TPROC_TYPE"' )
    END IF

    IF ( EFLAG ) THEN
        CALL M3EXIT( PNAME, 0,0, 'Environment error(s)', 2 )
    END IF

    IF( NPFLAG ) THEN

        NVARS   = NVARS3D
        DO  V = 1, NVARS3D
           INAME( V )  = VNAME3D( V )
           ONAME( V )  = VNAME3D( V )
           UNITS( V )  = UNITS3D( V )
           VDESC( V )  = VDESC3D( V )
           VTYPE( V )  = VTYPE3D( V )
           AGGOP( V )  = AGGOP( 1 )
        END DO

    ELSE

        V = 0

        DO N = 1, MXVARS3        !  loop getting variables-list for extraction

            IF( MOD( N,10 ) .EQ. 0 ) THEN
                MESG = 'The list of variables in file "' // TRIM( IFILE ) // '" is:'
                CALL M3MESG( BLANK )
                CALL M3MESG( MESG  )
                WRITE( *,'( 1X, I5, A )' )                      &
                    ( I, ':  ' //                               &
                      VNAME3D( I )( 1:VMAX ) // ' (' //         &
                      UNITS3D( I )( 1:UMAX ) // '): ' //        &
                      VDESC3D( I )( 1:DMAX ), I = 1, NVSAV  )
                CALL M3MESG( BLANK )
            END IF

            CALL M3MSG2( BLANK )
            MESG = 'Enter number for variable to extract (0 to end vbles)'
            V = GETNUM( 0, NVSAV, 1 + MOD( V, NVSAV ), MESG )

            IF ( V .EQ. 0 ) EXIT      !  to end of loop

            !!...............  Type of analysis to perform

            AGGOP( N ) = GETMENU( M3MIN, AGGOP( 1 ), 'Enter type of operation to perform', MENUITMS )

            !!...............   Optional renaming of this variable:

122         CONTINUE

                ANAME = TRIM( VNAME3D( V ) )//OPNAMES( AGGOP( N ) )
                ALINE = 'Enter output-name for this variable [' // TRIM( ANAME ) // '] >> '
                CALL M3PROMPT( ALINE, ANAME, IOS )

                IF ( IOS .GT. 0 ) THEN
                    CALL M3WARN( PNAME, 0, 0, 'Error reading output-name; please try again' )
                    GO TO 122
                END IF

            IF( ANAME .EQ. BLANK ) THEN
                ONAME( N ) = VNAME3D( V )
            ELSE
                ONAME( N ) = ANAME
            END IF
            INAME( N ) = VNAME3D( V )
            UNITS( N ) = UNITS3D( V )
            VDESC( N ) = VDESC3D( V )
            VTYPE( N ) = VTYPE3D( V )

            NVARS   = N

       END DO   !  to head of loop

199     CONTINUE        !  end loop getting variables-list for analysis

    END IF  ! If prompting or not

    IF ( NVARS .EQ. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'No variables selected', 2 )
    ELSE
        NVARS3D = NVARS
    END IF

    !!.......   Get starting date and time, and duration:

    CALL M3MSG2( BAR )
    WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'File has start date&time', SDATE3D, ':', STIME3D
    CALL M3MSG2( MESG )
    WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'File has final date&time', EDATE, ':', ETIME
    CALL M3MSG2( MESG )
    WRITE( MESG, '( A, I8.6 )' )          'File has       time step', TSTEP3D
    CALL M3MSG2( MESG )

    SDATE = GETNUM( SDATE3D, 9999999, SDATE, 'Enter start date for analysis' )

    STIME = GETNUM(     0, 999999999, STIME, 'Enter start time for analysis' )

    AGLEN = GETNUM(     0, 999999999, 240000, 'Enter analysis-window duration' )

    OSTEP = GETNUM(     0, 999999999,  AGLEN, 'Enter output time step' )

    CALL NEXTIME( EDATE, ETIME, -AGLEN )
    NRECS = CURREC( EDATE, ETIME, SDATE, STIME, OSTEP, KDATE, KTIME )

    EDATE = GETNUM( SDATE3D, 9999999, KDATE, 'Enter final date for analysis' )

    ETIME = GETNUM(       0, 9999999, KTIME, 'Enter final time for analysis' )

    NRECS = CURREC( EDATE, ETIME, SDATE, STIME, OSTEP, KDATE, KTIME )
    ARECS = TIME2SEC( AGLEN ) / TIME2SEC( INSTEP )


    !!.......   Build description for the output file, and create accordingly:
    !!.......   Re-use all but the starting date&time of the input-file description.

    SDATE3D = SDATE
    STIME3D = STIME
    TSTEP3D = OSTEP

    DO  V = 1, NVARS

        VNAME3D( V ) = ONAME( V )
        UNITS3D( V ) = UNITS( V )
        VDESC3D( V ) = VDESC( V )
        VTYPE3D( V ) = VTYPE( V )

    END DO

    IF ( ARGCNT .EQ. 0 ) THEN
        OFILE = PROMPTMFILE( 'Enter OUTPUT FILE name', FSUNKN3, 'OUTFILE', PNAME )
    ELSE    !  argcnt = 2:
        IF ( .NOT. OPEN3( OFILE, FSUNKN3, PNAME ) ) THEN
            MESG = 'Could not open output file "' // TRIM( OFILE ) // '"'
            CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
        END IF
    END IF      !  if argcnt zero, or 2


    !!.......   Process this period in the input file:

    JDATE = SDATE
    JTIME = STIME
    WRITE( MESG, '( A, I5, 2X, A, I9.7, A,I6.6 )' )         &
          'Processing', NRECS, 'output steps starting', JDATE, ':', JTIME
    CALL M3MSG2( BAR )
    CALL M3MSG2( MESG )
    WRITE( MESG, '( A, 2X, I7.6, 2X, A, I4, 2X, A  )' )     &
          'Aggregation period', AGLEN, ' (', ARECS, 'input time steps)'
    CALL M3MSG2( MESG )
    CALL M3MSG2( BAR )

    DO  I = 1, NRECS

        DO  V = 1, NVARS

            ITYPE = AGGOP(V)
            CALL TIMEAGG( IFILE, INAME(V), VTYPE(V), VSIZE, ITYPE,   &
                          JDATE, JTIME, INSTEP, ARECS,              &
                          OFILE, ONAME(V) )

        END DO        !  end loop on variables

        CALL NEXTIME( JDATE, JTIME, OSTEP )

    END DO            !  end loop on analysis periods


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE TIMEAGG( IFILE, INAME, VTYPE, VSIZE, ATYPE,           &
                        JDATE, JTIME, TSTEP, ARECS, OFILE, ONAME )

        !!...........   ARGUMENTS and their descriptions:

        CHARACTER*(*),INTENT( IN )::  IFILE   ! input file name
        CHARACTER*(*),INTENT( IN )::  INAME   ! input vble name
        INTEGER,      INTENT( IN )::  VTYPE   ! type M3INT|M3REAL|M3DBLE
        INTEGER,      INTENT( IN )::  VSIZE   ! total array dimensions
        INTEGER,      INTENT( IN )::  JDATE   ! current model date
        INTEGER,      INTENT( IN )::  JTIME   ! current model time
        INTEGER,      INTENT( IN )::  ATYPE   ! type of aggregation to perform
        INTEGER,      INTENT( IN )::  TSTEP   ! input-file time step
        INTEGER,      INTENT( IN )::  ARECS  ! number of timesteps to aggregate
        CHARACTER*(*),INTENT( IN )::  OFILE   ! output file name
        CHARACTER*(*),INTENT( IN )::  ONAME   ! output vble name

        CHARACTER(LEN= 32), PARAMETER ::  ANAME = 'M3TPROC/TIMEAGGS'

        !!...........   LOCAL VARIABLES and their descriptions:

        INTEGER         I, N
        INTEGER         ISTEP
        INTEGER         IDATE, ITIME
        REAL            RDIV
        REAL*8          DDIV

        CHARACTER(LEN=256)::    MESG

        INTEGER         IGRD ( VSIZE )
        INTEGER         ISCR ( VSIZE )
        REAL            RGRD ( VSIZE )
        REAL            RSCR ( VSIZE )
        REAL*8          DGRD ( VSIZE )
        REAL*8          DSCR ( VSIZE )
        INTEGER*8       LGRD ( VSIZE )
        INTEGER*8       LSCR ( VSIZE )


        !!***********************************************************************
        !!   begin body of VINITAGG entry:

        IDATE  = JDATE
        ITIME  = JTIME
        N = 0

        IF ( VTYPE .EQ. M3INT ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    IGRD( I ) = -1999999999
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    IGRD( I ) = 1999999999
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' ) 'Aggregation type', ATYPE, 'not supported for INT'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            DO ISTEP = 1, ARECS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3, IDATE, ITIME, ISCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) // '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                    EFLAG = .TRUE.
                    RETURN

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD, ISCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        IGRD( I ) = MAX( IGRD( I ), ISCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, IGRD, ISCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        IGRD( I ) = MIN( IGRD( I ), ISCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=max or min

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, IGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) // '" and variable "'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

        ELSE IF ( VTYPE .EQ. M3INT8 ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    LGRD( I ) = -1999999999
                END DO
            ELSE IF( ATYPE .EQ. M3MIN ) THEN
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    LGRD( I ) = 1999999999
                END DO
            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' ) 'Aggregation type', ATYPE, 'not supported for INT'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            DO ISTEP = 1, ARECS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3, IDATE, ITIME, LSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) // '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                    EFLAG = .TRUE.
                    RETURN

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD, LSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        LGRD( I ) = MAX( LGRD( I ), LSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, LGRD, LSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        LGRD( I ) = MIN( LGRD( I ), LSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=max or min

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, LGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) // '" and variable "'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

        ELSE IF ( VTYPE .EQ. M3REAL ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    RGRD( I ) = BADVAL3         !  -9.999E36 is VERY < 0
                END DO

            ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    RGRD( I ) = -BADVAL3         !  9.999E36 is HUGE
                END DO

            ELSE IF( ATYPE .EQ. M3SUM .OR.      &
                     ATYPE .EQ. M3AVG ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD ), PRIVATE( I )
                 DO I = 1, VSIZE
                    RGRD( I ) = 0.0
                END DO

            ELSE

                WRITE( MESG, '( A, I12, 2X, A )' ) 'Aggregation type', ATYPE, 'not supported'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN

            END IF

            DO ISTEP = 1, ARECS

                N = N + 1

                IF ( .NOT. READ3( IFILE, INAME, ALLAYS3, IDATE, ITIME, RSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) // '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                    EFLAG = .TRUE.
                    RETURN

                ELSE IF( ATYPE .EQ. M3SUM .OR.      &
                         ATYPE .EQ. M3AVG ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD, RSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = RGRD( I ) + RSCR( I )
                    END DO

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD, RSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = MAX( RGRD( I ), RSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD, RSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        RGRD( I ) = MIN( RGRD( I ), RSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or atype=..., or not

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            IF( ATYPE .EQ. M3AVG ) THEN

                RDIV = 1.0 /FLOAT( N )
!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, RGRD, RDIV ), PRIVATE( I )
                DO  I = 1, VSIZE
                    RGRD( I ) = RGRD( I ) * RDIV
                END DO

            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, RGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) // '" variable "' // TRIM( ONAME ) // '"'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

        ELSE IF ( VTYPE .EQ. M3DBLE ) THEN

            IF( ATYPE .EQ. M3MAX ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = BADVAL3
                END DO

            ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = -BADVAL3
                END DO

            ELSE IF( ATYPE .EQ. M3SUM .OR.          &
                     ATYPE .EQ. M3AVG ) THEN

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD ), PRIVATE( I )
                DO I = 1, VSIZE
                    DGRD( I ) = 0.0
                END DO

            ELSE
                WRITE( MESG, '( A, I12, 2X, A )' ) 'Aggregation type', ATYPE, 'not supported'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF


            DO ISTEP = 1, ARECS

                N = N + 1

                IF ( .NOT.READ3( IFILE, INAME, ALLAYS3, IDATE, ITIME, DSCR ) ) THEN

                    MESG = 'Read failure:  file "' // TRIM( IFILE ) // '" variable "' // TRIM( INAME ) // '"'
                    CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                    EFLAG = .TRUE.
                    RETURN

                ELSE IF( ATYPE .EQ. M3SUM .OR.      &
                         ATYPE .EQ. M3AVG ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD, DSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        DGRD( I ) = DGRD( I ) + DSCR( I )
                    END DO

                ELSE IF( ATYPE .EQ. M3MAX ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD, DSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        DGRD( I ) = MAX( DGRD( I ), DSCR( I ) )
                    END DO

                ELSE IF( ATYPE .EQ. M3MIN ) THEN

!$OMP               PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD, DSCR ), PRIVATE( I )
                    DO I = 1, VSIZE
                        DGRD( I ) = MIN( DGRD( I ), DSCR( I ) )
                    END DO

                END IF              !  if read3() worked, or not

                CALL NEXTIME( IDATE, ITIME, TSTEP )

            END DO              !  end loop on time steps

            IF ( N .EQ. 0 ) THEN
                WRITE( MESG, '( A, I9.7, A, I6.6 )' ) 'No data starting at', JDATE, ':', JTIME
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

            IF( ATYPE .EQ. M3AVG ) THEN

                DDIV = 1.0D0 / DBLE( N )

!$OMP           PARALLEL DO DEFAULT( NONE ), SHARED( VSIZE, DGRD, DDIV ), PRIVATE( I )
                DO  I = 1, VSIZE
                    DGRD( I ) = DGRD( I ) * DDIV
                END DO

            END IF

            IF ( .NOT.WRITE3( OFILE, ONAME, JDATE, JTIME, DGRD ) ) THEN
                MESG = 'Write failure:  file "' // TRIM( OFILE ) // '" variable "' // TRIM( ONAME ) // '"'
                CALL M3WARN( ANAME, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                RETURN
            END IF

        ELSE            !  vtype not m3int,m3real,m3dble

            WRITE( MESG, '( A, I10, 2X, 2A )' ) 'Unknown type', VTYPE, 'for variable', INAME
            CALL M3WARN( ANAME, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            RETURN

        END IF          !  if vtype = m3int,m3real,m3dble, or not

        RETURN

    END  SUBROUTINE TIMEAGG


END PROGRAM  M3TPROC


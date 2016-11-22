
        PROGRAM  M3AGMASK

C***********************************************************************
C Version "$Id: m3agmask.f 435 2016-11-22 18:10:58Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line 13
C
C  FUNCTION:
C       Sums, give max, or gives average over a specified time period
C       for a subset of variables from the input file, and writes the
C       processed data to the output file.
C       Computes statistics for a user-selected set of mask regions
C       (defined by GRID==1 in a set of user-input mask files).
C       Logs sorted list of max values, their locations, and times.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C       consistency with FORIO:PARMS3.EXT for name and description lengths.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       GETNUM, NEXTIME, Models-3 I/O.
C
C  REVISION  HISTORY:
C       Prototype 5/1997 by M Houyoux
C
C       Modified 11/1999 by Carlie J. Coats, Jr.:  max-value report
C
C       Version  11/2001 by CJC for I/O API Version 2.1
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 10/2014 by CJC:  Check status of ENV*() calls.
C***********************************************************************

      USE M3UTILIO
      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME    = 'M3AGMASK'
        CHARACTER*16, PARAMETER :: BLANK16  = ' '
        INTEGER,      PARAMETER :: M3SUM    =    1
        INTEGER,      PARAMETER :: M3AVE    =    2
        INTEGER,      PARAMETER :: M3MAX    =    3
        INTEGER,      PARAMETER :: MAXRECS  = 5000
        INTEGER,      PARAMETER :: MAXITEMS =    3

        CHARACTER*80, PARAMETER :: MENUITMS( MAXITEMS ) = !  buffer for operations menu items
     &       (/  'Calculate   SUM   over time window    ',
     &           'Calculate AVERAGE over time window    ',
     &           'Determine MAXIMUM over time window    '   /)

        CHARACTER*2, PARAMETER :: SUFFIX( 6 ) =
     &       (/  '_1', '_2', '_3', '_4', '_5', '_6' /)

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    ANAME   !  scratch buffer for names
        CHARACTER*16    BNAME   !  scratch buffer for names
        CHARACTER*16    INAME   !  logical name of the  input file
        CHARACTER*16    ONAME   !  logical name of the aggregate-output file
        CHARACTER*16    MNAME   !  logical name of the max-output file
        INTEGER         VTYPE ( MXVARS3 ) !  data-type of variables
        CHARACTER*16    UNITS ( MXVARS3 ) !  list of vble units
        CHARACTER*16    VNAMEI( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    VNAMEO( MXVARS3 ) !  list of vble names, from user
        CHARACTER*80    ALINE   !  scratch buffer for prompt
        CHARACTER*80    VDESC ( MXVARS3 ) !  list of vble descs
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*256   MESG    !  for M3WARN(), M3EXIT()

        CHARACTER*16    MASKS( MXFILE3 ) !  logical names of the mask files
        CHARACTER*16    MASKV( MXFILE3 ) !  Variable-name for the mask
        INTEGER         NMASKS          !  number of masks

        INTEGER         I, M, T, V !  loop counter (time step #)

        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        INTEGER         BDATE   !  beginning analysis window date
        INTEGER         BTIME   !  beginning analysis window time
        INTEGER         DAY     !  temporary day
        INTEGER         DMAX    !  string length for descriptions
        INTEGER         IOS     !  I/O status
        INTEGER         JDATE   !  current  input date
        INTEGER         JTIME   !  current  input time
        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         MON     !  temporary month
        INTEGER         NVSAV   !  number of variables in input file
        INTEGER         NVARS   !  number of vbles in ONAME
        INTEGER         PERIOD  !  Period length (HHMMSS) for repetitive analysis
        INTEGER         PERSEC  !  Period length in seconds
        INTEGER         RUNSEC  !  Run length in seconds
        INTEGER         PDATE   !  current output date
        INTEGER         PTIME   !  current output time
        INTEGER         NCOLS   !  dimension
        INTEGER         NROWS   !  dimension
        INTEGER         NLAYS   !  dimension
        INTEGER         SDATE   !  starting  input date, from user
        INTEGER         STIME   !  starting  input time, from user
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         NOUTS   !  Number of output time steps
        INTEGER         TSTEP   !  time step, from INAME header
        INTEGER         INSECS  !  time2sec(tstep)
        INTEGER         ATYPE   !  type of aggregation being performed
        INTEGER         UMAX    !  string length for units
        INTEGER         WSTEPS  !  window duration in TSTEPs
        INTEGER         VMAX    !  string length for names
        INTEGER         WINLEN  !  duration (HHMMSS) of analysis window
        INTEGER         YR      !  temporary year

        REAL            CBAR

        REAL,    ALLOCATABLE::  CMAX ( :, : )        !  data structures for
        INTEGER, ALLOCATABLE::  CDATE( :, : )        !  computing and sorting
        INTEGER, ALLOCATABLE::  CTIME( :, : )        !  hourly max of the
        INTEGER, ALLOCATABLE::  CCOL ( :, : )        !  agg. concentrations
        INTEGER, ALLOCATABLE::  CROW ( :, : )
        INTEGER, ALLOCATABLE::  CLAY ( :, : )
        INTEGER, ALLOCATABLE::  CDEX ( :, : )

        LOGICAL         NPFLAG  !  iff no prompting for variables

C.........................................................................
C   begin body of program  M3AGMASK

        LOGDEV = INIT3()
        WRITE ( *,92000 )
     &  ' ',
     &  'Program M3AGMASK to sum, average, or find the maximum values',
     &  'over a repeating time period from a selected time window.',
     &  'The time period and starting time window set the start and',
     &  'duration of all subsequent time windows. The program inputs',
     &  'and outputs Models-3 files.',
     &  ' ',
     &  'This version of the program will also report to the program',
     &  'log a sorted list of the hourly maxes of the output, taken',
     &  'over a set of selected subregions, by variable, together with',
     &  'their times and locations.  The subregions are defined by',
     &  'user-input "mask" files, each of which is defined by an input',
     &  'INTEGER grid of zeros and ones, the region being specified as',
     &  'the locus where the mask-grid value is 1.  The program will',
     &  'prompt you for the set of files used to define these',
     &  'subregions, and the names of the mask variables within those',
     &  'files.'
        WRITE ( *,92000 )
     &  ' ',
     &  'You need to have set environment variables for the input',
     &  'and output file logical names.  You will be asked to select',
     &  'the time period to be copied and the start of the time ',
     &  'period to receive the results.',
     &  ' ',
     &  'USAGE:  M3AGMASK [INFILE OUTFILE MAXFILE] ',
     &  '(and then answer the prompts).',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    carlie@jyarborough.com',
     &'or',
     &'    UNC Institute for the Environment',
     &'    137 E. Franklin St. Suite 602 Room 613-C',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: m3agmask.f 435 2016-11-22 18:10:58Z coats               $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            INAME = PROMPTMFILE( 'Enter logical name for  INPUT FILE',
     &                           FSREAD3, 'INFILE', PNAME )

        ELSE IF ( ARGCNT .EQ. 3 ) THEN

            CALL GETARG( 1, ENVBUF )
            INAME = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                       // TRIM( INAME ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            ONAME = ENVBUF( 1:16 )

            CALL GETARG( 3, ENVBUF )
            MNAME = ENVBUF( 1:16 )

        ELSE

            CALL M3EXIT( PNAME, 0, 0,
     &               'usage:  M3AGMASK [INFILE OUTFILE MAXFILE ]', 2 )

        END IF


        IF ( .NOT. DESC3( INAME ) ) THEN
            MESG = 'Could not get description of input file "' //
     &             TRIM( INAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            NCOLS = NCOLS3D
            NROWS = NROWS3D
            NLAYS = NLAYS3D
        ELSE
            WRITE( MESG, 94011 )
     &      'Input file "', TRIM( INAME ),
     &      '" has unsupported type', FTYPE3D
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NVSAV  = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        TSTEP  = TSTEP3D

C.......   Get max string-lengths for use in variables-listing:

        VMAX = LEN_TRIM( VNAME3D( 1 ) )
        UMAX = LEN_TRIM( UNITS3D( 1 ) )
        DMAX = LEN_TRIM( VDESC3D( 1 ) )
        DO  11  V = 1, NVARS3D
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( V ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( V ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( V ) ) )
11      CONTINUE

C.......  Determine if all variables are to be used

        NPFLAG = ENVYN( 'M3AGMASK_ALLV',
     &                  'true if no prompting for variables to output',
     &                  .FALSE., IOS )
        IF ( IOS .GT. 0 ) THEN
            CALL M3EXIT( PNAME, 0,0,
     &                   'Bad environment variable "M3AGMASK_ALLV"', 2 )
        END IF

C.......  If no prompting set to total number of vars, or prompt

        IF( NPFLAG ) THEN

            NVARS = NVARS3D

            DO 22 V = 1, NVARS3D
               VNAMEI( V )  = VNAME3D( V )
               VNAMEO( V )  = VNAME3D( V )
               UNITS ( V )  = UNITS3D( V )
               VDESC ( V )  = VDESC3D( V )
               VTYPE ( V )  = VTYPE3D( V )
22          CONTINUE

        ELSE

            NVARS = 0
            V     = 0

111         CONTINUE        !  loop getting variables-list for extraction

                IF( MOD( NVARS,10 ) .EQ. 0 ) THEN
                    WRITE( *,92000 )
     &              ' ', 'The list of variables in file "'
     &              // TRIM( INAME ) // '" is:', ' '
                    WRITE( *,92010 )
     &              ( I,
     &                VNAME3D( I )( 1:VMAX ) // ' (' //
     &                UNITS3D( I )( 1:UMAX ) // '): ' //
     &                VDESC3D( I )( 1:DMAX ), I = 1, NVSAV  )
                END IF

                V = GETNUM( 0, NVSAV, 1 + MOD( V, NVSAV ),
     &            'Enter number for variable to extract (0 to quit)' )

                IF ( V .EQ. 0 ) GO TO  133      !  to end of loop

                NVARS = NVARS + 1

C...............   Optional renaming of this variable:

122             CONTINUE
                    ALINE = 'Enter output-name for this variable [' //
     &                      TRIM( VNAME3D( V ) ) // '] >> '
                    WRITE( *,95000 ) ALINE( 1:1+LEN_TRIM( ALINE ) )
                    READ ( *,93010,IOSTAT=IOS ) ANAME

                    IF ( IOS .GT. 0 ) THEN
                        CALL M3WARN( PNAME, 0, 0,
     &                  'Error reading output-name; please try again' )
                        GO TO 122
                    END IF

                IF( ANAME .EQ. BLANK16 ) THEN
                    VNAMEO( NVARS ) = VNAME3D( V )
                ELSE
                    VNAMEO( NVARS ) = ANAME
                END IF
                VNAMEI( NVARS ) = VNAME3D( V )
                UNITS ( NVARS ) = UNITS3D( V )
                VDESC ( NVARS ) = VDESC3D( V )
                VTYPE ( NVARS ) = VTYPE3D( V )

                IF( NVARS .EQ. NVARS3D ) GO TO 133

                IF ( NVARS .LT. MXVARS3 )  GO TO  111   !  to head of loop

133         CONTINUE        !  end loop getting variables-list for analysis

        END IF  ! If prompting or not

        IF ( NVARS .EQ. 0 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                  'No variables selected', 2 )
        END IF

C.......   Get starting date and time, and duration:

        IF ( TSTEP .EQ. 0 ) THEN        !  time-independent file

            MESG = 'Input file "' // TRIM( INAME ) //
     &             '" is only one time step- no output written.'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        ELSE                            !  time-dependent file

C.......   Prompt for parameters if defaults aren't set by specific env.
C.......   variable setting. Get default settings from the environment.

C...........  Period length, Start date&time BDATE:BTIME, duration WINLEN
C...........  Number NOUTS of periods to process, analysis type ATYPE

            PERIOD = ENVINT( 'M3AGMASK_PLEN', 'Output time step',
     &                        TSTEP, IOS )
            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME, 0,0,
     &                   'Bad environment variable "M3AGMASK_PLEN"', 2 )
            END IF
            PERSEC = TIME2SEC( PERIOD )
            INSECS = TIME2SEC( TSTEP )
            IF ( MOD( PERSEC, INSECS ) .NE. 0 ) THEN
                MESG =  'Output time step mismatch'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            RUNSEC = MXREC3D * TIME2SEC( TSTEP )
            RUNLEN = SEC2TIME( RUNSEC )

            IF( IOS .NE. 0 ) PERIOD = GETNUM( TSTEP, RUNLEN, TSTEP,
     &       'Enter output time step (HHMMSS) for repeating analysis' )

            BDATE = ENVINT( 'M3AGMASK_BDATE', 'Window start date',
     &                      SDATE, IOS )

            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME, 0,0,
     &                 'Bad environment variable "M3AGMASK_BDATE"', 2 )
            ELSE IF( IOS .NE. 0 ) THEN
                MESG = 'Enter starting date for run (YYYYDDD|YYYYMMDD)'
                BDATE  = GETDATE( BDATE, MESG )

            ELSE IF( BDATE .GT. 9999366 ) THEN   ! Convert to Julian

                YR    = BDATE/10000
                MON   = ( BDATE-YR*10000 ) / 100
                DAY   = MOD( BDATE-YR*10000, 100 )
                BDATE = YR*1000 + JULIAN( YR, MON, DAY )

            END IF

            BTIME = ENVINT( 'M3AGMASK_BTIME', 'Window start time',
     &                       STIME, IOS )

            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME, 0,0,
     &                 'Bad environment variable "M3AGMASK_BTIME"', 2 )
            ELSE IF( IOS .NE. 0 )  THEN
                I = BTIME
                BTIME  = GETNUM( 0, 239999, I,
     &                  'Enter starting time for run (HHMMSS)' )
            END IF

            T = SECSDIFF( SDATE, STIME, BDATE, BTIME )
            IF ( MOD( T, INSECS ) .NE. 0 ) THEN
                MESG =  'Run-start not exact time step from file start'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            RUNSEC = RUNSEC - T
            T = SEC2TIME( RUNSEC )

            I = MIN( SEC2TIME( 8*PERSEC ), T )
            WINLEN = ENVINT( 'M3AGMASK_WLEN',
     &                       'Aggregation-period duration',
     &                        I, IOS )

            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME, 0,0,
     &                 'Bad environment variable "M3AGMASK_WLEN"', 2 )
            ELSE IF( IOS .NE. 0 ) THEN
                I = WINLEN
                WINLEN = GETNUM( 1, T, I,
     &                'Enter duration of aggregation-period (HHMMSS)' )
            END IF

            RUNSEC = RUNSEC - TIME2SEC( WINLEN )

            NOUTS = ENVINT( 'M3AGMASK_NPER',
     &                      'Number of output time steps',
     &                       RUNSEC / PERSEC, IOS )

            IF( IOS .NE. 0 )  THEN
                I = NOUTS
                NOUTS = GETNUM( 1, I, T,
     &             'Enter number of output time steps to analyze' )
            END IF

            WSTEPS = TIME2SEC( WINLEN )  / INSECS

            ATYPE = ENVINT( 'M3AGMASK_TYPE',
     &                      'Type of analysis',
     &                       M3AVE, IOS )

            IF ( IOS .GT. 0 ) THEN
                CALL M3EXIT( PNAME, 0,0,
     &                   'Bad environment variable "M3AGMASK_TYPE"', 2 )
            ELSE IF( IOS .NE. 0 ) THEN
                ATYPE = GETMENU( MAXITEMS, ATYPE,
     &                        'Enter type of operation to perform',
     &                        MENUITMS )
            END IF

        END IF          !  time-independent file, or not


C.......   Build description for the output file, and create accordingly:
C.......   Re-use all but the starting date&time of the input-file description.

        SDATE3D = BDATE
        STIME3D = BTIME
        TSTEP3D = PERIOD

        IF ( ARGCNT .EQ. 0 ) THEN

            NVARS3D = NVARS
            DO   V = 1, NVARS
                VNAME3D( V ) = VNAMEO( V )
                UNITS3D( V ) = UNITS ( V )
                VDESC3D( V ) = VDESC ( V )
                VTYPE3D( V ) = VTYPE ( V )
            END DO
            ONAME = PROMPTMFILE( 'Enter logical name for AGG FILE',
     &                           FSUNKN3, 'OUTFILE', PNAME )

            NVARS3D = 8 * NVARS
            I = 0
            DO  V = 1, NVARS
                DO  T = 1, 6
                    I = I + 1
                    VNAME3D( I ) = TRIM( VNAMEO( V ) ) // SUFFIX( T )
                    UNITS3D( I ) = UNITS ( V )
                    VDESC3D( I ) = SUFFIX( T )(2:2)//'th highest value'
                    VTYPE3D( I ) = VTYPE ( V )
                END DO
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_BAR'
                UNITS3D( I ) = UNITS ( V )
                VDESC3D( I ) = 'Seasonal mean of aggregation-period max'
                VTYPE3D( I ) = M3REAL
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_1_125'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '1-hour 125 PPB exceedance count'
                VTYPE3D( I ) = M3INT
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_8_85'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '8-hour 85 ppb exceedance count'
                VTYPE3D( I ) = M3INT
            END DO

            MNAME = PROMPTMFILE( 'Enter logical name for AGG-MAX FILE',
     &                           FSUNKN3, 'MAXFILE', PNAME )

        ELSE        !  argcnt = 3:

            NVARS3D = NVARS
            DO   V = 1, NVARS
                VNAME3D( V ) = VNAMEO( V )
                UNITS3D( V ) = UNITS ( V )
                VDESC3D( V ) = VDESC ( V )
                VTYPE3D( V ) = VTYPE ( V )
            END DO
            IF ( .NOT. OPEN3( ONAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open output AGG-file "' //
     &                 TRIM( ONAME ) // '"'
                CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
            END IF

            NVARS3D = 9 * NVARS
            I = 0
            DO  V = 1, NVARS
                DO  T = 1, 6
                    I = I + 1
                    VNAME3D( I ) = TRIM( VNAMEO( V ) ) // SUFFIX( T )
                    UNITS3D( I ) = UNITS ( V )
                    VDESC3D( I ) = SUFFIX( T )(2:2)//'th highest value'
                    VTYPE3D( I ) = VTYPE ( V )
                END DO
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_BAR'
                UNITS3D( I ) = UNITS ( V )
                VDESC3D( I ) = 'Seasonal mean of aggregation-period max'
                VTYPE3D( I ) = M3REAL
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_1_125'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '1-hour 125 PPB exceedance count'
                VTYPE3D( I ) = M3INT
                I = I + 1
                VNAME3D( I ) = TRIM( VNAMEO( V ) ) // '_8_85'
                UNITS3D( I ) = 'none'
                VDESC3D( I ) = '8-hour 85 ppb exceedance count'
                VTYPE3D( I ) = M3INT
            END DO
            NVARS3D = I

            IF ( .NOT. OPEN3( MNAME, FSUNKN3, PNAME ) ) THEN
                MESG = 'Could not open output AGG-MAX file "' //
     &                 TRIM( MNAME ) // '"'
                CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
            END IF

        END IF      !  if argcnt zero, or 2


C.......   Count/Get the mask files and mask variables:

        NMASKS = 0
144     CONTINUE

            WRITE( ANAME, '( A, I3.3 )' ) 'MASKFILE', I
            BNAME = PROMPTMFILE(
     &             'Enter the name for the next MASK FILE, or "NONE"',
     &             FSREAD3, ANAME, PNAME )
            IF ( BNAME .NE. 'NONE ' ) THEN
                 NMASKS = NMASKS + 1
                 MASKS( NMASKS ) = BNAME

                 IF ( .NOT. DESC3( BNAME ) ) THEN
                     MESG = 'Could not get file description for "' //
     &                      TRIM( BNAME ) // '"'
                     CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
                 END IF
                 MASKV( NMASKS ) = VNAME3D( GETMENU( NVARS3D, 1,
     &                      'Enter number for the MASK VARIABLE',
     &                      VNAME3D ) )
                 GO TO  144
            END IF

C.......   Allocate masked-aggregation arrays:

        ALLOCATE ( CMAX ( NOUTS, NMASKS ),
     &             CDATE( NOUTS, NMASKS ),
     &             CTIME( NOUTS, NMASKS ),
     &             CCOL ( NOUTS, NMASKS ),
     &             CROW ( NOUTS, NMASKS ),
     &             CLAY ( NOUTS, NMASKS ),
     &             CDEX ( NOUTS, NMASKS ), STAT = I )
        IF ( I .NE. 0 ) THEN
            MESG = 'Allocation failure'
            CALL M3EXIT( PNAME, SDATE, STIME, MESG, 2 )
        END IF


C.......   Log run-characteristics:

        WRITE( MESG, '(A, I9, A, I6.6)' )
     &          'Starting date and time:', BDATE, ':', BTIME
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, 2X, I6.6)' )
     &          'Output timestep (HHMMSS)', PERIOD
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I6)' ) 'Number of output time steps', NOUTS
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I6)' ) 'Number of aggregation steps', WSTEPS
        CALL M3MSG2( MESG )

        WRITE( MESG, '(A, I4)' ) 'Number of variables', NVARS
        CALL M3MSG2( MESG )

        MESG = 'Time-Stepped output file "' // TRIM( ONAME ) // '"'
        CALL M3MSG2( MESG )

        MESG = 'Cumulative output file "' // TRIM( MNAME ) // '"'
        CALL M3MSG2( MESG )


C.......   Process this period in the input file:

        DO  322  V = 1, NVARS

            PDATE = BDATE
            PTIME = BTIME
            MESG  = 'Processing variable "'//TRIM( VNAMEI( V ) )//'"'
            CALL M3MSG2( MESG )

            DO  311  T = 1, NOUTS

                JDATE = PDATE
                JTIME = PTIME

                CALL INITAGG( NCOLS, NROWS, NLAYS, T, JDATE, JTIME,
     &                        INAME, VNAMEI(V), NMASKS, MASKS, MASKV,
     &                        LOGDEV )

                CALL NEXTIME( JDATE, JTIME, TSTEP )

                DO  301 I = 2, WSTEPS


                    CALL AGGREG( NCOLS, NROWS, NLAYS, JDATE, JTIME,
     &                           INAME, VNAMEI( V ), ATYPE, LOGDEV )


                    CALL NEXTIME( JDATE, JTIME, TSTEP )

301             CONTINUE    !  end loop on window time steps

                CDATE( T, : ) = PDATE
                CTIME( T, : ) = PTIME
                CDEX ( T, : ) = T
                CALL OUTAGG( NCOLS, NROWS, NLAYS, NOUTS, NMASKS,
     &                       T, PDATE, PTIME,
     &                       ONAME, VNAMEO( V ), ATYPE, WSTEPS,
     &                       CMAX, CCOL,  CROW, CLAY, LOGDEV )

            CALL NEXTIME( PDATE, PTIME, PERIOD )

311         CONTINUE            !  end loop on analysis periods


C...........   Sort the aggregate-maxes:

            MESG = 'Processing ranked cumulative run-statistics'
            CALL M3MSG2( MESG )
            WRITE( LOGDEV, '(/5X, 4 A, /)' )
     &              'Processing ranked cumulative run-statistics ',
     &              'for variable "', TRIM( VNAMEO( V ) ), '"'
            CALL MAXAGG( BDATE, BTIME, MNAME, NOUTS, VNAMEO( V ) )

            DO  M = 1, NMASKS

                WRITE( LOGDEV, '(/5X, A, I3 )' ) 'Analysis domain ', M
                CALL SORTR1( NOUTS, CDEX(1,M), CMAX(1,M) )

                CBAR = 0.0
                DO  I = NOUTS, 1, -1

                    T = CDEX( I,M )
                    CBAR = CBAR + CMAX( T,M )
                    WRITE( LOGDEV, '( 5X, A, 2X, I4, 2X,
     &                          A, 2X, 1PE14.7, 2X,
     &                          A, I7.7, A, I6.6, 2X,
     &                          A, 3( I3, A ) )' )
     &              'Rank:', I,
     &              'Max:', CMAX( T,M ),
     &              'Date&Time:',  CDATE( T,M ), ':', CTIME( T,M ),
     &              'at (C,R,L)=(', CCOL( T,M ), ',',  CROW( T,M ),
     &                                           ',',  CLAY( T,M ), ')'
                END DO          !  end loop on time steps

                CBAR = CBAR / FLOAT( NOUTS )
                WRITE( LOGDEV, '( 5X, A, 1PE14.7, / )' )
     &              'Study-period mean of analysis domain max:', CBAR

            END DO              !  end loop on analysis domains M

322     CONTINUE            !  end loop on variables

        CALL M3EXIT( PNAME, 0, 0,
     &               'Program  M3AGMASK  completed successfully', 0 )


C..............  FORMAT STATEMENTS:  ....................................

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( 1X , I5, ':  ', A )

C...........   Formatted file I/O formats............ 93xxx

93010   FORMAT( A16 )

C...........   Internal buffering formats............ 94xxx

94010   FORMAT ( 100( A, :, 2X, I5, :, 2X ) )

94011   FORMAT ( 3A, I5 )

C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.

        END PROGRAM  M3AGMASK


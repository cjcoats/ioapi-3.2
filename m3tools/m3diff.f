
        PROGRAM  M3DIFF

C***********************************************************************
C Version "$Id: m3diff.f 117 2019-06-15 14:56:29Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
C (C) 2015 UNC Institute for the Environment.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  126
C
C  FUNCTION:
C       For a user-specified pair of GRIDDED Models-3 file and lists of
C       variables within them, compute statistics for each specified
C       variable and for the difference.
C
C  PRECONDITIONS REQUIRED:
C       Machine with stack-allocated AUTO local variables (e.g., CRAY)
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       Models-3 I/O; emissions utilities.
C
C  REVISION  HISTORY:
C       Prototype 3/1993 by CJC
C       Version   5/1995 by CJC:  new I/O API; command line arguments
C       Modified  9/1999 by CJC for enhanced portability
C       Version  11/2001 by CJC for I/O API Version 2.1; fixed buffer
C       length bug for EQNAMEs
C       Version   1/2006 by CJC:  bug-fix for VARMODE file output
C
C       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C
C       Version 02/2010 by CJC for I/O API v3.2:  Use FIKCHK3(), GRDCHK3()
C
C       Version  06/2019 by CJC:  Bugfix for RUNLEN
C***********************************************************************

      USE M3UTILIO

      IMPLICIT NONE

C...........   EXTERNAL FUNCTIONS and their descriptions:

         INTEGER :: IARGC

C...........   PARAMETERS and their descriptions:

        CHARACTER*16, PARAMETER :: PNAME = 'M3DIFF'

C...........   LOCAL VARIABLES and their descriptions:

        INTEGER         LOGDEV  !  unit number for log file
        INTEGER         ARGCNT  !  number of command-line args, from IARGC()
        CHARACTER*256   ENVBUF  !  value from command line arguments
        CHARACTER*16    RNAME   !  logical name of report file
        INTEGER         RDEV    !  report-file unit number

        CHARACTER*16    NAMEA   !  logical name of the first  input file
        CHARACTER*16    NAMEB   !  logical name of the second input file
        CHARACTER*16    NAMEC   !  logical name of the output file

        INTEGER         FTYPE   !  file type (GRDDED3, CUSTOM3, BNDARY3)
        INTEGER         FSIZE   !  file layer-size

        INTEGER         NCOLS   !  grid dimensions, from file headers
        INTEGER         NROWS   !  grid dimensions, from file headers
        INTEGER         NLAYS   !  grid dimensions, from file headers
        INTEGER         NVARS1  !  number of vbles in NAMEA
        INTEGER         NVARS2  !  number of vbles in NAMEB
        INTEGER         NVARS   !  number of vbles to be totaled, from user
        CHARACTER*16    VNAME1( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS1( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC1( MXVARS3 ) !  list of vble descs
        CHARACTER*16    VNAME2( MXVARS3 ) !  list of vble names, from user
        CHARACTER*16    UNITS2( MXVARS3 ) !  list of vble units
        CHARACTER*80    VDESC2( MXVARS3 ) !  list of vble descs
        CHARACTER*16    VNAME3( MXVARS3 ) !  list of output vble names
        CHARACTER*16    WNAMES( 2, MXVARS3 ) !  list of vble names, from user
        INTEGER         WTYPES( 2, MXVARS3 ) !  variable-types
        CHARACTER*16    OPNAME( MXVARS3 ) !  list of operation names
        INTEGER         VTYPE1( MXVARS3 ) !  datatype (must be M3REAL)
        INTEGER         VTYPE2( MXVARS3 )
        INTEGER         SDATE   !  common starting date, from user
        INTEGER         STIME   !  common starting time, from user
        INTEGER         SDATEA  !  File A starting date, from user
        INTEGER         STIMEA  !  File A starting time, from user
        INTEGER         SDATEB  !  File B starting date, from user
        INTEGER         STIMEB  !  File B starting time, from user
        INTEGER         SDATEC  !  File C starting date, from user
        INTEGER         STIMEC  !  File C starting time, from user
        INTEGER         JDATEA  !  File A current date
        INTEGER         JTIMEA  !  File A current time
        INTEGER         JDATEB  !  File B current date
        INTEGER         JTIMEB  !  File B current time
        INTEGER         JDATEC  !  File C current date
        INTEGER         JTIMEC  !  File C current time
        INTEGER         RUNLEN  !  duration, HHMMSS from user
        INTEGER         TSTEP   !  common time step
        INTEGER         TSTEPA  !  File A time step
        INTEGER         TSTEPB  !  File B time step
        INTEGER         TSTEPC  !  File C time step
        INTEGER         NSTEPS  !  duration in TSTEPs

        INTEGER         I, J, K, L !  scratch variables
        INTEGER         VMAX    !  string length for names
        INTEGER         UMAX    !  string length for units
        INTEGER         DMAX    !  string length for descriptions

        LOGICAL         VARMODE !  present stats by variable, then timestep
        LOGICAL         DEFAULT !  command-line flag:  do default analysis
        LOGICAL         OUTMODE !  generating output file?

        CHARACTER*256   SCRBUF  !  buffer for GETARG()
        CHARACTER*16    SCRNAM  !  scratch variable name
        CHARACTER*256   MESG    !  buffer for messages
        CHARACTER*256   ENAME1  !  physical file name # 1
        CHARACTER*256   ENAME2  !  physical file name # 2

C.........................................................................
C   begin body of program  M3DIFF

        LOGDEV = INIT3()
        WRITE ( *, '( 5X , A )' )
     &  ' ',
     &  'Program M3DIFF to compute difference statistics of selected ',
     &  'variables from a pair of user-specified GRIDDED, BOUNDARY, ',
     &  'CUSTOM or Models-3 files.  ',
     &  'You need to have assigned logical names to the physical file',
     &  'names of both files, according to Models-3 conventions, ',
     &  'using the operation "setenv <lname> <pname>".',
     &  'You will have the choice of either the default analysis, ',
     &  'which computes statistics for the variables common to both ',
     &  'files and for the pointwise difference "A - B" between their',
     &  'values, or customized analysis in which you select lists of ',
     &  'which of the comparison operations is applied to which pairs',
     &  'of variables. ',
     &  ' ',
     &  'USAGE:  m3diff [INFILEA INFILEB [REPORTFILE]] [DEFAULT]',
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
     &'$Id:: m3diff.f 117 2019-06-15 14:56:29Z coats                 $',
     &' '

        ARGCNT = IARGC()

        IF ( ARGCNT .GE. 1 ) THEN
            CALL GETARG( ARGCNT, SCRBUF )
            CALL LUSTR( SCRBUF )
            IF ( 'DEFAULT '  .EQ. SCRBUF( 1:8 ) .OR.
     &           '-DEFAULT ' .EQ. SCRBUF( 1:9 ) ) THEN
                DEFAULT = .TRUE.
                OUTMODE = .FALSE.
                ARGCNT = ARGCNT - 1
            ELSE
                DEFAULT = GETYN( 'Do you want the default analysis?',
     &                           .TRUE. )
                OUTMODE = GETYN( 'Generate an output data file?',
     &                           .FALSE. )
            END IF
        ELSE
            DEFAULT = GETYN( 'Do you want the default analysis?',
     &                       .TRUE. )
            OUTMODE = GETYN( 'Generate an output data file?',
     &                       .FALSE. )
        END IF

        IF ( ARGCNT .EQ. 1  .OR.  ARGCNT .GT. 3 ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &      'usage:  m3diff [INFILEA INFILEB [REPORTFILE]] [DEFAULT]',
     &      2 )
        END IF

        IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

            NAMEA = PROMPTMFILE( 'Enter logical name for INPUT FILE A',
     &                           FSREAD3, 'AFILE', PNAME )

            NAMEB = PROMPTMFILE( 'Enter logical name for INPUT FILE B',
     &                           FSREAD3, 'BFILE', PNAME )

            RDEV = PROMPTFFILE(
     &          'Enter logical name for  REPORT FILE, or "NONE"',
     &                         .FALSE., .TRUE., 'REPORT', PNAME )
            IF ( RDEV .LE. 0 ) RDEV = LOGDEV

        ELSE            !  argcnt = 2 or 3: get file names from command line

            CALL GETARG( 1, ENVBUF )
            NAMEA = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( NAMEA, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                    // TRIM( NAMEA ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF

            CALL GETARG( 2, ENVBUF )
            NAMEB = ENVBUF( 1:16 )
            IF ( .NOT. OPEN3( NAMEB, FSREAD3, PNAME ) ) THEN
                MESG = 'Could not open input file "'
     &                   // TRIM( NAMEB ) // '"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
            END IF

            IF ( ARGCNT .EQ. 2 ) THEN
                RDEV = LOGDEV
            ELSE
                CALL GETARG( 3, ENVBUF )
                RNAME = ENVBUF( 1:16 )
                RDEV  = GETEFILE( RNAME, .FALSE., .TRUE., PNAME )
                IF ( RDEV .LT. 0 ) THEN
                    MESG = 'Could not open report file "'
     &                      // TRIM( RNAME ) // '"'
                    CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF          !  if rdev < 0 (getefile() failed)
            END IF      !  if argcnt=1, or else 2

        END IF

        IF ( .NOT. DESC3( NAMEA ) ) THEN
            MESG = 'Could not get description of input file '//NAMEA
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        IF ( FTYPE3D .EQ. GRDDED3 ) THEN
            FTYPE = GRDDED3
            FSIZE = NCOLS3D * NROWS3D
        ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
            FTYPE = BNDARY3
            FSIZE = 2*ABS( NTHIK3D )*( NCOLS3D + NROWS3D + 2*NTHIK3D )
        ELSE IF ( FTYPE3D .EQ. CUSTOM3 ) THEN
            FTYPE = CUSTOM3
            FSIZE = NCOLS3D
        ELSE
            WRITE( MESG, '( 3A, I9 )' )
     &             'Input file "', NAMEA,
     &             '" has type unsupported by M3DIFF:', FTYPE3D
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

        NCOLS  = NCOLS3D
        NROWS  = NROWS3D
        NLAYS  = NLAYS3D
        TSTEPA = TIME2SEC( TSTEP3D )
        NVARS1 = NVARS3D
        SDATE  = SDATE3D
        STIME  = STIME3D
        SDATEA = SDATE3D
        STIMEA = STIME3D
        RUNLEN = MXREC3D * TSTEPA


C.......   Copy variable-names.  Get max string-lengths for use in
C.......   variables-listing:

        VMAX = LEN_TRIM( VNAME3D( 1 ) )
        UMAX = LEN_TRIM( UNITS3D( 1 ) )
        DMAX = LEN_TRIM( VDESC3D( 1 ) )
        DO  11  I = 1, NVARS3D
            VNAME1( I ) = VNAME3D( I )
            UNITS1( I ) = UNITS3D( I )
            VDESC1( I ) = VDESC3D( I )
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
            VTYPE1( I ) = VTYPE3D( I )
11      CONTINUE


        IF ( .NOT. DESC3( NAMEB ) ) THEN
            MESG = 'Could not get description of input file ' // NAMEB
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        ELSE IF ( .NOT.FILCHK3( NAMEB,  FTYPE,
     &                  NCOLS, NROWS, NLAYS, NTHIK3D ) ) THEN
            MESG = 'Inconsistent dimensions  for ' // NAMEB
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF

C.......   Copy variable-names.  Get max string-lengths for use in
C.......   variables-listing:

        SDATEB = SDATE3D
        STIMEB = STIME3D
        IF ( SDATEA .LT. SDATEB ) THEN
            SDATE = SDATEB
            STIME = STIMEB
        ELSE IF ( SDATEA .LT. SDATEB ) THEN
            SDATE = SDATEA
            STIME = STIMEA
        ELSE
            SDATE = SDATEB
            STIME = MAX( STIMEA, STIMEB )
        END IF

        TSTEPB = TIME2SEC( TSTEP3D )
        RUNLEN = MIN( RUNLEN, MXREC3D * TSTEPB )
        L      = GCD( TSTEPA, TSTEPB )
        IF ( L .GT. 0 ) THEN
            TSTEP  = TSTEPA * ( TSTEPB / L )
        ELSE
            TSTEP  = TSTEPA * TSTEPB
        END IF
        TSTEPA = SEC2TIME( TSTEPA )
        TSTEPB = SEC2TIME( TSTEPB )
        NVARS2 = NVARS3D
        DO  22  I = 1, NVARS3D
            VNAME2( I ) = VNAME3D( I )
            UNITS2( I ) = UNITS3D( I )
            VDESC2( I ) = VDESC3D( I )
            VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
            UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
            DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
            VTYPE2( I ) = VTYPE3D( I )
22      CONTINUE


        IF ( DEFAULT ) THEN

            IF ( TSTEPA .EQ. 0 .OR. TSTEPB .EQ. 0 ) THEN
                NSTEPS  = 1
                VARMODE = .TRUE.
            ELSE
                NSTEPS = ( RUNLEN + TSTEP - 1 ) / TSTEP   !  "rounds up"
                TSTEP  = SEC2TIME( TSTEP )
                VARMODE = .FALSE.
            END IF          !  time-independent file, or not

            WRITE( *, '( 5X , A )' )
     &          ' ', 'The list of variables in file '
     &          // TRIM( NAMEA ) // ' is:',
     &          ( VNAME1( I )( 1:VMAX ) // ' (' //
     &            UNITS1( I )( 1:UMAX ) // '):',
     &            VDESC1( I )( 1:DMAX ), I = 1, NVARS1 )

            NVARS = 0
            DO  33  I = 1 , NVARS1

                J = INDEX1( VNAME1( I ), NVARS2, VNAME2 )
                IF ( J .EQ. 0 ) THEN
                    WRITE( *,92000 )
     &              ' ',
     &              'Variable ' // VNAME1( I ) //
     &              ' not found in file ' // NAMEB,
     &              ' '
                ELSE
                    NVARS = NVARS + 1
                    WNAMES( 1, NVARS ) = VNAME1( I )
                    WNAMES( 2, NVARS ) = VNAME1( I )
                    WTYPES( 1, NVARS ) = VTYPE1( I )
                    WTYPES( 2, NVARS ) = VTYPE1( I )
                    OPNAME( NVARS )    = '(A - B)'      !  GRIDOPS vble OP(1)
                END IF

33          CONTINUE

            DO  44  I = 1 , NVARS2

                J = INDEX1( VNAME2( I ), NVARS1, VNAME1 )
                IF ( J .EQ. 0 ) THEN
                    WRITE( *,92000 )
     &              ' ',
     &              'Variable ' // VNAME2( I ) // ' not found in file '
     &                          // NAMEA,
     &              ' '
                END IF

44          CONTINUE

            IF ( NVARS .EQ. 0 ) THEN
                MESG = 'No variables shared between '
     &                       // NAMEA // ' and ' // NAMEB
                CALL M3WARN( PNAME, 0, 0, MESG )
                GO TO  999
            ELSE
                WRITE( *,92000 )
     &               ' ',
     &               'The list of variables common to both files is:',
     &               ( WNAMES( 1,I )( 1:VMAX ) , I = 1, NVARS )
            END IF

            VARMODE = ( VARMODE .OR. ( NVARS .GT. 1 ) )


        ELSE            !  manually-selected analysis:

                WRITE( *, '( 5X , A )' )
     &          ' ', 'The list of variables in file "'
     &          // TRIM( NAMEA ) // '" is:',
     &          ( VNAME1( L )( 1:VMAX ) // ' (' //
     &            UNITS1( L )( 1:UMAX ) // '):',
     &            VDESC1( L )( 1:DMAX ), ' ', L = 1, NVARS1 )

                WRITE( *, '( 5X , A )' )
     &          ' ', 'The list of variables in file "'
     &          // TRIM( NAMEB ) // '" is:',
     &          ( VNAME2( L )( 1:VMAX ) // ' (' //
     &            UNITS2( L )( 1:UMAX ) // '):',
     &            VDESC2( L )( 1:DMAX ), ' ', L = 1, NVARS2 )


            NVARS = 0
            I     = 0
            J     = 0
111         CONTINUE        !  loop getting variables-list for analysis

                WRITE( *, '( 5X , A )' )
     &          ' ', 'The list of variables in file "'
     &          // TRIM( NAMEA ) // '" is:'
                WRITE( *,92010 )
     &          ( L, VNAME1( L )( 1:VMAX ), L = 1, NVARS1 )

                I = GETNUM( 0, NVARS1, 1 + MOD( I, NVARS1 ),
     &                  'Enter number for operand A (0 to end list)' )

                IF ( I .EQ. 0 ) THEN
                    GO TO  122      !  to end of loop
                END IF

                NVARS = NVARS + 1
                WNAMES( 1, NVARS ) = VNAME1( I )
                WTYPES( 1, NVARS ) = VTYPE1( I )

                WRITE( *, '( 5X , A )' )
     &          ' ', 'The list of variables in file "'
     &          // TRIM( NAMEB ) // '" is:'
                WRITE( *,92010 )
     &          ( L, VNAME2( L )( 1:VMAX ), L = 1, NVARS2 )

                K = INDEX1( VNAME1( I ), NVARS2, VNAME2 )
                IF ( K .EQ. 0 ) THEN
                    J = 1 + MOD( J , NVARS2 )
                ELSE
                    J = K
                END IF

                J = GETNUM( 0, NVARS2, J ,
     &                      'Enter number for operand B (0 to cancel)' )

                IF ( J .EQ. 0 ) THEN
                    NVARS = NVARS - 1   !  back out of this choice
                ELSE
                    WNAMES( 2, NVARS ) = VNAME2( J )
                    WTYPES( 2, NVARS ) = VTYPE2( J )
                    CALL PICKOPS( OPNAME( NVARS ) )
                END IF

                IF ( NVARS .LT. MXVARS3 )  GO TO  111   !  to head of loop

122         CONTINUE        !  end loop getting variables-list for analysis

            IF ( NVARS .EQ. 0 ) THEN
                CALL M3EXIT( PNAME, 0, 0,
     &                       'No variables selected', 2 )
                GO TO  999
            END IF


C.......   Get mode of operation:

            NSTEPS = -1         !  magic number -- "not yet set"

            IF ( TSTEPA .EQ. 0 ) THEN
                SDATEA  = 0
                STIMEA  = 0
                NSTEPS  = 1
                VARMODE = .TRUE.
            ELSE
                SDATEA = GETNUM( SDATEA, 9999999, SDATEA,
     &              'Enter FILE A starting date (YYYYDDD) for run' )
                STIMEA = GETNUM( 0, 9999999, STIMEA,
     &              'Enter FILE A starting time (HHMMSS) for run' )
            END IF          !  time-independent file, or not

            IF ( TSTEPB .EQ. 0 ) THEN
                SDATEB = 0
                STIMEB = 0
                NSTEPS = 1
                VARMODE = .TRUE.
            ELSE
                SDATEB = GETNUM( SDATEB, 9999999, SDATEB,
     &              'Enter FILE B starting date (YYYYDDD) for run' )
                STIMEB = GETNUM( 0, 9999999, STIMEB,
     &              'Enter FILE B starting time (HHMMSS) for run' )
            END IF

            IF ( NSTEPS .EQ. -1 ) THEN  !  "not yet set"

                IF ( NVARS .GT. 1 ) THEN
                    WRITE( *,92000 ) ' ',
     &      'You have the options of either separate time series for ',
     &      'each variable,or a joint time series for all variables',
     &      'simultaneously.'
                    VARMODE = GETYN(
     &                  'Separate time series for each variable?',
     &                  .TRUE. )
                ELSE
                    VARMODE = .FALSE.
                END IF

                RUNLEN = GETNUM( 0, 999999999, SEC2TIME( RUNLEN ),
     &                      'Enter duration (HHMMSS) for run' )
                NSTEPS = ( TIME2SEC( RUNLEN ) + TSTEP ) / TSTEP
                TSTEP  = SEC2TIME( TSTEP )

            END IF          !  time-independent file, or not

        END IF          !  default or manually-selected analysis


C.......   If generating an output file, construct it (according to
C.......   user-supplied description):

        IF ( OUTMODE ) THEN

            SDATEC = GETNUM( 0, 9999999, SDATEA,
     &                           'Enter OUTPUT FILE starting date' )
            STIMEC = GETNUM( 0, 9999999, STIMEA,
     &                           'Enter OUTPUT FILE starting time' )
            TSTEPC = GETNUM( 0, 9999999, TSTEPA,
     &                           'Enter OUTPUT FILE time step' )
            SDATE3D = SDATEC
            STIME3D = STIMEC
            TSTEP3D = TSTEPC
            NVARS3D = NVARS

            WRITE( *,92000 ) ' ',
     &      'Now select names, units, and descriptions for variables',
     &      'to be stored in the output file (one for each paired',
     &      'difference calculation to be performed', ' '

            DO 133  I = 1, NVARS

                MESG = 'Generating variable for "' //
     &              TRIM( NAMEA ) // ':' //
     &              TRIM( WNAMES( 1,I ) )// ' ' //
     &              TRIM( OPNAME( I ) )  // ' ' //
     &              TRIM( NAMEB ) // ':' //
     &              TRIM( WNAMES( 2,I ) )// '"'
                WRITE( *,  '( 5X , A )' ) ' ', TRIM( MESG )

                MESG = 'Enter NAME for this variable'
                CALL GETSTR( MESG, WNAMES( 1,I ), VNAME3( I ) )

                VTYPE3D( I ) = M3REAL

                J = INDEX1( WNAMES( 1,I ), NVARS1, VNAME1 )
                MESG = 'Enter UNITS for this variable'
                CALL GETSTR( MESG, UNITS1( J ), UNITS3D( I ) )

                SCRBUF =
     &              TRIM( NAMEA ) // ':' // TRIM( WNAMES( 1,I ) )//
     &              ' ' // TRIM( OPNAME( I ) ) // ' ' //
     &              TRIM( NAMEB ) // ':' // TRIM( WNAMES( 2,I ) )// '"'
                MESG = 'Description "'   // TRIM( SCRBUF ) // '"'
                WRITE( *, 92000 ) TRIM( MESG )
                MESG =
     &          'Enter description for this variable'
                CALL GETSTR( MESG, SCRBUF, VDESC3D( I ) )

133         CONTINUE

            WRITE( *,92000 )
     &      ' ',
     &      'Now please enter the output file description',
     &      '(up to 60 lines, terminated by a blank line)',
     &      ' '
            DO 144  I = 1, MXDESC3
                WRITE( *,95000 ) ' >> '
                READ ( *,93000 ) FDESC3D( I )
                IF ( FDESC3D( I )( 1:1 ) .EQ. ' ' ) THEN
                    J = I
                    GO TO  145
                END IF
144         CONTINUE
            J = MXDESC3
145         CONTINUE
            DO 155  I = J+1, MXDESC3
                FDESC3D( I ) = ' '
155         CONTINUE
            DO 166  I = 1, NVARS3D
                VTYPE3D( I ) = M3REAL
166         CONTINUE

            NAMEC = PROMPTMFILE( 'Enter logical name for OUTPUT FILE',
     &                           FSUNKN3, 'OFILE', PNAME )

        ELSE

            SDATEC = 0
            STIMEC = 0
            TSTEPC = 0
            NAMEC  = 'NONE'

        END IF      !  if outmode or not:  generating an output file


C.......   Perform this M3DIFF run:

        CALL NAMEVAL( NAMEA, ENAME1 )
        CALL NAMEVAL( NAMEB, ENAME2 )
        WRITE( *,92000 ) ' ', 'Processing . . .' , ' '

        WRITE( RDEV,92000 )
     &  ' ',
     &  'FILE A:  ' // TRIM( NAMEA ) // ' (' //  TRIM( ENAME1 ) // ')',
     &  ' ',
     &  'FILE B:  ' // TRIM( NAMEB ) // ' (' //  TRIM( ENAME2 ) // ')'

C.......   Process this period in the input file:

        IF ( RDEV .LT. 0 ) RDEV = LOGDEV        !  to report-file, or to log

        IF ( VARMODE ) THEN

            DO  222  I = 1, NVARS       !  construct time series of variable I

                JDATEA = SDATEA
                JTIMEA = STIMEA
                JDATEB = SDATEB
                JTIMEB = STIMEB
                JDATEC = SDATEC
                JTIMEC = STIMEC
                WRITE( RDEV,92000 )
     &' ', ' ',
     &'-----------------------------------------------------------',
     &' ', ' '

                DO  211  J = 1, NSTEPS

                    IF ( FTYPE .EQ. GRDDED3 ) THEN
                        CALL DIFFSTEP( NCOLS, NROWS, NLAYS, 1,
     &                                 JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                                 NAMEA, NAMEB,
     &                                 WNAMES( 1,I ), WTYPES( 1,I ),
     &                                 OPNAME( I ), RDEV ,
     &                                 NAMEC, VNAME3( I ),
     &                                 JDATEC, JTIMEC )

                    ELSE
                        CALL CDIFFSTEP( FSIZE, NLAYS, 1,
     &                                  JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                                  NAMEA, NAMEB,
     &                                  WNAMES( 1,I ), WTYPES( 1,I ),
     &                                  OPNAME( I ), RDEV ,
     &                                  NAMEC, VNAME3( I ),
     &                                  JDATEC, JTIMEC )

                    END IF              !  if grdded3 or not
                    CALL NEXTIME( JDATEA, JTIMEA, TSTEP )
                    CALL NEXTIME( JDATEB, JTIMEB, TSTEP )
                    CALL NEXTIME( JDATEC, JTIMEC, TSTEP )

211             CONTINUE        !  end loop on time steps

222         CONTINUE        !  end loop on variables

        ELSE            !  not varmode:  time series of all variables

            JDATEA = SDATEA
            JTIMEA = STIMEA
            JDATEB = SDATEB
            JTIMEB = STIMEB
            JDATEC = SDATEC
            JTIMEC = STIMEC

            DO  333  J = 1, NSTEPS

                IF ( FTYPE .EQ. GRDDED3 ) THEN
                    CALL DIFFSTEP( NCOLS, NROWS, NLAYS, NVARS,
     &                             JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                             NAMEA, NAMEB,
     &                             WNAMES, WTYPES,
     &                             OPNAME, RDEV ,
     &                             NAMEC, VNAME3,
     &                             JDATEC, JTIMEC )

                ELSE
                    CALL CDIFFSTEP( FSIZE, NLAYS, NVARS,
     &                              JDATEA, JTIMEA, JDATEB, JTIMEB,
     &                              NAMEA, NAMEB,
     &                              WNAMES, WTYPES,
     &                              OPNAME, RDEV ,
     &                              NAMEC, VNAME3,
     &                              JDATEC, JTIMEC )

                END IF              !  if grdded3 or not

                CALL NEXTIME( JDATEA, JTIMEA, TSTEP )
                CALL NEXTIME( JDATEB, JTIMEB, TSTEP )
                CALL NEXTIME( JDATEC, JTIMEC, TSTEP )

333         CONTINUE        !  end loop on time steps

        END IF


999     CONTINUE        !  end of program

        CALL M3EXIT( PNAME, 0, 0,
     &               'M3DIFF  completed successfully', 0 )

C******************  FORMAT  STATEMENTS   ******************************

C...........   Error and warning message formats..... 91xxx

C...........   Informational (LOG) message formats... 92xxx

92000   FORMAT ( 5X , A )

92010   FORMAT ( 3( I6, ':  ', A, : ) )


C...........   Formatted file I/O formats............ 93xxx

93000   FORMAT ( A )


C...........   Internal buffering formats............ 94xxx

94010   FORMAT ( A, 2( I4, A ) )


C...........   Miscellaneous formats................. 95xxx

95000   FORMAT ( /5X , A , $ )          !  generic prompt format.


        END


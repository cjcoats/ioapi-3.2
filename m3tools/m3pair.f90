
PROGRAM  M3PAIR

    !!***********************************************************************
    !! Version "$Id: m3pair.f90 1 2017-06-10 18:05:20Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC, 
    !! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and 
    !! (C) 2015 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  112
    !!
    !!  FUNCTION:
    !!       For a user-specified pair of GRIDDED Models-3 files and
    !!       pair of variables within them, generate an ASCII file of
    !!       matched value-pairs, one per line.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 4/1998 by CJC
    !!      Version 11/2001 by CJC for I/O API Version 2.1
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source,
    !!      CONTAINed SUBROUTINE PAIRSTEP
    !!      Version  02/2015 by CJC: Support for M3INT8 variables.
    !!***********************************************************************

    USE M3UTILIO
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3PAIR'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*72, PARAMETER :: BAR   =  &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LOGDEV  !  unit number for log file
    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*16    RNAME   !  logical name of report file
    INTEGER         RDEV    !  report-file unit number

    CHARACTER*16    FILEA   !  logical name of the first  input file
    CHARACTER*16    FILEB   !  logical name of the second input file

    INTEGER         NCOLS   !  grid dimensions, from file headers
    INTEGER         NROWS   !  grid dimensions, from file headers
    INTEGER         NLAYS   !  grid dimensions, from file headers
    INTEGER         NTHIK   !  grid dimensions, from file headers
    INTEGER         CLO     !  column range limit
    INTEGER         CHI     !  column range limit
    INTEGER         RLO     !  row    range limit
    INTEGER         RHI     !  row    range limit
    INTEGER         LLO     !  layer  range limit
    INTEGER         LHI     !  layer  range limit
    INTEGER         NVARS1  !  number of vbles in FILEA
    INTEGER         NVARS2  !  number of vbles in FILEB
    CHARACTER*16    VNAME1( MXVARS3 ) !  list of vble names, from user
    CHARACTER*16    UNITS1( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESC1( MXVARS3 ) !  list of vble descs
    CHARACTER*16    VNAME2( MXVARS3 ) !  list of vble names, from user
    CHARACTER*16    UNITS2( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESC2( MXVARS3 ) !  list of vble descs
    CHARACTER*16    WNAMES( 2 ) !  list of vble names, from user
    INTEGER         WTYPES( 2 ) !  variable-types
    INTEGER         VTYPE1( MXVARS3 ) !  datatype (must be M3REAL)
    INTEGER         VTYPE2( MXVARS3 )
    INTEGER         SDATE   !  common starting date, from user
    INTEGER         STIME   !  common starting time, from user
    INTEGER         SDATEA  !  File A starting date, from user
    INTEGER         STIMEA  !  File A starting time, from user
    INTEGER         SDATEB  !  File B starting date, from user
    INTEGER         STIMEB  !  File B starting time, from user
    INTEGER         JDATEA  !  File A current date
    INTEGER         JTIMEA  !  File A current time
    INTEGER         JDATEB  !  File B current date
    INTEGER         JTIMEB  !  File B current time
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         TSTEP   !  common time step
    INTEGER         TSTEPA  !  File A time step
    INTEGER         TSTEPB  !  File B time step
    INTEGER         NSTEPS  !  duration in TSTEPs

    INTEGER         I, J, L !  scratch variables
    INTEGER         VMAX    !  string length for names
    INTEGER         UMAX    !  string length for units
    INTEGER         DMAX    !  string length for descriptions

    INTEGER         ISTAT
    LOGICAL         EFLAG
    CHARACTER*256   MESG    !  buffer for m3exit(), etc

    !!.........................................................................
    !!   begin body of program  M3PAIR

    LOGDEV = INIT3()
    EFLAG  = .FALSE.
    WRITE ( *,'( 5X, A )' ) BLANK, BAR, BLANK,                              &
'Program M3PAIR to construct file of ASCII value pairs for selected',       &
'variables from a pair of user-specified GRIDDED Models-3 files.',          &
'You need to have assigned logical names to the physical file names',       &
'of both files, according to Models-3 conventions, using the operation',    &
'',                                                                         &
'    "setenv <lname> <path-name>".',                                        &
'',                                                                         &
'USAGE:  m3pair [INFILEA INFILEB OUTFILE]   (and then answer the prompts).',&
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
'$Id: m3pair.f 43 2014-09-12 14:06:19Z coats$',&
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .EQ. 0 ) THEN       !  get names from user

        FILEA = PROMPTMFILE( 'Enter logical name for INPUT FILE A', FSREAD3, 'INFILEA', PNAME )

        FILEB = PROMPTMFILE( 'Enter logical name for INPUT FILE B', FSREAD3, 'INFILEB', PNAME )

        RDEV = PROMPTFFILE( 'Enter logical name for  OUTPUT FILE', .FALSE., .TRUE., 'OUTFILE', PNAME )
        IF ( RDEV .LE. 0 ) RDEV = LOGDEV

    ELSE IF ( ARGCNT .EQ. 3 ) THEN

        CALL GETARG( 1, ENVBUF )
        FILEA = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( FILEA, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file "' // TRIM( FILEA ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF

        CALL GETARG( 2, ENVBUF )
        FILEB = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( FILEB, FSREAD3, PNAME ) ) THEN
            MESG = 'Could not open input file "' // TRIM( FILEB ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF

        CALL GETARG( 3, ENVBUF )
        RNAME = ENVBUF( 1:16 )
        RDEV  = GETEFILE( RNAME, .FALSE., .TRUE., PNAME )
        IF ( RDEV .LT. 0 ) THEN
            MESG = 'Could not open output file "' // TRIM( RNAME ) // '"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF          !  if rdev < 0 (getefile() failed)

    ELSE

        MESG = 'usage:  m3pair [INFILEA INFILEB OUTFILE]'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    END IF      !  if argcnt=0, or else 3, or not

    IF ( .NOT. DESC3( FILEA ) ) THEN
        MESG = 'Could not get description of input file ' // FILEA
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( FTYPE3D .NE. GRDDED3 ) THEN
        MESG = 'Input file '// FILEA //'not a GRIDDED file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    NCOLS  = NCOLS3D
    NROWS  = NROWS3D
    NLAYS  = NLAYS3D
    NTHIK  = NTHIK3D
    TSTEPA = TIME2SEC( TSTEP3D )
    NVARS1 = NVARS3D
    SDATE  = SDATE3D
    STIME  = STIME3D
    SDATEA = SDATE3D
    STIMEA = STIME3D
    RUNLEN = SEC2TIME( ( MXREC3D - 1 ) * TSTEPA )


    !!.......   Copy variable-names.  Get max string-lengths for use in
    !!.......   variables-listing:

    VMAX = LEN_TRIM( VNAME3D( 1 ) )
    UMAX = LEN_TRIM( UNITS3D( 1 ) )
    DMAX = LEN_TRIM( VDESC3D( 1 ) )
    DO   I = 1, NVARS3D
        VNAME1( I ) = VNAME3D( I )
        UNITS1( I ) = UNITS3D( I )
        VDESC1( I ) = VDESC3D( I )
        VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
        UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
        DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
        VTYPE1( I ) = VTYPE3D( I )
    END DO


    IF ( .NOT. DESC3( FILEB ) ) THEN
        MESG = 'Could not get description of input file ' // FILEB
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    ELSE IF ( .NOT.FILCHK3( FILEB,  GRDDED3, NCOLS, NROWS, NLAYS, NTHIK ) ) THEN
        MESG = 'Inconsistent dimensions  for ' // FILEB
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF

    !!.......   Copy variable-names.  Get max string-lengths for use in
    !!.......   variables-listing:

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
    RUNLEN = MIN( RUNLEN, SEC2TIME( MXREC3D * TSTEPB ) )
    L      = GCD( TSTEPA, TSTEPB )
    IF ( L .GT. 0 ) THEN
        TSTEP  = SEC2TIME( TSTEPA * ( TSTEPB / L ) )
    ELSE
        TSTEP  = 0
    END IF
    TSTEPA = SEC2TIME( TSTEPA )
    TSTEPB = SEC2TIME( TSTEPB )
    NVARS2 = NVARS3D
    DO  I = 1, NVARS3D
        VNAME2( I ) = VNAME3D( I )
        UNITS2( I ) = UNITS3D( I )
        VDESC2( I ) = VDESC3D( I )
        VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
        UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
        DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
        VTYPE2( I ) = VTYPE3D( I )
    END DO


    WRITE( *,'( 5X , 3A, /, 100( 1X, I3, 2X, 5A, /  ) )' )          &
           'The list of variables in file "', TRIM(FILEA), '" is:', &
           ( L, VNAME1( L )( 1:VMAX ) , ' (' ,                      &
                UNITS1( L )( 1:UMAX ) , '):  ' ,                    &
                VDESC1( L )( 1:DMAX ), L = 1, NVARS1 )
    I = GETNUM( 1, NVARS1, 1, 'Enter number for first variable' )
    WNAMES( 1 ) = VNAME1( I )
    WTYPES( 1 ) = VTYPE1( I )

    IF ( FILEB .NE. FILEA ) THEN
        WRITE( *,'( 5X , 3A, /, 100( 1X, I3, 2X, 5A, /  ) )' )      &
           'The list of variables in file "', TRIM(FILEB), '" is:', &
            ( L, VNAME2( L )( 1:VMAX ) , ' (' ,                     &
                 UNITS2( L )( 1:UMAX ) , '):  ' ,                   &
                 VDESC2( L )( 1:DMAX ), L = 1, NVARS2 )
    END IF
    J = GETNUM( 1, NVARS1, 1, 'Enter number for second variable' )

    WNAMES( 2 ) = VNAME2( J )
    WTYPES( 2 ) = VTYPE2( J )

    !!.......   Get mode of operation:

    NSTEPS = -1         !  magic number -- "not yet set"

    IF ( TSTEPA .EQ. 0 ) THEN
        SDATEA = 0
        STIMEA = 0
        NSTEPS = 1
    ELSE
        SDATEA = GETNUM( SDATEA, 9999999, SDATEA, 'Enter FILE A starting date (YYYYDDD) for run' )
        STIMEA = GETNUM(      0,  239999, STIMEA, 'Enter FILE A starting time (HHMMSS) for run' )
    END IF          !  time-independent file, or not

    IF ( TSTEPB .EQ. 0 ) THEN
        SDATEB = 0
        STIMEB = 0
        NSTEPS = 1
    ELSE
        SDATEB = GETNUM( SDATEB, 9999999, SDATEB, 'Enter FILE B starting date (YYYYDDD) for run' )
        STIMEB = GETNUM(      0,  239999, STIMEB, 'Enter FILE B starting time (HHMMSS) for run' )
    END IF

    IF ( NSTEPS .EQ. -1 ) THEN  !  "not yet set"

        RUNLEN = GETNUM( 0, 999999999, RUNLEN, 'Enter duration (HHMMSS) for run' )
        NSTEPS = TIME2SEC( TSTEP )
        NSTEPS = ( TIME2SEC( RUNLEN ) + NSTEPS - 1 ) / NSTEPS

    END IF          !  default or manually-selected analysis

    CLO = GETNUM( 1,   NCOLS, 1,     'Enter min for column range' )
    CHI = GETNUM( CLO, NCOLS, NCOLS, 'Enter MAX for column range' )

    RLO = GETNUM( 1,   NROWS, 1,     'Enter min for row range' )
    RHI = GETNUM( RLO, NROWS, NROWS, 'Enter MAX for row range' )

    IF ( NLAYS .EQ. 1 ) THEN
        LLO = 1
        LHI = 1
    ELSE
        LLO = GETNUM( 1,   NLAYS, 1,     'Enter min for layer range' )
        LHI = GETNUM( LLO, NLAYS, NLAYS, 'Enter MAX for layer range' )
    END IF


    !!.......   Process this period in the input file:

    CALL M3MESG( 'Processing . . .' )

    IF ( RDEV .LT. 0 ) RDEV = LOGDEV

    JDATEA = SDATEA
    JTIMEA = STIMEA
    JDATEB = SDATEB
    JTIMEB = STIMEB

    DO  J = 1, NSTEPS

        CALL PAIRSTEP( )

        CALL NEXTIME( JDATEA, JTIMEA, TSTEP )
        CALL NEXTIME( JDATEB, JTIMEB, TSTEP )

    END DO        !  end loop on time steps


    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  PAIRSTEP()

        IMPLICIT NONE

        CHARACTER*1, PARAMETER :: COMMA = ','

        !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

        REAL             GRID1( NCOLS, NROWS, NLAYS )
        REAL             GRID2( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE1( NCOLS, NROWS, NLAYS )
        DOUBLE PRECISION DBLE2( NCOLS, NROWS, NLAYS )
        INTEGER          INTG1( NCOLS, NROWS, NLAYS )
        INTEGER          INTG2( NCOLS, NROWS, NLAYS )
        INTEGER          LONG1( NCOLS, NROWS, NLAYS )
        INTEGER*8        LONG2( NCOLS, NROWS, NLAYS )

        LOGICAL         FLAG1, FLAG2

        INTEGER         C, R, L      !  col, row, level, variable, counters
        CHARACTER*256   MESG

        INTEGER         SIZE

        !!***********************************************************************
        !!   begin body of subroutine  PAIRSTEP

        SIZE = NCOLS * NROWS * NLAYS

        IF ( WTYPES( 1 ) .EQ. M3REAL ) THEN

            FLAG1 = READ3( FILEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, GRID1 )

        ELSE IF ( WTYPES( 1 ) .EQ. M3DBLE ) THEN

            FLAG1 = READ3( FILEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, DBLE1 )
            CALL DBLE2REAL( SIZE, DBLE1, GRID1 )

        ELSE IF ( WTYPES( 1 ) .EQ. M3INT ) THEN

            FLAG1 = READ3( FILEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, INTG1 )
            CALL INTG2REAL( SIZE, INTG1, GRID1 )

        ELSE IF ( WTYPES( 1 ) .EQ. M3INT8 ) THEN

            FLAG1 = READ3( FILEA,  WNAMES( 1 ), ALLAYS3, JDATEA, JTIMEA, LONG1 )
            CALL INT82REAL( SIZE, LONG1, GRID1 )

        ELSE

            FLAG1 = .FALSE.

        END IF

        IF ( WTYPES( 2 ) .EQ. M3REAL ) THEN

            FLAG2 = READ3( FILEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, GRID2 )

        ELSE IF ( WTYPES( 2 ) .EQ. M3DBLE ) THEN

            FLAG2 = READ3( FILEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, DBLE2 )
            CALL DBLE2REAL( SIZE, DBLE2, GRID2 )

        ELSE IF ( WTYPES( 2 ) .EQ. M3INT ) THEN

            FLAG2 = READ3( FILEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, INTG2 )
            CALL INTG2REAL( SIZE, INTG2, GRID2 )

        ELSE IF ( WTYPES( 2 ) .EQ. M3INT8 ) THEN

            FLAG2 = READ3( FILEB,  WNAMES( 2 ), ALLAYS3, JDATEA, JTIMEA, LONG2 )
            CALL INT82REAL( SIZE, LONG2, GRID2 )

        ELSE

            FLAG2 = .FALSE.

        END IF

        IF ( .NOT. FLAG1 .OR. .NOT. FLAG2 ) THEN

            EFLAG = .TRUE.

        ELSE            !  both reads OK

            DO  L = LLO, LHI   !  3-D traversal:  selected layers
            DO  R = RLO, RHI
            DO  C = CLO, CHI
                WRITE( RDEV, '( 1X, 1PE15.7,        A, 1X, 1Pe15.7 )' )  &
                                    GRID1( C,R,L ), COMMA, GRID2( C,R,L )
            END DO
            END DO
            END DO

        END IF          !  if !flag1, else if !flag2, else...

        RETURN

    END  SUBROUTINE  PAIRSTEP


END PROGRAM  M3PAIR


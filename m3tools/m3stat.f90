
PROGRAM  M3STAT

    !!***********************************************************************
    !! Version "$Id: m3stat.f90 280 2025-04-12 15:34:39Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002,2005-2013,2017,2021- Carlie J. Coats, Jr.,
    !! and (C) 2002-2011 Baron Advanced Meteorological Systems. LLC.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  113
    !!
    !!  FUNCTION:
    !!       Compute statistics for a user-specified GRIDDED, BOUNDARY,
    !!       CUSTOM. IDDATA, or SPARSE-MATRIX Models-3 file and list of
    !!       variables within it.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!       consistency with FORIO:PARMS3.EXT for name and description lengths.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       GETEFILE, GETNUM, GETREAL, GETYN, NEXTIME,
    !!       STATGRID, STATSPARS
    !!       Models-3 I/O.
    !!
    !!  REVISION  HISTORY:
    !!      Prototype 3/1993 by CJC
    !!
    !!      Version   5/1995 by CJC:  new I/O API; non-gridded file types;
    !!      command line arguments
    !!
    !!      Modified  9/1999 by CJC for enhanced portability
    !!
    !!      Version 11/2001 by CJC for I/O API Version 2.1
    !!
    !!      Version 12/2003 by CJC:  SIZE bugfix for case BNDARY3  VARMODE
    !!
    !!       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!       USE M3UTILIO, and related changes.
    !!
    !!      Version 05/2011 by CJC:  STATACUST() arglist bugfix for case CUSTOM3  VARMODE
    !!
    !!      Version  09/2017 by CJC for I/O API v3.2:  Enhanced default RUNLEN
    !!
    !!      Version  06/2019 by CJC:  Bugfix for RUNLEN
    !!
    !!      Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0;
    !!      new loop-structure
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'M3STAT'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         ARGCNT  !  number of command-line args, from IARGC()
    INTEGER         LOGDEV  !  unit number for log file

    CHARACTER*256   ENVBUF  !  value from command line arguments
    CHARACTER*16    INAME   !  logical name of the first  input file

    CHARACTER*16    RNAME   !  logical name of report file
    INTEGER         RDEV    !  report-file unit number

    INTEGER         FTYPE   ! file type, from INAME header
    INTEGER         TSTEP   ! time step, from INAME header
    INTEGER         NCOLS   ! grid dimensions, from INAME header
    INTEGER         NROWS   ! grid dimensions, from INAME header
    INTEGER         NLAYS   ! grid dimensions, from INAME header
    INTEGER         NTHIK   ! bdry dimensions, from INAME header
    INTEGER         NVARS   !  number of vbles in INAME
    INTEGER         VTYPE ( MXVARS3 ) !  list of vble types (must be REAL)
    CHARACTER*16    VNAME ( MXVARS3 ) !  list of vble names, from user
    CHARACTER*16    UNITS ( MXVARS3 ) !  list of vble units
    CHARACTER*80    VDESC ( MXVARS3 ) !  list of vble descs
    INTEGER         NTHRES( MXVARS3 )  !  number of thresholds
    REAL            THRES ( 10, MXVARS3 ) !  lists of vble thresholds
    REAL            T       !  scratch threshold
    INTEGER         SDATE   !  starting date, from user
    INTEGER         STIME   !  starting time, from user
    INTEGER         EDATE   ! ending date
    INTEGER         ETIME   ! ending time
    INTEGER         JDATE   !  current date
    INTEGER         JTIME   !  current time
    INTEGER         RUNLEN  !  duration, HHMMSS from user
    INTEGER         NSTEPS  !  duration in TSTEPs
    INTEGER         I, J    !  scratch variables
    INTEGER         VMAX    !  string length for names
    INTEGER         UMAX    !  string length for units
    INTEGER         DMAX    !  string length for descriptions
    INTEGER         SIZE    !  size of data structure

    LOGICAL         VARMODE !  present stats by variable, then timestep
    LOGICAL         DEFAULT !  command-line flag:  do default analysis

    CHARACTER*256   MESG

    !!.........................................................................
    !!   begin body of program  M3STAT

    LOGDEV = INIT3()
    WRITE ( *, '( 5X, A )' ) ' ',                                       &
'Program M3STAT to compute statistics of selected variables from a',    &
'user-specified GRIDDED, BOUNDARY, CUSTOM, IDDATA, or  SPARSE-MATRIX',  &
'Models-3 file.',                                                       &
'You need to have assigned a logical name to the physical file name',   &
'of the input file, and optionally the report file, according to',      &
'Models-3 conventions, using the operation ',                           &
' ',                                                                    &
'   setenv <lname> <pname>.',                                           &
' ',                                                                    &
'You will have the choice of either the default analysis, which',       &
'computes statistics for the variables in the file, or customized',     &
'analysis in which you select lists of variables to be analyzed, and',  &
'the thresholds to be applied to each.',                                &
' ',                                                                    &
'USAGE:  m3stat [INFILE [REPORTFILE]] [DEFAULT]',                       &
'(and then answer the prompts).',                                       &
' ',                                                                    &
'Note that RUNLEN=0 for single-step runs ("fenceposts"...)',            &
' ',                                                                    &
'See URLs  https://cjcoats.github.io/ioapi/AA.html#tools or',           &
'  https://www.cmascenter.org/ioapi/documentation/all_versions/html/AA.html#tools', &
' ',                                                                    &
'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013, 2021-',           &
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
' ',                                                                    &
'Program version: ',                                                    &
'$Id:: m3stat.f90 280 2025-04-12 15:34:39Z coats                    $', &
' '

    ARGCNT = IARGC()

    IF ( ARGCNT .GE. 1 ) THEN
        CALL GETARG( ARGCNT, MESG )
        CALL LUSTR( MESG )
        IF ( 'DEFAULT '  .EQ. MESG( 1:8 ) .OR.&
             '-DEFAULT ' .EQ. MESG( 1:9 ) ) THEN
            DEFAULT = .TRUE.
            ARGCNT = ARGCNT - 1
        ELSE
            DEFAULT = .FALSE.
        END IF
    END IF

    IF ( ARGCNT .GT. 2 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'usage:  M3STAT [INFILE [REPORTFILE]]', 2 )
    END IF

    IF ( ARGCNT .EQ. 0 ) THEN   !  get names from user

    !!...........   Open input file

        INAME = PROMPTMFILE( 'Enter logical name for INPUT FILE', FSREAD3, 'INFILE', PNAME )

        MESG = 'Enter logical name for  REPORT FILE, or "NONE"'
        RDEV = PROMPTFFILE( MESG, .FALSE., .TRUE., 'REPORT', PNAME )

        IF ( RDEV .EQ. -2 ) RDEV = LOGDEV

    ELSE            !  else argcnt = 1 or 2:

        CALL GETARG( 1, ENVBUF )
        INAME = ENVBUF( 1:16 )
        IF ( .NOT. OPEN3( INAME, FSREAD3, PNAME ) ) THEN
            MESG =  'Could not open input file "'//TRIM(INAME)//'"'
            CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
        END IF

        IF ( ARGCNT .EQ. 1 ) THEN
            RDEV = LOGDEV
        ELSE
            CALL GETARG( 2, ENVBUF )
            RNAME = ENVBUF( 1:16 )
            RDEV  = GETEFILE( RNAME, .FALSE., .TRUE., PNAME )
            IF ( RDEV .LT. 0 ) THEN
                MESG = 'Could not open rpt file "'//TRIM(RNAME)//'"'
                CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
            END IF          !  if rdev < 0 (getefile() failed)
        END IF  !  if argcnt=1, or else 2

    END IF      !  if argcnt > 2, =0, or not


    !!...........   Get and save input file description:

    IF ( .NOT. DESC3( INAME ) ) THEN
        MESG = 'Could not DESC3(' // TRIM( INAME ) // ')'
        CALL M3EXIT( PNAME, 0, 0, MESG, 3 )
    END IF

    FTYPE  = FTYPE3D
    TSTEP  = TSTEP3D
    NCOLS  = NCOLS3D
    NROWS  = NROWS3D
    NLAYS  = NLAYS3D
    NTHIK  = NTHIK3D
    TSTEP  = TSTEP3D
    NVARS  = NVARS3D
    SDATE  = SDATE3D
    STIME  = STIME3D
    NSTEPS = MXREC3D
    CALL LASTTIME( SDATE, STIME, TSTEP, NSTEPS, EDATE, ETIME )


    !!.......   Copy variable-names.  Get max string-lengths for use in
    !!.......   variables-listing:

    VMAX = 0
    UMAX = 0
    DMAX = 0
    DO  I = 1, NVARS3D
        NTHRES( I ) = 0
        VTYPE ( I ) = VTYPE3D( I )
        VNAME ( I ) = VNAME3D( I )
        UNITS ( I ) = UNITS3D( I )
        VDESC ( I ) = VDESC3D( I )
        VMAX = MAX( VMAX , LEN_TRIM( VNAME3D( I ) ) )
        UMAX = MAX( UMAX , LEN_TRIM( UNITS3D( I ) ) )
        DMAX = MAX( DMAX , LEN_TRIM( VDESC3D( I ) ) )
    END DO

    DMAX = MIN( DMAX, 67 - VMAX - UMAX )


    !!.......   Set up specifications for the statistical analysis, dependent
    !!.......   upon DEFAULT and on file type:

    IF( DEFAULT ) THEN

        WRITE( LOGDEV,92000 )                           &
            ' ', 'The list of variables in file "'      &
            // TRIM( INAME ) // '" is:', ' ',           &
            ( VNAME( I )( 1:VMAX ) // ' (' //           &
              UNITS( I )( 1:UMAX ) // '): ' //          &
              VDESC( I )( 1:DMAX ), I = 1, NVARS3D  ), ' '

        VARMODE = .FALSE.

        IF ( TSTEP .EQ. 0 ) THEN    !  time-independent

            SDATE  = 0
            STIME  = 0
            NSTEPS = 1

        ELSE            ! time-dependent:  choose period for analysis

            SDATE  = SDATE3D
            STIME  = STIME3D
            NSTEPS = MXREC3D

        END IF          !  time-independent file, or not


    ELSE IF ( ( FTYPE .EQ. BNDARY3 )    &   !!  if default, else if...
         .OR. ( FTYPE .EQ. CUSTOM3 )    &
         .OR. ( FTYPE .EQ. GRDDED3 ) ) THEN

        IF ( GETYN( 'Do you want the default analysis?', .TRUE. ) ) THEN

            WRITE( LOGDEV,92000 )                           &
                ' ', 'The list of variables in file "'      &
                // TRIM( INAME ) // '" is:', ' ',           &
                ( VNAME( I )( 1:VMAX ) // ' (' //           &
                  UNITS( I )( 1:UMAX ) // '): ' //          &
                  VDESC( I )( 1:DMAX ), I = 1, NVARS  ), ' '

            VARMODE = ( TSTEP .EQ. 0 )

        ELSE            !  manually-selected analysis:

            NVARS = 0
            I     = 0
            T     = 0.0

111         CONTINUE        !  loop getting variables-list for analysis

            WRITE( *,92000 ) ' ',                       &
                'The list of variables in file "' // TRIM( INAME ) // '" is:'
            WRITE( *,92010 )                            &
                ( J,                                    &
                  VNAME( J )( 1:VMAX ) // ' (' //       &
                  UNITS( J )( 1:UMAX ) // '): ' //      &
                  VDESC( J )( 1:DMAX ), J = 1, NVARS3D  )

            I = GETNUM( 0, NVARS3D, 1 + MOD( I, NVARS3D ), 'Enter number for operand A (0 to end list)' )

            IF ( I .EQ. 0 ) GO TO  119      !  to end of loop

            NVARS = NVARS + 1

            VNAME( NVARS ) = VNAME3D( I )

            J = 0
112         CONTINUE    !  loop getting thresholds for this variable
            IF ( GETYN( 'Select a threshold?', .TRUE. ) ) THEN
                J = J + 1
                THRES( J,NVARS ) = GETREAL( -9.9E37, 9.9E37, T, 'Enter the desired threshold' )
                IF ( J .LT. 10 )  GO TO  112
            END IF

            NTHRES( NVARS ) = J

            IF ( NVARS .LT. MXVARS3 )  GO TO  111   !  to head of loop

119         CONTINUE        !  end loop getting variables-list for analysis

            IF ( NVARS .EQ. 0 ) THEN
                CALL M3WARN( PNAME, 0, 0, 'No variables selected' )
                GO TO  999
            END IF


            !!...........   Get mode of operation:

            IF ( TSTEP .EQ. 0 ) THEN
                VARMODE = .TRUE.
            ELSE IF ( NVARS .GT. 1 ) THEN
                WRITE( *,92000 ) ' ',                                               &
                    'You have the options of either separate time series for ',     &
                    'each variable,or a joint time series for all variables',       &
                    'simultaneously.'
                VARMODE = GETYN( 'Separate time series for each variable?', .TRUE. )
            ELSE
                VARMODE = .FALSE.
            END IF          !  time-independent file, or not

        END IF          !  default or manually-selected analysis

    ELSE IF ( FTYPE .EQ. IDDATA3 ) THEN

        VARMODE = .FALSE.

        IF ( GETYN( 'Do you want the default analysis?', .TRUE. ) ) THEN

            WRITE( LOGDEV,92000 )                           &
                ' ', 'The list of variables in file "'      &
                // TRIM( INAME ) // '" is:', ' ',           &
                ( VNAME( I )( 1:VMAX ) // ' (' //           &
                  UNITS( I )( 1:UMAX ) // '): ' //          &
                  VDESC( I )( 1:DMAX ), I = 1, NVARS  ), ' '

        ELSE            !  manually-selected analysis:

            NVARS = 0
            I     = 0
            T     = 0.0

121         CONTINUE        !  loop getting variables-list for analysis

            WRITE( *,92000 ) ' ', 'The list of variables in file "' // TRIM( INAME ) // '" is:'
            WRITE( *,92010 )                                &
                ( J,                                        &
                  VNAME( J )( 1:VMAX ) // ' (' //           &
                  UNITS( J )( 1:UMAX ) // '): '//           &
                  VDESC( J )( 1:DMAX ), J = 1, NVARS3D  )

            I = GETNUM( 0, NVARS3D, 1 + MOD( I, NVARS3D ),  &
                        'Enter number for variable  for thresholding (0 to end)' )

            IF ( I .EQ. 0 ) GO TO  129      !  to end of loop

            NVARS = NVARS + 1

            VNAME( NVARS ) = VNAME3D( I )

            J = 0
122         CONTINUE    !  loop getting thresholds for this variable

            IF ( GETYN( 'Select a threshold?', .TRUE. ) ) THEN
                J = J + 1
                THRES( J,NVARS ) = GETREAL( -9.9E37, 9.9E37, T,     &
                                           'Enter the desired threshold' )
                IF ( J .LT. 10 )  GO TO  122
            END IF

            NTHRES( NVARS ) = J

            IF ( NVARS .LT. MXVARS3 )  GO TO  121   !  to head of loop

129         CONTINUE        !  end loop getting variables-list for analysis

            IF ( NVARS .EQ. 0 ) THEN
                CALL M3WARN( PNAME, 0, 0, 'No variables selected' )
                GO TO  999
            END IF

        END IF          !  default or manually-selected analysis

    ELSE IF ( FTYPE .EQ. SMATRX3 ) THEN

        WRITE( LOGDEV,92000 )                               &
                ' ', 'The list of variables in file "'      &
                // TRIM( INAME ) // '" is:', ' ',           &
                ( VNAME( I )( 1:VMAX ) // ' (' //           &
                  UNITS( I )( 1:UMAX ) // '): ' //          &
                  VDESC( I )( 1:DMAX ), I = 1, NVARS  ), ' '

        VARMODE = .FALSE.
        NVARS   =  NVARS3D

        DO J = 1, NVARS
            VNAME  ( J ) = VNAME3D( J )
        END DO

    ELSE   !  ftype not bndary3, custom3, grdded3, iddata3, nor smatrx3

        WRITE( MESG,94010 )&
            'Input file "'// TRIM( INAME ) // '" has unsupported type:', FTYPE
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    END IF  !  if defaults; else if ftype is ...


    !!...........   Get time period studied:

    IF ( TSTEP .EQ. 0 ) THEN    !  time-independent

        SDATE  = 0
        STIME  = 0
        NSTEPS = 1

    ELSE IF ( .NOT. DEFAULT ) THEN

        SDATE = GETNUM( SDATE3D, 9999999, SDATE3D, 'Enter starting date (YYYYDDD) for run' )
        STIME = GETNUM(       0,  239999, STIME3D, 'Enter starting time  (HHMMSS) for run' )
        RUNLEN = SEC2TIME( SECSDIFF( SDATE, STIME, EDATE, ETIME ) )
        RUNLEN = GETNUM( 0, 999999999, RUNLEN, 'Enter duration (HHMMSS) for run' )
        JDATE = SDATE
        JTIME = STIME
        CALL NEXTIME( JDATE, JTIME, RUNLEN )
        NSTEPS = CURREC( JDATE,JTIME,SDATE,STIME,TSTEP,EDATE,ETIME )

    END IF          !  time-independent file, or not


    !!...........   Perform the analysis:
    !!...........   process this period in the input file:

    IF ( RDEV .LT. 0 )  RDEV = LOGDEV
    IF ( VARMODE ) THEN
    
        DO  I = 1, NVARS

            JDATE = SDATE
            JTIME = STIME
            
            DO J = 1, NSTEPS
            
                IF ( FTYPE .EQ. GRDDED3 ) THEN

                    CALL STATGRID( NCOLS, NROWS, NLAYS, 1,      &
                                   JDATE, JTIME,                &
                                   NTHRES( I ), THRES( 1,I ),   &
                                   INAME, VNAME( I ),           &
                                   VTYPE( I ), RDEV )
                
                ELSE IF ( FTYPE .EQ. BNDARY3 ) THEN

                    CALL STATBDRY( SIZE, NCOLS, NROWS, NLAYS, NTHIK,    &
                                   1, JDATE, JTIME,                     &
                                   NTHRES( I ), THRES( 1,I ),           &
                                   INAME, VNAME( I ),                   &
                                   VTYPE( I ), RDEV )
                
                ELSE IF ( FTYPE .EQ. CUSTOM3 ) THEN

                    CALL STATCUST( NCOLS, NLAYS, 1,                 &
                                   JDATE, JTIME,                    &
                                   NTHRES( I ), THRES( 1,I ),       &
                                   INAME, VNAME( I ),               &
                                   VTYPE( I ), RDEV )
                
                END IF      !  if ftype is gridded; else boundary, or...

                CALL NEXTIME( JDATE, JTIME, TSTEP )
                
            END DO
            
        END DO

    ELSE        !!  all-variables analysis:

        JDATE = SDATE
        JTIME = STIME

        DO  I = 1, NSTEPS
        
            IF ( FTYPE .EQ. GRDDED3 ) THEN

                CALL STATGRID( NCOLS, NROWS, NLAYS, NVARS,      &
                               JDATE, JTIME, NTHRES, THRES,     &
                               INAME, VNAME, VTYPE, RDEV )

            ELSE IF ( FTYPE .EQ. BNDARY3 ) THEN

                SIZE = NCOLS + NROWS + NLAYS + 2*NTHIK
                SIZE = ABS( 2*NTHIK )*SIZE

                CALL STATBDRY( SIZE, NCOLS, NROWS, NLAYS, NTHIK,    &
                               NVARS, JDATE, JTIME, NTHRES, THRES,  &
                               INAME, VNAME, VTYPE, RDEV )

            ELSE IF ( FTYPE .EQ. CUSTOM3 ) THEN

                CALL STATCUST( NCOLS, NLAYS, NVARS,                 &
                               JDATE, JTIME, NTHRES, THRES,         &
                               INAME, VNAME, VTYPE, RDEV )

            ELSE IF ( FTYPE .EQ. IDDATA3 ) THEN

                CALL STATIDDAT( NROWS, NLAYS, NVARS,                &
                                JDATE, JTIME, NTHRES, THRES,        &
                                INAME, VNAME, VTYPE, RDEV )

            ELSE IF ( FTYPE .EQ. SMATRX3 ) THEN

                CALL STATSPARS( NCOLS, NROWS, NLAYS, NVARS,         &
                                JDATE, JTIME,                       &
                                INAME, VNAME, VTYPE, RDEV )

            END IF      !  if ftype is gridded; else boundary, or...

            CALL NEXTIME( JDATE, JTIME, TSTEP )

        END DO        !  end loop on time steps

    END IF          !  if per-variable mode, or not


999 CONTINUE        !  end of program

    CALL M3EXIT( PNAME, 0, 0, 'Program  M3STAT  completed successfully', 0 )


    !!******************  FORMAT  STATEMENTS   ******************************

    !!...........   Informational (LOG) message formats... 92xxx

92000 FORMAT ( 5X , A )

92010 FORMAT ( /, I4, ':  ', A )


    !!...........   Internal buffering formats............ 94xxx

94010 FORMAT ( A, 2( I4, :, A ) )


END PROGRAM M3STAT


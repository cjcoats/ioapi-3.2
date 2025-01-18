
LOGICAL FUNCTION DDTVAR3( FNAME, VNAME, CALLER,             &
                          JDATE, JTIME, RSIZE, BUFFER )

    !***********************************************************************
    ! Version "$Id: ddtvar3.F 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014-2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  DDTVAR3 function body starts at line  151
    !
    !  NOTE:  MACHINE-DEPENDENT CODE !!!  depends upon FORTRAN : C
    !       coupling between DDTVAR3() and BUFDDT3()
    !
    !  FUNCTION:  calculates time-derivative for data from Models-3 file
    !       with logical name FNAME, variable VNAME for all layers for
    !       date and time JDATE (coded YYYYDDD) and time JTIME (HHMMSS),
    !	putting the result in BUFFER.
    !       File type must be CUSTOM, GRIDDED, or BOUNDARY.
    !       For time-independent files, JDATE:JTIME are ignored (derivative
    !	is identically zero).
    !       For JDATE:JTIME an exact time step of file FNAME, returns
    !       the LEFT-HANDED DERIVATIVE.
    !       Encapsulates file opens and reads, and all the book-keeping
    !       and data structures necessary for that task.
    !
    !  USES C routine BUFDDT3:  sets up all the data structures for the
    !       call so that BUFDDT3 can allocate and manage read-buffers
    !       internally, call NCVGT and perform the interpolation.
    !       (Actually, DDTVAR3() + BUFDDT3() should be considered as
    !       two parts of a single routine)
    !
    !  RETURN VALUE:  TRUE iff the operation succeeds (and the data is available)
    !
    !  PRECONDITIONS REQUIRED:  FNAME is a Models-3 data file containing
    !       REAL variable VNAME for a set of time steps enclosing JDATE:JTIME
    !       File type must be CUSTOM3, GRDDED3, or BNDARY3.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       BUFDDT3, CURRSTEP, INDEX1, JSTEP3, M3WARN, NEXTIME, OPEN3,
    !       SECSDIFF, TIME2SEC
    !
    !  REVISION  HISTORY:
    !       prototype 7/1994 by CJC
    !       modified 10/1994 by CJC for new WRITE3 -- granularity at the
    !       level of individual variables.  FNAME, VNAME are now CHAR*(*).
    !       modified  6/1995 by CJC to check VTYPE3( VID,FID )
    !       Modified  5/1998 by CJC for OpenMP thread-safety
    !       Modified  5/1999 by CJC and ALT for coupling-mode operation
    !       Modified  1/2002 by CJC:  check TRIMLEN() of FNAME
    !       Modified  3/2002 by CJC:  uses RDTFLAG()
    !       Modified  5/2002 by CJC:  support for DOUBLE variables
    !       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !       associated with INIT3() and OPEN3()
    !       Modified 10/2003 by CJC for I/O API version 3:  support for
    !       native-binary BINFIL3 file type; uses INTEGER NAME2FID
    !       Modified  8/17/2004 by CJC:  deal with virtual-file bug.
    !       Modified 11/2004 by CJC:  new "verbose-flag" argument to RDTFLAG
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified 08/2015 by CJC: eliminate unused NETCDF.EXT;
    !       support for type MPIGRD3 for MPI/PnetCDF distributed I/O
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'
#ifdef IOAPICPL
    INCLUDE 'STATE3V.EXT'
#endif

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME           !  logical file name
    CHARACTER*(*), INTENT(IN   ) :: VNAME           !  variable name, or 'ALL'
    CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of caller
    INTEGER      , INTENT(IN   ) :: JDATE           !  date, formatted YYYYDDD
    INTEGER      , INTENT(IN   ) :: JTIME           !  time, formatted HHMMSS
    INTEGER      , INTENT(IN   ) :: RSIZE           !  record dim, words, for error-check
    REAL         , INTENT(  OUT) :: BUFFER( RSIZE ) !  interpolation-output buffer array


    !...........   PARAMETER :

    CHARACTER*16, PARAMETER :: TYPNAMES( -4 : 5 ) = &
       (/ 'UNKNOWN',        &    !  -4:  file type error
          'KFEVNT3',        &    !  -3:  file type error:: KF 
          'DGRAPH3',        &    !  -2:  known file types
          'CUSTOM3',        &    !  -1:    "
          'DCTNRY3',        &    !   0:    "
          'GRDDED3',        &    !   1:    "
          'BNDARY3',        &    !   2:    "
          'IDDATA3',        &    !   3:    "
          'PROFIL3',        &    !   4:    "
          'GRNEST3' /)           !   5:    "


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INDEX1, SECSDIFF, TIME2SEC
    LOGICAL, EXTERNAL :: CURRSTEP, OPEN3
    INTEGER, EXTERNAL :: BUFDDT3         !  ddt() from circ. bufs. (C)
    INTEGER, EXTERNAL :: BUFVGT3         !  alloc/read for circ. bufs. (C)
    INTEGER, EXTERNAL :: NAME2FID        !  fname~~> fid lookup
    LOGICAL, EXTERNAL :: RDTFLAG         !  get date&time availability; rec #

#ifdef IOAPICPL
    REAL            P, Q
    LOGICAL, EXTERNAL :: INTPQV
#endif

    !...........   STATE VARIABLES:

    LOGICAL, SAVE :: FIRSTIME = .TRUE.


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         IDUM            !  scratch variable
    INTEGER         FID             !  file subscript for STATE3 arrays
    INTEGER         VID             !  variable subscript for STATE3 arrays
    INTEGER         FLEN, VLEN      !  name lengths for file, vble
    INTEGER         TDIM            !  subscript for time step
    INTEGER         STEP            !  time step record number
    INTEGER         DT              !  seconds in TSTEP3
    INTEGER         MDATE, MTIME    !  date:time args for CURRSTEP
    INTEGER         DTJL            !  difference LDATE:LTIME to JDATE:JTIME
    INTEGER         DTNJ            !  difference JDATE:JTIME to NDATE:NTIME
    CHARACTER*24    NAMBUF          !  buffer for names (CRAY-PORTING HACK)
    INTEGER         DELTA           !  d(INDX) / d(NCVGTcall)
    INTEGER         DIMS( 5 )       !  hyperslab corner   for NCVGT()
    INTEGER         DELS( 5 )       !  hyperslab diagonal for NCVGT()
    INTEGER         DATE1, DATE2    !
    INTEGER         TIME1, TIME2    !
    LOGICAL         RFLAG           !  BUFDDT3() read-data flag
    LOGICAL         EFLAG           !  error flag
    CHARACTER*256   MESG            !  for m3warn(), etc


    !***********************************************************************
    !   begin body of function  DDTVAR3

    !...........   Check length of name arguments; copy into length=16 buffers

    EFLAG = .FALSE.
    FLEN  = LEN_TRIM( FNAME )
    VLEN  = LEN_TRIM( VNAME )

    IF ( FLEN .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A , I10 )' ) 'Max file name length 16; actual:', FLEN
        CALL M3MSG2( MESG )
    END IF          !  if len( fname ) > 16

    IF ( VLEN .GT. NAMLEN3 ) THEN
        EFLAG = .TRUE.
        WRITE( MESG, '( A, I10 )'  ) 'Max vble name length 16; actual:', VLEN
        CALL M3MSG2( MESG )
    END IF          !  if len( vname ) > 16

    IF ( EFLAG ) THEN
        MESG  = 'File "' //FNAME// '" Variable "' //VNAME//'"'
        CALL M3MSG2( MESG )
        MESG = 'Invalid variable or file name arguments'
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
        DDTVAR3 = .FALSE.
        RETURN
    END IF          !  if len( fname ) > 16, or if len( vname ) > 16


    !.......   Find netCDF index for the file, and check time step availability:

    !$OMP CRITICAL( S_INTERP )

    FID = NAME2FID( FNAME )

    IF ( FID .EQ. 0 ) THEN  !  file needs opening

        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        IF ( .NOT.OPEN3( FNAME, FSREAD3, NAMBUF ) ) THEN

            MESG = 'Could not open ' // FNAME
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO 1

        END IF      !  if open3() failed

        FID = INDEX1( FNAME, COUNT3, FLIST3 )

    END IF          !  if file needs opening

1   CONTINUE

    !$OMP END CRITICAL( S_INTERP )

    IF ( EFLAG ) THEN
        DDTVAR3 = .FALSE.
        RETURN
    END IF

    IDUM = FTYPE3( FID )
    IF ( IDUM .EQ. CUSTOM3 ) THEN

        DIMS( 1 ) = 1
        DELS( 1 ) = NCOLS3( FID )

        DIMS( 2 ) = 1
        DELS( 2 ) = NLAYS3( FID )

        TDIM  = 3

    ELSE IF ( IDUM .EQ. GRDDED3 .OR. IDUM .EQ. MPIGRD3 ) THEN

        DIMS( 1 ) = 1
        DELS( 1 ) = NCOLS3( FID )

        DIMS( 2 ) = 1
        DELS( 2 ) = NROWS3( FID )

        DIMS( 3 ) = 1
        DELS( 3 ) = NLAYS3( FID )

        TDIM  = 4

    ELSE IF ( IDUM .EQ. BNDARY3 ) THEN

        DELTA = 2 * NTHIK3( FID )
        DELTA = DELTA*( NCOLS3( FID ) + NROWS3( FID ) + DELTA )

        DIMS( 1 ) = 1
        DELS( 1 ) = DELTA

        DIMS( 2 ) = 1
        DELS( 2 ) = NLAYS3( FID )

        TDIM  = 3

    ELSE        !  bad file type for DDTVAR3()

        IF ( IDUM .LT. -3   .OR.  IDUM .GT. 5 ) IDUM = -4
        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        MESG   = 'Can not interpolate:  file "' //&
            FNAME( 1:FLEN ) //&
            '" of type ' // TYPNAMES( IDUM )
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

        DDTVAR3 = .FALSE.
        RETURN

    END IF              !  check file types; set DIMS, DELS, TDIM

    IF ( RSIZE .NE. NLAYS3( FID ) * BSIZE3( FID ) ) THEN

        WRITE( MESG, "( 5 A, I7, A, I7, :, A )" )   &
            'Size error for "', VNAME( 1:VLEN ),    &
            '" from "',         FNAME( 1:FLEN ),    &
            '--REQ:', RSIZE, ' ACT:', NLAYS3( FID ) * BSIZE3( FID )
        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
        DDTVAR3 = .FALSE.
        RETURN

    END IF          !  requested record-size in error


    !.......   Find index for variable, and if necessary allocate circular
    !.......   buffers for interpolation, and initialize related data structures.

    VID   = INDEX1( VNAME, NVARS3( FID ), VLIST3( 1,FID ) )

    IF ( VID .EQ. 0 ) THEN  !  variable not available in that file

        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        MESG = 'Variable "' // VNAME( 1:VLEN ) // '" not in file "' // FNAME( 1:FLEN ) // '"'
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
        DDTVAR3 = .FALSE.
        RETURN

    ELSE IF ( VTYPE3( VID,FID ) .NE. M3REAL  .AND.      &
              VTYPE3( VID,FID ) .NE. M3DBLE ) THEN  ! variable of wrong type

        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        MESG =  'Variable "' // VNAME( 1:VLEN ) // '" not of type REAL nor DOUBLE in file "' //&
            FNAME( 1:FLEN ) // '"'
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
        DDTVAR3 = .FALSE.
        RETURN

    END IF    !  if variable not available; else if not already set up


#ifdef IOAPICPL
    IF ( CDFID3( FID ) .EQ. VIRFIL3 ) THEN !  virtual file
        EFLAG = (.NOT. INTPQV( FID, VID, JDATE, JTIME, P, Q ) )
        GO TO   1001
    END IF
#endif


    !.......   Interpolate/copy to JDATE:JTIME

    !$OMP       CRITICAL( S_INTERP )        !!  avoid race conditions for
    !!  concurrent same-variable calls
    !!  to INTERP3(), etc.

    IF ( TSTEP3( FID ) .EQ. 0 ) THEN        !  time-independent file

        DO  11  IDUM = 1, RSIZE
            BUFFER( IDUM ) = 0.0
11      CONTINUE
        GO TO 999

    END IF    !  if time-independent; else if virtual file


    IF ( CDFID3( FID ) .EQ. BUFFIL3 ) THEN	!  "buffered" file

        RFLAG = .FALSE.

        IF( LDATE3( VID,FID ) .LT. 0 ) THEN
            MESG = 'Variable "' // VNAME( 1:VLEN ) // '" not yet written to BUFFERED file "' // FNAME( 1:FLEN ) // '".'
            NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF

        DT   = TIME2SEC( TSTEP3( FID ) )
        DTJL = SECSDIFF( LDATE3( VID,FID ), LTIME3( VID,FID ),  &
                         JDATE,             JTIME )

        IF ( DTJL .LT. 0 ) THEN	!  ldate:ltime not OK for interp

            NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
            WRITE( MESG, "( I7.7, ':', I6.6, 5A, I7.7, ':', I6.6, A, I8, A )" ) &
                    JDATE, JTIME, ' n/a for ', VNAME( 1:VLEN ),                 &
                    ' from '   , FNAME( 1:FLEN ),                               &
                    ' (', SDATE3( FID ), STIME3( FID ), ' by', TSTEP3( FID ), ')'
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            EFLAG = .TRUE.
            GO TO 999

        ELSE IF ( DTJL .GT. 0 ) THEN !  interp between ld:lt and nd:nt

            DTNJ = SECSDIFF( JDATE,             JTIME,                          &
                             NDATE3( VID,FID ), NTIME3( VID,FID ) )
            IF ( ( DTNJ .LT. 0 ) .OR. DTNJ .GT. DT ) THEN
                WRITE( MESG, "( I7.7, ':', I6.6, 5A, I7.7, ':', I6.6, A, I8, A )" )&
                    JDATE, JTIME, ' n/a for ', VNAME( 1:VLEN ), ' from '   , FNAME( 1:FLEN ), &
                    ' (', SDATE3( FID ), STIME3( FID ), ' by', TSTEP3( FID ), ')'
                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                EFLAG = .TRUE.
                GO TO 999

            END IF	!  if ndate:ntime not OK for interp

        END IF	!  if dtjl < 0; else if dtjl > 0 (just copy if dtjl=0)

    ELSE            !  time-stepped file.

        DT = TIME2SEC( TSTEP3( FID ) )

    !...........   Check to see relationship between currently circular buffer
    !...........   start and ending times, and requested time.  Two cases
    !...........   require updates:  (1) need new data at both ends; and
    !...........   (2) need new data at the futureward end only. (Needing
    !...........   data at the pastward end is still treated under (1).)

        IF ( LDATE3( VID,FID ) .GT. 0 ) THEN
            DTJL = SECSDIFF( LDATE3( VID,FID ), LTIME3( VID,FID ),  &
                             JDATE,             JTIME )
        ELSE                !  ldate set to "invalid"
            DTJL = IMISS3   !  dtjl "invalid"
        END IF

        IF ( ( DTJL .LT. 0 ) .OR. ( DTJL .GT. 2*DT ) ) THEN ! read both

            IF ( CURRSTEP( JDATE, JTIME,                                    &
                           SDATE3( FID ), STIME3( FID ), TSTEP3( FID ),     &
                           MDATE, MTIME ) ) THEN

                RFLAG = .TRUE.

                DATE1 = MDATE
                DATE2 = MDATE
                TIME1 = MTIME
                TIME2 = MTIME
                CALL NEXTIME( DATE2, TIME2, TSTEP3( FID ) )

                !  **NOTE** --  use DATE1:TIME1 "step" in DIMS(:) below.

                IF ( .NOT. RDTFLAG( FID, VID,           &
                                    DATE2, TIME2,       &
                                    STEP, .TRUE. ) ) THEN

                    MESG = 'Time step not available for ' // VNAME( 1:VLEN ) // ' from ' // FNAME
                    CALL M3WARN( NAMBUF, DATE2, TIME2, MESG )
                    EFLAG   = .TRUE.
                    GO TO 999

                ELSE IF ( .NOT. RDTFLAG( FID, VID,          &
                                         DATE1, TIME1,      &
                                         STEP, .TRUE. ) ) THEN

                    MESG = 'Time step not available for ' // VNAME( 1:VLEN ) // ' from ' // FNAME
                    CALL M3WARN( NAMBUF, DATE1, TIME1, MESG )
                    EFLAG   = .TRUE.
                    GO TO 999

                END IF      !  end checking time step flags.

                DELTA =  0                      !  read starts at 2
                ILAST3( VID, FID ) = 0          !  buffer starts at 0
                LDATE3( VID, FID ) = MDATE
                LTIME3( VID, FID ) = MTIME
                DELS( TDIM ) = 2                !  read BOTH time steps
                NDATE3( VID,FID ) = DATE2
                NTIME3( VID,FID ) = TIME2

                DTJL = SECSDIFF( MDATE, MTIME, JDATE, JTIME )

                IF ( TSTEP3( FID ) .LT. 0 ) THEN
                    DIMS( TDIM ) = 1 + MOD( STEP-1, 2 )
                ELSE
                    DIMS( TDIM ) = STEP
                END IF

            ELSE

                NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
                WRITE( MESG, "( I7.7, ':', I6.6, 4A, I9.7, ':', I6.6, A, I9 )" )                &
                    JDATE, JTIME, ' n/a for ', VNAME( 1:VLEN ), ' from ' , FNAME( 1:FLEN ),     &
                    SDATE3( FID ), STIME3( FID ), '  by', TSTEP3( FID )
                CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
                EFLAG   = .TRUE.
                GO TO 999

            END IF          !  if requested date&time before start of file

        ELSE IF ( DTJL .GE. DT ) THEN      !  advance by one time step

            DATE1 = NDATE3( VID, FID )
            TIME1 = NTIME3( VID, FID )
            CALL NEXTIME( DATE1, TIME1, TSTEP3( FID ) )

            IF ( .NOT. RDTFLAG( FID, VID, DATE1, TIME1, STEP, .TRUE. ) ) THEN

                MESG = 'Time step not available for "' // VNAME( 1:VLEN ) // '" from ' // FNAME
                CALL M3WARN( NAMBUF, DATE1, TIME1, MESG )
                EFLAG   = .TRUE.
                GO TO 999

            END IF

            DELTA = ILAST3( VID,FID )        !  overwrite ILAST entry
            ILAST3( VID,FID ) = 1 - DELTA    !  new ILAST after read:
            !  formula swaps 0,1
            DTJL  = DTJL - DT
            RFLAG = .TRUE.
            DELS( TDIM ) = 1                      !  read one time

            LDATE3( VID,FID ) = NDATE3( VID, FID )
            LTIME3( VID,FID ) = NTIME3( VID, FID )
            NDATE3( VID,FID ) = DATE1
            NTIME3( VID,FID ) = TIME1

            IF ( TSTEP3( FID ) .LT. 0 ) THEN
                DIMS( TDIM ) = 1 + MOD( STEP-1, 2 )
            ELSE
                DIMS( TDIM ) = STEP
            END IF

        ELSE        !  do not need to read this variable from this file:

            RFLAG = .FALSE.

        END IF      !  if circular-buffer structure needed updating

    END IF          ! time stepped file or not

999 CONTINUE


    !..........   If read will be necessary, check timestep flags first:

    IF ( RFLAG .AND. .NOT. EFLAG ) THEN       !  check record-available flag

        IF ( 0 .EQ. BUFVGT3( FID, VID, RFLAG, DIMS, DELS,       &
                             NLAYS3( FID ) * BSIZE3( FID ),     &
                             DELTA, TSTEP3( FID ) ) ) THEN

            WRITE( MESG, "( 4A, 2( A, I9.7, ':', I6.6 ) )" )    &
                'Error reading ', VNAME( 1:VLEN ),              &
                ' from ',         FNAME( 1:FLEN ),              &
                '--LAST',  LDATE3( VID,FID ), LTIME3( VID,FID ),&
                '; NEXT',  NDATE3( VID,FID ), NTIME3( VID,FID )
            CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )
            EFLAG = .TRUE.

        END IF              !  if bufvgt() fails

    END IF          !  if RFLAG:  read will be necessary

    !$OMP   END CRITICAL( S_INTERP )

1001 CONTINUE
    IF ( EFLAG ) THEN
        DDTVAR3 = .FALSE.
        RETURN
    END IF


    !...........   Call BUFDDT3 to read  compute difference divided by DT;
    !...........   check its success/failure status and return accordingly:

    IF ( 0 .NE. BUFDDT3( FID, VID, NLAYS3( FID ) * BSIZE3( FID ),   &
                         ILAST3( VID,FID ), TSTEP3( FID ),          &
                         1.0 / FLOAT( TIME2SEC( TSTEP3( FID ) ) ),  &
                         BUFFER ) ) THEN

        DDTVAR3 = .TRUE.

    ELSE

        DDTVAR3 = .FALSE.
        LDATE3( VID, FID ) = IMISS3   !  "invalid"

        NAMBUF = TRIM( CALLER ) // ':DDTVAR3'
        MESG   = 'Could not read "' // TRIM( VNAME ) // '" from "' // TRIM( FNAME ) // '"'
        CALL M3WARN( NAMBUF, JDATE, JTIME, MESG )

    END IF          !  if bufint3() succeeded, or failed

    RETURN          !  from DDTVAR3

END FUNCTION DDTVAR3


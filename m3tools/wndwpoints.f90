
PROGRAM  WNDWPOINTS

    !!***********************************************************************
    !! Version "$Id: wndwpoints.f90 240 2023-03-16 18:44:28Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! (C) 2023 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE vnpntoersion 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  98
    !!
    !!  FUNCTION:
    !!      For a given SMOKE POINTS file and output grid, window the set of
    !!      point sources to the interior of the grid-coverage, and output
    !!      the result as a SMOKE POINTS file
    !!      Returns failure (STATUS=3) if there are no points in this window.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O
    !!
    !!  REVISION  HISTORY::
    !!      Prototype  1/2023 by Carlie J. Coats, Jr., UNC IE
    !!      Bug-fix    3/2023 by CJC:  bad FDESC for OPEN3
    !!***********************************************************************

    USE M3UTILIO
    USE MODGCTP
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'WNDWPOINTS'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*72, PARAMETER :: BAR   =  &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV        !  log-device
    INTEGER         ISTAT       !  allocation-status
    CHARACTER*256   MESG
    LOGICAL         EFLAG

    INTEGER         NCOLS1      ! number of grid columns
    INTEGER         NROWS1      ! number of grid rows
    INTEGER         NTHIK1      ! bdy thickness
    INTEGER         GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    INTEGER         FTYPE1      ! file type:  gridded, custom, boundary...
    REAL*8          P_ALP1      ! first, second, third map
    REAL*8          P_BET1      ! projection descriptive
    REAL*8          P_GAM1      ! parameters.
    REAL*8          XCENT1      ! lon for coord-system X=0
    REAL*8          YCENT1      ! lat for coord-system Y=0
    REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
    REAL*8          YORIG1      ! Y-coordinate origin of grid
    REAL*8          XFINL1      ! X-coordinate UR corner of grid (map units)
    REAL*8          YFINL1      ! Y-coordinate UR corner of grid
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    INTEGER         NPNTSIN     ! number of input point sources
    INTEGER         SDATEIN, STIMEIN    !  starting date&time for "POINTS"
                                        !  This file *should* be time independent, damn it !!
    INTEGER         NVARSIN     ! number of variables for input-data file
    INTEGER         TSTEPIN     ! time step
    INTEGER         NRECSIN     ! number of records

    INTEGER         NPNTOUT     ! number of output point sources

    INTEGER         JDATE, JTIME, TSTEP, NRECS
    INTEGER         K, N, V
    REAL*8          DDX1, DDY1

    CHARACTER*16    CNAME, GNAME
    CHARACTER*16    VNAMES( MXVARS3 )
    INTEGER         VTYPES( MXVARS3 )

    INTEGER,    ALLOCATABLE :: INDX( : )      !! (NPNTSIN); effective dimension NPNTOUT
    INTEGER,    ALLOCATABLE :: FLAG( : )      !! 1: in-window; 0: outside window
    REAL,       ALLOCATABLE :: RBUF( : )      !! (NPNTSIN)...
    REAL*8,     ALLOCATABLE :: YLAT( : )
    REAL*8,     ALLOCATABLE :: XLON( : )
    REAL*8,     ALLOCATABLE :: XLOC( : )
    REAL*8,     ALLOCATABLE :: YLOC( : )
    INTEGER,    ALLOCATABLE :: ICOL( : )
    INTEGER,    ALLOCATABLE :: IROW( : )

    !!***********************************************************************
    !!   begin body of program M3COMBO

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BLANK, BAR, BLANK,                                  &
'Program WNDWPOINTS to take a SMOKE POINTS file and subset it so that',         &
'all the output sources lie in the coverage of the indicated output grid',      &
'and write the result to a new SMOKE POINTS file.',                             &
'',                                                                             &
'Returns EXIT-STATUS=3 if there are no points found in this window.',           &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'     setenv  GRIDDESC        <path-name>',                                     &
'     setenv  OUTGRID         <GRIDDESC-name for the output grid>',             &
'     setenv  POINTS          <path-name for  input SMOKE POINTS file>',        &
'     setenv  PTFLAGS         <path-name for output window-flags file>',        &
'     setenv  OUTFILE         <path-name for output SMOKE POINTS file>',        &
'',                                                                             &
'See URL',                                                                      &
'',                                                                             &
'   https://www.cmascenter.org/ioapi/documentation/3.1/html/AA.html#tools',     &
'',                                                                             &
'Program copyright (C) 2023 UNC Institute for the Environment.',                &
'Released under Version 2 of the GNU General Public License. See',              &
'enclosed GPL.txt, or URL',                                                     &
''  ,                                                                           &
'    https://www.gnu.org/licenses/old-licenses/gpl-2.0.html',                   &
''  ,                                                                           &
'Comments and questions are welcome and can be sent to'  ,                      &
'',                                                                             &
'    Carlie J. Coats, Jr.    carlie@jyarborough.com',                           &
'or',                                                                           &
'    UNC Institute for the Environment',                                        &
'    100 Europa Dr., Suite 490 Rm 405',                                         &
'    Campus Box 1105',                                                          &
'    Chapel Hill, NC 27599-1105',                                               &
'',                                                                             &
'Program version: ',                                                            &
'$Id: wndwpoints.f90 240 2023-03-16 18:44:28Z coats $',&
' '

    IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program aborted', 2 )
    END IF


    !!...............  Get output grid description

    CALL ENVSTR( 'OUTGRID', 'GRIDDESC name for output grid', 'WRF_FOO_CRO', GNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Only positive XCELL, YCELL supported.', 2 )
    END IF

    IF ( .NOT.DSCGRID( GNAME, CNAME,                                &
                  GDTYP1, P_ALP1, P_BET1, P_GAM1,  XCENT1, YCENT1,  &
                  XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1, NTHIK1 ) ) THEN

        MESG = 'Could not get grid description for "OUTGRID" in GRIDDESC file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

    ELSE IF ( XCELL1 .LT. 0.0d0 .OR. YCELL1 .LT. 0.0d0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Only positive XCELL, YCELL supported.', 2 )
    END IF

    CALL M3MESG( BAR )
    CALL M3MESG( 'Output-window grid parameters' )
    WRITE( MESG, '( A, I10 )' ) 'NCOLS=', NCOLS1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, I10 )' ) 'NROWS=', NROWS1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, I10 )' ) 'GDTYP=', GDTYP1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'P_ALP', P_ALP1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'P_BET', P_BET1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'P_GAM', P_GAM1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'XCENT', XCENT1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'YCENT', YCENT1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'XORIG', XORIG1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'YORIG', YORIG1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'XCELL', XCELL1
    CALL M3MESG( MESG )
    WRITE( MESG, '( A, 1PD24.16 )' ) 'YCELL', YCELL1
    CALL M3MESG( MESG )
    CALL M3MESG( BAR )


    !!...............  Open and get descriptions for input "points":

    IF ( .NOT. OPEN3( 'POINTS', FSREAD3,PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open "POINTS"', 2 )
    ELSE IF ( .NOT. DESC3( 'POINTS' ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not get description for "POINTS"', 2 )
    ELSE IF ( NCOLS3D .NE. 1 .OR. NLAYS3D .NE. 1 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Invalid dimensions:  not a correct "POINTS" file', 2 )
    ELSE IF ( .NOT.GRDCHK3( 'POINTS', P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,   &
                            XORIG3D, YORIG3D, XCELL3D, YCELL3D,                 &
                            NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent map-proj parameters for "POINTS"', 2 )
    ELSE

        NPNTSIN = NROWS3D
        SDATEIN = SDATE3D
        STIMEIN = STIME3D
        NVARSIN = NVARS3D
        SDATEIN = SDATE3D
        STIMEIN = STIME3D
        VNAMES( 1:NVARSIN ) = VNAME3D( 1:NVARSIN )
        VTYPES( 1:NVARSIN ) = VTYPE3D( 1:NVARSIN )

    END IF      !!  if open3('points'...) worked


    !!...............  Allocate working arrays:

    ALLOCATE( INDX( NPNTSIN ),     &
              FLAG( NPNTSIN ),     &
              RBUF( NPNTSIN ),     &
              ICOL( NPNTSIN ),     &
              IROW( NPNTSIN ),     &
              YLAT( NPNTSIN ),     &
              XLON( NPNTSIN ),     &
              XLOC( NPNTSIN ),     &
              YLOC( NPNTSIN ),     STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10)' ) 'Buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Read and transform latitudes, longitudes:
    !!...............  Compute index:

    IF ( .NOT.READ3( 'POINTS', 'LATITUDE', 1, SDATEIN, STIMEIN, RBUF ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not read "LATITUDE" from "POINTS"', 2 )
    END IF
    YLAT = DBLE( RBUF )

    IF ( .NOT.READ3( 'POINTS', 'LONGITUDE', 1, SDATEIN, STIMEIN, RBUF ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not read "LONGITUDE" from "POINTS"', 2 )
    END IF
    XLON = DBLE( RBUF )

    CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,          &
                LATGRD3, 0.0D0,   0.0D0,  0.0D0,  0.0D0,  0.0D0,          &
                NPNTSIN, XLON, YLAT, XLOC, YLOC )

    FLAG   = 0
    XFINL1 = XORIG1 + DBLE( NCOLS1 )*XCELL1
    YFINL1 = YORIG1 + DBLE( NROWS1 )*YCELL1
    DDX1   = 1.0D0 / XCELL1
    DDY1   = 1.0D0 / YCELL1
    K      = 0

    DO N = 1, NPNTSIN

        IF ( XLOC( N ) .LT. XORIG1 ) CYCLE      !!  skip:  outside output-grid
        IF ( XLOC( N ) .GT. XFINL1 ) CYCLE
        IF ( YLOC( N ) .LT. YORIG1 ) CYCLE
        IF ( YLOC( N ) .GT. YFINL1 ) CYCLE

        K         = K + 1
        FLAG( N ) = 1
        INDX( K ) = N
        ICOL( K ) = MIN( 1 + INT( DDX1 * ( XLOC( N ) - XORIG1 ) ), NCOLS1 )  !  trap extremely-unlikely but possible
        IROW( K ) = MIN( 1 + INT( DDY1 * ( YLOC( N ) - YORIG1 ) ), NROWS1 )  !  round-off problems...
        XLOC( K ) = XLOC( N )
        YLOC( K ) = YLOC( N )
    END DO

    NPNTOUT = K
    WRITE( MESG, '( A, I10 )' ) 'output window NPNTS=', NPNTOUT
    CALL M3MESG( MESG )
    IF ( NPNTOUT .EQ. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'No pount sources found in window.  Exiting', 3 )
    END IF


    !!...............  Open output  window-flags file using description edited from "POINTS":

     NVARS3D = 1
     VNAME3D(1) = 'FLAG'
     VTYPE3D(1) = M3INT
     UNITS3D(1) = 'none'
     VDESC3D(1) = 'inside-window:  FLAG=1, outside-window:  FLAG == 0'

    IF ( .NOT. OPEN3( 'PTFLAGS', FSUNKN3,PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open "PTFLAGS"', 2 )
    END IF      !!  if open3('ptflags'...) worked

    IF ( .NOT.WRITE3( 'PTFLAGS', 'FLAG', SDATEIN, STIMEIN, FLAG ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not write "FLAG" to "PTFLAGS"', 2 )
    END IF


    !!...............  Create "OUTFILE" using description borrowed from "POINTS" and edited:

    IF ( .NOT. DESC3( 'POINTS' ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not get description for "POINTS"', 2 )
    END IF
    NROWS3D = NPNTOUT
    XORIG3D = XORIG1
    YORIG3D = YORIG1
    XCELL3D = XCELL1
    YCELL3D = YCELL1
    IF ( .NOT.OPEN3( 'OUTFILE', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open/create "OUTFILE"', 2 )
    END IF


    !!...............  Process time step sequence

    EFLAG = .FALSE.
    JDATE = SDATEIN
    JTIME = STIMEIN
    TSTEP = TSTEPIN

    DO V = 1, NVARSIN

        IF      ( VNAMES( V ) .EQ. 'COL' ) THEN

            IF ( .NOT.WRITE3( 'OUTFILE', 'COL', JDATE, JTIME, ICOL ) ) THEN
                EFLAG = .TRUE.
            END IF

        ELSE IF ( VNAMES( V ) .EQ. 'ROW' ) THEN

            IF ( .NOT.WRITE3( 'OUTFILE', 'ROW', JDATE, JTIME, IROW ) ) THEN
                EFLAG = .TRUE.
            END IF

        ELSE IF ( VNAMES( V ) .EQ. 'XLOCA' ) THEN

            RBUF(1:NPNTOUT) = REAL( XLOC(1:NPNTOUT) )
            IF ( .NOT.WRITE3( 'OUTFILE', 'XLOCA', JDATE, JTIME, RBUF ) ) THEN
                EFLAG = .TRUE.
            END IF

        ELSE IF ( VNAMES( V ) .EQ. 'YLOCA' ) THEN

            RBUF(1:NPNTOUT) = REAL( YLOC(1:NPNTOUT) )
            IF ( .NOT.WRITE3( 'OUTFILE', 'YLOCA', JDATE, JTIME, RBUF ) ) THEN
                EFLAG = .TRUE.
            END IF

        ELSE IF ( VTYPES( V ) .EQ. M3REAL ) THEN

            CALL WNDWREAL( VNAMES(V), EFLAG )

        ELSE IF ( VTYPES( V ) .EQ. M3INT  ) THEN

            CALL WNDWINT( VNAMES(V), EFLAG )

        ELSE IF ( VTYPES( V ) .EQ. M3DBLE ) THEN

            CALL WNDWDBLE( VNAMES(V), EFLAG )

        ELSE

            WRITE( MESG, '( A,I10 )' ) 'Unsupported variable type', VTYPES( V )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.

        END IF

    END DO


    !!...............  Completion;

    IF ( EFLAG ) THEN
        MESG  = 'Failure in program'
        ISTAT = 2
    ELSE
        MESG  = 'Success in program'
        ISTAT = 0
    END IF

    CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE WNDWREAL( VNAME, EFLAG )

        CHARACTER(16), INTENT(IN   ) :: VNAME
        LOGICAL,       INTENT(INOUT) :: EFLAG

        INTEGER     N, P

        REAL        IBUF( NPNTSIN )
        REAL        OBUF( NPNTOUT )

        IF ( .NOT.READ3( 'POINTS', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N ) = IBUF( P )
        END DO

        IF ( .NOT.WRITE3( 'OUTFILE', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWREAL


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE WNDWINT( VNAME, EFLAG )

        CHARACTER(16), INTENT(IN   ) :: VNAME
        LOGICAL,       INTENT(INOUT) :: EFLAG

        INTEGER     N, P

        INTEGER     IBUF( NPNTSIN )
        INTEGER     OBUF( NPNTOUT )

        IF ( .NOT.READ3( 'POINTS', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N ) = IBUF( P )
        END DO

        IF ( .NOT.WRITE3( 'OUTFILE', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWINT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE WNDWDBLE( VNAME, EFLAG )

        CHARACTER(16), INTENT(IN   ) :: VNAME
        LOGICAL,       INTENT(INOUT) :: EFLAG

        INTEGER     N, P

        REAL*8      IBUF( NPNTSIN )
        REAL*8      OBUF( NPNTOUT )

        IF ( .NOT.READ3( 'POINTS', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N ) = IBUF( P )
        END DO

        IF ( .NOT.WRITE3( 'OUTFILE', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWDBLE


END PROGRAM WNDWPOINTS


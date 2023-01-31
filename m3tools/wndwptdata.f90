
PROGRAM  WNDWPTDATA

    !!***********************************************************************
    !! Version "$Id: wndwptdata.f90 228 2023-01-12 19:13:43Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! (C) 2023 UNC Institute for the Environment.
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  program body starts at line  96
    !!
    !!  FUNCTION:
    !!      For a given SMOKE POINTS file, matching point-data file, and
    !!      output grid, window the set of point sources to the interior of
    !!      the grid-coverage, and output the corresponding point-data.
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       Machine with stack-allocated AUTO local variables (e.g., CRAY)
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       Models-3 I/O
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  1/2023 by Carlie J. Coats, Jr., UNC IE
    !!***********************************************************************

    USE M3UTILIO
    USE MODGCTP
    IMPLICIT NONE

    !!...........   EXTERNAL FUNCTIONS and their descriptions:

     INTEGER :: IARGC

    !!...........   PARAMETERS and their descriptions:

    CHARACTER*16, PARAMETER :: PNAME = 'WNDWPTDATA'
    CHARACTER*16, PARAMETER :: BLANK = ' '
    CHARACTER*72, PARAMETER :: BAR   =  &
    '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

    !!...........   LOCAL VARIABLES and their descriptions:

    INTEGER         LDEV        !  log-device
    INTEGER         ISTAT       !  allocation-status
    CHARACTER*256   MESG
    LOGICAL         EFLAG

    INTEGER         SDATE1      ! for PTFLAGS file
    INTEGER         STIME1      ! "
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
    REAL*8          XCELL1      ! X-coordinate cell dimension
    REAL*8          YCELL1      ! Y-coordinate cell dimension

    INTEGER         FTYPEPT     ! file type
    INTEGER         NPNTSIN     ! number of input point sources
    INTEGER         SDATEPT, STIMEPT    !  starting date&time for "POINTS"
                                        !  This file *should* be time independent, damn it !!

    INTEGER         NLAYSIN     ! number of layers for input-data file
    INTEGER         NVARSIN     ! number of variables for input-data file
    INTEGER         SDATEIN     ! starting date
    INTEGER         STIMEIN     ! starting time
    INTEGER         TSTEPIN     ! time step
    INTEGER         NRECSIN     ! number of records

    INTEGER         NPNTOUT    ! number of output point sources

    INTEGER         JDATE, JTIME, TSTEP, NRECS
    INTEGER         N, T, V

    CHARACTER*16    CNAME, GNAME

    CHARACTER*16    VNAMES( MXVARS3 )
    INTEGER         VTYPES( MXVARS3 )

    INTEGER, ALLOCATABLE :: INDX( : )      !! (NPNTSIN); effective dimension NPNTOUT
    INTEGER, ALLOCATABLE :: FLAG( : )      !! (NPNTSIN); effective dimension NPNTOUT


    !!***********************************************************************
    !!   begin body of program M3COMBO

    LDEV  = INIT3()
    EFLAG = .FALSE.

    WRITE( *, '( 5X, A )' ) BLANK, BAR, BLANK,                                  &
'Program WNDWPTDATA to take a SMOKE points-data file (e.g., PTMP, PLAY), and',  &
'subset it so that all the output sources lie in the coverage of the indicated',&
'output grid using index-file PTFLAGS from program WNDWPOINTS, and write the',  &
'resulting points-data to a new SMOKE point-data file.',                        &
'',                                                                             &
'PRECONDITIONS REQUIRED:',                                                      &
'     setenv  GRIDDESC        <path-name>',                                     &
'     setenv  OUTGRID         <GRIDDESC-name for the output grid>',             &
'     setenv  PTFLAGS         <path-name for  input window-flags file>',        &
'     setenv  INDATA          <path-name for  input SMOKE data file>',          &
'     setenv  OUTDATA         <path-name for output SMOKE data file>',          &
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
'$Id: wndwptdata.f90 228 2023-01-12 19:13:43Z coats $',&
' '

    IF ( .NOT.GETYN( 'Continue with program?', .TRUE. ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Program aborted', 2 )
    END IF


    !!...............  Get output grid description
    !!...............  Open and get descriptions for input "points", "INDATA":

    CALL ENVSTR( 'OUTGRID', 'GRIDDESC name for output grid', 'WRF_FOO_CRO', GNAME, ISTAT )
    IF ( ISTAT .GT. 0 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Bad environment variable "OUTGRID"', 2 )
   END IF

    IF ( .NOT.DSCGRID( GNAME, CNAME,                                &
                  GDTYP1, P_ALP1, P_BET1, P_GAM1,  XCENT1, YCENT1,  &
                  XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1, NTHIK1 ) ) THEN

        MESG = 'Could not get grid description for "'// TRIM( GNAME ) //'" in GRIDDESC file'
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

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


    IF ( .NOT. OPEN3( 'PTFLAGS', FSREAD3,PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open "POINTS"', 2 )
    ELSE IF ( .NOT. DESC3( 'PTFLAGS' ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not get description for "PTFLAGS"', 2 )
    ELSE IF ( NCOLS3D .NE. 1 .OR. NLAYS3D .NE. 1 ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Invalid dimensions:  not a correct "PTFLAGS" file', 2 )
    ELSE IF ( .NOT.GRDCHK3( 'PTFLAGS', P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                            XORIG3D, YORIG3D, XCELL3D, YCELL3D,                 &
                            NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Inconsistent map-proj parameters for "POINTS"', 2 )
    ELSE

        NPNTSIN = NROWS3D
        SDATE1  = SDATE3D
        STIME1  = STIME3D

    END IF      !!  if open3('points'...) worked etc.


    !!...............  Allocate working arrays:

    ALLOCATE( INDX( NPNTSIN ),     &
              FLAG( NPNTSIN ),     STAT = ISTAT )

    IF ( ISTAT .NE. 0 ) THEN
        WRITE( MESG, '( A, I10)' ) 'Buffer allocation failed:  STAT=', ISTAT
        CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
    END IF


    !!...............  Read and transform coordinates and compute index:

    IF ( .NOT.READ3( 'PTFLAGS', 'FLAG', 1, SDATE1, STIME1, FLAG ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not read "XLOCA" from "POINTS"', 2 )
    END IF

    NPNTOUT = 0

    DO N = 1, NPNTSIN

        IF ( FLAG( N ) .NE. 0 ) THEN      !!  skip:  outside output-grid
            NPNTOUT = NPNTOUT + 1
            INDX( NPNTOUT ) = N
        END IF

    END DO

    WRITE( MESG, '( A, I10 )' ) 'Outout window NPNTS=', NPNTOUT
    CALL M3MESG( MESG )


    !!...............  Open INDATA:

    IF ( .NOT.OPEN3( 'INDATA', FSREAD3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open "INDATA"', 2 )
    ELSE IF ( .NOT.DESC3( 'INDATA' ) ) THEN    
        CALL M3EXIT( PNAME, 0, 0, 'Could not DESC3( "INDATA" )', 2 )
    ELSE IF ( NCOLS3D .NE. 1 .OR. NROWS3D .NE. NPNTSIN ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Invalid dimensions:  not a correct "INDATA" file', 2 )
    ELSE IF ( .NOT.GRDCHK3( 'INDATA', P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                            XORIG3D, YORIG3D, XCELL3D, YCELL3D,                 &
                            NLAYS3D, VGTYP3D, VGTOP3D, VGLVS3D ) ) THEN
    ELSE

        NRECSIN = MXREC3D
        NVARSIN = NVARS3D
        NLAYSIN = NLAYS3D
        SDATEIN = SDATE3D
        STIMEIN = STIME3D
        TSTEPIN = TSTEP3D
        VNAMES( 1:NVARSIN ) = VNAME3D( 1:NVARSIN )
        VTYPES( 1:NVARSIN ) = VTYPE3D( 1:NVARSIN )

    END IF      !!  if open3('indata'...) worked etc.


    !!...............  Create "OUTDATA" using description borrowed from "INDATA" and edited:

    NROWS3D = NPNTOUT
    XORIG3D = XORIG1
    YORIG3D = YORIG1
    XCELL3D = XCELL1
    YCELL3D = YCELL1
    IF ( .NOT.OPEN3( 'OUTDATA', FSUNKN3, PNAME ) ) THEN
        CALL M3EXIT( PNAME, 0, 0, 'Could not open/create "OUTDATA"', 2 )
    END IF


    !!...............  Process time step sequence

    EFLAG = .FALSE.
    JDATE = SDATEIN
    JTIME = STIMEIN
    TSTEP = TSTEPIN

    DO T = 1, NRECSIN

        DO V = 1, NVARSIN

            IF (      VTYPES( V ) .EQ. M3REAL ) THEN

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

        CALL NEXTIME( JDATE, JTIME, TSTEP )

    END DO


    !!...............  Completion


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

        INTEGER     L, N, P

        REAL        IBUF( NPNTSIN, NLAYSIN )
        REAL        OBUF( NPNTOUT, NLAYSIN )

        IF ( .NOT.READ3( 'INDATA', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO L = 1, NLAYSIN
        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N,L ) = IBUF( P,L )
        END DO
        END DO

        IF ( .NOT.WRITE3( 'OUTDATA', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWREAL


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE WNDWINT( VNAME, EFLAG )

        CHARACTER(16), INTENT(IN   ) :: VNAME
        LOGICAL,       INTENT(INOUT) :: EFLAG

        INTEGER     L, N, P

        INTEGER     IBUF( NPNTSIN, NLAYSIN )
        INTEGER     OBUF( NPNTOUT, NLAYSIN )

        IF ( .NOT.READ3( 'INDATA', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO L = 1, NLAYSIN
        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N,L ) = IBUF( P,L )
        END DO
        END DO

        IF ( .NOT.WRITE3( 'OUTDATA', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWINT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE WNDWDBLE( VNAME, EFLAG )

        CHARACTER(16), INTENT(IN   ) :: VNAME
        LOGICAL,       INTENT(INOUT) :: EFLAG

        INTEGER     L, N, P

        REAL*8      IBUF( NPNTSIN, NLAYSIN )
        REAL*8      OBUF( NPNTOUT, NLAYSIN )

        IF ( .NOT.READ3( 'INDATA', VNAME, ALLAYS3, JDATE, JTIME, IBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        DO L = 1, NLAYSIN
        DO N = 1, NPNTOUT
            P = INDX( N )
            OBUF( N,L ) = IBUF( P,L )
        END DO
        END DO

        IF ( .NOT.WRITE3( 'OUTDATA', VNAME, JDATE, JTIME, OBUF ) ) THEN
            EFLAG = .FALSE.
            RETURN
        END IF

        RETURN

    END SUBROUTINE WNDWDBLE


END PROGRAM WNDWPTDATA


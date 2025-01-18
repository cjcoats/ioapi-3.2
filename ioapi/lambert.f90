
LOGICAL FUNCTION LAMBERT( CNAME, A, B, C, X, Y )

    IMPLICIT NONE

    LOGICAL          POLSTE
    LOGICAL          TRMERC
    LOGICAL          EQMERC
    LOGICAL          ALBERS
    LOGICAL          SETLAM
    LOGICAL          SETPOL
    LOGICAL          SETTRM
    LOGICAL          SETEQM
    LOGICAL          SETALB
    LOGICAL          LL2LAM
    LOGICAL          LAM2LL
    LOGICAL          UTM2LAM
    LOGICAL          LAM2UTM
    LOGICAL          LL2POL
    LOGICAL          POL2LL
    LOGICAL          POL2LAM
    LOGICAL          LAM2POL
    LOGICAL          UTM2POL
    LOGICAL          POL2UTM
    LOGICAL          TRM2LL
    LOGICAL          LL2TRM
    LOGICAL          TRM2LAM
    LOGICAL          LAM2TRM
    LOGICAL          TRM2UTM
    LOGICAL          UTM2TRM
    LOGICAL          TRM2POL
    LOGICAL          POL2TRM
    LOGICAL          EQM2LL
    LOGICAL          LL2EQM
    LOGICAL          EQM2LAM
    LOGICAL          LAM2EQM
    LOGICAL          EQM2UTM
    LOGICAL          UTM2EQM
    LOGICAL          EQM2TRM
    LOGICAL          TRM2EQM
    LOGICAL          EQM2POL
    LOGICAL          POL2EQM
    LOGICAL          ALB2LL
    LOGICAL          LL2ALB

    !***********************************************************************
    ! Version "$Id: lambert.f90 212 2021-11-10 20:39:53Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine LAMBERT body starts at line  316
    !  entry      POLSTE       starts at line  415
    !  entry      TRMERC       starts at line  526
    !  entry      EQMERC       starts at line  653
    !  entry      ALBERS       starts at line  781
    !  entry      SETLAM       starts at line  872
    !  entry      SETPOL       starts at line  944
    !  entry      SETTRM       starts at line 1016
    !  entry      SETEQM       starts at line 1078
    !  entry      SETALB       starts at line 1136
    !  entry      LAM2LL       starts at line 1210
    !  entry      LL2LAM       starts at line 1272
    !  entry      LAM2UTM      starts at line 1340
    !  entry      UTM2LAM      starts at line 1404
    !  entry      LAM2POL      starts at line 1472
    !  entry      POL2LAM      starts at line 1551
    !  entry      POL2LL       starts at line 1637
    !  entry      LL2POL       starts at line 1697
    !  entry      POL2UTM      starts at line 1764
    !  entry      UTM2POL      starts at line 1826
    !  entry      TRM2LL       starts at line 1895
    !  entry      LL2TRM       starts at line 1957
    !  entry      LAM2TRM      starts at line 2024
    !  entry      TRM2LAM      starts at line 2103
    !  entry      TRM2UTM      starts at line 2185
    !  entry      UTM2TRM      starts at line 2250
    !  entry      TRM2POL      starts at line 2318
    !  entry      POL2TRM      starts at line 2402
    !  entry      EQM2LL       starts at line 2480
    !  entry      LL2EQM       starts at line 2542
    !  entry      LAM2EQM      starts at line 2609
    !  entry      EQM2LAM      starts at line 2687
    !  entry      EQM2UTM      starts at line 2772
    !  entry      UTM2EQM      starts at line 2838
    !  entry      EQM2POL      starts at line 2904
    !  entry      POL2EQM      starts at line 2988
    !  entry      EQM2TRM      starts at line 3065
    !  entry      TRM2EQM      starts at line 3148
    !  entry      ALB2LL       starts at line 3228
    !  entry      LL2ALB       starts at line 3290
    !
    !  FUNCTION:
    !     LAMBERT:  set up  GTPZ0() for a particular named Lambert.
    !               If CNAME is a _grid_ name, returns corresponding
    !               _coordinate_system_name_ and coordinate definition
    !               parms A,B,C,X,Y.
    !     SETLAM:   set up  GTPZ0() for a argument-specified anonymous Lambert.
    !     POLSTE:   set up  GTPZ0() for a particular named (secant) polar
    !               stereographic.
    !               If CNAME is a _grid_ name, returns corresponding
    !               _coordinate_system_name_ and coordinate definition
    !               parms A,B,C,X,Y.
    !     SETPOL:   set up  GTPZ0() for a argument-specified anonymous
    !               (secant) polar stereographic.
    !     LL2LAM:   Convert LAT-LON coordinates to Lambert coordinates
    !     LAM2LL:   Convert Lambert coordinates to LAT-LON coordinates
    !     UTM2LAM:  Convert UTM     coordinates to Lambert coordinates
    !     LAM2UTM:  Convert Lambert coordinates to UTM     coordinates
    !     LL2POL:   Convert LAT-LON coordinates to Polar   coordinates
    !     POL2LL:   Convert Polar   coordinates to LAT-LON coordinates
    !     POL2LAM:  Convert Polar   coordinates to Lambert coordinates
    !     LAM2POL:  Convert Lambert coordinates to Polar   coordinates
    !     UTM2POL:  Convert UTM     coordinates to Polar   coordinates
    !     POL2UTM:  Convert Polar   coordinates to UTM     coordinates
    !     TRM2LL:   Convert TRM     coordinates to LAT-LON coordinates
    !     LL2TRM:   Convert LAT-LON coordinates to TRM     coordinates
    !     TRM2LAM:  Convert TRM     coordinates to Lambert coordinates
    !     LAM2TRM:  Convert Lambert coordinates to TRM     coordinates
    !     TRM2UTM:  Convert TRM     coordinates to UTM     coordinates
    !     UTM2TRM:  Convert UTM     coordinates to TRM     coordinates
    !     TRM2POL:  Convert TRM     coordinates to Polar   coordinates
    !     POL2TRM:  Convert Polar   coordinates to TRM     coordinates
    !     EQM2LL:   Convert EQM     coordinates to LAT-LON coordinates
    !     LL2EQM:   Convert LAT-LON coordinates to EQM     coordinates
    !     EQM2LAM:  Convert EQM     coordinates to Lambert coordinates
    !     LAM2EQM:  Convert Lambert coordinates to EQM     coordinates
    !     EQM2UTM:  Convert EQM     coordinates to UTM     coordinates
    !     UTM2EQM:  Convert UTM     coordinates to EQM     coordinates
    !     EQM2TRM:  Convert EQM     coordinates to TRM     coordinates
    !     TRM2EQM:  Convert TRM     coordinates to EQM     coordinates
    !     EQM2POL:  Convert EQM     coordinates to Polar   coordinates
    !     POL2EQM:  Convert Polar   coordinates to EQM     coordinates
    !     ALB2LL:   Convert Albers  coordinates to EQM     coordinates
    !     LL2ALB:   Convert LAT-LON coordinates to Albers  coordinates
    !
    !  PRECONDITIONS REQUIRED:
    !       For LAMBERT(), POLSTE(), etc., CNAME must be the name either of a
    !       coordinate system or a grid found in file GRIDDESC; furthermore,
    !       the projection-type must be LAMGRD3 (i.e., Lambert) or POLGRD3
    !       (i.e., (secant) Polar Stereographic), respectively
    !       Must call LAMBERT() or SETLAM() before calling conversion
    !       functions involving Lambert projections; similarly must call
    !       other coordinate-initialization routines before calling conversion
    !       functions involving other projections.
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       USGS National Mapping Division General Cartographic Transformation
    !       Package, routine GTPZ0().
    !       I/O API routines INIT3(), DSCGRID(), DSCOORD()
    !
    !  REVISION  HISTORY:
    !       Prototype 11/1995 by CJC:  uses GTPZ0()
    !       Revised   11/2000 by CJC:  additional wrappers to support polar
    !               stereographic, transverse Mercator, and equatorial
    !               Mercator map projections.
    !       Revised   03/2002 by CJC: bug fixes in SET*() entries.
    !       Revised   10/2002 by CJC: bug fixes in *[LAM|POL|TRM|EQM]*() entries.
    !       Revised    4/2003 by CJC: add INITSPHERES/SPHEREDAT sphere
    !                                 selection functionality
    !       Revised    3/2004 by D. Yin:
    !              change the sign for false easting and false northing;
    !              add CRDIN(1) and CRDIN(2) for POL2LL
    !       Revised    1/2006 by CJC: enforce zero-initialization of
    !       TPARIN, TPARIO for environments with default-initialization of NaN
    !       Revised    7/2008 by CJC: ALBERS, SETALB, ALB2LL, LL2ALB
    !       Modified 03/20010 by CJC: F90 changes for I/O API v3.1
    !
    !       Version 11/2015 by CJC for I/O API 3.2:  stand-alone versions
    !       of routines, for call from C, etc.
    !
    !       Version 11/2015 by CJC:  bug-fixes for SETEQM, EQMERC, TRMERC.
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    INCLUDE 'PARMS3.EXT'

    !...........   ARGUMENTS:

    CHARACTER*(*)  CNAME
    REAL           A          !  first secant latitude
    REAL           B          !  second secant latitude.  B > A
    REAL           C          !  central meridian
    REAL           X          !  Lambert easting  in meters
    REAL           Y          !  Lambert northing in meters
    REAL           U          !  UTM easting  in meters
    REAL           V          !  UTM northing in meters
    REAL           LON        !  East longitude in decimal degrees
    REAL           LAT        !  North latitude in decimal degrees
    INTEGER        Z          !  UTM zone (1...36)


    !...........   PARAMETERS:

    REAL*8, PARAMETER :: D60 = 1.0D0 / 60.0D0
    REAL*8, PARAMETER :: PI  = 3.141592653589793238462643383279D0
    REAL*8, PARAMETER :: PI180  = PI / 180.0D0
    REAL*8, PARAMETER :: RPI180 = 180.0D0 / PI


    !...........   External Functions

    LOGICAL, EXTERNAL :: DSCOORD, DSCGRID, INITSPHERES, SPHEREDAT
    INTEGER, EXTERNAL :: INIT3


    !.......   LOCAL VARIABLES:
    !.......   Arguments for GTPZ0:

    REAL*8            CRDIN( 2 )      !  input coordinates x,y
    INTEGER           INSYS           !  input projection code
    INTEGER           INZONE          !  input utm zone, etc.
    REAL*8            TPAIN( 15 )     !  input projection parameters
    INTEGER           INUNIT          !  input units code
    INTEGER           INSPH           !  spheroid code
    INTEGER           IPR             !  error print flag
    INTEGER           JPR             !  projection parameter print flag
    INTEGER           LEMSG           !  error message unit number
    INTEGER           LPARM           !  projection parameter unit number
    REAL*8            CRDIO( 2 )      !  output coordinates x,y
    INTEGER           IOSYS           !  output projection code
    INTEGER           IOZONE          !  output utm zone, etc.
    REAL*8            TPARO( 15 )     !  output projection parameters
    INTEGER           IOUNIT          !  output units code
    INTEGER           LN27            !  NAD1927 file unit number
    INTEGER           LN83            !  NAD1983 file unit number
    CHARACTER*128     FN27            !  NAD1927 file name
    CHARACTER*128     FN83            !  NAD1983 file name
    INTEGER           LENGTH          !  NAD* record-length
    INTEGER           IFLG            !  error flag

    !.......   Error codes for GTPZ0:

    CHARACTER*64, SAVE :: GMESG( 9 ) = (/                       &
          'Illegal  input system code INSYS               ',    &
          'Illegal output system code IOSYS               ',    &
          'Illegal  input unit code INUNIT                ',    &
          'Illegal output unit code IOUNIT                ',    &
          'Inconsistent unit and system codes for  input  ',    &
          'Inconsistent unit and system codes for output  ',    &
          'Illegal  input zone code INZONE                ',    &
          'Illegal output zone code IOZONE                ',    &
          'Projection-specific error                      ' /)

    !.......   Arguments for DSCGRID() and DSCCORD():
    !.......   Lambert calls use *L; Polar calls use *P, etc.

    CHARACTER*16  ANAME
    INTEGER       CTYPE
    REAL*8        P_ALP     !  first, second, third map
    REAL*8        P_BET     !  projection descriptive
    REAL*8        P_GAM     !  parameters
    REAL*8        XCENT     !  lon for coord-system X=0
    REAL*8        YCENT     !  lat for coord-system Y=0
    REAL*8        XORIG     !  X-coordinate origin of grid (map units)
    REAL*8        YORIG     !  Y-coordinate origin of grid
    REAL*8        XCELL     !  X-coordinate cell dimension
    REAL*8        YCELL     !  Y-coordinate cell dimension
    INTEGER       NCOLS     !  number of grid columns
    INTEGER       NROWS     !  number of grid rows
    INTEGER       NTHIK     !  BOUNDARY:  perimeter thickness (cells)

    REAL*8, SAVE :: P_ALPL     !  first, second, third map
    REAL*8, SAVE :: P_BETL     !  projection descriptive
    REAL*8, SAVE :: P_GAML     !  parameters
    REAL*8, SAVE :: XCENTL     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTL     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPP     !  first, second, third map
    REAL*8, SAVE :: P_BETP     !  projection descriptive
    REAL*8, SAVE :: P_GAMP     !  parameters
    REAL*8, SAVE :: XCENTP     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTP     !  lat for coord-system Y=0


    REAL*8, SAVE :: P_ALPT     !  first, second, third map
    REAL*8, SAVE :: P_BETT     !  projection descriptive
    REAL*8, SAVE :: P_GAMT     !  parameters
    REAL*8, SAVE :: XCENTT     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTT     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPE     !  first, second, third map
    REAL*8, SAVE :: P_BETE     !  projection descriptive
    REAL*8, SAVE :: P_GAME     !  parameters
    REAL*8, SAVE :: XCENTE     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTE     !  lat for coord-system Y=0

    REAL*8, SAVE :: P_ALPA     !  first, second, third map
    REAL*8, SAVE :: P_BETA     !  projection descriptive
    REAL*8, SAVE :: P_GAMA     !  parameters
    REAL*8, SAVE :: XCENTA     !  lon for coord-system X=0
    REAL*8, SAVE :: YCENTA     !  lat for coord-system Y=0

    !.......   Scratch variables:

    CHARACTER*256   MESG
    INTEGER         DEG, MNT, I

    !.......   SAVED Local Variables:

    !!  Coordinate system ID's. Note that each call to set a new
    !!  coordinate system of the indicated type increments the
    !!  corresponding ID by 4 = Number of types implemented

    INTEGER, SAVE :: LZONE = 61   !  Lambert
    INTEGER, SAVE :: PZONE = 62   !  Polar Stereographic
    INTEGER, SAVE :: TZONE = 63   !  Transverse Mercator
    INTEGER, SAVE :: EZONE = 64   !  Equatorial Mercator
    INTEGER, SAVE :: AZONE = 65   !  Equatorial Mercator


    !***********************************************************************
    !.......   LAMBERT():  Set up a particular named Lambert projection:

    !...........   Get coordinate system description, using DSCOORD or
    !...........   DSCGRID, as appropriate:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT',0,0,'Bad geodetic sphere info' )
    END IF

    IF ( .NOT. DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN

        IF ( DSCGRID( CNAME, ANAME,                                     &
                      CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,         &
                      XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN

            CNAME = ANAME

        ELSE        !  dscgrid and dscoord both failed

            CALL M3WARN( 'LAMBERT', 0, 0, 'Projection not found in GRIDDESC' )
            LAMBERT = .FALSE.
            RETURN

        END IF      !  if dscgrid()) succeeded or failed

    END IF          !  if dscoord failed

    IF ( CTYPE .NE. LAMGRD3 ) THEN
        WRITE( MESG,94010 ) 'Projection not Lambert:  type ', CTYPE
        CALL M3WARN( 'LAMBERT', 0, 0, MESG )
        LAMBERT = .FALSE.
        RETURN
    END IF

    !.......   Return the projection parameters as REAL   A,B,C,X,Y:

    A = SNGL( P_ALP )
    B = SNGL( P_BET )
    C = SNGL( P_GAM )
    X = SNGL( XCENT )
    Y = SNGL( YCENT )
    LAMBERT = .TRUE.

    !.......   Convert from real degrees to GTPZ0() format  dddmmmsss.sssD0

111 CONTINUE

    LZONE = LZONE + 5
    XCENT = XCENT - P_GAM   !  convert from lon to offset from P_GAM

    DEG   = INT( P_ALP )                            !  int degrees
    P_ALP = 60.0D0 * ( P_ALP - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_ALP )                            !  int minutes
    P_ALP = 60.0D0 * ( P_ALP - DBLE( MNT ) )        !  seconds
    P_ALPL= P_ALP + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( P_BET )                            !  int degrees
    P_BET = 60.0D0 * ( P_BET - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_BET )                            !  int minutes
    P_BET = 60.0D0 * ( P_BET - DBLE( MNT ) )        !  seconds
    P_BETL= P_BET + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( P_GAM )                            !  int degrees
    P_GAM = 60.0D0 * ( P_GAM - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_GAM )                            !  int minutes
    P_GAM = 60.0D0 * ( P_GAM - DBLE( MNT ) )        !  seconds
    P_GAML= P_GAM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( XCENT )                            !  int degrees
    XCENT = 60.0D0 * ( XCENT - DBLE( DEG ) )        !  minutes
    MNT   = INT( XCENT )                            !  int minutes
    XCENT = 60.0D0 * ( XCENT - DBLE( MNT ) )        !  seconds
    XCENTL= XCENT + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( YCENT )                            !  int degrees
    YCENT = 60.0D0 * ( YCENT - DBLE( DEG ) )        !  minutes
    MNT   = INT( YCENT )                            !  int minutes
    YCENT = 60.0D0 * ( YCENT - DBLE( MNT ) )        !  seconds
    YCENTL= YCENT + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    RETURN

    !.....................................................................
    !.......   POLSTE():  Set up a particular named Polar projection:

  ENTRY POLSTE( CNAME, A, B, C, X, Y )

    !...........   Get coordinate system description, using DSCOORD or
    !...........   DSCGRID, as appropriate:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/POLSTE',0,0,'Bad geodetic sphere' )
    END IF

    IF ( .NOT. DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN

        IF ( DSCGRID( CNAME, ANAME,                                 &
                      CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,     &
                      XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN

            CNAME = ANAME

        ELSE        !  dscgrid and dscoord both failed

            CALL M3WARN( 'LAMBERT/POLSTE', 0, 0, 'Projection not found in GRIDDESC' )
            POLSTE = .FALSE.
            RETURN

        END IF      !  if dscgrid()) succeeded or failed

    END IF          !  if dscoord failed

    IF ( CTYPE .NE. POLGRD3 ) THEN
        WRITE( MESG,94010 ) 'Projection not POLSTE:  type ', CTYPE
        CALL M3WARN( 'LAMBERT/POLSTE', 0, 0, MESG )
        POLSTE = .FALSE.
        RETURN
    END IF

    !.......   Return the projection parameters as REAL   A,B,C,X,Y:

    A = SNGL( P_ALP )
    B = SNGL( P_BET )
    C = SNGL( P_GAM )
    X = SNGL( XCENT )
    Y = SNGL( YCENT )
    POLSTE = .TRUE.

    !.......   Convert from real degrees to GTPZ0() format  dddmmmsss.sssD0

122 CONTINUE

    PZONE = PZONE + 5

    P_ALPP= P_ALP

    DEG   = INT( P_BET )                            !  int degrees
    P_BET = 60.0D0 * ( P_BET - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_BET )                            !  int minutes
    P_BET = 60.0D0 * ( P_BET - DBLE( MNT ) )        !  seconds
    P_BETP= P_BET + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( P_GAM )                            !  int degrees
    P_GAM = 60.0D0 * ( P_GAM - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_GAM )                            !  int minutes
    P_GAM = 60.0D0 * ( P_GAM - DBLE( MNT ) )        !  seconds
    P_GAMP= P_GAM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    !.......   Convert <XCENT,YCENT> to POL-Cartesian offsets
    !.......   Set up input arguments for GTPZ0():

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = XCENT
    CRDIN( 2 ) = YCENT
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  Polar stereographic
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/POLSTE', 0,0, GMESG( IFLG ) )
        POLSTE = .FALSE.
        RETURN
    END IF

    !.......   Decode output arguments for GTPZ0();
    !.......   update PZONE for the new false-easting/false-northing offsets

    XCENTP = -SNGL( CRDIO( 1 ) )
    YCENTP = -SNGL( CRDIO( 2 ) )

    PZONE = PZONE + 5

    RETURN

    !.....................................................................
    !.......   TRMERC():  Set up a particular named transverse Mercator projection:

  ENTRY TRMERC( CNAME, A, B, C, X, Y )

    !...........   Get coordinate system description, using DSCOORD or
    !...........   DSCGRID, as appropriate:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/TRMERC',0,0,'Bad geodetic sphere' )
    END IF

    IF ( .NOT. DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN

        IF ( DSCGRID( CNAME, ANAME,                                     &
                      CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,         &
                      XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN

            CNAME = ANAME

        ELSE        !  dscgrid and dscoord both failed

            CALL M3WARN( 'LAMBERT/TRMERC', 0, 0, 'Projection not found in GRIDDESC' )
            TRMERC = .FALSE.
            RETURN

        END IF      !  if dscgrid()) succeeded or failed

    END IF          !  if dscoord failed

    IF ( CTYPE .NE. TRMGRD3 ) THEN
        WRITE( MESG,94010 ) 'Projection not TRM:  type ', CTYPE
        CALL M3WARN( 'LAMBERT/TRMERC', 0, 0, MESG )
        TRMERC = .FALSE.
        RETURN
    END IF

    !.......   Return the projection parameters as REAL   A,B,C,X,Y:

    A = SNGL( P_ALP )
    B = SNGL( P_BET )
    C = SNGL( P_GAM )
    X = SNGL( XCENT )
    Y = SNGL( YCENT )
    TRMERC = .TRUE.

    !.......   Convert from real degrees to GTPZ0() format  dddmmmsss.sssD0

133 CONTINUE

    TZONE   = TZONE + 5

    DEG   = INT( P_ALP )                            !  int degrees
    P_ALP = 60.0D0 * ( P_ALP - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_ALP )                            !  int minutes
    P_ALP = 60.0D0 * ( P_ALP - DBLE( MNT ) )        !  seconds
    P_ALPT= P_ALP + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    P_BETT= P_BET

    DEG   = INT( P_GAM )                            !  int degrees
    P_GAM = 60.0D0 * ( P_GAM - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_GAM )                            !  int minutes
    P_GAM = 60.0D0 * ( P_GAM - DBLE( MNT ) )        !  seconds
    P_GAMT= P_GAM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    !.......   Convert <XCENT,YCENT> to TRM-Cartesian offsets
    !.......   Set up input arguments for GTPZ0():

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = XCENT
    CRDIN( 2 ) = YCENT

    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/TRMERC', 0,0, GMESG( IFLG ) )
        TRMERC = .FALSE.
        RETURN
    END IF

    !.......   Decode output arguments from GTPZ0();
    !.......   update TZONE for the new false-easting/false-northing offsets

    XCENTT = CRDIO( 1 )
    YCENTT = CRDIO( 2 )

    TZONE   = TZONE + 5

    RETURN

    !.....................................................................
    !.......   EQMERC():  Set up a particular named Equatorial Mercator projection:

  ENTRY EQMERC( CNAME, A, B, C, X, Y )

    !...........   Get coordinate system description, using DSCOORD or
    !...........   DSCGRID, as appropriate:

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/EQMERC',0,0,'Bad geodetic sphere' )
    END IF

    IF ( .NOT. DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN

        IF ( DSCGRID( CNAME, ANAME,                                     &
                      CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,         &
                      XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN

            CNAME = ANAME

        ELSE        !  dscgrid and dscoord both failed

            CALL M3WARN( 'LAMBERT/EQMERC', 0, 0, 'Projection not found in GRIDDESC' )
            EQMERC = .FALSE.
            RETURN

        END IF      !  if dscgrid()) succeeded or failed

    END IF          !  if dscoord failed

    IF ( CTYPE .NE. EQMGRD3 ) THEN
        WRITE( MESG,94010 ) 'Projection not EQM:  type ', CTYPE
        CALL M3WARN( 'LAMBERT/EQMERC', 0, 0, MESG )
        EQMERC = .FALSE.
        RETURN
    END IF

    !.......   Return the projection parameters as REAL   A,B,C,X,Y:

    A = SNGL( P_ALP )
    B = SNGL( P_BET )
    C = SNGL( P_GAM )
    X = SNGL( XCENT )
    Y = SNGL( YCENT )
    EQMERC = .TRUE.

    !.......   Convert from real degrees to GTPZ0() format  dddmmmsss.sssD0

144 CONTINUE

    EZONE   = EZONE + 5

    DEG   = INT( P_ALP )                            !  int degrees
    P_ALP = 60.0D0 * ( P_ALP - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_ALP )                            !  int minutes
    P_ALP = 60.0D0 * ( P_ALP - DBLE( MNT ) )        !  seconds
    P_ALPE= P_ALP + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    P_BETE= P_BET

    DEG   = INT( P_GAM )                            !  int degrees
    P_GAM = 60.0D0 * ( P_GAM - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_GAM )                            !  int minutes
    P_GAM = 60.0D0 * ( P_GAM - DBLE( MNT ) )        !  seconds
    P_GAME= P_GAM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    !.......   Convert <XCENT,YCENT> to TRM-Cartesian offsets
    !.......   Set up input arguments for GTPZ0():

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = XCENT
    CRDIN( 2 ) = YCENT

    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/EQMERC', 0,0, GMESG( IFLG ) )
        EQMERC = .FALSE.
        RETURN
    END IF

    !.......   Decode output arguments for GTPZ0()
    !.......   update EZONE for the new false-easting/false-northing offsets

    XCENTE = CRDIO( 1 )
    YCENTE = CRDIO( 2 )

    EZONE   = EZONE + 5

    RETURN

    !.....................................................................
    !.......   ALBERS():  Set up a particular named Albers conic Equal Area projection:

  ENTRY ALBERS( CNAME, A, B, C, X, Y )

    !...........   Get coordinate system description, using DSCOORD or
    !...........   DSCGRID, as appropriate:

    IF ( .NOT. INITSPHERES() ) THEN
        MESG = 'Bad geodetic sphere info'
        CALL M3WARN( 'LAMBERT/ALBERS', 0, 0, MESG )
    END IF

    IF ( .NOT. DSCOORD( CNAME, CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT ) ) THEN

        IF ( DSCGRID( CNAME, ANAME,                                 &
                CTYPE, P_ALP, P_BET, P_GAM, XCENT, YCENT,           &
                XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS, NTHIK ) ) THEN

            CNAME = ANAME

        ELSE        !  dscgrid and dscoord both failed

            CALL M3WARN( 'ALBERS', 0, 0, 'Projection not found in GRIDDESC' )
            ALBERS = .FALSE.
            RETURN

        END IF      !  if dscgrid()) succeeded or failed

    END IF          !  if dscoord failed

    IF ( CTYPE .NE. ALBGRD3 ) THEN
        WRITE( MESG,94010 ) 'Projection not Albers:  type ', CTYPE
        CALL M3WARN( 'LAMBERT/ALBERS', 0, 0, MESG )
        ALBERS = .FALSE.
        RETURN
    END IF

    !.......   Return the projection parameters as REAL   A,B,C,X,Y:

    A = SNGL( P_ALP )
    B = SNGL( P_BET )
    C = SNGL( P_GAM )
    X = SNGL( XCENT )
    Y = SNGL( YCENT )
    ALBERS = .TRUE.

    !.......   Convert from real degrees to GTPZ0() format  dddmmmsss.sssD0

155 CONTINUE

    AZONE = AZONE + 5
    XCENT = XCENT - P_GAM   !  convert from lon to offset from P_GAM

    DEG   = INT( P_ALP )                            !  int degrees
    P_ALP = 60.0D0 * ( P_ALP - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_ALP )                            !  int minutes
    P_ALP = 60.0D0 * ( P_ALP - DBLE( MNT ) )        !  seconds
    P_ALPA= P_ALP + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( P_BET )                            !  int degrees
    P_BET = 60.0D0 * ( P_BET - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_BET )                            !  int minutes
    P_BET = 60.0D0 * ( P_BET - DBLE( MNT ) )        !  seconds
    P_BETA= P_BET + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( P_GAM )                            !  int degrees
    P_GAM = 60.0D0 * ( P_GAM - DBLE( DEG ) )        !  minutes
    MNT   = INT( P_GAM )                            !  int minutes
    P_GAM = 60.0D0 * ( P_GAM - DBLE( MNT ) )        !  seconds
    P_GAMA= P_GAM + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( XCENT )                            !  int degrees
    XCENT = 60.0D0 * ( XCENT - DBLE( DEG ) )        !  minutes
    MNT   = INT( XCENT )                            !  int minutes
    XCENT = 60.0D0 * ( XCENT - DBLE( MNT ) )        !  seconds
    XCENTA= XCENT + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0

    DEG   = INT( YCENT )                            !  int degrees
    YCENT = 60.0D0 * ( YCENT - DBLE( DEG ) )        !  minutes
    MNT   = INT( YCENT )                            !  int minutes
    YCENT = 60.0D0 * ( YCENT - DBLE( MNT ) )        !  seconds
    YCENTA= YCENT + 1000.0D0 * ( MNT + 1000 * DEG ) !  dddmmmsss.sssD0


    RETURN


    !.....................................................................
    !.......   Set up anonymous lambert from arguments:

  ENTRY SETLAM( A, B, C, X, Y )

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/SETLAM',0,0,'Bad geodetic sphere' )
    END IF

    !.......   Check validity of input parameters:

    IF ( A .LT. -90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad first latitude A =', A
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( A .GT. B ) THEN
        WRITE( MESG, 94020 ) 'Bad latitudes A ', A, 'B =', B
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( B .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad second latitude B =', B
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( C .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( C .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( X .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( X .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( Y .LT. -90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    ELSE IF ( Y .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETLAM', 0, 0, MESG )
        SETLAM = .FALSE.
        RETURN
    END IF

    !.......   Convert to double, then go to GTPZ0() format conversion
    !.......   Note that P_ALP, etc., are scratch variables;
    !.......   P_ALPL, etc., are set by LAMBERT() code after 111.

    P_ALP  = DBLE( A )
    P_BET  = DBLE( B )
    P_GAM  = DBLE( C )
    XCENT  = DBLE( X )
    YCENT  = DBLE( Y )
    SETLAM = .TRUE.

    GO TO  111      !  convert projection parms to dddmmmsss.sssD0 format


    !.....................................................................
    !.......   Set up anonymous polar from arguments:

  ENTRY SETPOL( A, B, C, X, Y )

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/SETPOL',0,0,'Bad geodetic sphere' )
    END IF

    !.......   Check validity of input parameters:

    IF ( NINT( ABS( A ) ) .NE. 1 ) THEN
        WRITE( MESG, 94020 ) 'Bad pole A =', A
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( B .GT.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad secant latitude B =', B
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( B .LT.  0.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad secant latitude B =', B
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( C .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( C .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( X .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( X .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( Y .LT.    0.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    ELSE IF ( Y .GT.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETPOL', 0, 0, MESG )
        SETPOL = .FALSE.
        RETURN
    END IF

    !.......   Convert to double, then go to GTPZ0() format conversion
    !.......   Note that P_ALP, etc., are scratch variables;
    !.......   P_ALPP, etc., are set by LAMBERT() code after 122.

    P_ALP  = DBLE( A )
    P_BET  = DBLE( B )
    P_GAM  = DBLE( C )
    XCENT  = DBLE( X )
    YCENT  = DBLE( Y )
    SETPOL  = .TRUE.

    GO TO  122      !  convert projection parms to dddmmmsss.sssD0 format


    !.....................................................................
    !.......   Set up anonymous Transverse Mercator from arguments:

  ENTRY SETTRM( A, B, C, X, Y )

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/SETTRM',0,0,'Bad geodetic sphere' )
    END IF

    !.......   Check validity of input parameters:

    IF ( ABS( A ) .GT. 90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude', A
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( B .GT.   1.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad scale factor B =', B
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( B .LE.  0.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad scale factor B =', B
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( ABS( C ) .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( ABS( X ) .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( Y .LT.    0.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    ELSE IF ( Y .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETTRM', 0, 0, MESG )
        SETTRM = .FALSE.
        RETURN
    END IF

    !.......   Convert to double, then go to GTPZ0() format conversion
    !.......   Note that P_ALP, etc., are scratch variables;
    !.......   P_ALPP, etc., are set by LAMBERT() code after 122.

    P_ALP  = DBLE( A )
    P_BET  = DBLE( B )
    P_GAM  = DBLE( C )
    XCENT  = DBLE( X )
    YCENT  = DBLE( Y )
    SETTRM = .TRUE.

    GO TO  133      !  convert projection parms to dddmmmsss.sssD0 format


    !.....................................................................
    !.......   Set up anonymous Equatorial Mercator from arguments:

  ENTRY SETEQM( A, B, C, X, Y )

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/SETEQM',0,0,'Bad geodetic sphere' )
    END IF

    !.......   Check validity of input parameters:

    IF ( C .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    ELSE IF ( C .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    ELSE IF ( X .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    ELSE IF ( X .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    ELSE IF ( Y .LT.    0.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    ELSE IF ( Y .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETEQM', 0, 0, MESG )
        SETEQM = .FALSE.
        RETURN
    END IF

    !.......   Convert to double, then go to GTPZ0() format conversion
    !.......   Note that P_ALP, etc., are scratch variables;
    !.......   P_ALPE, etc., are set by LAMBERT() code after 122.


    P_ALP  = DBLE( A )
    P_BET  = DBLE( B )
    P_GAM  = DBLE( C )
    XCENT  = DBLE( X )
    YCENT  = DBLE( Y )
    SETEQM = .TRUE.

    GO TO  144      !  convert projection parms to dddmmmsss.sssD0 format


    !.....................................................................
    !.......   Set up anonymous Equatorial Mercator from arguments:

  ENTRY SETALB( A, B, C, X, Y )

    IF ( .NOT. INITSPHERES() ) THEN
        CALL M3WARN( 'LAMBERT/SETALB',0,0,'Bad geodetic sphere' )
    END IF

    !.......   Check validity of input parameters:

    IF ( A .LT. -90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad first latitude A =', A
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( A .GT. B ) THEN
        WRITE( MESG, 94020 ) 'Bad latitudes A ', A, 'B =', B
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( B .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad second latitude B =', B
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( C .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( C .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad central longitude C =', C
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( X .LT. -180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( X .GT.  180.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin longitude X =', X
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( Y .LT. -90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    ELSE IF ( Y .GE.   90.0 ) THEN
        WRITE( MESG, 94020 ) 'Bad origin latitude Y =', Y
        CALL M3WARN( 'LAMBERT/SETALB', 0, 0, MESG )
        SETALB = .FALSE.
        RETURN
    END IF

    !.......   Convert to double, then go to GTPZ0() format conversion
    !.......   Note that P_ALP, etc., are scratch variables;
    !.......   P_ALPE, etc., are set by LAMBERT() code after 122.


    P_ALP  = DBLE( A )
    P_BET  = DBLE( B )
    P_GAM  = DBLE( C )
    XCENT  = DBLE( X )
    YCENT  = DBLE( Y )
    SETALB = .TRUE.

    GO TO  155      !  convert projection parms to dddmmmsss.sssD0 format


    !.....................................................................
    !.......   convert from Lambert to lat-lon
    !.......   Set up input arguments for GTPZ0()

  ENTRY LAM2LL( X, Y, LON, LAT )

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2LL', 0, 0, 'Projection not initialized' )
        LAM2LL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = P_ALPL
    TPAIN( 4 ) = P_BETL
    TPAIN( 5 ) = P_GAML
    TPAIN( 6 ) = YCENTL
    TPAIN( 7 ) = 0.0D0
    TPAIN( 8 ) = 0.0D0
    INSYS  = 4       !  Lambert conformal conic
    INZONE = LZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 0       !  geographic (lat-lon)
    IOUNIT = 4       !  output units: degrees

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/LAM2LL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    LON    = SNGL( CRDIO( 1 ) )
    LAT    = SNGL( CRDIO( 2 ) )
    LAM2LL = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Lat-Lon to Lambert:

  ENTRY  LL2LAM( LON, LAT, X, Y )

    !.......   Check initialization:

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LL2LAM', 0, 0, 'Projection not initialized' )
        LL2LAM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 4       !  Lambert conformal conic
    IOZONE = LZONE   !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPL
    TPARO( 4 ) = P_BETL
    TPARO( 5 ) = P_GAML
    TPARO( 6 ) = YCENTL
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/LL2LAM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X      = SNGL( CRDIO( 1 ) )
    Y      = SNGL( CRDIO( 2 ) )
    LL2LAM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Lambert to UTM
    !.......   Set up input arguments for GTPZ0()

  ENTRY LAM2UTM( X, Y, Z, U, V )

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2UTM', 0, 0, 'Projection not initialized' )
        LAM2UTM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = P_ALPL
    TPAIN( 4 ) = P_BETL
    TPAIN( 5 ) = P_GAML
    TPAIN( 6 ) = YCENTL
    TPAIN( 7 ) = 0.0D0
    TPAIN( 8 ) = 0.0D0
    INSYS  = 4       !  Lambert conformal conic
    INZONE = LZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 1       !  UTM
    IOZONE = Z       !  UTM zone
    IOUNIT = 2       !  output units: meters

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/LAM2UTM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    LAM2UTM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from UTM to Lambert:

  ENTRY  UTM2LAM( U, V, Z, X, Y )

    !.......   Check initialization:

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/UTM2LAM', 0, 0, 'Projection not initialized' )
        UTM2LAM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 1
    INZONE = Z
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 4       !  Lambert conformal conic
    IOZONE = LZONE    !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPL
    TPARO( 4 ) = P_BETL
    TPARO( 5 ) = P_GAML
    TPARO( 6 ) = YCENTL
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/UTM2LAM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    UTM2LAM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Lambert to Polar Stereographic

  ENTRY LAM2POL( X, Y, U, V )

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2POL', 0, 0, 'Lambert projection not initialized' )
        LAM2POL = .FALSE.
        RETURN
    END IF

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2POL', 0, 0, 'Polar projection not initialized' )
        LAM2POL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = P_ALPL
    TPAIN( 4 ) = P_BETL
    TPAIN( 5 ) = P_GAML
    TPAIN( 6 ) = 0.0D0
    TPAIN( 7 ) = 0.0D0
    TPAIN( 8 ) = 0.0D0
    INSYS  = 4       !  Lambert conformal conic
    INZONE = LZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  POL
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = XCENTP
    TPARO( 8 ) = YCENTP

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/LAM2POL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    LAM2POL = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from Polar to Lambert:

  ENTRY  POL2LAM( U, V, X, Y )

    !.......   Check initialization:

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2LAM', 0, 0, 'Lambert projection not initialized' )
        POL2LAM = .FALSE.
        RETURN
    END IF

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2LAM', 0, 0, 'Polar projection not initialized' )
        POL2LAM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 6
    INZONE = PZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMP
    TPAIN( 6 ) = P_BETP
    TPAIN( 7 ) = XCENTP
    TPAIN( 8 ) = YCENTP
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 4       !  Lambert conformal conic
    IOZONE = LZONE   !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPL
    TPARO( 4 ) = P_BETL
    TPARO( 5 ) = P_GAML
    TPARO( 6 ) = 0.0D0
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/POL2LAM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    POL2LAM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Polar to lat-lon
    !.......   Set up input arguments for GTPZ0()

  ENTRY POL2LL( X, Y, LON, LAT )

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2LL', 0, 0, 'Polar projection not initialized' )
        POL2LL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMP
    TPAIN( 6 ) = P_BETP
    TPAIN( 7 ) = XCENTP
    TPAIN( 8 ) = YCENTP
    INSYS  = 6       !  Lambert conformal conic
    INZONE = PZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 0       !  geographic (lat-lon)
    IOUNIT = 4       !  output units: degrees

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/POL2LL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    LON    = SNGL( CRDIO( 1 ) )
    LAT    = SNGL( CRDIO( 2 ) )
    POL2LL = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Lat-Lon to Polar:

  ENTRY  LL2POL( LON, LAT, X, Y )

    !.......   Check initialization:

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LL2POL', 0, 0, 'Polar projection not initialized' )
        LL2POL = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  Polar Stereographic
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = XCENTP
    TPARO( 8 ) = YCENTP


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/LL2POL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X      = SNGL( CRDIO( 1 ) )
    Y      = SNGL( CRDIO( 2 ) )
    LL2POL = .TRUE.
    RETURN

    !.....................................................................
    !.......   convert from Polar to UTM
    !.......   Set up input arguments for GTPZ0()

  ENTRY POL2UTM( X, Y, Z, U, V )

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2UTM', 0, 0, 'Projection not initialized' )
        POL2UTM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMP
    TPAIN( 6 ) = P_BETP
    TPAIN( 7 ) = XCENTP
    TPAIN( 8 ) = YCENTP
    INSYS  = 6       !  Polar stereographic
    INZONE = PZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 1       !  UTM
    IOZONE = Z       !  UTM zone
    IOUNIT = 2       !  output units: meters

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/POL2UTM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    POL2UTM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from UTM to Polar:

  ENTRY  UTM2POL( U, V, Z, X, Y )

    !.......   Check initialization:

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/UTM2POL', 0, 0, 'Polar projection not initialized' )
        UTM2POL = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 1
    INZONE = Z
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  Polar stereographic
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = XCENTP
    TPARO( 8 ) = YCENTP


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/UTM2POL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    UTM2POL = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Transverse Mercator to lat-lon
    !.......   Set up input arguments for GTPZ0()

  ENTRY TRM2LL( X, Y, LON, LAT )

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2LL', 0, 0, 'Transverse Mercator  projection not initialized' )
        TRM2LL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    INSYS  = 9
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMT
    TPAIN( 6 ) = P_ALPT
    TPAIN( 7 ) = XCENTT
    TPAIN( 8 ) = YCENTT
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 0       !  geographic (lat-lon)
    IOUNIT = 4       !  output units: degrees

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/TRM2LL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    LON    = SNGL( CRDIO( 1 ) )
    LAT    = SNGL( CRDIO( 2 ) )
    TRM2LL = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Lat-Lon to Transverse Mercator:

  ENTRY  LL2TRM( LON, LAT, X, Y )

    !.......   Check initialization:

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LL2TRM', 0, 0, 'Transverse Mercator projection not initialized' )
        LL2TRM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = XCENTT
    TPARO( 8 ) = YCENTT


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/LL2TRM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X      = SNGL( CRDIO( 1 ) )
    Y      = SNGL( CRDIO( 2 ) )
    LL2TRM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Lambert to Transverse Mercator

  ENTRY LAM2TRM( X, Y, U, V )

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2TRM', 0, 0, 'Lambert projection not initialized' )
        LAM2TRM = .FALSE.
        RETURN
    END IF

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2TRM', 0, 0,&
        &'Transverse Mercator projection not initialized' )
        LAM2TRM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = P_ALPL
    TPAIN( 4 ) = P_BETL
    TPAIN( 5 ) = P_GAML
    TPAIN( 6 ) = YCENTL
    TPAIN( 7 ) = 0.0D0
    TPAIN( 8 ) = 0.0D0
    INSYS  = 4       !  Lambert conformal conic
    INZONE = LZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = XCENTT
    TPARO( 8 ) = YCENTT

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/LAM2TRM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    LAM2TRM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from Transverse Mercator to Lambert:

  ENTRY  TRM2LAM( U, V, X, Y )
    !.......   Check initialization:

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2LAM', 0, 0, 'Lambert projection not initialized' )
        TRM2LAM = .FALSE.
        RETURN
    END IF

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2LAM', 0, 0, 'Transverse Mercator projection not initialized' )
        TRM2LAM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 9
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMT
    TPAIN( 6 ) = P_ALPT
    TPAIN( 7 ) = XCENTT
    TPAIN( 8 ) = YCENTT
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 4       !  Lambert conformal conic
    IOZONE = LZONE   !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPL
    TPARO( 4 ) = P_BETL
    TPARO( 5 ) = P_GAML
    TPARO( 6 ) = 0.0D0
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/TRM2LAM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    TRM2LAM = .TRUE.
    RETURN

    !.....................................................................
    !.......   convert from Transverse Mercator to UTM
    !.......   Set up input arguments for GTPZ0()

  ENTRY TRM2UTM( X, Y, Z, U, V )

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2UTM', 0, 0, 'Projection not initialized' )
        TRM2UTM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    INSYS  = 9
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMT
    TPAIN( 6 ) = P_ALPT
    TPAIN( 7 ) = XCENTT
    TPAIN( 8 ) = YCENTT
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 1       !  UTM
    IOZONE = Z       !  UTM zone
    IOUNIT = 2       !  output units: meters

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/TRM2UTM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    TRM2UTM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from UTM to Transverse Mercator:

  ENTRY  UTM2TRM( U, V, Z, X, Y )

    !.......   Check initialization:

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/UTM2TRM', 0, 0, 'Projection not initialized' )
        UTM2TRM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 1
    INZONE = Z
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = XCENTT
    TPARO( 8 ) = YCENTT


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/UTM2TRM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    UTM2TRM = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Transverse Mercator to Polar:

  ENTRY  TRM2POL( U, V, X, Y )

    !.......   Check initialization:

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2POL', 0, 0, 'Polar projection not initialized' )
        TRM2POL = .FALSE.
        RETURN
    END IF

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2POL', 0, 0, 'Transverse Mercator projection not initialized' )
        TRM2POL = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 9
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMT
    TPAIN( 6 ) = P_ALPT
    TPAIN( 7 ) = XCENTT
    TPAIN( 8 ) = YCENTT
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  Lambert conformal conic
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = XCENTP
    TPARO( 8 ) = YCENTP


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/TRM2POL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    TRM2POL = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Polar to Transverse Mercator
    !.......   Set up input arguments for GTPZ0()

  ENTRY POL2TRM( X, Y, U, V )

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2TRM', 0, 0, 'Projection not initialized' )
        POL2TRM = .FALSE.
        RETURN
    END IF

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2TRM', 0, 0, 'Transverse Mercator projection not initialized' )
        POL2TRM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMP
    TPAIN( 6 ) = P_BETP
    TPAIN( 7 ) = XCENTP
    TPAIN( 8 ) = YCENTP
    INSYS  = 6       !  Polar stereographic
    INZONE = PZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = XCENTT
    TPARO( 8 ) = YCENTT

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/POL2TRM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    POL2TRM = .TRUE.

    RETURN


    !.....................................................................
    !.......   convert from Equatorial Mercator to lat-lon
    !.......   Set up input arguments for GTPZ0()

  ENTRY EQM2LL( X, Y, LON, LAT )

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2LL', 0, 0, 'Equatorial Mercator  projection not initialized' )
        EQM2LL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    INSYS  = 5
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAME
    TPAIN( 6 ) = P_ALPE
    TPAIN( 7 ) = XCENTE
    TPAIN( 8 ) = YCENTE
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 0       !  geographic (lat-lon)
    IOUNIT = 4       !  output units: degrees

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/EQM2LL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    LON    = SNGL( CRDIO( 1 ) )
    LAT    = SNGL( CRDIO( 2 ) )
    EQM2LL = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Lat-Lon to Equatorial Mercator:

  ENTRY  LL2EQM( LON, LAT, X, Y )

    !.......   Check initialization:

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LL2EQM', 0, 0, 'Equatorial Mercator projection not initialized' )
        LL2EQM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = XCENTE
    TPARO( 8 ) = YCENTE


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/LL2EQM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X      = SNGL( CRDIO( 1 ) )
    Y      = SNGL( CRDIO( 2 ) )
    LL2EQM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Lambert to Equatorial Mercator

  ENTRY LAM2EQM( X, Y, U, V )

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2EQM', 0, 0, 'Lambert projection not initialized' )
        LAM2EQM = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/LAM2EQM', 0, 0, 'Equatorial Mercator projection not initialized' )
        LAM2EQM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = XCENTE
    TPARO( 8 ) = YCENTE
    INSYS  = 4       !  Lambert conformal conic
    INZONE = LZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/LAM2EQM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    LAM2EQM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from Equatorial Mercator to Lambert:

  ENTRY  EQM2LAM( U, V, X, Y )

    !.......   Check initialization:

    IF ( LZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2LAM', 0, 0, 'Lambert projection not initialized' )
        EQM2LAM = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2LAM', 0, 0, 'Equatorial Mercator projection not initialized' )
        EQM2LAM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 5
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAME
    TPAIN( 6 ) = P_ALPE
    TPAIN( 7 ) = XCENTE
    TPAIN( 8 ) = YCENTE
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 4       !  Lambert conformal conic
    IOZONE = LZONE   !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPL
    TPARO( 4 ) = P_BETL
    TPARO( 5 ) = P_GAML
    TPARO( 6 ) = 0.0D0
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/EQM2LAM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    EQM2LAM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Equatorial Mercator to UTM
    !.......   Set up input arguments for GTPZ0()

  ENTRY EQM2UTM( X, Y, Z, U, V )

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2UTM', 0, 0, 'Projection not initialized' )
        EQM2UTM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    INSYS  = 5
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAME
    TPAIN( 6 ) = P_ALPE
    TPAIN( 7 ) = XCENTE
    TPAIN( 8 ) = YCENTE
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 1       !  UTM
    IOZONE = Z       !  UTM zone
    IOUNIT = 2       !  output units: meters

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/EQM2UTM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    EQM2UTM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from UTM to Equatorial Mercator:

  ENTRY  UTM2EQM( U, V, Z, X, Y )

    !.......   Check initialization:

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/UTM2EQM', 0, 0, 'Projection not initialized' )
        UTM2EQM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 1
    INZONE = Z
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = XCENTE
    TPARO( 8 ) = YCENTE


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/UTM2EQM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    UTM2EQM = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Equatorial Mercator to Polar:

  ENTRY  EQM2POL( U, V, X, Y )

    !.......   Check initialization:

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2POL', 0, 0, 'Polar projection not initialized' )
        EQM2POL = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2POL', 0, 0, 'Equatorial Mercator projection not initialized' )
        EQM2POL = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 5
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAME
    TPAIN( 6 ) = P_ALPE
    TPAIN( 7 ) = XCENTE
    TPAIN( 8 ) = YCENTE
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 6       !  Lambert conformal conic
    IOZONE = PZONE   !  POL zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMP
    TPARO( 6 ) = P_BETP
    TPARO( 7 ) = XCENTP
    TPARO( 8 ) = YCENTP


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/EQM2POL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    EQM2POL = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Polar to Equatorial Mercator
    !.......   Set up input arguments for GTPZ0()

  ENTRY POL2EQM( X, Y, U, V )

    IF ( PZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2EQM', 0, 0, 'Projection not initialized' )
        POL2EQM = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/POL2EQM', 0, 0, 'Equatorial Mercator projection not initialized' )
        POL2EQM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMP
    TPAIN( 6 ) = P_BETP
    TPAIN( 7 ) = XCENTP
    TPAIN( 8 ) = YCENTP
    INSYS  = 6       !  Polar stereographic
    INZONE = PZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = XCENTE
    TPARO( 8 ) = YCENTE

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/POL2EQM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    POL2EQM = .TRUE.

    RETURN


    !.....................................................................
    !.......   Convert from Equatorial Mercator to Transverse Mercator:

  ENTRY  EQM2TRM( U, V, X, Y )

    !.......   Check initialization:

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2TRM', 0, 0, 'Transverse Mercator projection not initialized' )
        EQM2TRM = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/EQM2TRM', 0, 0, 'Equatorial Mercator projection not initialized' )
        EQM2TRM = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( U )
    CRDIN( 2 ) = DBLE( V )
    INSYS  = 5
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAME
    TPAIN( 6 ) = P_ALPE
    TPAIN( 7 ) = XCENTE
    TPAIN( 8 ) = YCENTE
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 9       !  TRM
    IOZONE = TZONE   !  TRM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAMT
    TPARO( 6 ) = P_ALPT
    TPARO( 7 ) = XCENTT
    TPARO( 8 ) = YCENTT

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/EQM2TRM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X       = SNGL( CRDIO( 1 ) )
    Y       = SNGL( CRDIO( 2 ) )
    EQM2TRM = .TRUE.
    RETURN


    !.....................................................................
    !.......   convert from Transverse Mercator to Equatorial Mercator
    !.......   Set up input arguments for GTPZ0()

  ENTRY TRM2EQM( X, Y, U, V )

    IF ( TZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2EQM', 0, 0, 'Transverse Mercator projection not initialized' )
        TRM2EQM = .FALSE.
        RETURN
    END IF

    IF ( EZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/TRM2EQM', 0, 0, 'Equatorial Mercator projection not initialized' )
        TRM2EQM = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    INSYS  = 9
    INZONE = TZONE
    INUNIT = 2       !  meters
    INSPH  = 8       !  GRS 1980 spheroid
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = 0.0D0
    TPAIN( 4 ) = 0.0D0
    TPAIN( 5 ) = P_GAMT
    TPAIN( 6 ) = P_ALPT
    TPAIN( 7 ) = XCENTT
    TPAIN( 8 ) = YCENTT
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 5       !  EQM
    IOZONE = TZONE   !  EQM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = 0.0D0
    TPARO( 4 ) = 0.0D0
    TPARO( 5 ) = P_GAME
    TPARO( 6 ) = P_ALPE
    TPARO( 7 ) = XCENTE
    TPARO( 8 ) = YCENTE

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        CALL M3WARN( 'LAM2LL',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/TRM2EQM', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    U       = SNGL( CRDIO( 1 ) )
    V       = SNGL( CRDIO( 2 ) )
    TRM2EQM = .TRUE.

    RETURN


    !.....................................................................
    !.......   convert from Lambert to lat-lon
    !.......   Set up input arguments for GTPZ0()

  ENTRY ALB2LL( X, Y, LON, LAT )

    IF ( AZONE .LT. 64 ) THEN
        CALL M3WARN( 'LAMBERT/ALB2LL', 0, 0, 'Projection not initialized' )
        ALB2LL = .FALSE.
        RETURN
    END IF

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( X )
    CRDIN( 2 ) = DBLE( Y )
    TPAIN( 1 ) = 0.0D0
    TPAIN( 2 ) = 0.0D0
    TPAIN( 3 ) = P_ALPA
    TPAIN( 4 ) = P_BETA
    TPAIN( 5 ) = P_GAMA
    TPAIN( 6 ) = YCENTA
    TPAIN( 7 ) = 0.0D0
    TPAIN( 8 ) = 0.0D0
    INSYS  = 3       !  Albers Azimuthal Equal Area conic
    INZONE = AZONE
    INUNIT = 2       !  input units:  meters
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 0       !  geographic (lat-lon)
    IOUNIT = 4       !  output units: degrees

    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        MESG = 'Bad geodetic sphere info'
        CALL M3WARN( 'LAMBERT/ALB2LL', 0, 0, MESG )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  trap between 1 and 9
        CALL M3WARN( 'LAMBERT/ALB2LL', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    LON    = SNGL( CRDIO( 1 ) )
    LAT    = SNGL( CRDIO( 2 ) )
    ALB2LL = .TRUE.
    RETURN


    !.....................................................................
    !.......   Convert from Lat-Lon to Albers:

  ENTRY  LL2ALB( LON, LAT, X, Y )

    !.......   Check initialization:

    IF ( AZONE .LT. 64 ) THEN
        MESG = 'Projection not initialized'
        CALL M3WARN( 'LAMBERT/LL2ALB', 0, 0, MESG )
        LL2ALB = .FALSE.
        RETURN
    END IF

    !.......   Set up input arguments for GTPZ0()

    DO I = 1, 15
        TPAIN( I ) = 0.0D0
        TPARO( I ) = 0.0D0
    END DO

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LEMSG  = INIT3() !  unit number for log file
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 3       !  Albers Azimuthal Equal Area conic
    IOZONE = AZONE   !  LAM zone
    IOUNIT = 2       !  output units: meters
    TPARO( 1 ) = 0.0D0
    TPARO( 2 ) = 0.0D0
    TPARO( 3 ) = P_ALPA
    TPARO( 4 ) = P_BETA
    TPARO( 5 ) = P_GAMA
    TPARO( 6 ) = YCENTA
    TPARO( 7 ) = 0.0D0
    TPARO( 8 ) = 0.0D0


    !.......   Call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPAIN, TPARO ) ) THEN
        MESG = 'Bad geodetic sphere info'
        CALL M3WARN( 'LAMBERT/LL2ALB', 0, 0, MESG )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPAIN, INUNIT, INSPH,         &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,      &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )     !  between 1 and 9
        CALL M3WARN( 'LAMBERT/LL2ALB', 0,0, GMESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X      = SNGL( CRDIO( 1 ) )
    Y      = SNGL( CRDIO( 2 ) )
    LL2ALB = .TRUE.
    RETURN

    !.....................................................................
    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Internal buffering formats............ 94xxx

94010 FORMAT( A, I10, :, 2X )

94020 FORMAT( A, 1PG14.5, :, 2X )

END FUNCTION LAMBERT

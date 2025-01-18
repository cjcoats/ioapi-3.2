
SUBROUTINE LL2UTM( LON, LAT, Z, X, Y )

    !***********************************************************************
    ! Version "$Id: ll2utm.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003 Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  110
    !
    !  FUNCTION:
    !     Convert LAT-LON coords to UTM zone-Z coordinates
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       USGS National Mapping Division General Cartographic Transformation
    !       Package, routine GTPZ0()
    !
    !  REVISION  HISTORY:
    !       Prototype 6/1995 by CJC, adapted from UAM EPS MAPUTG()
    !       X and Y are now in meters
    !       Version 10/1995 uses GTPZ0()
    !       Version 4/2003 by Carlie J. Coats, Jr., BAMS:  support for
    !       additional (non-GRS80) spheres, via INITSPHERES
    !       Modified 7/2003 by CJC:  bugfix -- clean up critical sections
    !       associated with INIT3(); also clean up initialization logic
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS:

    REAL   , INTENT(IN   ) :: LON    !  Longitude in decimal degrees
    REAL   , INTENT(IN   ) :: LAT    !  Latitude in decimal degrees
    REAL   , INTENT(  OUT) :: X      !  Returned UTM Easting in meters
    REAL   , INTENT(  OUT) :: Y      !  Returned UTM Northing in meters
    INTEGER, INTENT(IN   ) :: Z      !  UTM zone

    !...........   External Functions

    INTEGER, EXTERNAL :: INIT3           !  from M3IO
    LOGICAL, EXTERNAL :: INITSPHERES, SPHEREDAT


    !...........   Local Variables
    !.......   Arguments for GTPZ0:

    REAL*8            CRDIN( 2 )      !  input coordinates x,y
    INTEGER           INSYS           !  input projection code
    INTEGER           INZONE          !  input utm zone, etc.
    REAL*8            TPARIN( 15 )    !  input projection parameters
    INTEGER           INUNIT          !  input units code
    INTEGER           INSPH           !  spheroid code
    INTEGER           IPR             !  error print flag
    INTEGER           JPR             !  projection parameter print flag
    INTEGER           LPARM           !  projection parameter unit number
    REAL*8            CRDIO( 2 )      !  output coordinates x,y
    INTEGER           IOSYS           !  output projection code
    INTEGER           IOZONE          !  output utm zone, etc.
    REAL*8            TPARIO( 15 )    !  output projection parameters
    INTEGER           IOUNIT          !  output units code
    INTEGER           LN27            !  NAD1927 file unit number
    INTEGER           LN83            !  NAD1983 file unit number
    CHARACTER*128     FN27            !  NAD1927 file name
    CHARACTER*128     FN83            !  NAD1983 file name
    INTEGER           LENGTH          !  NAD* record-length
    INTEGER           IFLG            !  error flag

    !...........   SAVED LOCAL VARIABLES and their descriptions:
    !...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !...........   for variables which must retain their values from call to call.

    INTEGER, SAVE :: LEMSG = -9999

    !.......   Error codes for GTPZ0:

    CHARACTER*64, PARAMETER :: MESG( 9 ) = (/               &
      'Illegal input system code INSYS                ',    &
      'Illegal output system code IOSYS               ',    &
      'Illegal input unit code INUNIT                 ',    &
      'Illegal output unit code IOUNIT                ',    &
      'Inconsistent unit and system codes for input   ',    &
      'Inconsistent unit and system codes for output  ',    &
      'Illegal input zone code INZONE                 ',    &
      'Illegal output zone code IOZONE                ',    &
      'Projection-specific error                      ' /)


    !..........................................................................
    !.......   begin body of LL2UTM()

    IF ( LEMSG .LT. 0 ) THEN
!$OMP       CRITICAL( S_INIT )
        LEMSG  = INIT3() !  unit number for log file
        IF ( .NOT. INITSPHERES() ) THEN
            CALL M3WARN( 'LL2UTM',0,0,'Bad geodetic sphere info' )
        END IF
!$OMP       END CRITICAL( S_INIT )
    END IF          !  if firstime

    !.......   Set up input arguments for GTPZ0()
    !.......   Set up input arguments for GTPZ0()

    CRDIN( 1 ) = DBLE( LON )
    CRDIN( 2 ) = DBLE( LAT )
    INSYS  = 0       !  projection default (lat-lon)
    INUNIT = 4       !  input units:  degrees
    INSPH  = 8       !  GRS 1980 spheroid
    IPR    = 0       !  print error messages, if any
    JPR    = 1       !  do NOT print projection parameters
    LPARM  = LEMSG   !  projection parameters file
    IOSYS  = 1       !  UTM
    IOZONE = Z       !  UTM zone
    IOUNIT = 2       !  output units: meters


    !.......   Set up and call GTPZ0()

    IF ( .NOT.SPHEREDAT( INSPH, TPARIN, TPARIO ) ) THEN
        CALL M3WARN( 'LL2UTM',0,0,'Bad geodetic sphere info' )
    END IF

    CALL GTPZ0( CRDIN, INSYS, INZONE, TPARIN, INUNIT, INSPH,        &
                IPR, JPR, LEMSG, LPARM, CRDIO, IOSYS, IOZONE,       &
                TPARIO, IOUNIT, LN27, LN83, FN27, FN83, LENGTH,     &
                IFLG )

    IF ( IFLG .NE. 0 ) THEN
        IFLG = MAX( MIN( 9, IFLG ), 1 )	!  between 1 and 9
        CALL M3WARN( 'LL2UTM', 0,0, MESG( IFLG ) )
    END IF

    !.......   Decode output arguments for GTPZ0()

    X = SNGL( CRDIO( 1 ) )
    Y = SNGL( CRDIO( 2 ) )

    RETURN
END  SUBROUTINE LL2UTM

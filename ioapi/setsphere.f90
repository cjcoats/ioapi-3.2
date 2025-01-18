
FUNCTION SETSPHERE( PARM1, PARM2 )

    !***********************************************************************
    ! Version "$Id: setsphere.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright  (C) 2003-2010,2021 Carlie J. Coats, Jr., and
    ! (C) 2003-2005 Baron Advanced Meteorological Systems.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  Function body      starts at line  155
    !  Entry  INITSPHERES starts at line  197
    !  Entry  SPHEREDAT   starts at line  267
    !
    !  FUNCTION:
    !       Set input and output sphere projections for Lambert and
    !       related wrapper functions for GCTP.  In the absence of
    !       user-set spheres, the system will default to GRS-80.
    !
    !  PRECONDITIONS REQUIRED:
    !       Optionally:
    !
    !               setenv IOAPI_ISPH  <value>
    !
    !       where <value> is one of the following:
    !               small integer [0...21] for USGS sphere code; or
    !               double sphere radius; or
    !               pair of doubles:  major axis, eccentricity**2; or
    !               pair of doubles:  major axis, minor axis
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       I/O API ENVSTR(), M3MSG2(), and STR2DBLE()
    !
    !  REVISION  HISTORY:
    !       Prototype 4/7/2003 by Carlie J. Coats, Jr., BAMS
    !
    !       Version 6/3/2008 by Steve Howard, USEPA:  additional sphere
    !       types 20, 21 (normal spheres with R=6370000, 6371200 meters
    !       matching MM5/WRF-ARW and WRF-NMM, respectively
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    LOGICAL SETSPHERE, INITSPHERES, SPHEREDAT

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure
    INCLUDE 'IODECL3.EXT'     ! I/O API function declarations


    !...........   ARGUMENTS and their descriptions:

    REAL*8 , INTENT(IN   ) :: PARM1, PARM2
    INTEGER, INTENT(  OUT) :: INSPHERE
    REAL*8 , INTENT(  OUT) :: INPARAM( 15 ), IOPARAM( 15 )

    !...........   PARAMETERS and their descriptions:

    CHARACTER*1, PARAMETER :: BLANK = ' '

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: LBLANK
    REAL*8 , EXTERNAL :: STR2DBLE


    !...........   SAVED LOCAL VARIABLES and their descriptions:
    !...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !...........   for variables which must retain their values from call to call.

    INTEGER, SAVE :: ISPH = 8      !  default GRS80

    REAL*8, SAVE :: AXISMAJ = 0.0D0
    REAL*8, SAVE :: AXISMIN = 0.0D0

    INTEGER, PARAMETER :: STDSPHERES( 0:21 ) =    &
        (/ 0,    &
           1,    &
           2,    &
           3,    &
           4,    &
           5,    &
           6,    &
           7,    &
           8,    &
           9,    &
          10,    &
          11,    &
          12,    &
          13,    &
          14,    &
          15,    &
          16,    &
          17,    &
          18,    &
          19,    &
          20,    &
          21 /)

    CHARACTER*40, PARAMETER :: SPHERENAMES( 0:21 ) =                    &    
        (/ 'Clarke 1866                                  ',             &    !!  0
           'Clarke 1880                                  ',             &    !!  1
           'Bessel                                       ',             &    !!  2
           'New International 1967                       ',             &    !!  3
           'International 1909                           ',             &    !!  4
           'WGS 72                                       ',             &    !!  5
           'Everest                                      ',             &    !!  6
           'WGS 66                                       ',             &    !!  7
           'GRS 1980                                     ',             &    !!  8
           'Airy                                         ',             &    !!  9
           'Modified Everest                             ',             &    !! 10
           'Modified Airy                                ',             &    !! 11
           'WGS 84                                       ',             &    !! 12
           'Southeast Asia                               ',             &    !! 13
           'Australian National                          ',             &    !! 14
           'Krassovsky                                   ',             &    !! 15
           'Hough                                        ',             &    !! 16
           'Mercury 1960                                 ',             &    !! 17
           'Modified Mercury 1968                        ',             &    !! 18
           'Normal Sphere, R_Earth=6370997               ',             &    !! 19
           'Normal Sphere (MM5  WRF-ARW) R=6370000       ',             &    !! 20
           'Normal Sphere (WRF-NMM) R=6371200            '       /)          !! 21

    INTEGER, SAVE ::  NCALLS = 0


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         STATUS, L, M
    LOGICAL         EFLAG
    INTEGER         I1
    REAL*8          P1, P2, PP
    CHARACTER*256   MESG
    CHARACTER*256   EVALUE


    !...........   STATEMENT FUNCTION:  REAL*8 "definitely unequal"

    LOGICAL         DBLERR
    REAL*8          P, Q

    DBLERR( P, Q ) = ( (P - Q)**2  .GT.  1.0E-10*( P*P + Q*Q + 1.0E-5 ) )


    !***********************************************************************
    !   begin main body of subroutine SETSPHERE()

    EFLAG = .FALSE.

    P1 = PARM1
    P2 = PARM2

    IF ( P1 .GT. -0.5D0 .AND. P1 .LT. 21.5D0 ) THEN
        I1 = NINT( P1 )
        PP = DBLE( I1 )
        IF ( DBLERR( P1, PP ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Bad standard input sphere-number'
            CALL M3MSG2( MESG )
        ELSE
            MESG = 'SETSPHERE:  sphere ' // SPHERENAMES( I1 )
            CALL M3MSG2( MESG )
        END IF
    ELSE
        I1   = -NCALLS - 1
        NCALLS = NCALLS + 1
        WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'SETSPHERE:  major axis', P1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'SETSPHERE:  minor axis/eccentricity^2', P2
        CALL M3MSG2( MESG )
    END IF

    IF ( EFLAG ) THEN
        SETSPHERE = .FALSE.
    ELSE
        NCALLS    = NCALLS + 1
        ISPH      = I1
        AXISMAJ   = P1
        AXISMIN   = P2
        SETSPHERE = .TRUE.
    END IF

    RETURN

    !***********************************************************************
    !   begin body of entry  INITSPHERES

  ENTRY INITSPHERES

    IF ( NCALLS .GT. 0 ) THEN
        INITSPHERES = .TRUE.
        RETURN
    END IF

    EFLAG  = .FALSE.
    NCALLS = NCALLS + 1

    CALL ENVSTR( 'IOAPI_ISPH', 'Input sphere for LAM2LL, etc.', '8', EVALUE, STATUS )

    IF ( STATUS .GT. 0 ) THEN
        EFLAG = .TRUE.
        MESG = 'Bad environment value for "IOAPI_ISPH"'
        CALL M3MSG2( MESG )
    ELSE IF ( STATUS .EQ. 0 ) THEN
        L = LBLANK( EVALUE )
        M = L + INDEX( EVALUE( L+1:256 ), BLANK )
        P1 = STR2DBLE( EVALUE( L+1:M ) )
        IF ( EVALUE( M:256 ) .NE. ' ' ) THEN
            P2 = STR2DBLE( EVALUE( M+1:256 ) )
        ELSE
            P2 = 0.0D0
        END IF
    ELSE
        P1 = 8.0D0
        P2 = 0.0D0
    END IF

    IF ( P1 .GT. -0.5D0 .AND. P1 .LT. 21.5D0 ) THEN
        I1 = NINT( P1 )
        PP = DBLE( I1 )
        IF ( DBLERR( P1, PP ) ) THEN
            EFLAG = .TRUE.
            MESG = 'Bad standard input sphere-number'
            CALL M3MSG2( MESG )
        ELSE
            MESG = 'INITSPHERES:  input sphere ' // SPHERENAMES( I1 )
            CALL M3MSG2( MESG )
        END IF
    ELSE
        I1   = -NCALLS - 1
        NCALLS = NCALLS + 1
        WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'INITSPHERES:  major axis', P1
        CALL M3MSG2( MESG )
        WRITE( MESG, '( A, 1X, 1PD25.16 )' ) 'INITSPHERES:  minor axis/eccentricity^2', P2
        CALL M3MSG2( MESG )
        CALL M3MSG2( MESG )
    END IF

    IF ( EFLAG ) THEN
        INITSPHERES = .FALSE.
    ELSE
        ISPH    = I1
        AXISMAJ = P1
        AXISMIN = P2
        INITSPHERES = .TRUE.
    END IF

    RETURN

    !***********************************************************************
    !   begin body of entry  SPHEREDAT

  ENTRY  SPHEREDAT( INSPHERE, INPARAM, IOPARAM )

    INSPHERE = ISPH
    INPARAM( 1 ) = AXISMAJ
    INPARAM( 2 ) = AXISMIN
    IOPARAM( 1 ) = AXISMAJ
    IOPARAM( 2 ) = AXISMIN

    SPHEREDAT    = .TRUE.

    RETURN

END FUNCTION SETSPHERE


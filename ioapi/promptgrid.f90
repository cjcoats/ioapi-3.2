
LOGICAL FUNCTION PROMPTGRID()

    !***********************************************************************
    ! Version "$Id: promptgrid.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 53
    !
    !  RETURNS:  TRUE iff success
    !
    !  FUNCTION:
    !       Prompts user repeatedly for grid/coordinate system name, then
    !       uses DSCGRID or DSCOORD to put grid/coordinate system description
    !       into FDESC3.EXT data structures.
    !
    !  PRECONDITIONS REQUIRED:
    !       Valid GRIDDESC file
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       DSCOORD, DSCGRID, GETSTR, GETYN, M3WARN
    !
    !  REVISION  HISTORY:
    !       prototype 11/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'      ! I/O API constants
    INCLUDE 'FDESC3.EXT'      ! I/O API file description data structure

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: DSCOORD, DSCGRID, GETYN

    !...........   Local Variables:

    CHARACTER*16  	ANAME, CNAME

    !***********************************************************************
    !   begin body of function  PROMPTGRID

11  CONTINUE

    CALL GETSTR( 'Enter name for output coordinate system', 'UTM_17', GDNAM3D )

    IF ( DSCOORD( GDNAM3D, GDTYP3D,             &
                  P_ALP3D, P_BET3D, P_GAM3D,    &
                  XCENT3D, YCENT3D ) ) THEN

        XORIG3D = DBLE( BADVAL3 )
        YORIG3D = DBLE( BADVAL3 )
        XCELL3D = DBLE( BADVAL3 )
        YCELL3D = DBLE( BADVAL3 )
        NCOLS3D = IMISS3
        NROWS3D = IMISS3
        NTHIK3D = IMISS3

        PROMPTGRID = .TRUE.

    ELSE IF ( DSCGRID( GDNAM3D, ANAME, GDTYP3D,             &   !  retry with dscgrid()
                       P_ALP3D, P_BET3D, P_GAM3D,           &
                       XCENT3D, YCENT3D, XORIG3D, YORIG3D,  &
                       XCELL3D, YCELL3D,                    &
                       NCOLS3D, NROWS3D, NTHIK3D ) ) THEN

        PROMPTGRID = .TRUE.

    ELSE

        WRITE( *,'( 5X, A )' )      &
            'Could not get description for coordinate system "' // TRIM( CNAME ) // '"'

        IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
            GO TO  11
        ELSE
            CALL M3EXIT( 'PROMPTGRID', 0, 0, 'Bad grid/coordinate system', 2 )
        END IF      !  if retry getstr() or not

    END IF          !  if dscoord() failed; end of loop

    GDNAM3D = CNAME

    RETURN

END FUNCTION PROMPTGRID


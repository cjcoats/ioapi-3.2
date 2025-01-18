
SUBROUTINE DBLE2REAL( SIZE, DBLG, GRID )

    !***********************************************************************
    ! Version "$Id: dble2real.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  44
    !
    !  FUNCTION:
    !	convert INTEGER input array DBLG( SIZE ) to REAL
    !
    !  PRECONDITIONS REQUIRED:
    !	none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !	none
    !
    !  REVISION  HISTORY:
    !	    prototype 6/1995 by CJC
    !       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version   10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER		, INTENT(IN   ) :: SIZE		!  array dimension
    DOUBLE PRECISION, INTENT(IN   ) :: DBLG( SIZE )	!  input double array
    REAL		, INTENT(  OUT) :: GRID( SIZE )	! output real   array

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER		I


    !***********************************************************************
    !   begin body of subroutine  INT2REAL

    DO  I = 1, SIZE
        GRID( I ) = REAL( DBLG( I ) )
    END DO

    RETURN
END SUBROUTINE DBLE2REAL



SUBROUTINE INTG2REAL( SIZE, INTG, GRID )

    !***********************************************************************
    ! Version "$Id: intg2real.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine INTG2REAL body starts at line  46
    !  subroutine INT82REAL body starts at line  75
    !
    !  FUNCTION:
    !       convert INTEGER input array INTG( SIZE ) to REAL
    !
    !  PRECONDITIONS REQUIRED:
    !       none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED: none
    !
    !  REVISION  HISTORY:
    !       prototype 6/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified 02/2015 by CJC for I/O API 3.2: support for M3INT8 variables
    !       Modified 01/2017 by CJC: M3INT8 case, FLOAT() ~~> REAL() for g95
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: SIZE          !  array dimension
    INTEGER, INTENT(IN   ) :: INTG( SIZE )  !  input integer array
    REAL   , INTENT(  OUT) :: GRID( SIZE )  ! output real array

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     I


    !***********************************************************************
    !   begin body of subroutine  INT2REAL

    DO  I = 1, SIZE
        GRID( I ) = FLOAT( INTG( I ) )
    END DO

    RETURN
END SUBROUTINE INTG2REAL


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE INT82REAL( SIZE, INT8, GRID )

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER  , INTENT(IN   ) :: SIZE          !  array dimension
    INTEGER*8, INTENT(IN   ) :: INT8( SIZE )  !  input integer array
    REAL     , INTENT(  OUT) :: GRID( SIZE )  ! output real array

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER     I


    !***********************************************************************
    !   begin body of subroutine  INT2REAL

    DO  I = 1, SIZE
        GRID( I ) = REAL( INT8( I ) )
    END DO

    RETURN
END SUBROUTINE INT82REAL



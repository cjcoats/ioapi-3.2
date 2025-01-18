
SUBROUTINE DMATVEC( N, A, V, C )

    !***********************************************************************
    ! Version "$Id: dmatvec.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2010-2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  44
    !
    !  FUNCTION:  apply a diagonal matrix to a vector
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 2/1995 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version   9/2014 by CJC:  modifications for OpenMP parallel
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: N		! length of input vector
    REAL   , INTENT(IN   ) :: A( N )		! diagonal coeff matrix
    REAL   , INTENT(IN   ) :: V( N )		! input  vector
    REAL   , INTENT(  OUT) :: C( N )		! output vector

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER		R


    !***********************************************************************
    !   begin body of subroutine  DMATVEC

    !$OMP   PARALLEL DO                 &
    !$OMP&    DEFAULT( NONE ),          &
    !$OMP&     SHARED( N, C, A, V ),    &
    !$OMP&    PRIVATE( R )

    DO  R = 1, N

        C( R ) = A( R ) * V( R )

    END DO

    RETURN
END SUBROUTINE DMATVEC


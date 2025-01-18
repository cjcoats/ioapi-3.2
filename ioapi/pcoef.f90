
SUBROUTINE  PCOEF( N, X, Y, C )

    !***********************************************************************
    ! Version "$Id: pcoef.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  55
    !
    !  FUNCTION:  returns array C of polynomial coefficients
    !	for the polynomial fitting the data
    !       Y( K ) = P( X( K ) ) = sum_I
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !	Prototype 11/95 by CJC adapted from _Numerical_Recipes_,
    !	section 3.5 (pp. 92-94), routine POLCOE
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: N
    REAL   , INTENT(IN   ) :: X( N )
    REAL   , INTENT(IN   ) :: Y( N )
    REAL   , INTENT(  OUT) :: C( N )


    !...........   PARAMETERS and their descriptions:

    INTEGER, PARAMETER :: NMAX = 15


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER		I, J
    REAL		P, F, B
    REAL		S( NMAX )
    REAL		T( NMAX )
    CHARACTER*80    MESG

    !***********************************************************************
    !   begin body of subroutine  PCOEF

    IF ( N .GT. NMAX ) THEN
        WRITE( MESG, '(A, I5, 2X, A )' ) 'Requested degree', N, 'exceeds max=15'
        CALL M3EXIT( 'PCOEF', 0, 0, MESG, 2 )
    END IF

    DO I = 1, N

        S( I ) = 0.0
        C( I ) = 0.0
        T( I ) = FLOAT( I )

    END DO

    S( N ) = -X( 1 )

    DO I = 2, N
    DO J = N - I + 1, N - 1

        S( J ) = S( J ) - X( I ) * S( J+1 )

    END DO
    END DO

    DO I = 1, N

        P = T( N )

        DO J = N - 1, 1, -1

            P = T( J ) * S( J+1 ) + P * X( I )

        END DO		!  end loop on J

        F = Y( I ) / P
        B = 1.0

        DO J = N, 1, -1

            C( J ) = C( J ) + B * F
            B      = S( J ) + B * X( I )

        END DO	!  end loop on J

    END DO	!  end loop on I

    RETURN

END SUBROUTINE PCOEF


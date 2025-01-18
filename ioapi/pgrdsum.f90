
SUBROUTINE  PGRDSUM( NCOLS, NROWS, NCOFF, N, I, PTR, U, V )

    !***********************************************************************
    ! Version "$Id: pgrdsum.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  51
    !
    !  FUNCTION:  multiply a sparse matrix <N,I> by a vector U, sum and
    !             then return the result V
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 4/97 by JMV
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !****************************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
    INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
    INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients

    INTEGER, INTENT(IN   ) :: N( NROWS )      ! # of entries per row
    INTEGER, INTENT(IN   ) :: I( NCOFF )      ! columns list

    INTEGER, INTENT(IN   ) :: PTR ( NCOLS )   !  summation pointer
    REAL   , INTENT(IN   ) :: U( NCOLS )      !  input vector
    REAL   , INTENT(  OUT) :: V( NROWS )      ! output vector


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, C, K, L, P


    !***********************************************************************
    !   begin body of subroutine  PSUMGRD

    K = 0

    DO  22  R = 1, NROWS

        DO  11  C = 1, N( R )
            K = K + 1
            L = I( K )
            P = PTR( L )
            V( P ) = V( P ) + U( L )
11      CONTINUE

22  CONTINUE

    RETURN

END SUBROUTINE PGRDSUM


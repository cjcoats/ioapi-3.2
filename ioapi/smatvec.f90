
SUBROUTINE SMATVEC( NCOLS, NROWS, NCOFF, NX, IX, CX, U, V )

    !***********************************************************************
    ! Version "$Id: smatvec.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine  SMATVEC body starts at line  59
    !  subroutine SMATVECP()    starts at line  90
    !
    !  FUNCTION:  multiply a sparse matrix <N,I,C> by a vector U and
    !             return the result V
    !
    !  PRECONDITIONS REQUIRED:  none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 2/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version   9/2014 by CJC:  modifications for OpenMP parallel
    !       SMATVECP assumes already-constructed cumulative NP(0:NROWS)
    !       equivalent to the CNT constructed in SMATVEC().
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
    INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
    INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients

    INTEGER, INTENT(IN   ) :: NX( NROWS )     ! # of entries per row
    INTEGER, INTENT(IN   ) :: IX( NCOFF )     ! columns list
    REAL   , INTENT(IN   ) :: CX( NCOFF )     ! coefficient array

    REAL   , INTENT(IN   ) :: U( NCOLS )      !  input vector
    REAL   , INTENT(  OUT) :: V( NROWS )      ! output vector


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, K
    REAL*8          SUM
    INTEGER         CNT( 0:NROWS )


    !***********************************************************************
    !   begin body of subroutine  SMATVEC

    CNT( 0 ) = 0
    DO  R = 1, NROWS
        CNT( R ) = CNT( R-1 ) + NX( R )
    END DO

    !$OMP   PARALLEL DO                                 &
    !$OMP&    DEFAULT( NONE ),                          &
    !$OMP&     SHARED( NROWS, CNT, IX, CX, U, V ),      &
    !$OMP&    PRIVATE( R, SUM )

    DO  R = 1, NROWS

        SUM = 0.0d0

        DO  K = CNT( R-1 )+1, CNT( R )
            SUM = SUM  +  CX( K ) * U( IX( K ) )
        END DO

        V( R ) = SUM

    END DO

    RETURN

END SUBROUTINE SMATVEC


!***********************************************************************
!  SMATVECP:  CNT is already the cumulative-counts constructed
!  from NX, as above

SUBROUTINE SMATVECP( NCOLS, NROWS, NCOFF, CNT, IX, CX, U, V )

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOLS           ! length of input vector
    INTEGER, INTENT(IN   ) :: NROWS           ! length of output vector
    INTEGER, INTENT(IN   ) :: NCOFF           ! max number of coefficients

    INTEGER, INTENT(IN   ) :: CNT( 0:NROWS )  ! # of entries per row
    INTEGER, INTENT(IN   ) ::  IX(   NCOFF )  ! columns list
    REAL   , INTENT(IN   ) ::  CX(   NCOFF )  ! coefficient array

    REAL   , INTENT(IN   ) :: U( NCOLS )      !  input vector
    REAL   , INTENT(  OUT) :: V( NROWS )      ! output vector

    INTEGER         R, K
    REAL*8          SUM

    !$OMP   PARALLEL DO                             &
    !$OMP&    DEFAULT( NONE ),                      &
    !$OMP&     SHARED( NROWS, CNT, IX, CX, U, V ),  &
    !$OMP&    PRIVATE( R, SUM )

    DO  R = 1, NROWS

        SUM = 0.0d0

        DO  K = CNT( R-1 )+1, CNT( R )
            SUM = SUM  +  CX( K ) * U( IX( K ) )
        END DO

        V( R ) = SUM

    END DO

    RETURN

END SUBROUTINE SMATVECP



!!***********************************************************************
!! Version "$Id: fills.f90 1 2017-06-10 18:05:20Z coats $"
!! EDSS/Models-3 M3TOOLS.
!! Copyright (C) 1992-2002 MCNC,
!! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
!! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
!! (C) 2015 UNC Institute for the Environment
!! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
!! See file "GPL.txt" for conditions of use.
!!.........................................................................
!!  subroutine IFILL starts at line  46
!!  subroutine RFILL starts at line 120
!!  subroutine DFILL starts at line 193
!!  subroutine LFILL starts at line 193
!!
!!  FUNCTION:
!!       fill GRID( NCOLS, NROWS, NLAYS ) with value-pattern determined
!!       by OP:
!!               1 -- column number
!!               2 -- row number
!!               3 -- layer number
!!               otherwise:  value of VAL
!!
!!       where GRID has type
!!               INTEGER for IFILL
!!               REAL    for RFILL
!!               DOUBLE  for DFILL
!!
!!  PRECONDITIONS REQUIRED:
!!       none
!!
!!  SUBROUTINES AND FUNCTIONS CALLED:
!!       none
!!
!!  REVISION  HISTORY:
!!       prototype 8/95 by CJC
!!
!!       Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
!!       USE M3UTILIO, and related changes.
!!
!!       Version 11/2013 by CJC:  OpenMP parallel.  Reorganize loops
!!       with R outermost to allow parallelism whether or not NLAYS=1
!!
!!      Version 02/2015 by CJC for I/O API v3.2:  free-form F90 source;
!!      support for M3INT8 variables
!!***********************************************************************


SUBROUTINE  IFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOLS, NROWS, NLAYS
    INTEGER, INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
    INTEGER, INTENT(IN   ) :: OP
    REAL   , INTENT(IN   ) :: VAL


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, C, L, V


    !!***********************************************************************
    !!   begin body of subroutine  IFILL

    IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = C
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = R
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = L
        END DO
        END DO
        END DO
    ELSE
        V = INT( VAL )
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, V ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = V
        END DO
        END DO
        END DO
    END IF

    RETURN

END SUBROUTINE  IFILL


!!...........................  begin RFILL()  .....................

SUBROUTINE  RFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: NCOLS, NROWS, NLAYS
    REAL   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
    INTEGER, INTENT(IN   ) :: OP
    REAL   , INTENT(IN   ) :: VAL


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, C, L


    !!***********************************************************************
    !!   begin body of subroutine  RFILL

    IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = FLOAT( C )
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = FLOAT( R )
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = FLOAT( L )
        END DO
        END DO
        END DO
    ELSE
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, VAL ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = VAL
        END DO
        END DO
        END DO
    END IF

    RETURN

END  SUBROUTINE  RFILL


!!...........................  begin DFILL()  .....................

SUBROUTINE  DFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    INTEGER         , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
    DOUBLE PRECISION, INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
    INTEGER         , INTENT(IN   ) :: OP
    REAL            , INTENT(IN   ) :: VAL


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER          R, C, L
    DOUBLE PRECISION V


    !!***********************************************************************
    !!   begin body of subroutine  DFILL

    IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = DBLE( C )
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = DBLE( R )
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = DBLE( L )
        END DO
        END DO
        END DO
    ELSE
        V = DBLE( VAL )
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, V ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = V
        END DO
        END DO
        END DO
    END IF

    RETURN

END SUBROUTINE  DFILL


!!...........................  begin LFILL()  .....................

SUBROUTINE  LFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL )

    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    INTEGER  , INTENT(IN   ) :: NCOLS, NROWS, NLAYS
    INTEGER*8, INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )
    INTEGER  , INTENT(IN   ) :: OP
    REAL     , INTENT(IN   ) :: VAL


    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         R, C, L, V


    !!***********************************************************************
    !!   begin body of subroutine  IFILL

    IF ( OP .EQ. 1 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = C
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 2 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = R
        END DO
        END DO
        END DO
    ELSE IF ( OP .EQ. 3 ) THEN
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = L
        END DO
        END DO
        END DO
    ELSE
        V = INT( VAL )
!$OMP       PARALLEL DO DEFAULT( NONE ),
!$OMP&                  SHARED( GRID, NCOLS, NROWS, NLAYS, V ),
!$OMP&                  PRIVATE( I )
        DO  R = 1, NROWS
        DO  L = 1, NLAYS
        DO  C = 1, NCOLS
            GRID( C,R,L ) = V
        END DO
        END DO
        END DO
    END IF

    RETURN

END SUBROUTINE  LFILL




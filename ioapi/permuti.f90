
    !!***********************************************************************
    !!
    !!  GENERIC SUBROUTINE PERMUTI( N, INDX, ARR1 [, ARR2...] )
    !!      for INTEGER, INTEGER*8, REAL, 
    !!.........................................................................
    !! Version "$Id: permuti.f90 262 2024-09-19 16:13:34Z coats $"
    !! (C) 2023 Carlie J./ Coats, Jr.
    !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !! See file "LGPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine permuti1 starts at line   42
    !!  subroutine permuti2 starts at line   67
    !!  subroutine permuti3 starts at line   96
    !!  subroutine permutl1 starts at line  129
    !!  subroutine permutl2 starts at line  148
    !!  subroutine permutl3 starts at line  183
    !!  subroutine permutr1 starts at line  216
    !!  subroutine permutr2 starts at line  241
    !!  subroutine permutr3 starts at line  270
    !!  subroutine permutr1 starts at line  303
    !!  subroutine permutr2 starts at line  328
    !!  subroutine permutr3 starts at line  357
    !!  subroutine permutc1 starts at line  390
    !!  subroutine permutc2 starts at line  416
    !!  subroutine permutc3 starts at line  446...
    !!  subroutine permutli1 (etc.) for INTEGER*8 INDX start at line... 
    !!.........................................................................
    !!
    !!  FUNCTION:
    !!      Uses index-arrays INDX such as produced by the SORTI*() to put
    !!      argument arrays *ARR*()in-place into the appropriate sorted order  
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       INDX from one of the SORTI*()
    !!
    !!  REVISION  HISTORY:
    !!      prototype 11/2023 by Carlie J. Coats, Jr., Ph.D.
    !!***********************************************************************


SUBROUTINE PERMUTI1( N, INDX, ARR1 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )

    INTEGER     SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTI1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTI2( N, INDX, ARR1, ARR2 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )
    INTEGER, INTENT(INOUT) :: ARR2( N )

    INTEGER     SCR1( N )
    INTEGER     SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTI2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTI3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )
    INTEGER, INTENT(INOUT) :: ARR2( N )
    INTEGER, INTENT(INOUT) :: ARR3( N )

    INTEGER     SCR1( N )
    INTEGER     SCR2( N )
    INTEGER     SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTI3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTL1( N, INDX, ARR1 )

    INTEGER,   INTENT(IN   ) :: N
    INTEGER,   INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )

    INTEGER*8   SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTL1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTL2( N, INDX, ARR1, ARR2 )

    INTEGER,   INTENT(IN   ) :: N
    INTEGER,   INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )
    INTEGER*8, INTENT(INOUT) :: ARR2( N )

    INTEGER*8   SCR1( N )
    INTEGER*8   SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTL2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTL3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER,   INTENT(IN   ) :: N
    INTEGER,   INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )
    INTEGER*8, INTENT(INOUT) :: ARR2( N )
    INTEGER*8, INTENT(INOUT) :: ARR3( N )

    INTEGER*8   SCR1( N )
    INTEGER*8   SCR2( N )
    INTEGER*8   SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTL3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTR1( N, INDX, ARR1 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL,    INTENT(INOUT) :: ARR1( N )

    REAL        SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTR1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTR2( N, INDX, ARR1, ARR2 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL,    INTENT(INOUT) :: ARR1( N )
    REAL,    INTENT(INOUT) :: ARR2( N )

    REAL        SCR1( N )
    REAL        SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTR2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTR3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL,    INTENT(INOUT) :: ARR1( N )
    REAL,    INTENT(INOUT) :: ARR2( N )
    REAL,    INTENT(INOUT) :: ARR3( N )

    REAL        SCR1( N )
    REAL        SCR2( N )
    REAL        SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTR3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTD1( N, INDX, ARR1 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL*8,  INTENT(INOUT) :: ARR1( N )

    REAL*8      SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTD1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTD2( N, INDX, ARR1, ARR2 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL*8,  INTENT(INOUT) :: ARR1( N )
    REAL*8,  INTENT(INOUT) :: ARR2( N )

    REAL*8      SCR1( N )
    REAL*8      SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTD2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTD3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER, INTENT(IN   ) :: N
    INTEGER, INTENT(IN   ) :: INDX( N )
    REAL*8,  INTENT(INOUT) :: ARR1( N )
    REAL*8,  INTENT(INOUT) :: ARR2( N )
    REAL*8,  INTENT(INOUT) :: ARR3( N )

    REAL*8      SCR1( N )
    REAL*8      SCR2( N )
    REAL*8      SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTD3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTC1( N, INDX, ARR1 )

    INTEGER,      INTENT(IN   ) :: N
    INTEGER,      INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTC1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTC2( N, INDX, ARR1, ARR2 )

    INTEGER,      INTENT(IN   ) :: N
    INTEGER,      INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )
    CHARACTER(*), INTENT(INOUT) :: ARR2( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )
    CHARACTER( LEN( ARR2(1) ) ) :: SCR2( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTC2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTC3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER,      INTENT(IN   ) :: N
    INTEGER,      INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )
    CHARACTER(*), INTENT(INOUT) :: ARR2( N )
    CHARACTER(*), INTENT(INOUT) :: ARR3( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )
    CHARACTER( LEN( ARR2(1) ) ) :: SCR2( N )
    CHARACTER( LEN( ARR3(1) ) ) :: SCR3( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTC3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLI1( N, INDX, ARR1 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )

    INTEGER     SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLI1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLI2( N, INDX, ARR1, ARR2 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )
    INTEGER, INTENT(INOUT) :: ARR2( N )

    INTEGER     SCR1( N )
    INTEGER     SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLI2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLI3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER, INTENT(INOUT) :: ARR1( N )
    INTEGER, INTENT(INOUT) :: ARR2( N )
    INTEGER, INTENT(INOUT) :: ARR3( N )

    INTEGER     SCR1( N )
    INTEGER     SCR2( N )
    INTEGER     SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLI3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLL1( N, INDX, ARR1 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )

    INTEGER*8   SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLL1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLL2( N, INDX, ARR1, ARR2 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )
    INTEGER*8, INTENT(INOUT) :: ARR2( N )

    INTEGER*8   SCR1( N )
    INTEGER*8   SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLL2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLL3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    INTEGER*8, INTENT(INOUT) :: ARR1( N )
    INTEGER*8, INTENT(INOUT) :: ARR2( N )
    INTEGER*8, INTENT(INOUT) :: ARR3( N )

    INTEGER*8   SCR1( N )
    INTEGER*8   SCR2( N )
    INTEGER*8   SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLL3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLR1( N, INDX, ARR1 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL,      INTENT(INOUT) :: ARR1( N )

    REAL        SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLR1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLR2( N, INDX, ARR1, ARR2 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL,      INTENT(INOUT) :: ARR1( N )
    REAL,      INTENT(INOUT) :: ARR2( N )

    REAL        SCR1( N )
    REAL        SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLR2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLR3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL,      INTENT(INOUT) :: ARR1( N )
    REAL,      INTENT(INOUT) :: ARR2( N )
    REAL,      INTENT(INOUT) :: ARR3( N )

    REAL        SCR1( N )
    REAL        SCR2( N )
    REAL        SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLR3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLD1( N, INDX, ARR1 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL*8,    INTENT(INOUT) :: ARR1( N )

    REAL*8      SCR1( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLD1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLD2( N, INDX, ARR1, ARR2 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL*8,    INTENT(INOUT) :: ARR1( N )
    REAL*8,    INTENT(INOUT) :: ARR2( N )

    REAL*8      SCR1( N )
    REAL*8      SCR2( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLD2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLD3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER*8, INTENT(IN   ) :: N
    INTEGER*8, INTENT(IN   ) :: INDX( N )
    REAL*8,    INTENT(INOUT) :: ARR1( N )
    REAL*8,    INTENT(INOUT) :: ARR2( N )
    REAL*8,    INTENT(INOUT) :: ARR3( N )

    REAL*8      SCR1( N )
    REAL*8      SCR2( N )
    REAL*8      SCR3( N )
    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLD3


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLC1( N, INDX, ARR1 )

    INTEGER*8,    INTENT(IN   ) :: N
    INTEGER*8,    INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLC1


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLC2( N, INDX, ARR1, ARR2 )

    INTEGER*8,    INTENT(IN   ) :: N
    INTEGER*8,    INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )
    CHARACTER(*), INTENT(INOUT) :: ARR2( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )
    CHARACTER( LEN( ARR2(1) ) ) :: SCR2( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLC2


!!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


SUBROUTINE PERMUTLC3( N, INDX, ARR1, ARR2, ARR3 )

    INTEGER*8,    INTENT(IN   ) :: N
    INTEGER*8,    INTENT(IN   ) :: INDX( N )
    CHARACTER(*), INTENT(INOUT) :: ARR1( N )
    CHARACTER(*), INTENT(INOUT) :: ARR2( N )
    CHARACTER(*), INTENT(INOUT) :: ARR3( N )

    CHARACTER( LEN( ARR1(1) ) ) :: SCR1( N )
    CHARACTER( LEN( ARR2(1) ) ) :: SCR2( N )
    CHARACTER( LEN( ARR3(1) ) ) :: SCR3( N )

    INTEGER     I

    DO I = 1, N
        SCR1( I ) = ARR1( I )
        SCR2( I ) = ARR2( I )
        SCR3( I ) = ARR3( I )
    END DO

    DO I = 1, N
        ARR1( I ) = SCR1( INDX( I ) )
        ARR2( I ) = SCR2( INDX( I ) )
        ARR3( I ) = SCR3( INDX( I ) )
    END DO

    RETURN

END SUBROUTINE PERMUTLC3


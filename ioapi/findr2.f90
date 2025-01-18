
INTEGER FUNCTION FINDR2( K1, K2, N, LIST1, LIST2 )

    !***********************************************************************
    ! Version "$Id: findr2.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  53
    !
    !  RETURNS:
    !       subscript at which the targeted key-pair appears, or
    !       -1 in case of failure.
    !       EXAMPLE:  search for <ID7,ID3> ASC-pair in table of ASC values
    !       (where ID7 is leading-7 digits and ID3 is trailing 3 digits
    !       in a 10-digit Area Source Code).
    !
    !  PRECONDITIONS REQUIRED:
    !       Sorted table <N,LIST1,LIST2> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 2/28/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    REAL   , INTENT(IN   ) :: K1             !  first  key
    REAL   , INTENT(IN   ) :: K2             !  second key
    INTEGER, INTENT(IN   ) :: N              !  table size
    REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !***********************************************************************
    !   begin body of function  FINDR2

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FINDR2 = -1
        RETURN

    END IF

    M = ( LO + HI ) / 2
    IF ( K1 .GT. LIST1( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K1 .LT. LIST1( M ) ) THEN
        HI = M - 1
        GO TO  11
    ELSE IF ( K2 .GT. LIST2( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K2 .LT. LIST2( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this <K1,K2>


    FINDR2 = M
    RETURN
END FUNCTION FINDR2


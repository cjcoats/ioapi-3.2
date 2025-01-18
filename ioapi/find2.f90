
INTEGER FUNCTION FIND2( K1, K2, N, LIST1, LIST2 )

    !***********************************************************************
    ! Version "$Id: find2.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  FIND2()  function body starts at line  57
    !  FINDL2() function body starts at line  95
    !
    !  RETURNS:
    !       subscript at which the targeted key-pair appears, or
    !       -1 in case of failure.
    !       EXAMPLE:  search for <ID7,ID3> ASC-pair in table of ASC values
    !       (where ID7 is leading-7 digits and ID3 is trailing 3 digits
    !       in a 10-digit Area Source Code).
    !
    !  PRECONDITIONS REQUIRED:
    !       FIND2()  for INTEGER   key-tuples and lists
    !       FINDL2() for INTEGER*8 key-tuples and lists
    !       Sorted table <N,LIST1,LIST2> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 2/28/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  08/2014 by CJC: Add FINDL2()
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: K1             !  first  key
    INTEGER, INTENT(IN   ) :: K2             !  second key
    INTEGER, INTENT(IN   ) :: N              !  table size
    INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !.......................................................................
    !   begin body of function  FIND2

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FIND2 = -1
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


    FIND2 = M
    RETURN
END FUNCTION FIND2


    !***********************************************************************


INTEGER FUNCTION FINDL2( K1, K2, N, LIST1, LIST2 )

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER(8), INTENT(IN   ) :: K1             !  first  key
    INTEGER(8), INTENT(IN   ) :: K2             !  second key
    INTEGER,    INTENT(IN   ) :: N              !  table size
    INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !.......................................................................
    !   begin body of function  FINDL2

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FINDL2 = -1
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


    FINDL2 = M
    RETURN
END FUNCTION FINDL2

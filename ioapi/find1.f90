
INTEGER FUNCTION FIND1( K, N, LIST )

    !***********************************************************************
    ! Version "$Id: find1.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  FIND1()  function body starts at line 54
    !  FINDL1() function body starts at line 83
    !
    !  RETURNS:
    !       subscript at which the targeted key appears, or
    !       -1 in case of failure.
    !       EXAMPLE:  search for FIP in table of FIP values
    !
    !  PRECONDITIONS REQUIRED:
    !       FIND1() for INTEGER  K, LIST; FINDL1() for INTEGER*8 K, LIST
    !       Sorted table <N,LIST> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 3/1/95 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  08/2014 by CJC: Add FINDL1()
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: K             !  first  key
    INTEGER, INTENT(IN   ) :: N             !  table size
    INTEGER, INTENT(IN   ) :: LIST( N )     !  table to search for K


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !.......................................................................
    !   begin body of function  FIND1

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FIND1 = -1
        RETURN

    END IF

    M = ( LO + HI ) / 2
    IF ( K .GT. LIST( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K .LT. LIST( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this K


    FIND1 = M
    RETURN
END FUNCTION FIND1


    !***********************************************************************

INTEGER FUNCTION FINDL1( K, N, LIST )


    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER(8), INTENT(IN   ) :: K             !  first  key
    INTEGER,    INTENT(IN   ) :: N             !  table size
    INTEGER(8), INTENT(IN   ) :: LIST( N )     !  table to search for K


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO, HI, M


    !.......................................................................
    !   begin body of function  FINDL1

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FINDL1 = -1
        RETURN

    END IF

    M = ( LO + HI ) / 2
    IF ( K .GT. LIST( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K .LT. LIST( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this K


    FINDL1 = M
    RETURN
END FUNCTION FINDL1


INTEGER FUNCTION LOCATR1( K1, N, LIST1 )

    !***********************************************************************
    ! Version "$Id: locatr1.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems
    ! (C) 22021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 49
    !
    !  RETURNS:
    !       subscript at which the targeted REAL key should be inserted
    !       or -1 if the key is found.
    !       EXAMPLE:  search for <FIP> in table of FIP values.
    !
    !  PRECONDITIONS REQUIRED:
    !       Sorted table <N,LIST1> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 12/95 by MRH copied from FIND1 of CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    REAL   , INTENT(IN   ) :: K1             !  first  key
    INTEGER, INTENT(IN   ) :: N              !  table size
    REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !***********************************************************************
    !   begin body of function  LOCATR1

    LO = 1
    HI = N

    IF( N .EQ. 0 ) THEN

        LOCATR1 = -1
        RETURN

    ENDIF

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        LOCATR1 = LO
        RETURN

    END IF

    M = ( LO + HI ) / 2
    IF ( K1 .GT. LIST1( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K1 .LT. LIST1( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this <K1>


    LOCATR1 = -1         ! key found
    RETURN
END FUNCTION LOCATR1


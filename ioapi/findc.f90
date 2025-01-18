
INTEGER FUNCTION FINDC( KEY, N, LIST )

    !***********************************************************************
    ! Version "$Id: findc.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 49
    !
    !  RETURNS:
    !       subscript at which the targeted key appears, or
    !       -1 in case of failure.
    !       EXAMPLE:  search for FIP in table of FIP values
    !
    !  PRECONDITIONS REQUIRED:
    !       Sorted table <N,LIST> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 8/1/96 by CJC
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: KEY           !  key
    INTEGER      , INTENT(IN   ) :: N             !  table size
    CHARACTER*(*), INTENT(IN   ) :: LIST( N )     !  table to search for KEY


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !***********************************************************************
    !   begin body of function  FINDC

    LO = 1
    HI = N

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        FINDC = -1
        RETURN

    END IF

    M = ( LO + HI ) / 2
    IF ( KEY .GT. LIST( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( KEY .LT. LIST( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this K


    FINDC = M
    RETURN
END FUNCTION FINDC


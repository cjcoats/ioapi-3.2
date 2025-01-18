
INTEGER FUNCTION LOCATR4( K1, K2, K3, K4, N,            &
                          LIST1, LIST2, LIST3, LIST4 )

    !***********************************************************************
    ! Version "$Id: locatr4.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 58
    !
    !  RETURNS:
    !       subscript at which the targeted REAL key-quadruple should be
    !       inserted, or -1 if the key-quadruple is found.
    !       EXAMPLE:  search for <FIP,ID7,ID3> in table of FIP and ASC values
    !       (where ID7 is leading-7 digits and ID3 is trailing 3 digits
    !       in a 10-digit Area Source Code).
    !
    !  PRECONDITIONS REQUIRED:
    !       Sorted table <N,LIST1,LIST3,LIST3,LIST4> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 12/95 by MRH copied from FIND3 of CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


!...........   ARGUMENTS and their descriptions:

    REAL   , INTENT(IN   ) :: K1             !  first  key
    REAL   , INTENT(IN   ) :: K2             !  second key
    REAL   , INTENT(IN   ) :: K3             !  third  key
    REAL   , INTENT(IN   ) :: K4             !  fourth key
    INTEGER, INTENT(IN   ) :: N              !  table size
    REAL   , INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    REAL   , INTENT(IN   ) :: LIST2( N )     !  table to search for K2
    REAL   , INTENT(IN   ) :: LIST3( N )     !  table to search for K3
    REAL   , INTENT(IN   ) :: LIST4( N )     !  table to search for K4


!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


!***********************************************************************
!   begin body of function  LOCATR4

    LO = 1
    HI = N

    IF( N .EQ. 0 ) THEN

        LOCATR4 = -1
        RETURN

    ENDIF

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        LOCATR4 = LO
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
    ELSE IF ( K3 .GT. LIST3( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K3 .LT. LIST3( M ) ) THEN
        HI = M - 1
        GO TO  11
    ELSE IF ( K4 .GT. LIST4( M ) ) THEN
        LO = M + 1
        GO TO  11
    ELSE IF ( K4 .LT. LIST4( M ) ) THEN
        HI = M - 1
        GO TO  11
    END IF          !  end of bin search loop for this <K1,K2,K3>


    LOCATR4 = -1         ! key-tuple found
    RETURN
END FUNCTION LOCATR4


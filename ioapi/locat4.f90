
INTEGER FUNCTION LOCAT4( K1, K2, K3, K4, N, LIST1, LIST2, LIST3, LIST4 )

    !***********************************************************************
    ! Version "$Id: locat4.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  LOCAT4()   function body starts at line  65
    !  LOCATL4()  function body starts at line 141
    !
    !  RETURNS:
    !       subscript at which the targeted INTEGER key-triple should be
    !       inserted, or -1 if the key-triple is found.
    !       EXAMPLE:  search for <FIP,ID7,ID3> in table of FIP and ASC values
    !       (where ID7 is leading-7 digits and ID3 is trailing 3 digits
    !       in a 10-digit Area Source Code).
    !
    !  PRECONDITIONS REQUIRED:
    !       LOCAT4()  for INTEGER   key-tuples and lists
    !       LOCATL4() for INTEGER*8 key-tuples and lists
    !       Sorted table <N,LIST1,LIST3,LIST3,LIST4> for searching
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 12/95 by MRH copied from FIND3 of CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  08/2014 by CJC: Add LOCATL4()
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: K1             !  first  key
    INTEGER, INTENT(IN   ) :: K2             !  second key
    INTEGER, INTENT(IN   ) :: K3             !  third  key
    INTEGER, INTENT(IN   ) :: K4             !  fourth key
    INTEGER, INTENT(IN   ) :: N              !  table size
    INTEGER, INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    INTEGER, INTENT(IN   ) :: LIST2( N )     !  table to search for K2
    INTEGER, INTENT(IN   ) :: LIST3( N )     !  table to search for K3
    INTEGER, INTENT(IN   ) :: LIST4( N )     !  table to search for K4


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !.......................................................................
    !   begin body of function  LOCAT4

    LO = 1
    HI = N

    IF( N .EQ. 0 ) THEN

        LOCAT4 = -1
        RETURN

    ENDIF

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        LOCAT4 = LO
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


    LOCAT4 = -1         ! key-tuple found
    RETURN
END FUNCTION LOCAT4


    !***********************************************************************


INTEGER FUNCTION LOCATL4( K1, K2, K3, K4, N, LIST1, LIST2, LIST3, LIST4 )

    IMPLICIT NONE


    !...........   ARGUMENTS and their descriptions:

    INTEGER(8), INTENT(IN   ) :: K1             !  first  key
    INTEGER(8), INTENT(IN   ) :: K2             !  second key
    INTEGER(8), INTENT(IN   ) :: K3             !  third  key
    INTEGER(8), INTENT(IN   ) :: K4             !  fourth key
    INTEGER   , INTENT(IN   ) :: N              !  table size
    INTEGER(8), INTENT(IN   ) :: LIST1( N )     !  table to search for K1
    INTEGER(8), INTENT(IN   ) :: LIST2( N )     !  table to search for K2
    INTEGER(8), INTENT(IN   ) :: LIST3( N )     !  table to search for K3
    INTEGER(8), INTENT(IN   ) :: LIST4( N )     !  table to search for K4


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER  LO
    INTEGER  HI
    INTEGER  M


    !.......................................................................
    !   begin body of function  LOCATL4

    LO = 1
    HI = N

    IF( N .EQ. 0 ) THEN

        LOCATL4 = -1
        RETURN

    ENDIF

11  CONTINUE

    IF ( LO .GT. HI ) THEN

        LOCATL4 = LO
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


    LOCATL4 = -1         ! key-tuple found
    RETURN
END FUNCTION LOCATL4


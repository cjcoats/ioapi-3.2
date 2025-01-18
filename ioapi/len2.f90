
INTEGER FUNCTION LEN2 (J1, J2, STRING)

    !***********************************************************************
    ! Version "$Id: len2.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !    function body starts at line 42
    !
    !  FUNCTION:
    !
    !    Returns the number of leading blanks in STRING( J1:J2 )
    !
    !  REVISION HISTORY:
    !
    !       5/88   Modified for ROMNET
    !       8/90   Modified for ROM 2.2:  simpler algorithm uses DO loop.
    !       2/93   Modified for CRAY by CJC.
    !       9/94   Simpler algorithm for Models-3 by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !.......   Arguments and their descriptions:

    INTEGER      , INTENT(IN   ) :: J1        !  First position in string to be searched
    INTEGER      , INTENT(IN   ) :: J2        !  Last     "
    CHARACTER*(*), INTENT(IN   ) :: STRING    !  Character string to search

    !.......   Local variable:  loop counter

    INTEGER       I

    !........................................................................
    !.......   begin body:  Scan from left to right until non blank character

    DO  100  I = J1 , J2

        IF ( STRING ( I:I ) .NE. ' ' )  THEN
            LEN2 = I - J1
            RETURN
        END IF

100 CONTINUE

    LEN2 = J2 - J1 + 1
    RETURN

END FUNCTION LEN2


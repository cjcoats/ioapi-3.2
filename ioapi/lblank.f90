
INTEGER FUNCTION LBLANK( STRING )

    !***********************************************************************
    ! Version "$Id: lblank.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !    function body starts at line 42
    !
    !  FUNCTION:
    !
    !    Returns the number of leading blanks in STRING
    !
    !  REVISION HISTORY:
    !
    !       Adapted  09/1995 from ROM utility routine LEN2() by CJC
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !.......   Arguments and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: STRING	!  Character string to search

    !.......   PARAMETERs:

    CHARACTER*1, PARAMETER :: BLANK = ' '


    !.......   Local variable:  loop counter

    INTEGER       I, L

    !........................................................................
    !.......   begin body:  Scan from left to right until non blank character

    L = LEN( STRING )
    DO  100  I = 1 , L

        IF ( ( STRING( I:I ) .NE. BLANK ) .AND.         &
             ( STRING( I:I ) .NE. CHAR( 9 ) ) ) THEN
            LBLANK = I - 1
            RETURN
        END IF

100 CONTINUE

    LBLANK = L
    RETURN

END FUNCTION LBLANK


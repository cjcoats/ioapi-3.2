
INTEGER FUNCTION INDEX1( NAME, N, NLIST )

    !***********************************************************************
    ! Version "$Id: index1.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! BAMS/MCNC/EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  INDEX1    body starts at line 58
    !  INDEXINT1 body starts at line 74
    !
    !  FUNCTION:
    !
    !       Search for character-string or integer key NAME or IKEY in list NLIST
    !       and return the subscript (1...N) at which it is found, or return 0
    !       when not found in NLIST
    !
    !  PRECONDITIONS REQUIRED:
    !       none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       none
    !
    !  REVISION HISTORY:
    !    INDEX1:
    !       5/1988   Modified for ROMNET
    !       9/1994   Modified for Models-3 by CJC
    !    INDEXINT1:
    !       Prototype 11/2004 by CJC:  for I/O API v3
    !       Modified   3/2006 by CJC:  moved INDEXINT1() to file "index1.f"
    !
    !       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !.......   Arguments and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: NAME        !  Character string being searched for
    INTEGER      , INTENT(IN   ) :: N           !  Length of array to be searched
    CHARACTER*(*), INTENT(IN   ) :: NLIST(*)    !  array to be searched

    !.......   Local variable:

    INTEGER       I   !  loop counter

    !.....................................................................
    !.......   begin body of INDEX1()

    DO I = 1, N
        IF ( NAME .EQ. NLIST( I ) ) THEN    ! Found NAME in NLIST
            INDEX1 = I
            RETURN
        ENDIF
    END DO

    INDEX1 = 0        !  not found
    RETURN

END FUNCTION INDEX1


 ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


INTEGER FUNCTION INDEXINT1( IKEY, NLIST, KEYLIST )

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !  Look up integer key IKEY in unsorted list <NLIST,KEYLIST>
    !  of integer keys.  Return the subscript at which IKEY
    !  occurs, or 0 in case of failure
    !
    !  PRECONDITIONS REQUIRED:
    !      none
    !
    !  REVISION  HISTORY:
    !      Prototype  11/2004 by CJC
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    IMPLICIT NONE


    !!........  Arguments:

    INTEGER, INTENT(IN   ) :: IKEY
    INTEGER, INTENT(IN   ) :: NLIST
    INTEGER, INTENT(IN   ) :: KEYLIST( NLIST )


    !!........  Local Variables:

    INTEGER I

    !!........  begin body ........................................

    DO  I = 1, NLIST
        IF ( IKEY .EQ. KEYLIST( I ) ) THEN
            INDEXINT1 = I
            RETURN
        END IF
    END DO

    INDEXINT1 = 0

    RETURN
END FUNCTION INDEXINT1


 ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


INTEGER FUNCTION INDEXL1( IKEY, NLIST, KEYLIST )

    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !  Look up integer key IKEY in unsorted list <NLIST,KEYLIST>
    !  of integer keys.  Return the subscript at which IKEY
    !  occurs, or 0 in case of failure
    !
    !  PRECONDITIONS REQUIRED:
    !      none
    !
    !  REVISION  HISTORY:
    !      Prototype  11/2004 by CJC
    !-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    IMPLICIT NONE


    !!........  Arguments:

    INTEGER*8, INTENT(IN   ) :: IKEY
    INTEGER,   INTENT(IN   ) :: NLIST
    INTEGER*8, INTENT(IN   ) :: KEYLIST( NLIST )


    !!........  Local Variables:

    INTEGER I

    !!........  begin body ........................................

    DO  I = 1, NLIST
        IF ( IKEY .EQ. KEYLIST( I ) ) THEN
            INDEXL1 = I
            RETURN
        END IF
    END DO

    INDEXL1 = 0

    RETURN
END FUNCTION INDEXL1




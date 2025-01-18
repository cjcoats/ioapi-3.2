
LOGICAL FUNCTION CKNAME( NAME )

    !***********************************************************************
    ! Version "$Id: ckname.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  50
    !
    !  RETURNS:  TRUE iff NAME has content but no embedded blanks.
    !
    !  PRECONDITIONS REQUIRED:  ASCII character representations
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !  	    Prototype 02/   997 by CJC
    !
    !       Version   10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) ::	NAME


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER		    K, L, M
    CHARACTER*1     CH


    !...........   SAVED LOCAL VARIABLES:  alpha, alphanumeric tables

    CHARACTER*56, PARAMETER :: ALPHA = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz$_#@'

    CHARACTER*10, PARAMETER :: NUMER = '0123456789'

    CHARACTER*66, SAVE :: ALNUM = ' '

    !***********************************************************************
    !   begin body of function  CKNAME

    IF ( ALNUM( 1:1 ) .EQ. ' ' ) THEN
        ALNUM = ALPHA // NUMER
    END IF

    L = LEN( NAME )
    DO  11  K = L, 1, -1
        CH = NAME( K:K )
        IF ( CH .NE. ' ' ) THEN
            GO TO 12
        END IF
11  CONTINUE

    !.......   If you get to here:  entire name blank.

    CKNAME = .FALSE.
    RETURN

    !.......   Number of trailing blanks found.
    !.......   Check rest of name for legality:

12  CONTINUE

    IF ( INDEX( ALPHA, NAME( 1:1 ) ) .EQ. 0 ) THEN
        CKNAME = .FALSE.
        RETURN
    END IF

    DO  22  M = 2, K
        IF ( INDEX( ALNUM, NAME( M:M ) ) .EQ. 0 ) THEN
            CKNAME = .FALSE.
            RETURN
        END IF
22  CONTINUE

    !...........   If you get to here:  entire name OK

    CKNAME = .TRUE.
    RETURN
END FUNCTION CKNAME


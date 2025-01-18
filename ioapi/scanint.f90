
SUBROUTINE SCANINT( STRING, VALUE, NCHARS, NDIGITS )

    !***********************************************************************
    ! Version "$Id: scanint.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., 
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line 70
    !
    !  RETURNS:
    !       INTEGER
    !               VALUE decoded from STRING, or IMISS3 for "missing",
    !               after skipping leading blanks.
    !               NCHARS the number of characters consumed (including
    !               leading whitespace
    !               NDIGITS the number of digits (counting leading
    !               minus-sign, if any)
    !
    !  PRECONDITIONS REQUIRED:
    !       ASCII.
    !       Properly formatted integer in STRING.  In particular:
    !         * no whitespace between sign and digits composing the rest
    !           of the value; and
    !         * leading whitespace is OK (and is skipped over and counted);
    !           whitespace is defined to be characters <= BLANK
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       M3WARN()
    !
    !  REVISION  HISTORY:
    !       Adapted 7/2001 by CJC from STR2INT()
    !
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'


    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: STRING
    INTEGER      , INTENT(  OUT) :: VALUE, NCHARS, NDIGITS


    !...........   PARAMETERS

    CHARACTER*1, PARAMETER :: BLANK = ' '

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         SUM, SIGN
    INTEGER         I, J, K, L
    INTEGER         IC, I0
    CHARACTER*256   MESG


    !***********************************************************************
    !   begin body of function  SCANINT

    L = LEN( STRING )

    DO  11  I = 1, L        !  skip leading whitespace
        IF ( STRING( I:I ) .GT. BLANK ) GO TO 12
11  CONTINUE

    !.......   If you get to here:  no number there

    VALUE   = IMISS3
    NCHARS  = L
    NDIGITS = 0
    RETURN

12  CONTINUE

    IF( STRING( I:I ) .EQ. '-' ) THEN       !  adjust for sign
        SIGN    = -1
        I       = I + 1
        NDIGITS = 1
    ELSE IF( STRING( I:I ) .EQ. '+' ) THEN
        SIGN    = 1
        I       = I + 1
        NDIGITS = 0
    ELSE
        SIGN = 1
        NDIGITS = 0
    END IF
    NCHARS  = I

    SUM = 0         !  accumulate as long as there are digits.
    K   = 0
    I0  = ICHAR( '0' )
    DO  22  J = I, L
        IC = ICHAR( STRING( J:J ) ) - I0
        IF ( IC .LT. 0  .OR.  IC .GT. 9 )  GO TO  23
        SUM = 10 * SUM  +  IC
        K   = K   +  1
22  CONTINUE
23  CONTINUE
    NCHARS  = NCHARS  + K
    NDIGITS = NDIGITS + K

    IF ( K .GT. 0 ) THEN
        VALUE = SIGN * SUM
    ELSE
        MESG = 'No digits in  "' // STRING // '"'
        CALL M3WARN( 'SCANINT', 0, 0, MESG )
        VALUE = IMISS3
    END IF

    RETURN
END SUBROUTINE SCANINT


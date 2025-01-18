
INTEGER FUNCTION STR2INT( STRING )

    !***********************************************************************
    ! Version "$Id: str2int.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    ! (C) 2016 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 63
    !
    !  RETURNS:
    !       INTEGER value decoded from STRING, or IMISS3 for "missing",
    !       after skipping leading blanks.
    !
    !  PRECONDITIONS REQUIRED:
    !       Properly formatted integer in STRING
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       M3WARN()
    !
    !  REVISION  HISTORY:
    !       Prototype 6/95 by CJC for point source prototype
    !
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Modified 06/20020 by CJC: expand MESG to CHARACTER*256
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'


    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: STRING


    !...........   PARAMETERS

    CHARACTER*1, PARAMETER :: BLANK = ' '

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         SUM, SIGN
    INTEGER         I, J, K, L
    INTEGER         IC, I0
    CHARACTER*80    MESG


    !***********************************************************************
    !   begin body of function  STR2INT

    L = LEN( STRING )

    DO  11  I = 1, L        !  skip leading whitespace
        IF ( STRING( I:I ) .GT. BLANK ) GO TO 12
11  CONTINUE

    !.......   If you get to here:  no number there

    STR2INT = IMISS3
    RETURN

12  CONTINUE

    IF( STRING( I:I ) .EQ. '-' ) THEN       !  adjust for sign
        SIGN = -1
        I    = I + 1
    ELSE IF( STRING( I:I ) .EQ. '+' ) THEN
        SIGN = 1
        I    = I + 1
    ELSE
        SIGN = 1
    END IF

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

    IF ( K .GT. 0 ) THEN
        STR2INT = SIGN * SUM
    ELSE
        MESG = 'No digits in  "' // STRING // '"'
        CALL M3WARN( 'STR2INT', 0, 0, MESG )
        STR2INT = IMISS3
    END IF

    RETURN
END FUNCTION STR2INT


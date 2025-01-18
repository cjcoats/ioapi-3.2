
SUBROUTINE LUSTR( STRING )

    !***********************************************************************
    ! Version "$Id: lustr.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  52
    !
    !  FUNCTION:
    !       left-justify and upcase contents of STRING
    !
    !  PRECONDITIONS REQUIRED: none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED: none
    !
    !  REVISION  HISTORY:
    !       Prototype 6/1995 by CJC
    !
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(INOUT) :: STRING


    !...........   PARAMETERS and their descriptions:

    CHARACTER*1, PARAMETER :: BLANK = ' '
    INTEGER    , PARAMETER :: IDIFF = 65 - 97       ! = ichar( 'A' ) -  ichar( 'a' )


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ILEN
    INTEGER         I, J, IBEG
    CHARACTER*1     CH


    !***********************************************************************
    !   begin body of subroutine  LUSTR

    ILEN = LEN( STRING )
    DO I = 1, ILEN
        IF( STRING(I:I) .NE. BLANK ) THEN
            IBEG = I
            GO TO  11
        END IF
    END DO

    !.......   If you get to here:  string all blanks.  No action necessary.

    RETURN

11  CONTINUE

    !.......   Go thru rest of string, replacing lower-case by corresponding upper

    J = 1
    DO  I = IBEG, ILEN
        CH = STRING( I:I )
        IF ( CH .GE. 'a'  .AND.  CH .LE. 'z' ) THEN
            STRING(J:J) = CHAR( ICHAR( CH ) + IDIFF )
        ELSE
            STRING(J:J) = CH
        END IF
        J = J + 1
    END DO

    !.......    pad trailing section of string with blank

    STRING(J:) = BLANK

    RETURN
END SUBROUTINE LUSTR



CHARACTER*2 FUNCTION CRLF()

    !***********************************************************************
    ! Version "$Id: crlf.F 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  36
    !
    !  FUNCTION:  Return character string of character return and line feed
    !
    !  PRECONDITIONS REQUIRED:
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 1/97 by M Houyoux
    !       Modified 5/2003 by Carlie J. Coats, Jr., BAMS,  so that it
    !       works correctly for "normal" UNIX systems.
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !****************************************************************************

    IMPLICIT NONE

    CHARACTER*2 VALUE
    SAVE        VALUE

    LOGICAL, SAVE :: FIRSTIME = .TRUE.

    !***********************************************************************
    !   begin body of subroutine CRLF

    IF( FIRSTIME ) THEN
        FIRSTIME = .FALSE.
#ifdef _WIN32
        VALUE( 1:1 ) = CHAR( 13 )
        VALUE( 2:2 ) = CHAR( 10 )
#endif
#ifndef _WIN32
        VALUE( 1:1 ) = ' '
        VALUE( 2:2 ) = CHAR( 10 )
#endif
    ENDIF

    CRLF = VALUE

    RETURN

END FUNCTION CRLF

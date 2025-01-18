
LOGICAL FUNCTION CHKBUF3( FDUM )

    !***********************************************************************
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2011 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  95
    !
    !  FUNCTION:  Check consistency pf BUFFERED file treatment between
    !             libioapi.a and model-code
    !
    !  RETURN VALUE:  TRUE iff consistent
    !
    !  PRECONDITIONS REQUIRED:  call after INIT3()
    !
    !  REVISION  HISTORY:
    !       prototype 04/2011 by Carlie J. Coats, Jr.
    !       Version   10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(  OUT) :: FDUM            !  prevents excessive optimization

    !.............................................................................
    !   begin body of subroutine  CHKBUF3

    FDUM    = VGTYP3( 1 )
    CHKBUF3 = .TRUE.
    RETURN

END FUNCTION CHKBUF3

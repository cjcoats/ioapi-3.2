
REAL FUNCTION STR2REAL( STRING )

    !***********************************************************************
    ! Version "$Id: str2real.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    ! (C) 2016-2020 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  63
    !
    !  RETURNS:
    !       REAL value decoded from STRING, or BADVAL3 for "missing",
    !       after skipping leading blanks.
    !
    !  PRECONDITIONS REQUIRED:
    !       Properly formatted REAL in STRING
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       M3ERR()
    !
    !  REVISION  HISTORY:
    !       Prototype 6/95 by CJC for point source prototype
    !
    !       Modified 03/20010 by CJC: F90 changes for I/O API v3.1
    !
    !       Modified 03/20016 by CJC: F90 change:  READ( STRING, *,...)
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

    REAL		    VAL
    INTEGER         IOS
    CHARACTER*256   MSG


    !***********************************************************************
    !   begin body of function  STR2REAL

    IF ( STRING .EQ. BLANK ) THEN
        STR2REAL = BADVAL3
        RETURN
    END IF

    READ( STRING, *, IOSTAT = IOS ) VAL

    IF( IOS .NE. 0 ) THEN
        WRITE( MSG, '( 3A, I10 )' ) 'Error reading REAL from "', TRIM( STRING ), '"; IOSTAT=', IOS
        CALL M3WARN( 'STR2REAL', 0, 0, MSG )
        STR2REAL = BADVAL3
    ELSE
        STR2REAL = VAL
    END IF

    RETURN

END FUNCTION STR2REAL


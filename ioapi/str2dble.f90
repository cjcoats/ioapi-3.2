
REAL*8 FUNCTION STR2DBLE( STRING )

    !***********************************************************************
    ! Version "$Id: str2dble.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems, and
    ! (C) 2016-2020 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  58
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
    !       Adapted 4/2003 by CJC from STR2DBLE()
    !       Bug-fix 5/2009 from B.H. Baek, UNC-CH:  VAL should be REAL*8
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !       Modified 06/20016 by CJC: F90 change:  READ( STRING, *,...)
    !       Modified 06/20020 by CJC: expand MESG to CHARACTER*256
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

    REAL*8          VAL
    INTEGER         IOS
    CHARACTER*256   MSG


    !***********************************************************************
    !   begin body of function  STR2DBLE

    IF ( STRING .EQ. BLANK ) THEN
        STR2DBLE = BADVAL3
        RETURN
    END IF

    READ( STRING, *, IOSTAT = IOS ) VAL

    IF( IOS .NE. 0 ) THEN
        WRITE( MSG, '( 3A, I10 )' ) 'Error reading DOUBLE from "', TRIM( STRING ), '"; IOSTAT=', IOS
        CALL M3WARN( 'STR2DBLE', 0, 0, MSG )
        STR2DBLE = BADVAL3
    ELSE
        STR2DBLE = VAL
    END IF

    RETURN

END FUNCTION STR2DBLE


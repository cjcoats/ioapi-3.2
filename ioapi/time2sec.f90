
INTEGER  FUNCTION  TIME2SEC ( TIME )

    !***********************************************************************
    ! Version "$Id: time2sec.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  41
    !
    !  FUNCTION:  convert time difference format HHMMSS to integer seconds
    !
    !  RETURN VALUE:  seconds
    !
    !  PRECONDITION:  integer TIME formatted HHMMSS
    !
    !  REVISION  HISTORY:
    !       Prototype  05/1992 by CJC
    !       Version    02/1993 by CJC for CRAY, etc.
    !       Modified   03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version    10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: TIME

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER      	ABST


    !***********************************************************************
    !   begin body of function  TIME2SEC

    IF ( TIME .GE. 0 ) THEN		!  div-mod arithmetic OK
        TIME2SEC = MOD( TIME, 100 )  +                      &
                        60 * ( MOD( TIME / 100, 100 )  +    &
                               60 * ( TIME / 10000 ) )
    ELSE				!  work with absolute values:
        ABST     = - TIME
        TIME2SEC = - ( MOD( ABST, 100 )  +                      &
                            60 * ( MOD( ABST / 100, 100 )  +    &
                                   60 * ( ABST / 10000 ) ) )
    END IF

    RETURN

END FUNCTION TIME2SEC


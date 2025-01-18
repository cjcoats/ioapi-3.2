
INTEGER  FUNCTION SEC2TIME( SECS )

    !***********************************************************************
    ! Version "$Id: sec2time.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  39
    !
    !  FUNCTION:  convert integer seconds to time difference format HHMMSS
    !
    !  RETURN VALUE:  integer HHMMSS formatted secs
    !
    !  REVISION  HISTORY:
    !       Prototype  5/92 by CJC
    !       Version    3/93 by CJC for CRAY, etc.
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: SECS


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER        	ABSS


    !***********************************************************************
    !   begin body of function  SEC2TIME

    IF ( SECS .GE. 0 ) THEN		!  div-mod arithmetic OK
        SEC2TIME = MOD( SECS, 60 )  +                   &
                100 * ( MOD( SECS / 60, 60 )  +         &
                100 * ( SECS / 3600 ) )
    ELSE				!  work with absolute values:
        ABSS     = - SECS
        SEC2TIME = - ( MOD( ABSS, 60 )  +               &
                    100 * ( MOD( ABSS / 60, 60 )  +     &
                    100 * ( ABSS / 3600 ) ) )
    END IF

    RETURN

END FUNCTION SEC2TIME


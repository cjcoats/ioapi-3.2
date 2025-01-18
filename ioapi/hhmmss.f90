
CHARACTER*10 FUNCTION HHMMSS ( JTIME )

    !***********************************************************************
    ! Version "$Id: hhmmss.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  57
    !
    !  FUNCTION:  format and return the time as a character string
    !             "HH:MM:SS"
    !
    !
    !  PRECONDITIONS REQUIRED:  valid time HHMMSS with hours component
    !             at most 9999
    !
    !  RETURN VALUE:  time, as "HH:MM:SS"
    !
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !
    !  REVISION  HISTORY:
    !	    prototype 10/90 by CJC
    !       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: JTIME   !  Julian time, coded YYYYDDD


    !...........   PARAMETERs

    CHARACTER*1, PARAMETER :: DIGITS( 0:9 ) = (/ '0','1','2','3','4','5','6','7','8','9' /)

    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       HOUR
    INTEGER       MINS
    INTEGER       SECS
    INTEGER       J , K

    CHARACTER*10    CHRBUF


    !***********************************************************************
    !   begin body of function  HHMMSS

    CHRBUF = '          '

    HOUR = JTIME
    SECS = MOD ( HOUR , 100 )
    HOUR = HOUR / 100
    MINS = MOD ( HOUR , 100 )
    HOUR = HOUR / 100

    J = 1
    K = HOUR / 1000
    IF ( K .GT. 9 ) THEN
        HHMMSS = '<TIMERROR>'
        RETURN
    ELSE IF ( K .NE. 0 ) THEN
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1
    END IF

    K = MOD( HOUR / 100 , 10 )
    IF ( K .NE. 0 ) THEN
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1
    END IF

    K = MOD( HOUR / 10 , 10 )
    IF ( K .NE. 0 ) THEN
        CHRBUF( J:J ) = DIGITS( K )
        J = J + 1
    END IF

    CHRBUF( J:J ) = DIGITS( MOD( HOUR, 10 ) )
    J = J + 1
    CHRBUF( J:J ) = ':'
    J = J + 1

    CHRBUF( J:J ) = DIGITS( MINS / 10 )
    J = J + 1
    CHRBUF( J:J ) = DIGITS( MOD( MINS, 10 ) )
    J = J + 1
    CHRBUF( J:J ) = ':'
    J = J + 1

    CHRBUF( J:J ) = DIGITS( SECS / 10 )
    J = J + 1
    CHRBUF( J:J ) = DIGITS( MOD( SECS, 10 ) )

    HHMMSS = CHRBUF

    RETURN

END FUNCTION HHMMSS



CHARACTER*24 FUNCTION  DT2STR( JDATE, JTIME )

    !***********************************************************************
    ! Version "$Id: dt2str.f 1 2017-06-10 18:05:20Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  60
    !
    !  FUNCTION:  format and return the date and time as a character string
    !             "HH:MM:SS  M+ D+, YYYY"
    !
    !
    !  PRECONDITIONS REQUIRED:  valid Julian date YYYYDDD, time HHMMSS
    !
    !
    !  RETURN VALUE:  date & time, as "HH:MM:SS  MMM DD, YYYY"
    !
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !
    !  REVISION  HISTORY:
    !       prototype 10/90 by CJC
    !
    !       Version    2/93 by CJC for CRAY, etc.
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT(IN   ) :: JDATE   !  Julian date, coded YYYYDDD
    INTEGER, INTENT(IN   ) :: JTIME   !  time, coded HHMMSS


    !...........  EXTERNAL FUNCTIONS:

    CHARACTER*10, EXTERNAL :: HHMMSS
    CHARACTER*14, EXTERNAL :: MMDDYY


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       J, T

    CHARACTER*10    TIMBUF
    CHARACTER*24    DATBUF


    !***********************************************************************
    !   begin body of function  DT2STR

    J = JDATE
    T = JTIME
    CALL NEXTIME( J, T, 0 )
    TIMBUF = HHMMSS( T )
    DATBUF = MMDDYY( J )
    DT2STR = TIMBUF // DATBUF
    RETURN

END FUNCTION  DT2STR


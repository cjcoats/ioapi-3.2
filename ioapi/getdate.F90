
INTEGER FUNCTION GETDATE ( DEFAULT , PROMPT )

    !********************************************************************
    ! Version "$Id: getdate.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2010 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !       function body starts at line  63
    !
    !  FUNCTION:
    !
    !       Display the  PROMPT  for a date in either of two formats:
    !       YYYYDDD or YYYYMMDD
    !       For non-IO_360, attempts to detect both Gregorian YYYYMMDD and
    !       Julian YYYYDDD representations and return the correct Julian in
    !       both cases.
    !
    !  CALLS:
    !       GETNUM. JULIAN
    !
    !  RETURNS:
    !       user response after checking its range; or default.
    !
    !  REVISION HISTORY:
    !
    !       Created 1/1997 by M Houyoux, MCNC Environmental Programs
    !
    !       2/2002 Unification by Carlie J. Coats, Jr., MCNC Environmental
    !       Programs with global-climate GETDATE, which uses a 360-day "year".
    !
    !       Modified 03/20010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 11/2015 by CJC: IO_365 changes
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !.......   ARGUMENTS:

    INTEGER      , INTENT(IN   ) :: DEFAULT         !  Default return date, YYYYDDD
    CHARACTER*(*), INTENT(IN   ) :: PROMPT          !  Prompt for user

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: GETNUM, JULIAN

    !.......   LOCAL VARIABLES:

    INTEGER         JDATE, YEAR, MONTH, DAY
    CHARACTER*256   MESG

    !......................................................................
    !       begin GETDATE

11  CONTINUE

    JDATE = GETNUM( 0, 999999999, DEFAULT, PROMPT )

#ifndef IO_360
#ifndef IO_365

    !.........  For Gregorian input date - convert to Julian

    IF( JDATE .GT. 9999999 ) THEN

        YEAR = JDATE/10000
        MONTH = ( JDATE-YEAR*10000 ) / 100
        DAY   = MOD( JDATE-YEAR*10000, 100 )
        JDATE = YEAR*1000 + JULIAN( YEAR, MONTH, DAY )

    ENDIF

    !.........  Check range

    IF( JDATE .LT. 1970001 .OR. JDATE .GT. 2100365 ) THEN

        MESG = 'Date is out of acceptable modeling range'
        CALL M3WARN( 'GETDATE', 0, 0, MESG )

        GO TO 11   ! Read in date again.

    ENDIF
#endif
#endif

    WRITE( MESG, '( A, I9.7 )' ) 'Using date', JDATE
    CALL M3MSG2( MESG )
    GETDATE = JDATE

    RETURN

END FUNCTION GETDATE


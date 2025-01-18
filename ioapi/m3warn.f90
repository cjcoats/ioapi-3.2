
SUBROUTINE M3WARN( CALLER, JDATE, JTIME, MSGTXT )

    !***********************************************************************
    ! Version "$Id: m3warn.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2012 by Baron Advanced Meteorological Systems
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  65
    !
    !  FUNCTION:  Generate simple warning messages for Models-3 core;
    !
    !  PRECONDITIONS REQUIRED:
    !	JDATE:JTIME represented as YYYYDDD:HHMMSS, or 0 if not relevant
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  DT2STR, INIT3
    !
    !  REVISION  HISTORY:
    !	adapted   9/1995 by CJC from M3ERR()
    !       modified  1/1997 by CJC to trim trailing blanks from MSGTXT
    !       modified 10/1998 by CJC:  Factor all output through m3msg2(), to
    !                      get around SGI OpenMP bug.
    !                      Caution:  messages may not be sequenced correctly
    !                      when called by multiple threads simultaneously
    !       Modified  5/1998 by CJC for OpenMP thread-safety:
    !                       factors through M3MSG2
    !       Modified  5/2003 by CJC:  factor all messages through M3MSG2()
    !       Modified  4/2004 by CJC:  factor through M3PARAG() -- fixes
    !       multi-thread sequencing problem.
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified  8/2012 by CJC: enhanced date&time message
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'IODECL3.EXT'


    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: CALLER          !  name of the caller
    INTEGER      , INTENT(IN   ) :: JDATE, JTIME    !  model date&time for the error
    CHARACTER*(*), INTENT(IN   ) :: MSGTXT          !  error message


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    CHARACTER*24, EXTERNAL :: DT2STR


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    CHARACTER*256   MESG( 5 )


    !***********************************************************************
    !   begin body of subroutine  M3WARN

    MESG( 1 ) = ' '
    MESG( 2 ) = '>>--->> WARNING in subroutine ' // CALLER
    MESG( 3 ) = MSGTXT

    IF ( JDATE .GT. 0  .OR.  JTIME .GT. 0 ) THEN
        WRITE( MESG( 4 ), '(  3A, I7, A, I6.6, A )' )       &
            'M3WARN:  DTBUF ', DT2STR( JDATE, JTIME ), '(', JDATE, ':', JTIME, ')'
        MESG( 5 ) = ' '
        CALL M3PARAG( 4, MESG )
    ELSE
        MESG( 4 ) = ' '
        CALL M3PARAG( 4, MESG )
    END IF

    RETURN

END SUBROUTINE M3WARN


SUBROUTINE SKIPL( UNIT, NLINES )

    !***********************************************************************
    ! Version "$Id: skipl.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (c) 2004-2007 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  44
    !
    !  SUBROUTINE:  Skips NLINES number of lines in file UNIT
    !
    !  PRECONDITIONS REQUIRED:
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 3/97 by M Houyoux for SMOKE
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Modified 02/2014 by CJC: Fix MH violation of coding-standards:
    !       check status ISTAT from  READ()!!
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !****************************************************************************

    IMPLICIT NONE

    !.........  Subroutine arguments

    INTEGER, INTENT(IN   ) :: UNIT
    INTEGER, INTENT(IN   ) :: NLINES

    !.........  Local variables

    INTEGER    I, ISTAT
    CHARACTER*256   MESG

    !***********************************************************************
    !   begin body of subroutine SKIPL

    DO I = 1, NLINES
        READ( UNIT, *, IOSTAT=ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I4, 2X, A, I9 )' )       &
                  'Error reading unit', UNIT, 'IOSTAT=', ISTAT
            CALL M3EXIT( 'SKIPL',0,0, MESG, 2)
        END IF
    END DO

    RETURN

END SUBROUTINE SKIPL

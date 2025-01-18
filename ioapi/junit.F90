
INTEGER FUNCTION  JUNIT()

    !***********************************************************************
    ! Version "$Id: junit.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 65
    !
    !  FUNCTION:
    !
    !    Routine returns next available FORTRAN unit number
    !
    !  ARGUMENT LIST DESCRIPTION:  empty argument list
    !
    !  RETURN VALUE:  JUNIT        Unit number selected
    !
    !  LOCAL VARIABLE DESCRIPTION:
    !
    !    IUNIT    state variable:  counts through available units
    !    BOT      parameter:  first POSIX-approved unit number for FORTRAN I/O
    !    TOP      parameter:  last   ...
    !
    !  REVISION HISTORY:
    !
    !    3/88  Maximum number of I/O unit numbers was increased from 50
    !          to 75 due to increased file I/O requirements.
    !    5/88  Modified for ROMNET
    !    7/90  Modified for ROM 2.2 -- uses EXWST for error abort.
    !    8/90  Algorithm simplification:  replaced IF-GOTO loop by DO loop.
    !    8/90  Algorithm simplification:  counting algorithm instead of table
    !          of flags; uses POSIX standards-approved unit numbers 11-99
    !    3/92  Models-3 Prototype version (eliminate EXWST)
    !    8/96  Modified by CJC -- On counting-algorithm failure, performs
    !          INQUIREs to find available unit.
    !    2/97  conditional definition of EXIT under AIX
    !    9/99  by CJC:  modifications for portability
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

#ifdef   _AIX
#define  EXIT exit_
#endif

    IMPLICIT NONE

    !...........   PARAMETERS and their descriptions:

    INTEGER, PARAMETER :: BOT = 10          !  1 less than initial unit number
    INTEGER, PARAMETER :: TOP = 99          !  final unit number


    !...........   LOCAL VARIABLES and their descriptions:

    INTEGER      J
    LOGICAL      FLAG

    !............................................................................
    !.......   begin body of JUNIT:

    DO J = TOP, BOT, -1
        INQUIRE( UNIT=J, OPENED=FLAG )
        IF ( .NOT. FLAG ) THEN
            JUNIT = J
            RETURN
        END IF
    END DO

    !.........   If you get to here: failure

    WRITE (*,91001) BOT, TOP
    CALL EXIT( 2 )
    RETURN


    !*************************  FORMAT  STATEMENTS  **************************

    !  Error and warning message formats     91xxx


91001 FORMAT (///, 1X, '*** ERROR ABORT IN ROUTINE JUNIT ***',      &
              /, 5X, 'NO MORE UNIT NUMBERS AVAILABLE FOR I/O',      &
              /, 5X, 'First POSIX-approved unit:', I4 ,             &
              /, 5X, 'Last  POSIX-approved unit:', I4 ,             &
              //)

END FUNCTION  JUNIT

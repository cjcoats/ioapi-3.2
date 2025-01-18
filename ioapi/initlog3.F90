
INTEGER FUNCTION INITLOG3 ( CALLER )

    !***********************************************************************
    ! Version "$Id: initlog3.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  85
    !
    !  FUNCTION:
    !       I/O API INTERNALS ONLY
    !       Initialize state for Models-3 I/O.
    !       May be called multiple times whenever the caller wants the
    !       unit number for the I/O API system log file.
    !
    !  RETURN VALUE:
    !       unit number for the log file
    !
    !  PRECONDITIONS REQUIRED:
    !       Only call from within "!$OMP CRITICAL( S_LOGDEV )" blocks
    !
    !  REVISION  HISTORY:
    !       Adapted 12/2003 by CJC from section of INIT3().
    !       Hack "CALL INITBLK3" to deal with failure of SGI version 7.4
    !       compilers to follow industry standards.
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'
    INCLUDE 'STATE3.EXT'


    !...........   ARGUMENT and its description:

    CHARACTER*(*), INTENT(IN   ) :: CALLER


    !...........   EXTERNAL FUNCTIONS and their descriptions:

#ifdef   _AIX
#define  FLUSH flush_
#endif

    INTEGER, EXTERNAL :: JUNIT
    EXTERNAL          :: INITBLK3


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    CHARACTER*512   EQNAME
    CHARACTER*256   MESG, TEXT          !  message/warning buffers

    INTEGER         L, IOST

    !...........   SAVED LOCAL VARIABLES and their descriptions:
    !...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !...........   for variables which must retain their values from call to call.

    CHARACTER*16, PARAMETER :: LOGFILE = 'LOGFILE'


    !***********************************************************************
    !   begin body of subroutine  INITLOG

#if defined(__sgi)    || defined(__mips__)

    !!==========>  *HACK*  to deal with SGI v7.4 or later compilers,
    !!             that do not follow industry standards with respect
    !!             to how to make sure that BLOCK DATA modules are
    !!             properly linked into an executable:

    CALL INITBLK3

#endif

    IF ( LOGDEV .LT. 0 ) THEN

        CALL NAMEVAL( LOGFILE, EQNAME )

        IF ( LOGFILE .EQ. EQNAME(1:16 ) ) THEN

            STDOUT = .TRUE.
            LOGDEV = 6
            IOST   = 0

        ELSE IF ( EQNAME .EQ. '/dev/null' ) THEN

            STDOUT = .FALSE.
            LOGDEV = JUNIT()
            OPEN ( UNIT   =  LOGDEV,        &
                   IOMSG  = TEXT,           &
                   IOSTAT =  IOST,          &
                   FILE   =  EQNAME,        &
                   ACCESS =  'SEQUENTIAL')

        ELSE

            LOGDEV = JUNIT()
            STDOUT = .FALSE.
            OPEN ( UNIT   =  LOGDEV,        &
                   IOMSG  = TEXT,           &
                   IOSTAT =  IOST,          &
                   FILE   =  EQNAME,        &
                   STATUS =  'NEW',         &
                   ACCESS =  'SEQUENTIAL')

        END IF

        IF ( IOST .NE. 0 ) THEN
            MESG =TRIM( CALLER ) // '/INITLOG3'
            WRITE( *, '( /, 5X, 3 A )' ) '***  ERROR in ', TRIM( MESG ), '  ***'
            WRITE( *, '( 5X, A, I10 )' ) 'Error opening log file on unit', LOGDEV, 'I/O STATUS =', IOST
            WRITE( *, '( 5X, 2 A, / )' ) 'DESCRIPTION: ', TRIM( TEXT )
            WRITE( *, '( 5X, 2 A, / )' ) 'File: ', TRIM( EQNAME )
            CALL EXIT( 2 )
        END IF

    END IF          !  if logdev < 0

    INITLOG3 = LOGDEV

    RETURN

END FUNCTION INITLOG3

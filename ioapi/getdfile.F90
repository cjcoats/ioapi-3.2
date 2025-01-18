
INTEGER FUNCTION GETDFILE( LNAME, RDONLY, FMTFLAG, RECLEN, CALLER )

    !***********************************************************************
    ! Version "$Id: getdfile.F90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2014-2016 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 79
    !
    !  DESCRIPTION:
    !    Gets value of logical name LNAME from the environment, checks for
    !    existence of a file whose file name is that value, then opens the
    !    file as a direct access file on unit IUNIT according to the flags
    !    RDONLY (open for read-only iff TRUE, read/write if FALSE),
    !    FMTFLAG (formatted iff TRUE, else unformatted), and with the
    !    indicated record length RECLEN.
    !    Logs the file-opening, together with the CALLER version, and
    !    returns the unit number (or -1 for failure)
    !
    !    RETURNS:   unit number, (or -1 for failure)
    !
    !  PRECONDITIONS REQUIRED:
    !
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !
    !  REVISION  HISTORY:
    !       Prototype  2/95 by CJC.
    !
    !       Modified  9/99 by CJC:  portability/standardization changes
    !
    !       Modified 7/2003 by CJC:  OMP thread safety -- critical sections
    !       associated with INIT3()
    !
    !       Modified 03/2010, 09/2014 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 02/2016 by CJC: eliminate non-F90 cases.
    !
    !       Modified 12/2016 by CJC: Log IOMESG for errors
    !
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:


    CHARACTER*(*), INTENT(IN   ) :: LNAME          !  logical file name
    LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
    LOGICAL      , INTENT(IN   ) :: FMTFLAG        !  TRUE iff file should be formatted
    INTEGER      , INTENT(IN   ) :: RECLEN         !  record length for direct access
    CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: INIT3, JUNIT, GET_ENDIAN

    !...........   LOCAL VARIABLES and their descriptions:

    INTEGER     IUNIT
    INTEGER		ISTAT, JSTAT, ENDIAN
    LOGICAL     LCHECK

    CHARACTER*512   PNAME, MESG, TEXT
    CHARACTER*16    FMTSTRING

    INTEGER, SAVE :: LOGDEV = -1

    !***********************************************************************
    !   begin body of GETDFILE()

    !$OMP   CRITICAL( S_INIT )
    IF ( LOGDEV .LT. 0 ) THEN
        LOGDEV = INIT3()
    END IF
    !$OMP   END CRITICAL( S_INIT )

    !...........   Read filename from environment ---

    CALL NAMEVAL(  LNAME, PNAME )

    IF ( FMTFLAG ) THEN
        FMTSTRING = 'FORMATTED'
    ELSE
        FMTSTRING = 'UNFORMATTED'
    END IF

    !.......   Check for existence of files ---

    INQUIRE( FILE=PNAME, EXIST=LCHECK , IOSTAT=JSTAT, IOMSG=TEXT )

    IF ( JSTAT .NE. 0 ) THEN
        CALL PERROR( TEXT )
        WRITE( MESG, '( A, I11, 2X, A, 1X, A)' ) 'GETDFILE:  Error', JSTAT, 'inquiring about', LNAME
        CALL M3MESG( MESG )
        MESG = 'Path-name: ' // PNAME
        CALL M3MESG( MESG )
        GETDFILE = -1
        RETURN
    END IF

    IF ( RDONLY ) THEN

        IF ( .NOT. LCHECK ) THEN
            WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )   &
                'GETDFILE ERROR: input file not found: ',   &
                TRIM( LNAME ), TRIM( PNAME )
            GETDFILE = -1
            RETURN
        END IF

        IUNIT = JUNIT()

        OPEN( UNIT   = IUNIT,       &
              FILE   = PNAME,       &
              FORM   = FMTSTRING,   &
              ACCESS = 'DIRECT',    &
              RECL   = RECLEN,      &
              STATUS = 'OLD',       &
              ACTION = 'READ',      &
              IOMSG  = TEXT,        &
              IOSTAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            CALL PERROR( 'GETDFILE ERROR: file-opening failure:')
            CALL PERROR( TEXT )
            WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )               &
                TRIM( LNAME ),  TRIM( PNAME ),  'I/O status:  ', ISTAT
            GETDFILE = -1
            RETURN
        END IF

        WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )                   &
            'File ', TRIM( LNAME ), ' opened for input on unit', IUNIT, &
            ' with record length', RECLEN, TRIM( PNAME )

    ELSE      !  not read-only:  open for write

        IF ( LCHECK ) THEN
            WRITE( LOGDEV, 9000, IOSTAT=JSTAT, ERR=9999 )   &
                'WARNING: output file already exists: ',    &
                TRIM( LNAME ), TRIM( PNAME )
        END IF

        IUNIT = JUNIT()

#ifdef __alpha
        OPEN( UNIT   = IUNIT,       &
              FILE   = PNAME,       &
              FORM   = FMTSTRING,   &
              ACCESS = 'DIRECT',    &
              RECL   = RECLEN,      &
              STATUS = 'UNKNOWN',   &
              IOMSG  = TEXT,        &
              IOSTAT = ISTAT )
#endif    /*  ifdef __alpha */
#ifdef _WIN32
        ENDIAN = GET_ENDIAN()
        IF ( ENDIAN .EQ. BIG_ENDIAN ) THEN
            OPEN( UNIT   = IUNIT,           &
                  FILE   = PNAME,           &
                  ACCESS = 'DIRECT',        &
                  RECL   = RECLEN,          &
                  FORM   = FMTSTRING,       &
                  CONVERT= 'BIG_ENDIAN',    &
                  IOMSG  = TEXT,            &
                  IOSTAT = ISTAT )
        ELSE
            OPEN( UNIT   = IUNIT,           &
                  FILE   = PNAME,           &
                  FORM   = FMTSTRING,       &
                  ACCESS = 'DIRECT',        &
                  RECL   = RECLEN,          &
                  IOMSG  = TEXT,            &
                  IOSTAT = ISTAT )
        END IF
#endif    /*  ifdef _WIN32 */
#ifndef __alpha
#ifndef _WIN32
        OPEN( UNIT   = IUNIT,       &
              FILE   = PNAME,       &
              FORM   = FMTSTRING,   &
              ACCESS = 'DIRECT',    &
              RECL   = RECLEN,      &
              IOMSG  = TEXT,        &
              IOSTAT = ISTAT )
#endif    /*  ifndef _WIN32  */
#endif    /*  ifndef __alpha */

        IF ( ISTAT .NE. 0 ) THEN
            CALL PERROR( 'GETDFILE ERROR: file-open failure:')
            CALL PERROR( TEXT )
            WRITE( LOGDEV, 9100, IOSTAT=JSTAT, ERR=9999 )   &
                TRIM( LNAME ),                              &
                TRIM( PNAME ),                              &
                'I/O status:  ', ISTAT
            GETDFILE = -1
            RETURN
        END IF

        WRITE( LOGDEV, 9001, IOSTAT=JSTAT, ERR=9999 )       &
            'File "', TRIM( LNAME ),                        &
            '" opened for output on unit:', IUNIT,          &
            ' with record length', RECLEN, TRIM( PNAME )

    END IF

    GETDFILE = IUNIT
    RETURN

    !----------------------------------------------------------------------
    !     handle messaging errors:

9999 CONTINUE

    IF ( JSTAT .NE. 0 ) THEN
        CALL PERROR( 'GETDFILE ERROR writing log message.')
        WRITE( LOGDEV, 9200, IOSTAT=ISTAT ) 'I/O status:', JSTAT
        GETDFILE = -1
        RETURN
    END IF

    !-----------------------------------------------------------------------
    !    Format statements:

8000 FORMAT( A )
9000 FORMAT( /, 1X, 2A, /, :, 1X, A, / )
9001 FORMAT( /, 5X, 3A, I4, A, I8,          &
             /, 5X,  A, /)
9100 FORMAT( /, 1X,  A, ':', A,             &
             /, 1X,  A, I7 , / )
9200 FORMAT( /, 1X,  A, I7 , / )

END FUNCTION GETDFILE

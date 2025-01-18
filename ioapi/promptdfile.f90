
INTEGER FUNCTION PROMPTDFILE( PROMPT, RDONLY, FMTTED, RECLEN, DEFAULT, CALLER )

    !***********************************************************************
    ! Version "$Id: promptdfile.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2013 Baron Advanced Meteorological Systems,
    ! (C) 2007-2013,2021 Carlie J. Coats, Jr., and
    ! (C) 2015 UNC Institute for the Environment.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line 88
    !
    !       Prompts user for logical file name, then opens the direct
    !	access Fortran file associated with it, for read-only or not,
    !	formatted or not, with the indicated record length, as
    !       indicated by RDONLY and FMTTED.
    !    !! WARNING !!  interpretation of RECLEN is MACHINE-DEPENDENT
    !
    !  RETURNS:
    !       unit number for the file opened, or
    !       -1 for failure,
    !       -2 for 'NONE', provided that '"NONE"' occurs within the prompt; or
    !       -3 for 'ALL',  provided that '"ALL"'  occurs within the prompt
    !
    !  PRECONDITIONS REQUIRED:
    !       "setenv <lname> <pathname>" for the file before program launch
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       GETDFILE
    !
    !  REVISION  HISTORY:
    !       prototype 11/95 by CJC
    !       Modified   8/96 by CJC:  ! is a comment-designator for input
    !       Modified   4/99 by CJC:  turn on/off prompting with environment
    !       variable "PROMPTFLAG"
    !       Revised 6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
    !       M3FLUSH to ensure flush() of PROMPT and of log-messages for
    !       IRIX F90v7.4
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !       Modified 02/2015 by CJC for I/O API 3.2:  Fix MH violation
    !       of coding-standards:  check status IOS from  ENVYN!!
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: PROMPT         !  prompt for user
    LOGICAL      , INTENT(IN   ) :: RDONLY         !  TRUE iff file is input-only
    LOGICAL      , INTENT(IN   ) :: FMTTED         !  TRUE iff file should be formatted
    INTEGER	     , INTENT(IN   ) :: RECLEN         !  record length
    CHARACTER*(*), INTENT(IN   ) :: DEFAULT        !  default logical file name
    CHARACTER*(*), INTENT(IN   ) :: CALLER         !  caller-name for logging messages

    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: GETDFILE
    LOGICAL, EXTERNAL :: ENVYN, GETYN

    !...........   PARAMETER

    CHARACTER*16, PARAMETER :: PNAME   = 'PROMPTDFILE'
    CHARACTER*16, PARAMETER :: BLANK16 = ' '
    CHARACTER*16, PARAMETER :: NONE16  = 'NONE'
    CHARACTER*16, PARAMETER :: ALL16   = 'ALL'


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    CHARACTER*16    LNAME        !  logical file name
    INTEGER         IOS          !  I/O error status
    INTEGER         IDEV         !  unit number
    INTEGER         PLEN, DLEN   !  trimlen( prompt | default )
    INTEGER         I            !  position at which "!" found
    CHARACTER*512   BUF          !  prompt/environment buffer
    CHARACTER*256   MESG         !  messages
    LOGICAL         AFLAG        !  "ALL"  is in the prompt
    LOGICAL         NFLAG        !  "NONE" is in the prompt

    LOGICAL, SAVE :: PROMPTON     !  Actually prompt or open default
    LOGICAL, SAVE :: FIRSTIME = .TRUE.


    !***********************************************************************
    !   begin body of function  PROMPTDFILE

    IF( FIRSTIME ) THEN

        PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',&
        &                  .TRUE., IOS )
        IF ( IOS .GT. 0 ) THEN
            CALL M3EXIT( PNAME,0,0,'Bad env vble "PROMPTFLAG"', 2 )
        END IF
        FIRSTIME = .FALSE.

    ENDIF

    !.......   Get file name; open input control definition file

    AFLAG = ( INDEX( PROMPT, '"ALL"'  ) .GT. 0 )
    NFLAG = ( INDEX( PROMPT, '"NONE"' ) .GT. 0 )

    PLEN  = LEN_TRIM( PROMPT  )
    DLEN  = LEN_TRIM( DEFAULT )

    IF ( DLEN .GT. 16 ) THEN
        WRITE( MESG, '( A, A, A, I6, 2X, A )' )     &
            'Length of DEFAULT "',  DEFAULT( 1:DLEN ) , '" exceeds 16; truncating'
        BUF = TRIM( CALLER ) // '/PROMPTDFILE'
        CALL M3WARN( BUF, 0, 0, MESG )
        DLEN = 16
    END IF

    IF( PROMPTON ) THEN

        IF ( DLEN + PLEN .GT. 250 ) THEN
            WRITE( MESG, '( A, A, A, I6, 2X, A )' ) 'Prompt too long; truncating'
            BUF = TRIM( CALLER ) // '/PROMPTDFILE'
            CALL M3WARN( BUF, 0, 0, MESG )
            PLEN = 250 - DLEN
        END IF

        BUF = TRIM( PROMPT  ) // ' [' // TRIM( DEFAULT ) // '] >> '

11      CONTINUE

        CALL M3PROMPT( BUF, LNAME, IOS )

        IF ( IOS .NE. 0 ) THEN

            MESG = 'Could not read your response'
            WRITE( 6,'( 5X, A )' ) MESG
            IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                GO TO  11
            ELSE
                MESG = 'Could not read logical name for file'
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
            END IF

        END IF      !  if could not read response

        I = INDEX( LNAME, '!' )
        IF ( I .GT. 0 ) LNAME( I : LEN( LNAME ) ) = ' '

        IF ( LNAME .EQ. BLANK16 )  THEN
            LNAME = DEFAULT
        END IF

        IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
            PROMPTDFILE = -3
            RETURN
        ELSE IF ( NFLAG .AND. LNAME .EQ. NONE16 )  THEN
            PROMPTDFILE = -2
            RETURN
        END IF

        IDEV = GETDFILE( LNAME, RDONLY, FMTTED, RECLEN, CALLER )
        IF ( IDEV .LT. 0 ) THEN     !  failure to open

            MESG = 'Could not open input file "' // TRIM( LNAME ) // '".'
            CALL M3MSG2( MESG )
            IF ( GETYN( 'Try again?', .TRUE. ) ) THEN
                GO TO  11
            ELSE
                MESG = 'Ending program "' // TRIM( CALLER ) // '".'
                CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
            END IF

        END IF      !  if getefile() failed

    ELSE   ! Do not prompt for output

        LNAME = DEFAULT

        IF ( AFLAG .AND. ( LNAME .EQ. ALL16 ) )  THEN
            PROMPTDFILE = -3
            RETURN

        ELSE IF ( NFLAG )  THEN

            IF ( LNAME .EQ. NONE16 )  THEN
                PROMPTDFILE = -2
                RETURN
            END IF

            !           ..  Check if logical name is set in order to permit
            !           ..  Study Planner to skip file without having to input "NONE"

            CALL ENVSTR( LNAME, 'Input file name', BLANK16, BUF, IOS )

            IF( IOS .LT. 0 ) THEN
                PROMPTDFILE = -2
                RETURN
            END IF

        END IF

        IDEV = GETDFILE( LNAME, RDONLY, FMTTED, RECLEN, CALLER )
        IF ( IDEV .LT. 0 ) THEN     !  failure to open
            MESG = 'Could not open input file "' // TRIM( LNAME ) // '".'
            CALL M3MSG2( MESG )
            MESG = 'Ending program "' // TRIM( CALLER ) // '".'
            CALL M3EXIT( CALLER, 0, 0, MESG, 2 )
        END IF

    END IF

    PROMPTDFILE = IDEV
    RETURN

END FUNCTION PROMPTDFILE



        INTEGER FUNCTION GETNUM ( LO , HI , DEFAULT , PROMPT )

C********************************************************************
C Version "$Id: getnum.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function body starts at line  83
C
C       CALLS:      TRIMLEN, ENVYN, M3EXIT
C
C  FUNCTION:
C
C       Display the  PROMPT  for an integer between  LO  and  HI,
C       get the user's response and check that it is within range.
C       Return DEFAULT if the user hits <RET>.  Re-prompts on error
C       for up to 5 attempts.
C
C  REVISION HISTORY:
C
C       Created 3/1989 by CJC.
C       Revised 5/1990 by CJC:  error abort is via EXWST.
C       Revised 8/1990 by CJC:  TEMP.LET; treat CONTROL-Z as exit signal.
C       Revised 2/1993 by CJC:  CRAY version using DECODE.
C       Revised 8/1996 by CJC:  treats "!" as a delimiter
C       Revised 1/1997 by CJC:  logs result
C       Revised 5/2003 by CJC:  factor through M3MSG2 to ensure flush()
C       of log-messages
C       Revised 6/2003 by CJC:  factor through M3PROMPT to ensure flush()
C       of PROMPT for IRIX F90v7.4  
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C        LO       Minimum allowed return value
C        HI       Maximum allowed return value
C        DEFAULT  Default return value
C        PROMPT   Prompt for user
C
C    Output arguments:  none
C
C  RETURNS   user response after checking its range; or default.
C
C********************************************************************

        IMPLICIT NONE

C.......   ARGUMENTS:

        INTEGER      , INTENT(IN   ) :: LO , HI , DEFAULT
        CHARACTER*(*), INTENT(IN   ) :: PROMPT


C.......   EXTERNAL FUNCTIONS:

        LOGICAL, EXTERNAL :: ENVYN
        INTEGER, EXTERNAL :: LBLANK          ! # of leading blanks


C.......   LOCAL VARIABLES:

        INTEGER         J, M
        INTEGER         LLO , LHI , LDF
        INTEGER         ANSWER
        INTEGER         ERRCNT
        INTEGER         IOS
        CHARACTER*16    BUFFER , DEFSTR
        CHARACTER*8     FMTSTR
        CHARACTER*256   MESG

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C......................................................................
C       begin GETNUM

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            FIRSTIME = .FALSE.
 
        END IF

        IF( .NOT. PROMPTON ) THEN
            GETNUM = DEFAULT
            WRITE( MESG,'( A, I10, 2X, A )' ) 
     &          'Using default value', DEFAULT, 'for query:'
            CALL M3MSG2( MESG )
            MESG = '"' // TRIM( PROMPT ) // '"'
            CALL M3MSG2( MESG )
            RETURN
        END IF

        ERRCNT =  0

        LLO  =  MIN ( LO , HI )
        LHI  =  MAX ( LO , HI )
        LDF  =  MIN ( LHI , MAX ( LLO , DEFAULT ) )

        WRITE ( DEFSTR , '( I15 )' ) LDF
        J  =  1 + LBLANK( DEFSTR )


100     CONTINUE
        MESG = TRIM( PROMPT ) // ' [' // DEFSTR ( J:15 ) // '] >> '
        CALL M3PROMPT( MESG, BUFFER, IOS )

        IF ( IOS .NE. 0 ) THEN
            GO TO  900
        ELSE IF ( BUFFER ( 1:1 )  .EQ. ' ' )  THEN
            GETNUM  =  LDF
            WRITE( MESG, '( A, I10 )' ) 'Using default', LDF
        ELSE

	        M = LEN_TRIM( BUFFER )
            WRITE( FMTSTR, 94010 ) M
            READ( BUFFER, FMTSTR, IOSTAT=IOS, ERR=400 )  ANSWER

            IF ( ANSWER .LT. LLO  .OR.  ANSWER .GT. LHI )  THEN
                ERRCNT  =  ERRCNT + 1
                WRITE ( 6,92100 ) ANSWER , LLO , LHI , ERRCNT
                IF ( ERRCNT .LT. 5 )  GO TO  100

                GO TO  900      !  max error count exceeded

            END IF

            GETNUM  =  ANSWER
            WRITE( MESG, '( A, I10 )'  ) 'Using response', ANSWER
        END IF

        CALL M3MSG2( MESG )
        RETURN


400     CONTINUE        !  error in read from BUFFER

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,92200 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100

900     CONTINUE        !  error in read from terminal, or more than 5 errors

        ERRCNT  =  ERRCNT + 1
        IF ( ERRCNT .LT. 5 ) THEN
            WRITE ( 6,92000 ) IOS , ERRCNT
            CALL M3FLUSH( 6 )
            GO TO  100
        ELSE
            CALL M3EXIT( 'GETNUM', 0, 0, 
     &                   'Maximum error count exceeded', 1 )
        END IF

C................   end body of GETNUM  .......................................


92000   FORMAT ( /5X , '>>> ERROR IN ROUTINE GETNUM <<< ' ,
     &         //10X , 'Error reading response'               ,
     &         /10X  , 'I/O error status number = ' , I3                 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'
     &         )

92100   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'      ,
     &         //10X , 'Your response '           , I7 ,
     &                 ' not in the range '       , I7 ,
     &                 ' ... ' , I7 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'            , /
     &         )

92200   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'         ,
     &         //10X , 'Did not understand your response' ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'                , /
     &         )

94010   FORMAT ( '(I', I3, ')' )

        END FUNCTION GETNUM

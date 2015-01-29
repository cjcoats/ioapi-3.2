
        REAL   FUNCTION GETREAL( LO , HI , DEFAULT , PROMPT )

C********************************************************************
C Version "$Id: getreal.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C       function body starts at line  83
C
C   CALLS:  TRIMLEN  
C
C   FUNCTION:
C
C       Display the  PROMPT  for an integer between  LO  and  HI,
C       get the user's response and check that it is within range.
C       Return DEFAULT if the user hits <RET>.  Reprompts on error
C       for up to 5 attempts.
C
C  REVISION HISTORY:
C
C       Adapted  9/1990 by CJC from GETNUM
C       Version  2/1993 by CJC for CRAY
C       Version  8/1996 by CJC treats ! as a terminator
C       Modified 1/1997 by CJC:  logs result
C       Modified 8/1998 by CJC:  PROMPT2 and strict f77 string-handling
C       Revised  5/2003 by CJC:  log result, factored through M3MSG2
C                to ensure flush() of log messages
C       Revised   6/2003 by CJC:  factor through M3MSG2, M3PROMPT, and
C                 M3FLUSH to ensure flush() of PROMPT and of log-messages
C                 for IRIX F90v7.4  
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

        REAL         , INTENT(IN   ) :: LO, HI, DEFAULT
        CHARACTER*(*), INTENT(IN   ) :: PROMPT


C.......   EXTERNAL FUNCTION:  interpret I/O errors:

        LOGICAL, EXTERNAL :: ENVYN


C.......   LOCAL VARIABLES:

        INTEGER         K , L , M , N
        REAL            LLO , LHI , LDF
        REAL            ANSWER
        INTEGER         ERRCNT
        INTEGER         IOS
        CHARACTER*64    BUFFER
        CHARACTER*256   MESG
        CHARACTER*16	FMTSTR
        CHARACTER*1	CH

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.

C*********************************************************************
C       begin GETREAL

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                        .TRUE., IOS )
            FIRSTIME = .FALSE.
 
        END IF

        IF( .NOT. PROMPTON ) THEN
            GETREAL = DEFAULT
            WRITE( MESG,94030 ) DEFAULT, TRIM( PROMPT )
            CALL M3MSG2( MESG )
            RETURN
        END IF

        ERRCNT =  0

        LLO  =  MIN ( LO , HI )
        LHI  =  MAX ( LO , HI )
        LDF  =  MIN ( LHI , MAX ( LLO , DEFAULT ) )


100     CONTINUE

        WRITE( MESG, '( 2A , 1PE12.5, A )' )
     &        TRIM( PROMPT ), ' [', LDF, '] >> '
        CALL M3PROMPT( MESG, BUFFER, IOS )

        IF ( IOS .NE. 0 ) GO TO 900

        N = INDEX( BUFFER, '!' )
        IF ( N .GT. 0 ) BUFFER( N : ) = ' '

        IF ( BUFFER ( 1:1 )  .EQ. ' ' )  THEN
            GETREAL =  LDF
            WRITE( MESG,94020 ) TRIM( PROMPT ), LDF
            CALL M3MSG2( MESG )
        ELSE

C...........   upcase and remove excess white space

            L = 0
            M = 0
            N = 0

            DO  111  K = 1, LEN( BUFFER )

                CH = BUFFER( K:K )
                IF ( CH .EQ. ' '       ) GO TO 111

                L = L + 1
                IF ( CH .EQ. 'e' ) CH = 'E'
                IF ( CH .EQ. 'E' )  M = L
                IF ( CH .EQ. '.' )  N = L
                BUFFER( L:L ) = CH

111         CONTINUE

            DO  122  K = L+1, LEN( BUFFER )
                BUFFER( K:K ) = ' '
122         CONTINUE

            IF ( N .EQ. 0 ) THEN		!  no decimal:  integer response
                IF ( M .EQ. 0 ) THEN		!  no exponent
                    WRITE( FMTSTR, 94010 ) L
                    READ ( BUFFER, FMTSTR, ERR=400 )  N
                    ANSWER = FLOAT( N )
                ELSE
                    WRITE( FMTSTR, 94010 ) M - 1
                    READ ( BUFFER( 1:M-1 ), FMTSTR, ERR=400 )  N
                    WRITE( FMTSTR, 94010 ) L - M
                    READ ( BUFFER( M+1:L ), FMTSTR, ERR=400 )  M
                    ANSWER = FLOAT( N ) * 10.0 ** M
                END IF
            ELSE
                IF ( M .EQ. 0 ) THEN		!  F format response
                    WRITE( FMTSTR, 94011 ) 'F', L, L - N
                ELSE IF ( M .GT. N ) THEN	!  E format response
                    WRITE( FMTSTR, 94011 ) 'E',  L, M - N
                ELSE				!  bad response
                    GO TO  900
                END IF
                READ ( BUFFER, FMTSTR, ERR=400 )  ANSWER
            END IF


            IF ( ANSWER .LT. LLO  .OR.  ANSWER .GT. LHI )  THEN
                ERRCNT  =  ERRCNT + 1
                WRITE ( 6,92100 ) ANSWER , LLO , LHI , ERRCNT
                IF ( ERRCNT .LT. 5 )  GO TO  100

                GO TO  500      !  max error count exceeded

            END IF

            GETREAL =  ANSWER
            WRITE( MESG,94020 ) TRIM( PROMPT ), ANSWER
            CALL M3MSG2( MESG )

        END IF

        RETURN


400     CONTINUE        !  error in read from BUFFER

        ERRCNT  =  ERRCNT + 1
        WRITE ( 6,92200 )  ERRCNT
        CALL M3FLUSH( 6 )
        IF ( ERRCNT .LT. 5 )  GO TO  100


500     CONTINUE        !  more than 5 entry errors

        CALL M3EXIT( 'GETREAL',0,0, 
     &               'Maximum entry-error count exceeded', 2 )


900     CONTINUE        !  error in read from terminal

        ERRCNT  =  ERRCNT + 1
        IF ( ERRCNT .LT. 5 ) THEN
            WRITE ( 6,92000 ) IOS , ERRCNT
            CALL M3FLUSH( 6 )
            GO TO  100
        ELSE
            CALL M3EXIT( 'GETREAL',0,0, 
     &                   'Maximum error count exceeded', 1 )
        END IF

C................   end body of GETREAL  .......................................


92000   FORMAT ( /5X , '>>> ERROR IN ROUTINE GETREAL <<< ' ,
     &         //10X , 'Error reading response'               ,
     &         /10X  , 'I/O error status number = ' , I3                 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'
     &         )

92100   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'      ,
     &         //10X , 'Your response '           , 1PE12.5 ,
     &                 ' not in the range '       , 1PE12.5 ,
     &                 ' ... ' , 1PE12.5 ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'            , /
     &         )

92200   FORMAT ( /5X , '>>>  RESPONSE ERROR  <<<'         ,
     &         //10X , 'Did not understand your response' ,
     &         /10X  , 'This is error ' , I1 , ' of 5 errors allowed ' ,
     &         /10X  , 'Please try again.'                , /
     &         )

94010	FORMAT ( '(I', I3, ')' )

94011	FORMAT ( '(', A1, I3, '.', I3 , ')' )

94020	FORMAT ( 10 ( A, :, 1PE12.5 , :, 2X ) )

94030	FORMAT ( 'Using default response', 1PE12.5 , 2X, 
     &           'for query "', A, '"' )

        END FUNCTION GETREAL


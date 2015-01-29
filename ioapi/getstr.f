
        SUBROUTINE GETSTR ( PROMPT, DEFAULT, RESPONSE )

            CHARACTER*(*), INTENT(IN   ) :: PROMPT, DEFAULT
            CHARACTER*(*), INTENT(  OUT) :: RESPONSE

C******************************************************************
C Version "$Id: getstr.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body begins at line 73
C
C  FUNCTION:
C
C       Safely get a character-string response from the keyboard (with
C       default value (<RETURN> response) supplied from caller.
C       Checks for validity of the response, and allows  MAX = 5 
C       unsuccessful attempts, then exits.  
C       Treats CONTROL-D (end-of-file) as exit signal.
C       Treats ! as initiation of a comment
C
C  REVISION HISTORY:
C
C       Prototype 9/1994 by CJC
C       Modified  1/1997 by CJC:  logs result
C       Revised   5/2003 by CJC:  factor through M3MSG2 to ensure flush()
C       of log-messages
C       Revised 6/2003 by CJC:  factor through M3PROMPT to ensure flush()
C       of PROMPT for IRIX F90v7.4  
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C
C  ARGUMENT LIST DESCRIPTION:
C
C    Input arguments:
C
C         PROMPT    prompt for user
C         DEFAULT   default value
C
C    Output arguments: none
C
C         RESPONSE  return value
C
C**********************************************************************

C.......   External functions:

        LOGICAL, EXTERNAL :: ENVYN
        INTEGER, EXTERNAL :: LBLANK


C.......   Parameter:  maximum number of attempts allowed to the user

        INTEGER, PARAMETER ::MAX = 5


C.......   Local Variables:

        INTEGER         A1, P1, D1, R1, EE
        INTEGER         A0, P0, D0
        INTEGER         COUNT , IOS
        CHARACTER*256   ANSWER, MESG

        LOGICAL, SAVE :: PROMPTON
        LOGICAL, SAVE :: FIRSTIME = .TRUE.


C*********************   begin  GETSTR   *******************************

        IF( FIRSTIME ) THEN

            PROMPTON = ENVYN( 'PROMPTFLAG', 'Prompt for input flag',
     &                      .TRUE., IOS )
            FIRSTIME = .FALSE.
 
        END IF

        P0    = LBLANK ( PROMPT  ) + 1	!  initial position (first nonblank)
        D0    = LBLANK ( DEFAULT ) + 1	!  initial position (first nonblank)
        P1    = LEN_TRIM( PROMPT  )	!  last nonblank
        D1    = LEN_TRIM( DEFAULT )	!  last nonblank
        R1    = LEN( RESPONSE )

        IF( .NOT. PROMPTON ) THEN
            IF ( D1 - D0 + 1 .GT. R1 ) THEN
                RESPONSE = DEFAULT( D0:R1 + D0 - 1 )
                MESG = 'Default "' // DEFAULT( D0:D1 )  //
     &                 ' truncated to "' // 
     &                  DEFAULT( D0:R1 + D0 - 1 ) // '" for query'
                CALL M3WARN( 'GETSTR', 0, 0, MESG )
                MESG = '"' // PROMPT( P0:P1 ) // '"'
                CALL M3MESG( MESG )
            ELSE
                RESPONSE = DEFAULT( D0:D1 )
                MESG = 'Using default "' // DEFAULT( D0:D1 ) // 
     &                 '" for query:'
                CALL M3MSG2( MESG )
                MESG = '"' // PROMPT( P0:P1 ) // '"'
                CALL M3MSG2( MESG )
            END IF
            RETURN
        END IF

        COUNT = 0

11      CONTINUE
            ANSWER  =  '    '
            MESG = PROMPT( P0:P1 )//' ["' //DEFAULT( D0:D1 )// '"] >> '
            CALL M3PROMPT( MESG, ANSWER, IOS )
            IF ( IOS .NE. 0 ) GO TO  900

            EE = INDEX( ANSWER, '!' )
            IF ( EE .NE. 0 ) ANSWER( EE : LEN( ANSWER ) ) = ' '

            A0 = LBLANK( ANSWER ) + 1		!  first nonblank
            IF ( A0 .GT. LEN( ANSWER ) ) THEN	!  all blanks
                IF ( D1 - D0 + 1 .GT. R1 ) THEN
                    RESPONSE = DEFAULT( D0:R1 + D0 - 1 )
                    MESG = 'Default "' // DEFAULT( D0:D1 )  //
     &                       ' truncated to "' //
     &                       DEFAULT( D0:R1 + D0 - 1 ) // '"'
                    CALL M3WARN( 'GETSTR', 0, 0, MESG )
                ELSE
                    RESPONSE = DEFAULT( D0: D1 )
                    MESG = 'Using default "'// DEFAULT( D0:D1 ) // '"'
                END IF
            ELSE
                A1 = LEN_TRIM( ANSWER )
                IF ( A1 - A0 + 1 .GT. R1 ) THEN
                    RESPONSE = ANSWER( A0:R1 + A0 - 1 )
                    MESG = 'Response "' // ANSWER( A0:A1 ) //
     &                     ' truncated to "' //
     &                     ANSWER( A0:R1 + A0 - 1 ) // '"'
                    CALL M3WARN( 'GETSTR', 0, 0, MESG )
                ELSE
                    RESPONSE = ANSWER( A0:A1 )
                    MESG = 'Using response "' // ANSWER( A0:A1 ) // '"'
                END IF
            END IF
            CALL M3MSG2( MESG )
                   
            RETURN
             
900	CONTINUE	!  error reading response

            COUNT  =  COUNT + 1
            IF ( COUNT .GE. MAX )  THEN
                CALL  M3EXIT( 'GETSTR', 0, 0, 
     &                'Maximum number of attempts exceeded.',IOS )
            END IF
            WRITE ( 6,92020 ) 'Did not understand your response; ' ,
     &              'I/O ERROR:  I/O status = ' , IOS ,
     &              'Please try again (You are allowed ' ,
     &              MAX - COUNT , ' more attempts)'
            CALL M3FLUSH( 6 )
            GO TO  11


C******************  FORMAT  STATEMENTS   ******************************

C...........   Informational (LOG) message formats... 92xxx

92020   FORMAT ( /5X , A , /5X , A , I3 , /5X , A , I1 , A )

        END SUBROUTINE GETSTR


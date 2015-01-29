
        LOGICAL FUNCTION STRLIST( ENAME, EDESC, NMAX, NCNT, LIST )

C***********************************************************************
C Version "$Id: strlist.f 122 2015-01-20 22:25:11Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (c) 2004-2007 Baron Advanced Meteorological Systems,
C (c) 2007-2013 Carlie J. Coats, Jr., and (C) 2014 UNC Institute
C for the Environment.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  61
C
C  RETURNS:  TRUE for success, FALSE for failure
C            Success implies NCNT > 0 ("we actually found something")
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <quoted, comma-delimited list of integers>
C       string-length( <list> <= 511
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       ENVINT, M3EXIT, STR2INT
C
C  REVISION  HISTORY:
C       prototype 04/15/1998 by CJC
C       Revised   02/09/1999 by CJC:  NCNT <= 0:  failure
C       Revised   02/11/2002 by CJC:  Deal with values "LIST:<list>"
C       Modified  03/2010 by CJC: F9x changes for I/O API v3.1
C       Modified  03/2014 by CJC: buffer-size 65535 to match "envgets.c" change
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(IN   ) :: ENAME           ! environment variable for the list
        CHARACTER*(*), INTENT(IN   ) :: EDESC           ! environment variable description
        INTEGER      , INTENT(IN   ) :: NMAX            ! dimension for list
        INTEGER      , INTENT(  OUT) :: NCNT            ! actual number of entries in list
        CHARACTER*(*), INTENT(  OUT) :: LIST( NMAX )    ! array of values found    

C...........   EXTERNAL FUNCTIONS:

        INTEGER, EXTERNAL :: LBLANK

C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        CHARACTER*65535 BUF       !  buffer for environment-variable value
        CHARACTER*256   MESG      !  buffer for error messages
        CHARACTER*5     PREFIX    !  buffer for checking "LIST:"
        INTEGER         ISTAT     !  return status for ENVSTR
        INTEGER         K         !  subscript/loop counter
        INTEGER         L         !  substring length
        INTEGER         LO, HI    !  substring bounds
        INTEGER         LMAX      !  max substring length
        LOGICAL         EFLAG

C***********************************************************************
C   begin body of function  STRLIST

        CALL ENVSTR( ENAME, EDESC, ' ', BUF, ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Could not get environment variable "'// ENAME// '"'
            CALL M3MSG2( MESG )
            STRLIST = .FALSE.
            RETURN
        END IF
        
        LMAX = LEN( LIST( 1 ) )
        EFLAG = .FALSE.

        PREFIX = ADJUSTL( BUF )
        CALL UPCASE( PREFIX )
        IF ( PREFIX .EQ. 'LIST:' ) THEN
            HI = 5
            LO = 6
        ELSE
            HI = 0
            LO = 1
        END IF
        DO  K = 1, NMAX
            LO = LO + LBLANK( BUF( LO : 512 ) )
            IF ( LO .GE. 512 ) THEN
                NCNT = K - 1
                GO TO 99                !  list exhausted
            END IF
            HI = INDEX( BUF( LO : 512 ), ',' )
            IF ( HI .EQ. 0 ) THEN          !  no more commas
                L  = LEN_TRIM( BUF( LO : 512 ) )
                HI = 512
            ELSE        !  comma is BUF( LO+HI-1:LO+HI-1 )
                L = LEN_TRIM( BUF( LO : LO+HI-2 ) )
            END IF
            IF ( L .GT. 0  .AND. L .LE. LMAX ) THEN
                LIST( K ) = BUF( LO : LO+L-1 )
            ELSE
                EFLAG = .TRUE.
            END IF
            LO = LO + HI                !  1 past the comma
            IF ( LO .GE. 512 )  THEN
                NCNT = K
                GO TO 99                !  list exhausted
            END IF
        END DO

        IF ( BUF( HI+1 : 512 ) .NE. ' ' )  THEN   !  fall-through:  list done?
            STRLIST = .FALSE.
            RETURN
         END IF

99      CONTINUE        !  exit from loop
        STRLIST = ( .NOT. EFLAG ) .AND. ( NCNT .GT. 0 )
        RETURN
        END FUNCTION STRLIST

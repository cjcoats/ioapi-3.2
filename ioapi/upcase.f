
        SUBROUTINE  UPCASE ( BUFFER )

C***********************************************************************
C Version "$Id: upcase.f 219 2015-08-17 18:05:54Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
C (C) 2003-2010 by Baron Advanced Meteorological Systems.
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  subroutine body starts at line  49
C
C  FUNCTION:  upcase the text in BUFFER
C
C  PRECONDITIONS REQUIRED:  text is ASCII
C
C  SUBROUTINES AND FUNCTIONS CALLED:  none
C
C  REVISION  HISTORY:
C       prototype 1/91 by CJC
C
C       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
C***********************************************************************

      IMPLICIT NONE

C...........   ARGUMENTS and their descriptions:

        CHARACTER*(*), INTENT(INOUT) :: BUFFER


C...........   PARAMETER:  ASCII for 'a', 'z', 'A'

        INTEGER, PARAMETER :: IA    =  97
        INTEGER, PARAMETER :: IZ    = 122
        INTEGER, PARAMETER :: AADIF =  32


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER       I, L
        INTEGER       C


C***********************************************************************
C   begin body of subroutine  UPCASE

        L  =  LEN ( BUFFER )
        DO  111  I = 1 , L
            C = ICHAR ( BUFFER ( I:I ) )
            IF ( C .GE. IA  .AND.  C .LE. IZ ) THEN
                BUFFER ( I:I ) = CHAR ( C - AADIF )
            END IF
111     CONTINUE        !  end loop on I

        RETURN

        END SUBROUTINE  UPCASE


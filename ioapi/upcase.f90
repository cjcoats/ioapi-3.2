
SUBROUTINE  UPCASE ( BUFFER )

    !***********************************************************************
    ! Version "$Id: upcase.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems.
    ! (C) 2016 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  subroutine body starts at line  50
    !  ENTRY DOWNCASE  starts at line  64
    !
    !  FUNCTION:  upcase the text in BUFFER
    !
    !  PRECONDITIONS REQUIRED:  text is ASCII
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:  none
    !
    !  REVISION  HISTORY:
    !       prototype 1/91 by CJC
    !
    !       Modified 03/2010 by CJC: F90 changes for I/O API v3.1
    !
    !       Modified 03/2016 by CJC: ENTRY DOWNCASE for I/O API v3.2
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !!............   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(INOUT) :: BUFFER

    !!............   PARAMETER:  ASCII for 'a', 'z', 'A'

    INTEGER, PARAMETER :: IA    =  97
    INTEGER, PARAMETER :: IZ    = 122
    INTEGER, PARAMETER :: AADIF =  32
    INTEGER, PARAMETER :: JA    =  IA - AADIF
    INTEGER, PARAMETER :: JZ    =  IZ - AADIF

    !!............   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER       I, L, C

    !!............   begin body of subroutine  UPCASE  ...........

    L  =  LEN ( BUFFER )
    DO  I = 1 , L
        C = ICHAR ( BUFFER ( I:I ) )
        IF ( C .GE. IA  .AND.  C .LE. IZ ) THEN
            BUFFER ( I:I ) = CHAR( C - AADIF )
        END IF
    END DO        !  end loop on I

    RETURN

    !!............   begin body of subroutine  DOWNCASE  ...........

  ENTRY   DOWNCASE( BUFFER )

    L  =  LEN ( BUFFER )
    DO  I = 1 , L
        C = ICHAR ( BUFFER ( I:I ) )
        IF ( C .GE. JA  .AND.  C .LE. JZ ) THEN
            BUFFER ( I:I ) = CHAR( C + AADIF )
        END IF
    END DO        !  end loop on I

    RETURN


END SUBROUTINE  UPCASE


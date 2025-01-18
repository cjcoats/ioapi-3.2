
SUBROUTINE IOPARMS3( MXDLEN, NAMLEN, MXFILE, MXVARS, MXDESC, MXLAYS, MXATTS )

    !***********************************************************************
    ! Version "$Id: ioparms3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  Subroutine body starts at line  51
    !
    !  FUNCTION:
    !       Return "compiled-into-the-library dimensioning PARAMETER values
    !       from PARMS3.EXT,so that user/model-level code can perform
    !       consistency checks of INCLUDEd values against "libioapi.a"
    !
    !  PRECONDITIONS REQUIRED:
    !       none
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       none
    !
    !  REVISION  HISTORY:
    !       Originated   4/2004 by Carlie J. Coats, Jr., BAMS
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE


    !...........   INCLUDES:

    INCLUDE 'PARMS3.EXT'

    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT( OUT ) :: MXDLEN    !  description line length
    INTEGER, INTENT( OUT ) :: NAMLEN    !  name length (logical names, etc.)
    INTEGER, INTENT( OUT ) :: MXFILE    !  max number of open files
    INTEGER, INTENT( OUT ) :: MXVARS    !  max number of variables per file
    INTEGER, INTENT( OUT ) :: MXDESC    !  max number of description lines
    INTEGER, INTENT( OUT ) :: MXLAYS    !  max # of layers per file
    INTEGER, INTENT( OUT ) :: MXATTS    !  max # ATDSC .EXT attributes per variable

    !***********************************************************************
    !   begin body of subroutine  M3MSG2

    MXDLEN = MXDLEN3
    NAMLEN = NAMLEN3
    MXFILE = MXFILE3
    MXVARS = MXVARS3
    MXDESC = MXDESC3
    MXLAYS = MXLAYS3
    MXATTS = MXATTS3

    RETURN

END SUBROUTINE IOPARMS3

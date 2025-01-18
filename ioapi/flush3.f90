
LOGICAL FUNCTION FLUSH3( FNAME )

    !***********************************************************************
    ! Version "$Id: flush3.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
    ! (C) 2003-2011 by Baron Advanced Meteorological Systems,
    ! (C) 2015 UNC Institute for the Environment
    ! (C) 2021 Carlie J. Coats, Jr..
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function  FLUSH3   starts at line   68
    !
    !  FUNCTION:
    !       Flushes I/O API file with logical name FNAME.
    !
    !  RETURN VALUE:
    !       TRUE iff it succeeds.
    !
    !  PRECONDITIONS REQUIRED:
    !       I/O API already initialized.
    !       File with logical name FNAME exists and has been opened
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       NAME2FID
    !       SYNCFID
    !
    !  REVISION  HISTORY:
    !       prototype 8/1995 by CJC
    !
    !       Modified  5/1998 by CJC for OpenMP thread-safety
    !
    !       Modified  5/1998 by CJC:  removed unused local variable "V"
    !
    !       Modified 10/2003 by CJC for I/O API version 3:
    !       Structure in terms of new LOGICAL SYNCFID, INTEGER NAME2FID;
    !       support for native-binary BINFILE3 and LISTFIL3 file types
    !
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !
    !       Modified 08/2015 by CJC: call SYNCFID()
    !
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS and their descriptions:

    CHARACTER*(*), INTENT(IN   ) :: FNAME   !  logical name of file to be closed


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    INTEGER, EXTERNAL :: NAME2FID        !  fname~~> fid lookup
    LOGICAL, EXTERNAL :: SYNCFID


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         FILE            !  file index
    CHARACTER*256   MESG


    !***********************************************************************
    !   begin body of function  FLUSH3

    !.......   Find STATE3 index for the file; then call SYNCFID:

    FILE = NAME2FID( FNAME )

    IF ( FILE .EQ. 0 ) THEN !  file not open.
        MESG = 'FLUSH3:  invalid file "' // FNAME // '"'
        CALL M3MSG2( MESG )
        FLUSH3 = .FALSE.
        RETURN
    END IF

    FLUSH3 = SYNCFID( FILE )
    RETURN

END FUNCTION FLUSH3


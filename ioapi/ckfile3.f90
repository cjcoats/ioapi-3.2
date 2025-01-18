
LOGICAL FUNCTION CKFILE3( FID )  RESULT( CKFLAG )

    !***********************************************************************
    ! Version "$Id: ckfile3.f 1 2017-06-10 18:05:20Z coats $"
    ! BAMS/MCNC/EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2011 Baron Advanced Meteorological Systems, and
    ! (C) 2015-2016 UNC Institute for the Environment
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  92
    !
    !  RETURNS:
    !       If environment variable IOAPI_CHECK_HEADERS begins with 'Y' or 'y',
    !       checks whether file attributes in FDESC3.EXT commons fit into
    !       standard valid ranges, and returns TRUE or FALSE accordingly.
    !       Returns TRUE otherwise.
    !
    !  PRECONDITIONS REQUIRED:
    !       FDESC3.EXT commons set by user
    !
    !  SUBROUTINES AND FUNCTIONS CALLED:
    !       m3err()
    !
    !  REVISION  HISTORY:
    !       Prototype 9/95 by CJC
    !       Modified  2/97 by CJC:  check for legality of variable-names
    !       Modified  3/04 by D.Yin: add check for POLGRD3
    !       Modified  6/06 by CJC: modification to support WRF vertical
    !       coordinates, from Tanya Otte
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Bug-fix  04/2011 in format 94030  from Matt Turner, UC Boulder.
    !       Modified 02/2015 by CJC for I/O API 3.2: Support for M3INT8.
    !       USE M3UTILIO
    !       Modified 07-08/2015 by CJC:  bug reported by Mogesh Naidoo:
    !       Add support for EQMGRD3, TRMGRD3, ALBGRD3, LEQGRD3, SINIGRD3.
    !       Eliminate unused NETCDF.EXT.  MPIGRD3 type for MPI/PnetCDF
    !       distributed I/O
    !       Modified 07-05/2016 by CJC:  bugs reported by Edward Anderson,
    !       Lockheed Martin,
    !       Version 10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !...........   INCLUDES:

    INCLUDE 'STATE3.EXT'


    !...........   ARGUMENTS and their descriptions:

    INTEGER, INTENT( IN ) :: FID     !  subscript for STATE3 arrays


    !...........   EXTERNAL FUNCTIONS and their descriptions:

    LOGICAL, EXTERNAL :: CKNAME     !  checks legality of variable-names


    !...........   PARAMETER

    CHARACTER*16, PARAMETER :: BLANK    = ' '
    CHARACTER*16, PARAMETER :: AIR_LAT  = 'AIR_LAT'
    CHARACTER*16, PARAMETER :: AIR_LON  = 'AIR_LON'
    CHARACTER*16, PARAMETER :: AIR_ELV  = 'AIR_ELV'
    CHARACTER*19, PARAMETER :: ENVCHK   = 'IOAPI_CHECK_HEADERS'


    !...........   SAVED LOCAL VARIABLES and their descriptions:
    !...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !...........   for variables which must retain their values from call to call.

    LOGICAL, SAVE :: CHKHDR
    LOGICAL, SAVE :: FIRSTIME =.TRUE.


    !...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         ENVSTAT         !  return value for ENVYN()
    INTEGER         L, U, V         !  loop counters
    LOGICAL         INCREASING
    CHARACTER*160   MESG
    INTEGER         VGTYP, IERR
    REAL            VGLVS( MXLAYS3 + 1 )  !  vertical coord values.


    !***********************************************************************
    !   begin body of function  CKFILE3

    IF ( FIRSTIME ) THEN

        FIRSTIME = .FALSE.

        CHKHDR   = ENVYN( ENVCHK, 'Perform file-header checks?', .FALSE., ENVSTAT )

        IF ( ENVSTAT .GT. 0 ) THEN
            MESG = 'Invalid value for environment vble "' // ENVCHK // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
        END IF

    END IF          !  if firstime


    !.......   If not chkhdr, just return TRUE:

    IF ( .NOT. CHKHDR ) THEN
        CKFLAG = .TRUE.
        RETURN
    END IF          !  if not chkhdr


    !.......   Else perform checks:
    !...........   First:  file type and type-specific dimension checks:

    IF ( FTYPE3( FID ) .EQ. DGRAPH3 ) THEN

        CKFLAG = .TRUE.
        RETURN

    ELSE IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

        CKFLAG = .TRUE.
        RETURN

    ELSE IF ( FTYPE3( FID ) .EQ. CUSTOM3 ) THEN

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad blob-size NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3  .OR.    &
              FTYPE3( FID ) .EQ. MPIGRD3  .OR.    &
              FTYPE3( FID ) .EQ. TSRIES3  .OR.    &            !  "exotic"
              FTYPE3( FID ) .EQ. PTRFLY3     ) THEN         !  grdded3's

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF


        IF ( FTYPE3( FID ) .EQ. PTRFLY3 ) THEN
            IF ( INDEX1( AIR_LAT, NVARS3( FID ),    &
                         VLIST3( 1,FID ) ) .LE. 0 ) THEN
                MESG = 'Variable AIR_LAT not found in PTRFLY3-type file "' // TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFLAG = .FALSE.
                RETURN
            END IF

            IF ( INDEX1( AIR_LON, NVARS3( FID ),    &
                         VLIST3( 1,FID ) ) .LE. 0 ) THEN
                MESG = 'Variable AIR_LON not found in PTRFLY3-type file "' // TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFLAG = .FALSE.
                RETURN
            END IF

            IF ( INDEX1( AIR_ELV, NVARS3( FID ),    &
                         VLIST3( 1,FID ) ) .LE. 0 ) THEN
                MESG = 'Variable AIR_ELV not found in PTRFLY3-type file "' // TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE', 0, 0, MESG )
                CKFLAG = .FALSE.
                RETURN
            END IF

        END IF              !  if ftype ptrfly3

    ELSE IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NCOLS', NCOLS3( FID ), 'for file "' //    &
               TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( ABS( NTHIK3( FID ) ) .GT. MIN( NCOLS3( FID ),    &
                                            NROWS3( FID ) )/2 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad boundary width NTHIK', NTHIK3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. IDDATA3 ) THEN

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max site count NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. PROFIL3 ) THEN

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max site count NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max level count NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. GRNEST3 ) THEN

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max cell-count NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max nest count NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. SMATRX3 ) THEN

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad max matrix coeff-count NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad matrix NROWS', NROWS3( FID ), 'for file "' // 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NTHIK3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad full-matrix col-count NTHIK', NTHIK3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

        IF ( NCOLS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NCOLS', NCOLS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NROWS3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NROWS', NROWS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( NTHIK3( FID ) .LE. 0 ) THEN
            WRITE( MESG, 94010 )    &
               'Bad NTHIK', NTHIK3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE

        WRITE( MESG, 94010 )    &
           'Illegal file type:', FTYPE3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )
        CKFLAG = .FALSE.
        RETURN

    END IF


    !...........   Next, checks on the variable-list

    IF ( NVARS3( FID ) .LT. 0 ) THEN

        WRITE( MESG, 94010 )    &
            'Illegal number of variables:', NVARS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )
        CKFLAG = .FALSE.
        RETURN

    ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN    !  _is_ legal, but unusual

        WRITE( MESG, 94010 )    &
            'WARNING:  number of variables:', NVARS3( FID ), 'for file "' // TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )

    END IF

    DO  22  U = 1, NVARS3( FID )

        IF ( .NOT. CKNAME( VLIST3( U,FID ) ) ) THEN
            WRITE( MESG,94000 )    &
                'Illegal variable name "' , VLIST3( U,FID ) , '" in file ' , FLIST3( FID )
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        V = INDEXINT1( VTYPE3( U,FID ), NM3TYPES, M3TYPES )
        IF ( V .LE. 0 ) THEN

            WRITE( MESG, 94010 )    &
                'Illegal data type ', VTYPE3( U,FID ), 'for variable "' // TRIM( VLIST3( U,FID ) ) //    &
                '" in file "' //  TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN

        END IF      !  end check on variable-type

        DO  11  V = 1, U-1          !  hunt for duplicate names

            IF( VLIST3( U,FID ) .EQ. VLIST3( V,FID ) ) THEN

                WRITE( MESG, 94030 )    &
                   'Variable name VLIST3D(', U, ') = "' // TRIM( VLIST3( U,FID ) ) //    &
                   '" duplicates VLIST3D(', V, ') = "'  // TRIM( VLIST3( V,FID ) ) //    &
                   '" in file "' // TRIM( FLIST3( FID ) ) // '"'

                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFLAG = .FALSE.
                RETURN

            END IF

11      CONTINUE    !  end check for duplicates of this name

22  CONTINUE        !  end loop on variables U


    !...........   Checks on the horizontal coordinate description:

    IF ( GDTYP3( FID ) .EQ. LATGRD3 ) THEN

        IF ( XORIG3( FID ) .LT. -180.0D0 .OR.    &
             XORIG3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad grid origin', XORIG3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YORIG3( FID ) .LT. -90.0D0 .OR.    &
             YORIG3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad grid origin', YORIG3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. LAMGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_BET3( FID ) .LT. P_ALP3( FID ) .OR.    &
             P_BET3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-BETA', P_BET3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. MERGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "'// TRIM( FLIST3( FID ) ) // '"'
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_BET3( FID ) .LT. -180.0D0 .OR.    &
             P_BET3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-BETA', P_BET3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID) .EQ. POLGRD3 ) THEN

        IF ( XCENT3( FID) .LT. -180.0D0 .OR.    &
             XCENT3( FID) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID) .LT. -90.0D0 .OR.    &
             YCENT3( FID) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID) .NE. -1.0D0 .AND.    &
             P_ALP3( FID) .NE.  1.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_BET3( FID) .LT. -90.0D0 .OR.    &
             P_BET3( FID) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-BETA', P_BET3( FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID) .LT. -180.0D0 .OR.    &
             P_GAM3( FID) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. STEGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_BET3( FID ) .LT. -180.0D0 .OR.    &
             P_BET3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-BETA', P_BET3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. UTMGRD3 ) THEN

        IF ( P_ALP3( FID ) .LT.  0.9D0 .OR.    &
             P_ALP3( FID ) .GT. 36.1D0 .OR.    &
             ABS( P_ALP3( FID ) -  DBLE( NINT( P_ALP3( FID ) ) ) ) .GT. 0.01D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. EQMGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' //    &
                TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. TRMGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. ALBGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_BET3( FID ) .LT. P_ALP3( FID ) .OR.    &
             P_BET3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-BETA', P_BET3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. LEQGRD3 ) THEN

        IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.    &
             P_ALP3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. SINUGRD3 ) THEN

        IF ( XCENT3( FID ) .LT. -180.0D0 .OR.    &
             XCENT3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', XCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( YCENT3( FID ) .LT. -90.0D0 .OR.    &
             YCENT3( FID ) .GT.  90.0D0 ) THEN
            WRITE( MESG, 94020 )    &
                'Bad X-Y origin', YCENT3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

        IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.    &
             P_GAM3( FID ) .GT.  180.0D0 ) THEN
            WRITE( MESG, 94020 ) 'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN
        END IF

    ELSE IF ( GDTYP3( FID ) .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

        WRITE( MESG, 94010 )    &
            'WARNING:  Horizontal grid/coordinate type:', GDTYP3( FID ),    &
            '"MISSING"  in file "' // TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )

    ELSE    !  illegal grid type

        WRITE( MESG, 94010 )    &
           'Illegal horizontal grid/coordinate type:', GDTYP3( FID ), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )
        CKFLAG = .FALSE.
        RETURN

    END IF  !  if  gdtyp3d = lamgrd3, etc.


    !...........   Checks on the vertical coordinate description:

    IF ( NLAYS3( FID ) .LT. 1 .AND.    &
         FTYPE3( FID ) .GE. CUSTOM3 ) THEN

        WRITE( MESG, 94010 )    &
           'Illegal vertical layer dimension:', NLAYS3( FID ), 'in file "' //    &
           TRIM( FLIST3( FID ) ) // '"'
        CALL M3WARN( 'CKFILE3', 0, 0, MESG )
        CKFLAG = .FALSE.
        RETURN

    ELSE IF ( NLAYS3( FID ) .GT. 1 ) THEN

        IF ( VGTYP3(FID) .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

            MESG = 'WARNING:  Vertical grid/coordinate type: "MISSING" in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )

        ELSE IF ( VGTYP3(FID) .EQ. TBLLAY3 .OR.     &
                  VGTYP3(FID) .EQ. GISLAY3   ) THEN      !  non-geometric layering

            CKFLAG = .TRUE.       !  don't need to check monotonicity
            RETURN

        ELSE IF ( ( VGTYP3(FID) .NE. VGSGPN3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGSGPH3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGSIGZ3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGPRES3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGZVAL3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGHVAL3 ) .AND.    &
                  ( VGTYP3(FID) .NE. VGWRFEM ) .AND.    &
                  ( VGTYP3(FID) .NE. VGWRFNM ) ) THEN

            WRITE( MESG, 94010 )    &
                'Unknown vertical grid/coordinate type:', VGTYP3(FID), 'in file "' // TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFLAG = .FALSE.
            RETURN

        END IF  !  if  vgtyp3d = vgsgph3, etc.

        !...........   Check monotonicity
        
        INCREASING = ( VGLVS3( 2,FID ) .GT. VGLVS3( 1,FID ) )

        DO  L = 2, MIN( NLAYS3( FID ), MXLAYS3 )

            IF ( INCREASING .NEQV. ( VGLVS3( L+1,FID ) .GT. VGLVS3( L,FID ) ) ) THEN

                WRITE( MESG, 94010 )    &
                   'Bad layer monotonicity at layer', L, 'in file "'// TRIM( FLIST3( FID ) ) // '"'

                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFLAG = .FALSE.
                RETURN

            END IF

        END DO

    END IF          !  if nlays < 1, etc.

    !...........   If you get to here:  all checks passed:

99  CKFLAG = .TRUE.
    RETURN

    !******************  FORMAT  STATEMENTS   ******************************

    !...........   Internal buffering formats............ 94xxx

94000 FORMAT( 99( A, : ) )

94010 FORMAT( A, I10, :, 2X, A )

94020 FORMAT( A, 1PG14.7, :, 2X, A )

94030 FORMAT( 4 ( A, I5, :, 2X ) )


END FUNCTION CKFILE3


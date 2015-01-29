
        LOGICAL FUNCTION CKFILE3( FID )

C***********************************************************************
C Version "$Id: ckfile3.f 100 2015-01-16 16:52:16Z coats $"
C EDSS/Models-3 I/O API.
C Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr., and
C (C) 2003-2011 Baron Advanced Meteorological Systems
C Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
C See file "LGPL.txt" for conditions of use.
C.........................................................................
C  function body starts at line  89
C
C  RETURNS:
C       If environment variable IOAPI_CHECK_HEADERS begins with 'Y' or 'y',
C       checks whether file attributes in FDESC3.EXT commons fit into
C       standard valid ranges, and returns TRUE or FALSE accordingly.
C       Returns TRUE otherwise.
C
C  PRECONDITIONS REQUIRED:
C       FDESC3.EXT commons set by user
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       m3err()
C
C  REVISION  HISTORY:
C       Prototype 9/95 by CJC
C       Modified  2/97 by CJC:  check for legality of variable-names
C       Modified  3/04 by D.Yin: add check for POLGRD3
C       Modified  6/06 by CJC: modification to support WRF vertical
C       coordinates, from Tanya Otte
C       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
C       Bug-fix  04/2011 in format 94030  from Matt Turner, UC Boulder.
C***********************************************************************

      IMPLICIT NONE

C...........   INCLUDES:

        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'
        INCLUDE 'FDESC3.EXT'
        INCLUDE 'NETCDF.EXT'


C...........   ARGUMENTS and their descriptions:

        INTEGER, INTENT( IN ) :: FID     !  subscript for STATE3 arrays


C...........   EXTERNAL FUNCTIONS and their descriptions:

        LOGICAL, EXTERNAL :: CKNAME  !  checks legality of variable-names
        LOGICAL, EXTERNAL :: ENVYN   !  get Y/N from environment
        INTEGER, EXTERNAL :: INDEX1  !  name-table lookup


C...........   PARAMETER

        CHARACTER*16, PARAMETER :: BLANK    = ' '
        CHARACTER*16, PARAMETER :: AIR_LAT  = 'AIR_LAT'
        CHARACTER*16, PARAMETER :: AIR_LON  = 'AIR_LON'
        CHARACTER*16, PARAMETER :: AIR_ELV  = 'AIR_ELV'
        CHARACTER*19, PARAMETER :: ENVCHK   = 'IOAPI_CHECK_HEADERS'


C...........   SAVED LOCAL VARIABLES and their descriptions:
C...........   NOTE:  the ANSI standard requires the use of SAVE statements
C...........   for variables which must retain their values from call to call.

        LOGICAL, SAVE :: CHKHDR
        LOGICAL, SAVE :: FIRSTIME =.TRUE.


C...........   SCRATCH LOCAL VARIABLES and their descriptions:

        INTEGER         ENVSTAT         !  return value for ENVYN()
        INTEGER         L, U, V         !  loop counters
        LOGICAL         INCREASING
        CHARACTER*80    MESG
        INTEGER         VGTYP, IERR
        REAL            VGLVS( MXLAYS3 + 1 )  !  vertical coord values.


C***********************************************************************
C   begin body of function  CKFILE3

        IF ( FIRSTIME ) THEN

            FIRSTIME = .FALSE.

            CHKHDR   = ENVYN( ENVCHK, 'Perform file-header checks?',
     &                       .FALSE., ENVSTAT )

            IF ( ENVSTAT .GT. 0 ) THEN
                MESG = 'Invalid value for environment vble "' //
     &                 ENVCHK // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            END IF

        END IF          !  if firstime


C.......   If not chkhdr, just return TRUE:

        IF ( .NOT. CHKHDR ) THEN
            CKFILE3 = .TRUE.
            RETURN
        END IF          !  if not chkhdr


C.......   Else perform checks:
C...........   First:  file type and type-specific dimension checks:

        IF ( FTYPE3( FID ) .EQ. DGRAPH3 ) THEN

            CKFILE3 = .TRUE.
            RETURN

        ELSE IF ( FTYPE3( FID ) .EQ. DCTNRY3 ) THEN

            CKFILE3 = .TRUE.
            RETURN

        ELSE IF ( FTYPE3( FID ) .EQ. CUSTOM3 ) THEN

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad blob-size NCOLS', NCOLS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. GRDDED3  .OR.
     &            FTYPE3( FID ) .EQ. TSRIES3  .OR.              !  "exotic"
     &            FTYPE3( FID ) .EQ. PTRFLY3     ) THEN         !  grdded3's

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NCOLS', NCOLS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NROWS', NROWS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF


            IF ( FTYPE3( FID ) .EQ. PTRFLY3 ) THEN
                IF ( INDEX1( AIR_LAT, NVARS3( FID ),
     &                       VLIST3( 1,FID ) ) .LE. 0 ) THEN
                    MESG = 'Variable AIR_LAT not found in ' //
     &              'PTRFLY3-type file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                    CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                    CKFILE3 = .FALSE.
                    RETURN
                END IF

                IF ( INDEX1( AIR_LON, NVARS3( FID ),
     &                       VLIST3( 1,FID ) ) .LE. 0 ) THEN
                    MESG = 'Variable AIR_LON not found in ' //
     &              'PTRFLY3-type file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                    CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                    CKFILE3 = .FALSE.
                    RETURN
                END IF

                IF ( INDEX1( AIR_ELV, NVARS3( FID ),
     &                       VLIST3( 1,FID ) ) .LE. 0 ) THEN
                    MESG = 'Variable AIR_ELV not found in ' //
     &              'PTRFLY3-type file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                    CALL M3WARN( 'CKFILE', 0, 0, MESG )
                    CKFILE3 = .FALSE.
                    RETURN
                END IF

            END IF              !  if ftype ptrfly3

        ELSE IF ( FTYPE3( FID ) .EQ. BNDARY3 ) THEN

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NCOLS', NCOLS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NROWS', NROWS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( ABS( NTHIK3( FID ) ) .GT. MIN( NCOLS3( FID ),
     &                                          NROWS3( FID ) )/2 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad boundary width NTHIK', NTHIK3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. IDDATA3 ) THEN

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max site count NROWS', NROWS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. PROFIL3 ) THEN

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max site count NROWS', NROWS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max level count NCOLS', NCOLS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. GRNEST3 ) THEN

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max cell-count NCOLS', NCOLS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max nest count NROWS', NROWS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. SMATRX3 ) THEN

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad max matrix coeff-count NCOLS', NCOLS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad matrix NROWS', NROWS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NTHIK3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad full-matrix col-count NTHIK', NTHIK3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( FTYPE3( FID ) .EQ. KFEVNT3 ) THEN

            IF ( NCOLS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NCOLS', NCOLS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NROWS3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NROWS', NROWS3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( NTHIK3( FID ) .LE. 0 ) THEN
                WRITE( MESG, 94010 )
     &          'Bad NTHIK', NTHIK3( FID ), 'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE

            WRITE( MESG, 94010 )
     &      'Illegal file type:', FTYPE3( FID ), 'for file "' //
     &      TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFILE3 = .FALSE.
            RETURN

        END IF


C...........   Next, checks on the variable-list

        IF ( NVARS3( FID ) .LT. 0 ) THEN

            WRITE( MESG, 94010 )
     &          'Illegal number of variables:', NVARS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFILE3 = .FALSE.
            RETURN

        ELSE IF ( NVARS3( FID ) .EQ. 0 ) THEN    !  _is_ legal, but unusual

            WRITE( MESG, 94010 )
     &          'WARNING:  number of variables:', NVARS3( FID ),
     &          'for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )

        END IF

        DO  22  U = 1, NVARS3( FID )

            IF ( .NOT. CKNAME( VLIST3( U,FID ) ) ) THEN
                WRITE( MESG,94000 )
     &              'Illegal variable name "' , VLIST3( U,FID ) ,
     &              '" in file ' , FLIST3( FID )
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( VTYPE3( U,FID ) .LT. M3INT  .OR.
     &           VTYPE3( U,FID ) .GT. M3DBLE ) THEN

                WRITE( MESG, 94010 )
     &              'Illegal data type ', VTYPE3( U,FID ),
     &              'for variable "' //
     &              TRIM( VLIST3( U,FID ) ) //
     &              '" in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN

            END IF      !  end check on variable-type

            DO  11  V = 1, U-1          !  hunt for duplicate names

                IF( VLIST3( U,FID ) .EQ. VLIST3( V,FID ) ) THEN

                    WRITE( MESG, 94030 )
     &              'Variable name VLIST3D(', U, ') = "' //
     &              TRIM( VLIST3( U,FID ) ) //
     &              '" duplicates VLIST3D(', V, ') = "' //
     &              TRIM( VLIST3( V,FID ) ) //
     &              '" in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'

                    CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                    CKFILE3 = .FALSE.
                    RETURN

                END IF

11          CONTINUE    !  end check for duplicates of this name

22      CONTINUE        !  end loop on variables U


C...........   Checks on the horizontal coordinate description:

        IF ( GDTYP3( FID ) .EQ. LATGRD3 ) THEN

            IF ( XORIG3( FID ) .LT. -180.0D0 .OR.
     &           XORIG3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad grid origin', XORIG3( FID ), 'in file "' //
     &               TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( YORIG3( FID ) .LT. -90.0D0 .OR.
     &           YORIG3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad grid origin', YORIG3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID ) .EQ. LAMGRD3 ) THEN

            IF ( XCENT3( FID ) .LT. -180.0D0 .OR.
     &           XCENT3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( YCENT3( FID ) .LT. -90.0D0 .OR.
     &           YCENT3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3( FID ),
     &              'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.
     &           P_ALP3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_BET3( FID ) .LT. P_ALP3( FID ) .OR.
     &           P_BET3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.
     &           P_GAM3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID ) .EQ. MERGRD3 ) THEN

            IF ( XCENT3( FID ) .LT. -180.0D0 .OR.
     &           XCENT3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
            END IF

            IF ( YCENT3( FID ) .LT. -90.0D0 .OR.
     &           YCENT3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
            END IF

            IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.
     &           P_ALP3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_BET3( FID ) .LT. -180.0D0 .OR.
     &           P_BET3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.
     &           P_GAM3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID) .EQ. POLGRD3 ) THEN

            IF ( XCENT3( FID) .LT. -180.0D0 .OR.
     &           XCENT3( FID) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3( FID), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( YCENT3( FID) .LT. -90.0D0 .OR.
     &           YCENT3( FID) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3( FID), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_ALP3( FID) .NE. -1.0D0 .AND.
     &           P_ALP3( FID) .NE.  1.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3( FID), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_BET3( FID) .LT. -90.0D0 .OR.
     &           P_BET3( FID) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3( FID), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_GAM3( FID) .LT. -180.0D0 .OR.
     &           P_GAM3( FID) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3( FID), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID ) .EQ. STEGRD3 ) THEN

            IF ( XCENT3( FID ) .LT. -180.0D0 .OR.
     &           XCENT3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', XCENT3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( YCENT3( FID ) .LT. -90.0D0 .OR.
     &           YCENT3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad X-Y origin', YCENT3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_ALP3( FID ) .LT. -90.0D0 .OR.
     &           P_ALP3( FID ) .GT.  90.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_BET3( FID ) .LT. -180.0D0 .OR.
     &           P_BET3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-BETA', P_BET3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

            IF ( P_GAM3( FID ) .LT. -180.0D0 .OR.
     &           P_GAM3( FID ) .GT.  180.0D0 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-GAMMA', P_GAM3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID ) .EQ. UTMGRD3 ) THEN

            IF ( P_ALP3( FID ) .LT.  0.9D0 .OR.
     &           P_ALP3( FID ) .GT. 36.1D0 .OR.
     &           ABS( P_ALP3( FID ) -
     &                DBLE( NINT( P_ALP3( FID ) ) ) ) .GT. 0.01 ) THEN
                WRITE( MESG, 94020 )
     &              'Bad PROJ-ALPHA', P_ALP3( FID ), 'in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF

        ELSE IF ( GDTYP3( FID ) .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

            WRITE( MESG, 94010 )
     &          'WARNING:  Horizontal grid/coordinate type:',
     &          GDTYP3( FID ),
     &          '"MISSING"  in file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )

        ELSE    !  illegal grid type

            WRITE( MESG, 94010 )
     &         'Illegal horizontal grid/coordinate type:',
     &          GDTYP3( FID ), 'in file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFILE3 = .FALSE.
            RETURN

        END IF  !  if  gdtyp3d = lamgrd3, etc.


C...........   Checks on the vertical coordinate description:

        IF ( NLAYS3( FID ) .LT. 1 .AND.
     &       FTYPE3( FID ) .GE. CUSTOM3 ) THEN

            WRITE( MESG, 94010 )
     &         'Illegal vertical layer dimension:', NLAYS3( FID ),
     &         'in file "' //
     &         TRIM( FLIST3( FID ) ) // '"'
            CALL M3WARN( 'CKFILE3', 0, 0, MESG )
            CKFILE3 = .FALSE.
            RETURN

        ELSE IF ( NLAYS3( FID ) .GT. 1 ) THEN

            CALL NCAGT( CDFID3( FID ), NCGLOBAL, 'VGTYP', VGTYP, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,94010 )
     &          'netCDF error', IERR, 'reading VGTYP for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
            END IF          !  ierr nonzero:  NCAGT() failed

            CALL NCAGT( CDFID3( FID ), NCGLOBAL, 'VGLVLS', VGLVS, IERR )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG,94010 )
     &          'netCDF error', IERR, 'reading VGLVLS for file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  NCAGT() failed

            INCREASING = ( VGLVS( 2 ) .GT. VGLVS( 1 ) )

            DO  111  L = 2, MIN( NLAYS3( FID ), MXLAYS3 )

                IF ( INCREASING .NEQV.
     &               ( VGLVS( L+1 ) .GT. VGLVS( L ) ) ) THEN

                    WRITE( MESG, 94010 )
     &              'Bad layer monotonicity at layer', L, 'in file "'//
     &              TRIM( FLIST3( FID ) ) // '"'

                    CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                    CKFILE3 = .FALSE.
                    RETURN

                END IF

111         CONTINUE

            IF ( VGTYP .EQ. IMISS3  ) THEN   !  "other" -- legal but unusual

                WRITE( MESG, 94010 )
     &              'WARNING:  Vertical grid/coordinate type:',
     &              VGTYP,
     &              '"MISSING" in file "' //
     &              TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )

            ELSE IF ( ( VGTYP .NE. VGSGPN3 ) .AND.
     &                ( VGTYP .NE. VGSGPH3 ) .AND.
     &                ( VGTYP .NE. VGSIGZ3 ) .AND.
     &                ( VGTYP .NE. VGPRES3 ) .AND.
     &                ( VGTYP .NE. VGZVAL3 ) .AND.
     &                ( VGTYP .NE. VGHVAL3 ) .AND.
     &                ( VGTYP .NE. VGWRFEM ) .AND.
     &                ( VGTYP .NE. VGWRFNM ) ) THEN

                WRITE( MESG, 94010 )
     &         'Unknown vertical grid/coordinate type:', VGTYP,
     &         'in file "' //
     &          TRIM( FLIST3( FID ) ) // '"'
                CALL M3WARN( 'CKFILE3', 0, 0, MESG )
                CKFILE3 = .FALSE.
                RETURN

            END IF  !  if  vgtyp3d = vgsgph3, etc.

        END IF          !  if nlays < 1, etc.

C...........   If you get to here:  all checks passed:

        CKFILE3 = .TRUE.
        RETURN

C******************  FORMAT  STATEMENTS   ******************************

C...........   Internal buffering formats............ 94xxx

94000   FORMAT( A )

94010   FORMAT( A, I10, :, 2X, A )

94020   FORMAT( A, 1PG14.7, :, 2X, A )

94030   FORMAT( 4 ( A, I5, :, 2X ) )


        END FUNCTION CKFILE3


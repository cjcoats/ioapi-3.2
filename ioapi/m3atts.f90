
MODULE M3ATTS

        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
        !! Version "$Id: m3atts.f90 100 2015-01-16 16:52:16Z coats $"
        !! Copyright (c) 2014 UNC Institute for the Environment
        !! Distributed under the GNU LESSER PUBLIC LICENSE version 2
        !! See file "LGPL.txt" for conditions of use.
        !!...................................................................
        !!  DESCRIPTION:
        !!
        !!  PRECONDITIONS:
        !!      Use these routines after calling INIT3() and before
        !!      calling OPEN3() on files to contain these extra attributes
        !!
        !!  REVISION  HISTORY:
        !!      Adapted 12/2014 by Carlie J. Coats, Jr., from I/O API 3.1
        !!      "matxatts.f" and other sources.
        !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

        USE M3UTILIO

        IMPLICIT NONE

        !!........  PUBLIC Routines:

        PUBLIC  INITCF, SETCF, ENDCF,                                       &
                INITMTXATT, GETMTXATT, SETMTXATT, CHKMTXATT, ENDMTXATT,     &
                INITCMAQ,    GETCMAQ,   SETCMAQ,  ENDCMAQ,                  &
                INITSMOKE,   GETSMOKE,  SETSMOKE, ENDSMOKE

        LOGICAL, PUBLIC, PROTECTED :: MATXMETA  = .FALSE.
        LOGICAL, PUBLIC, PROTECTED :: CMAQMETA  = .FALSE.
        LOGICAL, PUBLIC, PROTECTED :: SMOKEMETA = .FALSE.
        LOGICAL, PUBLIC, PROTECTED :: CFMETA    = .FALSE.

        INTEGER, PUBLIC, PARAMETER ::  INGRD3  = 1
        INTEGER, PUBLIC, PARAMETER ::  OUTGRD3 = 2

        INTERFACE GETMTXATT
            MODULE PROCEDURE  GETMTXATT1, GETMTXATT2
        END INTERFACE

        INTERFACE SETMTXATT
            MODULE PROCEDURE  SETMTXATT1, SETMTXATT2, SETMTXATT3
        END INTERFACE

        INTERFACE CHKMTXATT
            MODULE PROCEDURE  CHKMTXATT1, CHKMTXATT2, CHKMTXATT3
        END INTERFACE

        INTERFACE SETCMAQ
            MODULE PROCEDURE  SETCMAQA, SETCMAQC, SETCMAQ1
        END INTERFACE

        INTERFACE SETSMOKE
            MODULE PROCEDURE  SETSMOKEA, SETSMOKEC, SETSMOKE1
        END INTERFACE

        INTERFACE SETCF
            MODULE PROCEDURE  SETCFC, SETCF1
        END INTERFACE


        PRIVATE     !!  everything else


        !!........  EXTERNAL FUNCTION:  logical name to I/O API file ID

        INTEGER, EXTERNAL :: NAME2FID

        !!........  PRIVATE PARAMETERs

        CHARACTER(LEN=5), PARAMETER :: GDNAMSTR = 'GDNAM'
        CHARACTER(LEN=5), PARAMETER :: GDTYPSTR = 'GDTYP'
        CHARACTER(LEN=5), PARAMETER :: P_ALPSTR = 'P_ALP'
        CHARACTER(LEN=5), PARAMETER :: P_BETSTR = 'P_BET'
        CHARACTER(LEN=5), PARAMETER :: P_GAMSTR = 'P_GAM'
        CHARACTER(LEN=5), PARAMETER :: XCENTSTR = 'XCENT'
        CHARACTER(LEN=5), PARAMETER :: YCENTSTR = 'YCENT'
        CHARACTER(LEN=5), PARAMETER :: XORIGSTR = 'XORIG'
        CHARACTER(LEN=5), PARAMETER :: YORIGSTR = 'YORIG'
        CHARACTER(LEN=5), PARAMETER :: XCELLSTR = 'XCELL'
        CHARACTER(LEN=5), PARAMETER :: YCELLSTR = 'YCELL'
        CHARACTER(LEN=5), PARAMETER :: NCOLSSTR = 'NCOLS'
        CHARACTER(LEN=5), PARAMETER :: NROWSSTR = 'NROWS'

        CHARACTER(LEN=4), PARAMETER :: MODENAME( 2 ) = (/ '_IN ' , '_OUT' /)

        !!.......   LOCAL VARIABLES and their descriptions:
        !!.......   state variables for the extra-attributes module
        !!.......   First:  matrix attributes and att-name tables

        CHARACTER(NAMLEN3), SAVE :: GDNAM_IN, GDNAM_OUT

        REAL*8 , SAVE :: P_ALP_IN = BADVAL3     !! first, second, third map
        REAL*8 , SAVE :: P_BET_IN = BADVAL3     !! projection descriptive
        REAL*8 , SAVE :: P_GAM_IN = BADVAL3     !! parameters.
        REAL*8 , SAVE :: XCENT_IN = BADVAL3     !! lon for coord-system X=0
        REAL*8 , SAVE :: YCENT_IN = BADVAL3     !! lat for coord-system Y=0
        REAL*8 , SAVE :: XORIG_IN = BADVAL3     !! X-coordinate origin of grid (map units)
        REAL*8 , SAVE :: YORIG_IN = BADVAL3     !! Y-coordinate origin of grid
        REAL*8 , SAVE :: XCELL_IN = BADVAL3     !! X-coordinate cell dimension
        REAL*8 , SAVE :: YCELL_IN = BADVAL3     !! Y-coordinate cell dimension
        INTEGER, SAVE :: GDTYP_IN = IMISS3      !! number of grid columns
        INTEGER, SAVE :: NCOLS_IN = IMISS3      !! number of grid columns
        INTEGER, SAVE :: NROWS_IN = IMISS3      !! number of grid rows

        REAL*8 , SAVE :: P_ALP_OUT = BADVAL3    !! first, second, third map
        REAL*8 , SAVE :: P_BET_OUT = BADVAL3    !! projection descriptive
        REAL*8 , SAVE :: P_GAM_OUT = BADVAL3    !! parameters.
        REAL*8 , SAVE :: XCENT_OUT = BADVAL3    !! lon for coord-system X=0
        REAL*8 , SAVE :: YCENT_OUT = BADVAL3    !! lat for coord-system Y=0
        REAL*8 , SAVE :: XORIG_OUT = BADVAL3    !! X-coordinate origin of grid (map units)
        REAL*8 , SAVE :: YORIG_OUT = BADVAL3    !! Y-coordinate origin of grid
        REAL*8 , SAVE :: XCELL_OUT = BADVAL3    !! X-coordinate cell dimension
        REAL*8 , SAVE :: YCELL_OUT = BADVAL3    !! Y-coordinate cell dimension
        INTEGER, SAVE :: GDTYP_OUT = IMISS3     !! number of grid columns
        INTEGER, SAVE :: NCOLS_OUT = IMISS3     !! number of grid columns
        INTEGER, SAVE :: NROWS_OUT = IMISS3     !! number of grid rows

        CHARACTER*80, SAVE :: SVN_ID =  &
'$Id:: m3atts.f90 100 2015-01-16 16:52:16Z coats                                $'


CONTAINS    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  INITMTXATT(                                             &
            GDNAM1, GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                    XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1,     &
            GDNAM2, GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                    XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 )


        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: GDNAM1, GDNAM2
        INTEGER           , INTENT( IN ) :: GDTYP1, GDTYP2
        REAL*8            , INTENT( IN ) :: P_ALP1, P_ALP2
        REAL*8            , INTENT( IN ) :: P_BET1, P_BET2
        REAL*8            , INTENT( IN ) :: P_GAM1, P_GAM2
        REAL*8            , INTENT( IN ) :: XCENT1, XCENT2
        REAL*8            , INTENT( IN ) :: YCENT1, YCENT2
        REAL*8            , INTENT( IN ) :: XORIG1, XORIG2
        REAL*8            , INTENT( IN ) :: YORIG1, YORIG2
        REAL*8            , INTENT( IN ) :: XCELL1, XCELL2
        REAL*8            , INTENT( IN ) :: YCELL1, YCELL2
        INTEGER           , INTENT( IN ) :: NCOLS1, NCOLS2
        INTEGER           , INTENT( IN ) :: NROWS1, NROWS2

        !!........  body  .........................................

        GDNAM_IN = GDNAM1
        GDTYP_IN = GDTYP1
        P_ALP_IN = P_ALP1
        P_BET_IN = P_BET1
        P_GAM_IN = P_GAM1
        XCENT_IN = XCENT1
        YCENT_IN = YCENT1
        XORIG_IN = XORIG1
        YORIG_IN = YORIG1
        XCELL_IN = XCELL1
        YCELL_IN = YCELL1
        NCOLS_IN = NCOLS1
        NROWS_IN = NROWS1

        GDNAM_OUT = GDNAM2
        GDTYP_OUT = GDTYP2
        P_ALP_OUT = P_ALP2
        P_BET_OUT = P_BET2
        P_GAM_OUT = P_GAM2
        XCENT_OUT = XCENT2
        YCENT_OUT = YCENT2
        XORIG_OUT = XORIG2
        YORIG_OUT = YORIG2
        XCELL_OUT = XCELL2
        YCELL_OUT = YCELL2
        NCOLS_OUT = NCOLS2
        NROWS_OUT = NROWS2

        MATXMETA = .TRUE.
        RETURN

    END SUBROUTINE  INITMTXATT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  SETMTXATT1( FNAME, IMODE, GDNAM,              &
                    GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,       &
                    XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )


        !!........  Include file:

        INCLUDE 'NETCDF.EXT'      !! netCDF  declarations
        INCLUDE 'STATE3.EXT'      !! I/O API internal state


        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ):: FNAME
        INTEGER           , INTENT( IN ):: IMODE
        CHARACTER( LEN=* ), INTENT( IN ):: GDNAM
        INTEGER           , INTENT( IN ):: GDTYP
        REAL*8            , INTENT( IN ):: P_ALP
        REAL*8            , INTENT( IN ):: P_BET
        REAL*8            , INTENT( IN ):: P_GAM
        REAL*8            , INTENT( IN ):: XCENT
        REAL*8            , INTENT( IN ):: YCENT
        REAL*8            , INTENT( IN ):: XORIG
        REAL*8            , INTENT( IN ):: YORIG
        REAL*8            , INTENT( IN ):: XCELL
        REAL*8            , INTENT( IN ):: YCELL
        INTEGER           , INTENT( IN ):: NCOLS
        INTEGER           , INTENT( IN ):: NROWS


        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MATXATTS/SETMTXATT'


        !!........  External function:  logical name to I/O API file ID

        INTEGER     NAME2FID
        EXTERNAL    NAME2FID


        !!........  Local Variables:

        INTEGER             FID
        LOGICAL             EFLAG
        CHARACTER*16        ANAME, GNAME
        CHARACTER*256       MESG


        !!........  body:

        EFLAG = .FALSE.

        IF ( IMODE .NE. INGRD3 .AND. IMODE .NE. OUTGRD3 ) THEN
            WRITE( MESG, '(A, I10)' ) 'Unrecognized IMODE =', IMODE
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETMTXATT1 = .FALSE.
            RETURN
        END IF

        !!  Get I/O API file id, etc.

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN

            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETMTXATT1 = .FALSE.
            RETURN

        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN

            MESG = 'File "' // TRIM( FNAME ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETMTXATT1 = .FALSE.
            RETURN

        END IF

        ANAME = GDNAMSTR // MODENAME( IMODE )
        GNAME = GDNAM
        IF ( .NOT.WRATTC( FNAME, ALLVAR3, ANAME, GNAME ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = GDTYPSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT, 1, GDTYP ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
            CALL M3WARN( PNAME, 0, 0, MESG )
        END IF

        ANAME = P_ALPSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, P_ALP ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = P_BETSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, P_BET ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = P_GAMSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, P_GAM ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = XCENTSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, XCENT ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = YCENTSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, YCENT ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = XORIGSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, XORIG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = YORIGSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, YORIG ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = XCELLSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, XCELL ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = YCELLSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3DBLE, 1, YCELL ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = NCOLSSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT, 1, NCOLS ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        ANAME = NROWSSTR // MODENAME( IMODE )
        IF ( .NOT.WRATT3( FNAME, ALLVAR3, ANAME, M3INT, 1, NROWS ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not write attribute "' //TRIM( ANAME )// '" to ' // FNAME 
            CALL M3MESG( MESG )
        END IF

        IF ( EFLAG ) THEN
            SETMTXATT1 = .FALSE.
        ELSE IF ( IMODE .EQ. INGRD3 ) THEN
            GDNAM_IN = GDNAM
            GDTYP_IN = GDTYP
            P_ALP_IN = P_ALP
            P_BET_IN = P_BET
            P_GAM_IN = P_GAM
            XCENT_IN = XCENT
            YCENT_IN = YCENT
            XORIG_IN = XORIG
            YORIG_IN = YORIG
            XCELL_IN = XCELL
            YCELL_IN = YCELL
            NCOLS_IN = NCOLS
            NROWS_IN = NROWS
            MATXMETA = ( NCOLS_IN .GT. 0 .AND. NCOLS_OUT .GT. 0 )
            SETMTXATT1 = .TRUE.
        ELSE IF ( IMODE .EQ. OUTGRD3 ) THEN
            GDNAM_OUT = GDNAM
            GDTYP_OUT = GDTYP
            P_ALP_OUT = P_ALP
            P_BET_OUT = P_BET
            P_GAM_OUT = P_GAM
            XCENT_OUT = XCENT
            YCENT_OUT = YCENT
            XORIG_OUT = XORIG
            YORIG_OUT = YORIG
            XCELL_OUT = XCELL
            YCELL_OUT = YCELL
            NCOLS_OUT = NCOLS
            NROWS_OUT = NROWS
            MATXMETA = ( NCOLS_IN .GT. 0 .AND. NCOLS_OUT .GT. 0 )
            SETMTXATT1 = .TRUE.
        END IF


        SETMTXATT1 = ( .NOT.EFLAG )
        RETURN

    END FUNCTION  SETMTXATT1


    !! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  SETMTXATT2( FNAME,                                &
            GDNAM1, GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                    XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1,     &
            GDNAM2, GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                    XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME
        CHARACTER( LEN=* ), INTENT( IN ) :: GDNAM1, GDNAM2
        INTEGER           , INTENT( IN ) :: GDTYP1, GDTYP2
        REAL*8            , INTENT( IN ) :: P_ALP1, P_ALP2
        REAL*8            , INTENT( IN ) :: P_BET1, P_BET2
        REAL*8            , INTENT( IN ) :: P_GAM1, P_GAM2
        REAL*8            , INTENT( IN ) :: XCENT1, XCENT2
        REAL*8            , INTENT( IN ) :: YCENT1, YCENT2
        REAL*8            , INTENT( IN ) :: XORIG1, XORIG2
        REAL*8            , INTENT( IN ) :: YORIG1, YORIG2
        REAL*8            , INTENT( IN ) :: XCELL1, XCELL2
        REAL*8            , INTENT( IN ) :: YCELL1, YCELL2
        INTEGER           , INTENT( IN ) :: NCOLS1, NCOLS2
        INTEGER           , INTENT( IN ) :: NROWS1, NROWS2

        !!........  body  .........................................

        SETMTXATT2 = ( SETMTXATT1( FNAME, INGRD3, GDNAM1,                           &
                                   GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                                   XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1 ) &
                       .AND.                                                        &
                       SETMTXATT1( FNAME, OUTGRD3, GDNAM2,                          &
                                 GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,    &
                                 XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 ) )

        RETURN


    END FUNCTION  SETMTXATT2


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  SETMTXATT3( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Include file:

        INCLUDE 'STATE3.EXT'      !! I/O API state  declarations
        INCLUDE 'NETCDF.EXT'      !! netCDF  declarations

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETMATX'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        SETMTXATT3 = ( SETMTXATT1( FNAME, INGRD3, GDNAM_IN,                     &
                                   GDTYP_IN, P_ALP_IN, P_BET_IN, P_GAM_IN,      &
                                   XCENT_IN, YCENT_IN, XORIG_IN, YORIG_IN,      &
                                   XCELL_IN, YCELL_IN, NCOLS_IN, NROWS_IN )     &
                       .AND.                                                    &
                       SETMTXATT1( FNAME, OUTGRD3, GDNAM_OUT,                   &
                                  GDTYP_OUT, P_ALP_OUT, P_BET_OUT, P_GAM_OUT,   &
                                  XCENT_OUT, YCENT_OUT, XORIG_OUT, YORIG_OUT,   &
                                  XCELL_OUT, YCELL_OUT, NCOLS_OUT, NROWS_OUT ) )

        RETURN

    END FUNCTION  SETMTXATT3


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  GETMTXATT1( FNAME, IMODE, GDNAM, GDTYP,               &
                                  P_ALP, P_BET, P_GAM, XCENT, YCENT,        &
                                  XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        INTEGER           , INTENT(IN   ) :: IMODE      !!  ingrid or outgrid

        CHARACTER( LEN=* ), INTENT(  OUT) :: GDNAM
        INTEGER           , INTENT(  OUT) :: GDTYP
        REAL*8            , INTENT(  OUT) :: P_ALP
        REAL*8            , INTENT(  OUT) :: P_BET
        REAL*8            , INTENT(  OUT) :: P_GAM
        REAL*8            , INTENT(  OUT) :: XCENT
        REAL*8            , INTENT(  OUT) :: YCENT
        REAL*8            , INTENT(  OUT) :: XORIG
        REAL*8            , INTENT(  OUT) :: YORIG
        REAL*8            , INTENT(  OUT) :: XCELL
        REAL*8            , INTENT(  OUT) :: YCELL
        INTEGER           , INTENT(  OUT) :: NCOLS
        INTEGER           , INTENT(  OUT) :: NROWS

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/GETMTXATT1'

        !!........  Include file:

        INCLUDE 'STATE3.EXT'      !! I/O API state
        INCLUDE 'NETCDF.EXT'      !! netCDF  declarations

        !!........  Local Variables:

        INTEGER             FID, ASIZE
        LOGICAL             EFLAG
        CHARACTER*16        ANAME, GNAME
        CHARACTER*256       MESG

        !!........  body  .........................................

        EFLAG = .FALSE.

        !!  Get I/O API file id. etc.

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN

            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETMTXATT1 = .FALSE.
            RETURN

        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN

            MESG = 'File "' // TRIM( FNAME ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETMTXATT1 = .FALSE.
            RETURN

        END IF

        IF ( IMODE .EQ. INGRD3 ) THEN

            ANAME = GDNAMSTR // '_IN'
            IF ( .NOT.RDATTC( FLIST3(FID), ALLVAR3, ANAME, GDNAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = GDTYPSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, GDTYP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_ALPSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_ALP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_BETSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_BET ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_GAMSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_GAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XCENTSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YCENTSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XORIGSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YORIGSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XCELLSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YCELLSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = NCOLSSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, NCOLS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = NROWSSTR // '_IN'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, NROWS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

        ELSE IF ( IMODE .EQ. OUTGRD3 ) THEN

            ANAME = GDNAMSTR // '_OUT'
            IF ( .NOT.RDATTC( FLIST3(FID), ALLVAR3, ANAME, GDNAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = GDTYPSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, GDTYP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_ALPSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_ALP ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_BETSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_BET ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = P_GAMSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, P_GAM ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XCENTSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YCENTSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YCENT ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XORIGSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YORIGSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YORIG ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = XCELLSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, XCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = YCELLSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3DBLE, 1, ASIZE, YCELL ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = NCOLSSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, NCOLS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

            ANAME = NROWSSTR // '_OUT'
            IF ( .NOT.RDATT3( FLIST3(FID), ALLVAR3, ANAME, M3INT, 1, ASIZE, NROWS ) ) THEN
                EFLAG = .TRUE.
                MESG  = 'Could not read attribute "' // TRIM( ANAME ) // '" from ' // FNAME
                CALL M3MESG( MESG )
            END IF

        ELSE

            WRITE( MESG, '( A, I9 )' ) 'Unrecognized mode', IMODE
            CALL M3MESG( MESG )
            EFLAG =.TRUE.

        END IF

        GETMTXATT1 = ( .NOT.EFLAG )
        RETURN

    END FUNCTION  GETMTXATT1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  GETMTXATT2( FNAME,                                &
            GDNAM1, GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                    XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1,     &
            GDNAM2, GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,     &
                    XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 )

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        CHARACTER( LEN=* ), INTENT(  OUT) :: GDNAM1, GDNAM2
        INTEGER           , INTENT(  OUT) :: GDTYP1, GDTYP2
        REAL*8            , INTENT(  OUT) :: P_ALP1, P_ALP2
        REAL*8            , INTENT(  OUT) :: P_BET1, P_BET2
        REAL*8            , INTENT(  OUT) :: P_GAM1, P_GAM2
        REAL*8            , INTENT(  OUT) :: XCENT1, XCENT2
        REAL*8            , INTENT(  OUT) :: YCENT1, YCENT2
        REAL*8            , INTENT(  OUT) :: XORIG1, XORIG2
        REAL*8            , INTENT(  OUT) :: YORIG1, YORIG2
        REAL*8            , INTENT(  OUT) :: XCELL1, XCELL2
        REAL*8            , INTENT(  OUT) :: YCELL1, YCELL2
        INTEGER           , INTENT(  OUT) :: NCOLS1, NCOLS2
        INTEGER           , INTENT(  OUT) :: NROWS1, NROWS2

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/GETMTXATT2'

        !!........  Include file:

        INCLUDE 'STATE3.EXT'      !! I/O API state
        INCLUDE 'NETCDF.EXT'      !! netCDF  declarations

        !!........  body  .........................................

        GETMTXATT2 = ( GETMTXATT1( FNAME, INGRD3, GDNAM1,                           &
                                   GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                                   XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1 ) &
                      .AND.                                                         &
                      GETMTXATT1( FNAME, OUTGRD3, GDNAM2,                           &
                                  GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,   &
                                  XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 ) )
        RETURN

    END FUNCTION  GETMTXATT2


    !! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  CHKMTXATT1( FNAME, IMODE, GDNAM,                          &
                                  GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT,     &
                                  XORIG, YORIG, XCELL, YCELL, NCOLS, NROWS )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ):: FNAME
        INTEGER           , INTENT( IN ):: IMODE    !!  INGRID or OUTGRID
        CHARACTER( LEN=* ), INTENT( IN ):: GDNAM
        INTEGER           , INTENT( IN ):: GDTYP
        REAL*8            , INTENT( IN ):: P_ALP
        REAL*8            , INTENT( IN ):: P_BET
        REAL*8            , INTENT( IN ):: P_GAM
        REAL*8            , INTENT( IN ):: XCENT
        REAL*8            , INTENT( IN ):: YCENT
        REAL*8            , INTENT( IN ):: XORIG
        REAL*8            , INTENT( IN ):: YORIG
        REAL*8            , INTENT( IN ):: XCELL
        REAL*8            , INTENT( IN ):: YCELL
        INTEGER           , INTENT( IN ):: NCOLS
        INTEGER           , INTENT( IN ):: NROWS

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/CHKMTXATT'


        !!........  Local Variables:

        CHARACTER( LEN=NAMLEN3 ):: GDNAM1
        INTEGER                 :: GDTYP1
        REAL*8                  :: P_ALP1
        REAL*8                  :: P_BET1
        REAL*8                  :: P_GAM1
        REAL*8                  :: XCENT1
        REAL*8                  :: YCENT1
        REAL*8                  :: XORIG1
        REAL*8                  :: YORIG1
        REAL*8                  :: XCELL1
        REAL*8                  :: YCELL1
        INTEGER                 :: NCOLS1
        INTEGER                 :: NROWS1

        LOGICAL                 :: EFLAG
        CHARACTER*256           :: MESG


        !!........  body ..................................................

        EFLAG = .FALSE.

        IF ( IMODE .NE. INGRD3 .AND. IMODE .NE. OUTGRD3 ) THEN
            WRITE( MESG, '(A, I10)' ) 'Unrecognized IMODE =', IMODE
            CALL M3WARN( PNAME, 0, 0, MESG )
            CHKMTXATT1 = .FALSE.
            RETURN
        END IF

        IF ( .NOT.GETMTXATT( FNAME, IMODE, GDNAM1,              &
                             GDTYP1, P_ALP1, P_BET1, P_GAM1,    &
                             XCENT1, YCENT1, XORIG1, YORIG1,    &
                             XCELL1, YCELL1, NCOLS1, NROWS1 ) ) THEN
            MESG = 'Could not get attributes for checking'
            CALL M3WARN( PNAME, 0, 0, MESG )
            CHKMTXATT1 = .FALSE.
            RETURN
        END IF

        IF ( GDNAM .NE. GDNAM1 ) THEN
            MESG = 'GDNAM mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
        END IF

        IF ( GDTYP .NE. GDTYP1 ) THEN
            MESG = 'GDTYP mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( NCOLS .NE. NCOLS1 ) THEN
            MESG = 'NCOLS mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( NROWS .NE. NROWS1 ) THEN
            MESG = 'NROWS mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_ALP, P_ALP1 ) ) THEN
            MESG = 'P_ALP mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_BET, P_BET1 ) ) THEN
            MESG = 'P_BET mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( P_GAM, P_GAM1 ) ) THEN
            MESG = 'P_GAM mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XCENT, XCENT1 ) ) THEN
            MESG = 'XCENT mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YCENT, YCENT1 ) ) THEN
            MESG = 'YCENT mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XORIG, XORIG1 ) ) THEN
            MESG = 'XORIG mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YORIG, YORIG1 ) ) THEN
            MESG = 'YORIG mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( XCELL, XCELL1 ) ) THEN
            MESG = 'XCELL mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF

        IF ( DBLERR( YCELL, YCELL1 ) ) THEN
            MESG = 'YCELL mismatch, file ' // FNAME
            CALL M3MSG2( MESG )
            EFLAG = .TRUE.
        END IF


        CHKMTXATT1 = ( .NOT.EFLAG )
        RETURN


    END FUNCTION  CHKMTXATT1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  CHKMTXATT2( FNAME,                            &
           GDNAM1, GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,  &
                   XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1,  &
           GDNAM2, GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,  &
                   XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 )

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        CHARACTER( LEN=* ), INTENT(IN   ) :: GDNAM1, GDNAM2
        INTEGER           , INTENT(IN   ) :: GDTYP1, GDTYP2
        REAL*8            , INTENT(IN   ) :: P_ALP1, P_ALP2
        REAL*8            , INTENT(IN   ) :: P_BET1, P_BET2
        REAL*8            , INTENT(IN   ) :: P_GAM1, P_GAM2
        REAL*8            , INTENT(IN   ) :: XCENT1, XCENT2
        REAL*8            , INTENT(IN   ) :: YCENT1, YCENT2
        REAL*8            , INTENT(IN   ) :: XORIG1, XORIG2
        REAL*8            , INTENT(IN   ) :: YORIG1, YORIG2
        REAL*8            , INTENT(IN   ) :: XCELL1, XCELL2
        REAL*8            , INTENT(IN   ) :: YCELL1, YCELL2
        INTEGER           , INTENT(IN   ) :: NCOLS1, NCOLS2
        INTEGER           , INTENT(IN   ) :: NROWS1, NROWS2

        !!........  body  .........................................


        CHKMTXATT2 =  ( CHKMTXATT1( FNAME, INGRD3, GDNAM1,                              &
                                    GDTYP1, P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,     &
                                    XORIG1, YORIG1, XCELL1, YCELL1, NCOLS1, NROWS1 )    &
                       .AND.                                                            &
                       CHKMTXATT1( FNAME, OUTGRD3, GDNAM2,                              &
                                   GDTYP2, P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,      &
                                   XORIG2, YORIG2, XCELL2, YCELL2, NCOLS2, NROWS2 ) )
        RETURN


    END FUNCTION  CHKMTXATT2


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  CHKMTXATT3( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ):: FNAME

        !!........  body  .........................................


        CHKMTXATT3 = ( CHKMTXATT1( FNAME, INGRD3, GDNAM_IN,                         &
                                   GDTYP_IN, P_ALP_IN, P_BET_IN, P_GAM_IN,          &
                                   XCENT_IN, YCENT_IN, XORIG_IN, YORIG_IN,          &
                                   XCELL_IN, YCELL_IN, NCOLS_IN, NROWS_IN )         &
                       .AND.                                                        &
                       CHKMTXATT1( FNAME, OUTGRD3, GDNAM_OUT,                       &
                                   GDTYP_OUT, P_ALP_OUT, P_BET_OUT, P_GAM_OUT,      &
                                   XCENT_OUT, YCENT_OUT, XORIG_OUT, YORIG_OUT,      &
                                   XCELL_OUT, YCELL_OUT, NCOLS_OUT, NROWS_OUT ) )
        RETURN


    END FUNCTION  CHKMTXATT3



    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDMTXATT()

        MATXMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDMTXATT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      CMAQ Attributes
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  INITCMAQ( DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: DUMMYATT

        !!........  body  .........................................

        CALL M3MESG( 'CMAQ metadata not yet implemented' )
        CMAQMETA = .FALSE.
        RETURN

    END SUBROUTINE  INITCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION GETCMAQ( FNAME, DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        CHARACTER( LEN=* ), INTENT(  OUT) :: DUMMYATT

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/GETCMAQ'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................

        IF ( .NOT.CMAQMETA ) THEN
            MESG = 'CMAQ attributes not yet initialized'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETCMAQ = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'CMAQ metadata not yet implemented' )
        DUMMYATT = CMISS3
        GETCMAQ  = .FALSE.
        RETURN 

    END FUNCTION  GETCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCMAQA( FNAME, DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME
        CHARACTER( LEN=* ), INTENT( IN ) :: DUMMYATT

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETCMAQA'

        !!........  body  .........................................

        CALL INITCMAQ( DUMMYATT )
        SETCMAQA = SETCMAQC( FNAME )
        RETURN

    END FUNCTION  SETCMAQA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCMAQC( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETCMAQ'

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCMAQC = .FALSE.
        ELSE
            SETCMAQC = SETCMAQ1( FID )
        END IF

        RETURN

    END FUNCTION  SETCMAQC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCMAQ1( FID )

        !!........  Arguments:

        INTEGER, INTENT( IN ) :: FID

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETCMAQ1'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................

        IF ( .NOT.CMAQMETA ) THEN
            MESG = 'CMAQ attributes not yet initialized'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCMAQ1 = .FALSE.
            RETURN
        ELSE IF ( FID .LE. 0 ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Bad file-ID' )
            SETCMAQ1 = .FALSE.
            RETURN
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCMAQ1 = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'CMAQ metadata not yet implemented' )
        SETCMAQ1 = .FALSE.
        RETURN

    END FUNCTION  SETCMAQ1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDCMAQ()

        CMAQMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      SMOKE Attributes
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  INITSMOKE( DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: DUMMYATT

        !!........  body  .........................................

        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        SMOKEMETA = .FALSE.
        RETURN

    END SUBROUTINE  INITSMOKE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION GETSMOKE( FNAME, DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        CHARACTER( LEN=* ), INTENT(  OUT) :: DUMMYATT

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/GETSMOKE'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................

        IF ( .NOT.SMOKEMETA ) THEN
            MESG = 'SMOKE attributes not yet initialized'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETSMOKE = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        DUMMYATT = CMISS3
        GETSMOKE = .FALSE.
        RETURN 

    END FUNCTION  GETSMOKE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKEA( FNAME, DUMMYATT )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME
        CHARACTER( LEN=* ), INTENT( IN ) :: DUMMYATT

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETSMOKEA'

        !!........  body  .........................................

        CALL INITSMOKE( DUMMYATT )
        SETSMOKEA = SETSMOKEC( FNAME )
        RETURN

    END FUNCTION  SETSMOKEA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKEC( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETSMOKE'

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETSMOKEC = .FALSE.
        ELSE
            SETSMOKEC = SETSMOKE1( FID )
        END IF

        RETURN

    END FUNCTION  SETSMOKEC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKE1( FID )

        !!........  Arguments:

        INTEGER, INTENT( IN ) :: FID

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETSMOKE1'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................

        IF ( .NOT.SMOKEMETA ) THEN
            MESG = 'SMOKE attributes not yet initialized'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETSMOKE1 = .FALSE.
            RETURN
        ELSE IF ( FID .LE. 0 ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Bad file-ID' )
            SETSMOKE1 = .FALSE.
            RETURN
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETSMOKE1 = .FALSE.
            RETURN
        END IF

        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        SETSMOKE1 = .FALSE.
        RETURN

    END FUNCTION  SETSMOKE1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDSMOKE()

        SMOKEMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDSMOKE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      CF Attributes
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  INITCF()

        CFMETA = .TRUE.
        RETURN

    END SUBROUTINE  INITCF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCFC( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETCFC'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCFC = .FALSE.
        ELSE
            SETCFC = SETCF1( NAME2FID( FNAME ) )
        END IF

        RETURN

    END FUNCTION  SETCFC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCF1( FID )

        !!........  Arguments:

        INTEGER, INTENT( IN ) :: FID

        !!........  Include file:

        INCLUDE 'NETCDF.EXT'      !! netCDF  declarations
        INCLUDE 'STATE3.EXT'

        !!........  Parameters:

        CHARACTER*72, PARAMETER :: PRJNAMES( 0:TRMGRD3+1 ) = &    !!  map-projection names
                 (/  'unknown_cartesian                 ',   &
                     'geographic                        ',   &
                     'lambert_conformal_conic           ',   &
                     'general_mercator                  ',   &
                     'general_steereographic            ',   &
                     'transverse_mercator               ',   &
                     'polar_stereographic               ',   &
                     'mercator                          ',   &
                     'transverse_mercator               ',   &
                     'unknown_cartesian                 '   /)

        INTEGER,      PARAMETER :: MXDIM = 16384
        CHARACTER*24, PARAMETER :: PNAME = 'M3ATTS/SETCF'

        !!........  Local Variables:

        CHARACTER*16    ANAME, XNAME, YNAME, ZNAME
        CHARACTER*16    XUNIT, YUNIT, ZUNIT
        CHARACTER*80    XDESC, YDESC, ZDESC
        CHARACTER*80    XLONG, YLONG, ZLONG
        CHARACTER*80    DSCBUF          !!  scratch buffer for descriptions
        CHARACTER*80    DSCBU2          !!  scratch buffer for descriptions
        CHARACTER*80    MNAME
        CHARACTER*256   MESG            !!  scratch buffer

        INTEGER         DIMS( 5 )       !!  array of dims for NCVDEF()
        INTEGER         DELS( 5 )       !!  array of dims for NCVPT()
        INTEGER         C, R, L
        INTEGER         MID, CID, RID, LID, IERR
        INTEGER         NDIMS, CDIM, RDIM, LDIM, EDIM
        INTEGER         FNUM
        LOGICAL         EFLAG
        REAL*8          DARGS( MXDIM ), X0, XC


        !!........  body  .........................................

        EFLAG = .FALSE.

        IF ( FID .LE. 0 ) THEN
            CALL M3WARN( PNAME, 0, 0, 'Bad file-ID' )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF

        IF ( NF_REDEF( FNUM ) .NE. NF_NOERR ) THEN
            WRITE( MESG, '( A, I10, 2X, 3 A )' )                    &
                'Error', IERR, 'putting file "', FLIST3(FID) ,      &
                '" into define mode'
            CALL M3WARN( PNAME, 0, 0, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF

        XLONG  = 'projection_x_coordinate'
        YLONG  = 'projection_y_coordinate'
        ZLONG  = 'projection_x_coordinate'

        DSCBUF = 'CF-1.0'
        L = LEN_TRIM( DSCBUF )
        CALL NCAPTC( FNUM, NCGLOBAL, 'Conventions', NCCHAR, L, DSCBUF( 1:L ), IERR )
        IF ( IERR .NE. 0 ) THEN
            DSCBU2 = 'Error creating netCDF file attribute  "Conventions"'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  NCAPTC() failed

        L     = MAX( 0, MIN( TRMGRD3+1, GDTYP3(FID) ) )
        MNAME = PRJNAMES( L )
        MID   = NCVDEF( FNUM, MNAME, NF_CHAR, 0, DIMS, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,            &
                'Error creating netCDF variable "Coords"' )
             EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  NCVDEF() failed

        DIMS( 1 ) = EDIM
        DIMS( 2 ) = CDIM
        DIMS( 3 ) = RDIM
        NDIMS = 3

        XNAME = 'COL'
        YNAME = 'ROW'

        IF ( GDTYP3(FID) .EQ. LATGRD3 ) THEN

            XDESC = 'longitude'
            YDESC = 'latitude'
            XUNIT = 'degrees_east'
            YUNIT = 'degrees_north'

            DSCBUF = 'geographic'
            L = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( GDTYP3(FID) .EQ. LAMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'lambert_conformal_conic'
            L      = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            DSCBUF = 'standard_parallel'
            DARGS( 1 ) = P_ALP3(FID)
            DARGS( 2 ) = P_BET3(FID)

            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 2, DARGS, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'longitude_of_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_GAM3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'latitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, YCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'longitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, XCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_easting'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_northing'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

        ELSE IF ( GDTYP3(FID) .EQ. MERGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            CALL M3MESG( 'CF metadata not supported for MERCATOR' )

        ELSE IF ( GDTYP3(FID) .EQ. STEGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            CALL M3MESG( 'CF metadata not supported for STEREO' )

        ELSE IF ( GDTYP3(FID) .EQ. UTMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'transverse_mercator'
            L      = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            DSCBUF = 'longitude_of_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 6.0D0*P_ALP3D-183.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute  "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'scale_factor_at_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.9996D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'latitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_easting'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1,XCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_northing'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, YCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

        ELSE IF ( GDTYP3(FID) .EQ. POLGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'polar_stereographic'
            L      = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            DSCBUF = 'straight_vertical_longitude_from_pole'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_GAM3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "straight_vertical_longitude_from_pole"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'standard_parallel'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_BET3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'latitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 90.0D0*P_ALP3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_easting'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, XCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_northing'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, YCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

        ELSE IF ( GDTYP3(FID) .EQ. EQMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'mercator'
            L      = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            DSCBUF = 'standard_parallel'
            DARGS( 1 ) =  P_ALP3(FID)
            DARGS( 2 ) = -P_ALP3(FID)

            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 2, DARGS, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'longitude_of_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_GAM3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'latitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, YCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'longitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, XCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_easting'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_northing'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, 0.0D0, IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

        ELSE IF ( GDTYP3(FID) .EQ. TRMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'transverse_mercator'
            L      = LEN_TRIM( DSCBUF )
            CALL NCAPTC( FNUM, MID, 'grid_mapping_name', NCCHAR, L, DSCBUF( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            DSCBUF = 'longitude_of_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_GAM3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'scale_factor_at_central_meridian'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_BET3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'latitude_of_projection_origin'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, P_ALP3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_easting'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, XCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

            DSCBUF = 'false_northing'
            CALL NCAPT( FNUM, MID, DSCBUF, NCDOUBLE, 1, YCENT3(FID), IERR )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  NCAPT() failed

        END IF              !!  if gdtyp3(FID)==latgrd3, lamgrd3, ...


        IF ( NLAYS3(FID) .GT. 1 ) THEN

            IERR = NF_INQ_DIMID( FNUM, 'LAY', LDIM )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error inquiring for dimension "LAY"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF

            ZNAME = 'LAY'

            DIMS( 1 ) = LDIM
            LID = NCVDEF( FNUM, ZNAME, NCDOUBLE, 1,DIMS,IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating netCDF variable LAY'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999

            END IF              !!  ierr nonzero:  NCVDEF() failed

            CALL NCAPTC( FNUM, LID, 'axis', NCCHAR, 1, 'Z', IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating att AXIS for vble LVL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

        END IF          !!  if nlays3(FID) > 1

        IF ( NLAYS3(FID) .LE. 1 ) THEN

            CONTINUE        !!  do nothing: no vertical CF metadata

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPH3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPN3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSIGZ3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGPRES3 ) THEN

            ZDESC = 'pres'
            L     = LEN_TRIM( ZDESC )
            ZUNIT = 'Pa'
            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            L     = LEN_TRIM( ZUNIT )
            CALL NCAPTC( FNUM, LID, 'units', NCCHAR, L, ZUNIT( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGZVAL3 ) THEN

            ZDESC = 'height_above_terrain'
            ZUNIT = 'm'

            L     = LEN_TRIM( ZDESC )
            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            L     = LEN_TRIM( ZUNIT )
            CALL NCAPTC( FNUM, LID, 'units', NCCHAR, L, ZUNIT( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGHVAL3 ) THEN

            ZDESC = 'height_above_MSL'
            ZUNIT = 'm'

            L     = LEN_TRIM( ZDESC )
            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

            L     = LEN_TRIM( ZUNIT )
            CALL NCAPTC( FNUM, LID, 'units', NCCHAR, L, ZUNIT( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFEM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '

            L     = LEN_TRIM( ZDESC )
            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFNM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '

            L     = LEN_TRIM( ZDESC )
            CALL NCAPTC( FNUM, LID, 'standard_name', NCCHAR, L, ZDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCAPTC() failed

        END IF              !!  if nlays=1, or if vgtyp3(FID)==vgsgph3, ...

        IF ( FTYPE3(FID) .EQ. GRDDED3 ) THEN

            IERR = NF_INQ_DIMID( FNUM, 'COL', CDIM )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error inquiring for dimension "COL"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF

            IERR = NF_INQ_DIMID( FNUM, 'ROW', RDIM )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error inquiring for dimension "ROW"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF

            NDIMS = 1
            DIMS( 1 ) = CDIM
            CID = NCVDEF( FNUM, XNAME, NCDOUBLE, 1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCVDEF() failed

            L = LEN_TRIM( XDESC )
            CALL NCAPTC( FNUM, CID, 'long_name', NCCHAR, L, XDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute LONG_NAME for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            CALL NCAPTC( FNUM, CID, 'axis', NCCHAR, 1, 'X', IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute AXIS for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( XLONG )
            CALL NCAPTC( FNUM, CID, 'standard_name', NCCHAR, L, XLONG( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating att STANDARD_NAME for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( XUNIT )
            CALL NCAPTC( FNUM, CID, 'units', NCCHAR, L, XUNIT( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute UNITS for variable COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( MNAME )
            CALL NCAPTC( FNUM, CID, 'grid_mapping', NCCHAR, L, MNAME( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error creating netCDF attribute "grid_mapping" for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed


            NDIMS = 1
            DIMS( 1 ) = RDIM
            RID = NCVDEF( FNUM, YNAME, NCDOUBLE, 1, DIMS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ROW' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCVDEF() failed

            CALL NCAPTC( FNUM, RID, 'axis', NCCHAR, 1, 'Y', IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute AXIS for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( YDESC )
            CALL NCAPTC( FNUM, RID, 'long_name', NCCHAR, L, YDESC( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute LONG_NAME for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( YLONG )
            CALL NCAPTC( FNUM, RID, 'standard_name', NCCHAR, L, YLONG( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating atte STANDARD_NAME for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( YUNIT )
            CALL NCAPTC( FNUM, RID, 'units', NCCHAR, L, YUNIT( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute UNITS for variable ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

            L = LEN_TRIM( MNAME )
            CALL NCAPTC( FNUM, RID, 'grid_mapping', NCCHAR, L, MNAME( 1:L ), IERR )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error creating netCDF attribute "grid_mapping" for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  NCAPTC() failed

        END IF              !!  if GRIDDED file


        !!...........   Put FNUM back into data mode:  attributes and variables now defined.

        CALL NCENDF( FNUM, IERR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error putting netCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  NCENDF() failed


        IF ( CFMETA .AND. FTYPE3(FID) .EQ. GRDDED3 ) THEN

            X0 = XORIG3(FID)
            XC = XCELL3(FID)
            DO C = 1, MIN( NCOLS3(FID), MXDIM )
                DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
            END DO

            DIMS( 1 ) = 1
            DELS( 1 ) = MIN( NCOLS3(FID), MXDIM )

            CALL NCVPT( FNUM, CID, DIMS, DELS, DARGS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "COL"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCENDF() failed

            X0 = YORIG3(FID)
            XC = YCELL3(FID)
            DO C = 1, MIN( NROWS3(FID), MXDIM )
                DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
            END DO

            DIMS( 1 ) = 1
            DELS( 1 ) = MIN( NROWS3(FID), MXDIM )

            CALL NCVPT( FNUM, RID, DIMS, DELS, DARGS, IERR )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "ROW"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  NCENDF() failed

            IF ( NLAYS3(FID) .GT. 1 ) THEN

                DO C = 1, MIN( NLAYS3(FID), MXDIM )
                    DARGS( C ) = 0.5D0 * ( VGLVS3(   L,FID ) + VGLVS3( L+1,FID ) )
                END DO

                DIMS( 1 ) = 1
                DELS( 1 ) = MIN( NLAYS3(FID), MXDIM )

                CALL NCVPT( FNUM, LID, DIMS, DELS, DARGS, IERR )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "LAY"' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !!  ierr nonzero:  NCENDF() failed

            END IF      !!  if nlays3(FID) > 1

        END IF          !!  if cmfeta and gridded...

999     SETCF1 = ( .NOT. EFLAG )
        RETURN

    END FUNCTION  SETCF1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDCF()

        CFMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDCF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION DBLERR( P, Q )

        REAL*8, INTENT(IN   ) :: P, Q

        DBLERR = ( (P - Q)**2  .GT.  1.0E-10*(P*P + Q*Q + 1.0E-5) )

        RETURN

    END FUNCTION DBLERR


END MODULE M3ATTS


MODULE MODATTS3

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !! Version "$Id: modatts3.F90 358 2016-04-22 15:57:53Z coats $"
    !! Copyright (c) 2014-2015 UNC Institute for the Environment
    !! Distributed under the GNU LESSER PUBLIC LICENSE version 2
    !! See file "LGPL.txt" for conditions of use.
    !!...................................................................
    !!  DESCRIPTION:
    !!      Extra-attribute routines and data structures for
    !!      coordinate-transform matrices, extra CMAQ metadata, and
    !!      extra SMOKE metadata.
    !!
    !!  DO NOT EDIT !!!!
    !!
    !!        The EDSS/Models-3 I/O API depends in an essential manner
    !!        upon the contents of this MODULE file.  ANY CHANGES are
    !!        likely to result in very obscure, difficult-to-diagnose
    !!        bugs caused by an inconsistency between standard "libioapi.a"
    !!        object-libraries and whatever code is compiled with the
    !!        resulting modified MODULE-file.
    !!
    !!        By making any changes to this MODULE file, the user
    !!        explicitly agrees that in the case any assistance is
    !!        required of MCNC or of the I/O API author, Carlie J. Coats, Jr.
    !!        THE USER AND/OR HIS PROJECT OR CONTRACT AGREES TO REIMBURSE
    !!        UNC AND/OR THE I/O API AUTHOR, CARLIE J. COATS, JR., AT A
    !!        RATE TRIPLE THE NORMAL CONTRACT RATE FOR THE SERVICES
    !!        REQUIRED.
    !!
    !!  PRECONDITIONS:
    !!      Use these routines after calling INIT3() and before
    !!      calling OPEN3() on files to contain these extra attributes
    !!
    !!  REVISION  HISTORY:
    !!      Adapted 12/2014 by Carlie J. Coats, Jr., from I/O API 3.1
    !!      "matxatts.f" and other sources.
    !!
    !!      Modified 10/2015 by CJC for I/O API 3.2: use NF_*()
    !!      instead of NC*(), for netCDF-Fortran 4.x compatibility;
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

    USE M3UTILIO
    USE MODNCFIO

    IMPLICIT NONE

    !!........  PUBLIC Routines:

    PUBLIC  CMETA_T, SMETA_T,  INITCF, SETCF, ENDCF,                    &
            INITMTXATT, GETMTXATT, SETMTXATT, CHKMTXATT, ENDMTXATT,     &
            INITCMAQ,    GETCMAQ,   LOGCMAQ,  SETCMAQ,   ENDCMAQ,       &
            INITSMOKE,   GETSMOKE,            SETSMOKE,  ENDSMOKE,      &
            INITMTEXT,                        SETMTEXT,  ENDMTEXT


    !!........  Flags for "this type of metadata is active"

    LOGICAL, PUBLIC, PROTECTED, SAVE :: MATXMETA  = .FALSE.
    LOGICAL, PUBLIC, PROTECTED, SAVE :: CMAQMETA  = .FALSE.
    LOGICAL, PUBLIC, PROTECTED, SAVE :: SMOKEMETA = .FALSE.
    LOGICAL, PUBLIC, PROTECTED, SAVE :: CFMETA    = .FALSE.
    LOGICAL, PUBLIC, PROTECTED, SAVE :: TEXTMETA  = .FALSE.


    !!........  Derived types for standard CMAQ, SMOKE metadata

    TYPE CMETA_T
        INTEGER         :: VERSION3
        CHARACTER*32    :: PROJECT
        CHARACTER*32    :: CASE
        CHARACTER*32    :: EPISODE
        CHARACTER*32    :: SPONSOR
        CHARACTER*32    :: CMAQVERS
        CHARACTER*32    :: MCIPVERS
        CHARACTER*32    :: EMISVERS
        CHARACTER*32    :: METVERS
        CHARACTER*256   :: CMAQCFG
        CHARACTER*256   :: MCIPCFG
        CHARACTER*256   :: EMISCFG
        CHARACTER*256   :: ICBCCFG
        CHARACTER*256   :: METCFG
        CHARACTER*8     :: WBDUST_BELD
        REAL            :: SYNC_TOP
        INTEGER         :: MAXSYNC
        INTEGER         :: MINSYNC
        INTEGER         :: WB_DUST          !!  may be 0==FALSE, 1==TRUE, IMISS3="missing"
        INTEGER         :: ERODE_AGLAND     !! ...
        INTEGER         :: LTNG_NO
        INTEGER         :: KZMIN
        INTEGER         :: ILDEPV
        INTEGER         :: MOSAIC
        INTEGER         :: ABFLUX
        INTEGER         :: HGBIDI
        INTEGER         :: SFC_HONO
        INTEGER         :: BIOGEMIS
        INTEGER         :: PT3DEMIS
        INTEGER         :: CLD_DIAG
        INTEGER         :: AERDIAG
        INTEGER         :: PHOTDIAG
        INTEGER         :: SSEMDIAG
        INTEGER         :: DUSTEM_DIAG
        INTEGER         :: LTNGDIAG
        INTEGER         :: B3GTS_DIAG
        INTEGER         :: PT3DDIAG
        INTEGER         :: PT3DFRAC
    END TYPE CMETA_T

    TYPE SMETA_T
        INTEGER :: VERSION
    END TYPE SMETA_T


    !!........  Contents of current  standard CMAQ, SMOKE, TEXT metadata

    TYPE( CMETA_T ), PUBLIC, PROTECTED, SAVE ::  CMAQ_MDATA
    TYPE( SMETA_T ), PUBLIC, PROTECTED, SAVE :: SMOKE_MDATA

    INTEGER, PUBLIC, PARAMETER ::  INGRD3  = 1  !! this is an  input grid-spec for mtx metadata
    INTEGER, PUBLIC, PARAMETER ::  OUTGRD3 = 2  !! this is an output-grid...

    INTEGER,                             PUBLIC, PROTECTED, SAVE :: TEXT_MLINES = 0                            !!  number of lines in TEXT_MDATA( :,: )
    CHARACTER(LEN=MXDLEN3), ALLOCATABLE, PUBLIC, PROTECTED, SAVE :: TEXT_MDATA( : )     !!  (TEXT_MLINES)

    !!........  INTERFACE blocks for generic routines:

    INTERFACE GETMTXATT
        MODULE PROCEDURE  GETMTXATT1, GETMTXATT2
    END INTERFACE

    INTERFACE SETMTXATT
        MODULE PROCEDURE  SETMTXATT1, SETMTXATT2, SETMTXATT3
    END INTERFACE

    INTERFACE CHKMTXATT
        MODULE PROCEDURE  CHKMTXATT1, CHKMTXATT2, CHKMTXATT3
    END INTERFACE

    INTERFACE INITCMAQ
        MODULE PROCEDURE  INITCMAQA, INITCMAQT
    END INTERFACE

    INTERFACE INITMTEXT
        MODULE PROCEDURE  INITMTEXTA, INITMTEXTT
    END INTERFACE

    INTERFACE INITSMOKE
        MODULE PROCEDURE  INITSMOKEA, INITSMOKET
    END INTERFACE

    INTERFACE GETCMAQ
        MODULE PROCEDURE  GETCMAQT, GETCMAQF
    END INTERFACE

    INTERFACE GETSMOKE
        MODULE PROCEDURE  GETSMOKET, GETSMOKEF
    END INTERFACE

    INTERFACE LOGCMAQ
        MODULE PROCEDURE  LOGCMAQ1, LOGCMAQF, LOGCMAQM, LOGCMAQFM
    END INTERFACE

    INTERFACE SETCF
        MODULE PROCEDURE  SETCFF, SETCF1
    END INTERFACE

    INTERFACE SETCMAQ
        MODULE PROCEDURE  SETCMAQT, SETCMAQC, SETCMAQ1, SETCMAQ2
    END INTERFACE

    INTERFACE SETSMOKE
        MODULE PROCEDURE  SETSMOKEA, SETSMOKEC, SETSMOKE1
    END INTERFACE


    PRIVATE     !!  everything else

    CHARACTER*1,  PARAMETER ::  BLANK = ' '
    CHARACTER*64, PARAMETER ::  BAR   = '-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'


    !!........  EXTERNAL FUNCTION:  logical name to I/O API file-ID

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
'$Id:: modatts3.F90 358 2016-04-22 15:57:53Z coats                              $'


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

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETMTXATT'


        !!........  External function:  logical name to I/O API file ID

        INTEGER     NAME2FID
        EXTERNAL    NAME2FID


        !!........  Local Variables:

        INTEGER             FID
        LOGICAL             EFLAG
        CHARACTER*16        ANAME, GNAME
        CHARACTER*256       MESG


        !!........  body  ..............................................

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

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETMTXATT3'

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

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETMTXATT1'

        !!........  Include file:

        INCLUDE 'STATE3.EXT'      !! I/O API state

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

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETMTXATT2'

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

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/CHKMTXATT'


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
    !!      CMAQ Attributes:
    !!      INITCMAQA:  initialize from text-file with one def per line
    !!      INITCMAQT:  initialize from TYPE(CMETA_T) argument
    !!      INITCMAQE:  initialize from environment
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  INITCMAQA( )

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/INITCMAQ'
        
        CHARACTER*8   ANAME

        !!........  body  .........................................

        IF ( CMAQMETA ) THEN
            INITCMAQA = .TRUE.
            RETURN
        END IF

        INITCMAQA = INITCMAQT( CMAQ_MDATA )

        RETURN

    END FUNCTION  INITCMAQA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize MDATA from a text file or environment


    LOGICAL FUNCTION  INITCMAQT( MDATA )

        !!........  Arguments:

        TYPE(CMETA_T) , INTENT( OUT ) :: MDATA

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/INITCMAQ'

        !!........  Local Variables:

        INTEGER         MDEV, ISTAT, K, L
        LOGICAL         EFLAG

        CHARACTER*32    ANAME
        CHARACTER*256   MESG
        CHARACTER*512   LINE          !  scratch buffer to upcase EQNAME in.

        !!........  body  .........................................

        CALL NAMEVAL( 'IOAPI_CMAQMETA', ANAME )     !  get CMAQ meta from environment...
        CALL UPCASE( ANAME )
        IF ( ANAME .EQ. 'ENV' ) THEN
            INITCMAQT = INITCMAQE( MDATA )
            RETURN
        END IF

        MDEV = GETEFILE( 'IOAPI_CMAQMETA', .TRUE., .TRUE., PNAME )
        IF ( MDEV .LT. 0 ) THEN
            CALL M3MESG( 'Could not open CMAQ metadata file "IOAPI_CMAQMETA"' )
            INITCMAQT = .FALSE.
            RETURN
        END IF

        MDATA%VERSION3     = 1
        MDATA%PROJECT      = BLANK
        MDATA%CASE         = BLANK
        MDATA%EPISODE      = BLANK
        MDATA%SPONSOR      = BLANK
        MDATA%CMAQVERS     = BLANK
        MDATA%MCIPVERS     = BLANK
        MDATA%EMISVERS     = BLANK
        MDATA%METVERS      = BLANK
        MDATA%CMAQCFG      = BLANK
        MDATA%MCIPCFG      = BLANK
        MDATA%EMISCFG      = BLANK
        MDATA%ICBCCFG      = BLANK
        MDATA%METCFG       = BLANK
        MDATA%SYNC_TOP     = BADVAL3
        MDATA%MAXSYNC      = IMISS3
        MDATA%MINSYNC      = IMISS3
        MDATA%WB_DUST      = IMISS3
        MDATA%ERODE_AGLAND = IMISS3
        MDATA%WBDUST_BELD  = BLANK
        MDATA%LTNG_NO      = IMISS3
        MDATA%KZMIN        = IMISS3
        MDATA%ILDEPV       = IMISS3
        MDATA%MOSAIC       = IMISS3
        MDATA%ABFLUX       = IMISS3
        MDATA%HGBIDI       = IMISS3
        MDATA%SFC_HONO     = IMISS3
        MDATA%BIOGEMIS     = IMISS3
        MDATA%PT3DEMIS     = IMISS3
        MDATA%CLD_DIAG     = IMISS3
        MDATA%AERDIAG      = IMISS3
        MDATA%PHOTDIAG     = IMISS3
        MDATA%SSEMDIAG     = IMISS3
        MDATA%DUSTEM_DIAG  = IMISS3
        MDATA%LTNGDIAG     = IMISS3
        MDATA%B3GTS_DIAG   = IMISS3
        MDATA%PT3DDIAG     = IMISS3
        MDATA%PT3DFRAC     = IMISS3

        EFLAG = .FALSE.

        DO L = 1, 999999999

            READ( MDEV, '(A)', END=999, IOSTAT=ISTAT ) LINE

            IF ( ISTAT .NE. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I4 )' ) 'Error', ISTAT, 'reading "IOAPI_CMAQMETA" at line', L
                CALL M3MESG( MESG )
                CYCLE
            ELSE IF ( LINE .EQ. BLANK ) THEN
                CYCLE
            ELSE IF ( ISCOMMENT( LINE ) ) THEN
                CYCLE
            END IF

            K = INDEX( LINE, '=' )
            IF ( K .EQ. 0 ) THEN
                EFLAG = .TRUE.
                WRITE( MESG, '( A, I10, 2X, A, I4 )' ) 'No "=" in "IOAPI_CMAQMETA" at line', L
                CALL M3MESG( MESG )
                CYCLE
            END IF
            ANAME = LINE( 1:K-1 )
            CALL UPCASE( ANAME )

            SELECT CASE( ANAME )
                CASE ( 'VERSION3' )
                    MDATA%VERSION3    = STR2INT( LINE( K+1: ) )
                CASE ( 'PROJECT' )
                    MDATA%PROJECT     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'CASE' )
                    MDATA%CASE        = ADJUSTL( LINE( K+1: ) )
                CASE ( 'EPISODE' )
                    MDATA%EPISODE     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'SPONSOR' )
                    MDATA%SPONSOR     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'CMAQVERS' )
                    MDATA%CMAQVERS    = ADJUSTL( LINE( K+1: ) )
                CASE ( 'MCIPVERS' )
                    MDATA%MCIPVERS    = ADJUSTL( LINE( K+1: ) )
                CASE ( 'EMISVERS' )
                    MDATA%EMISVERS    = ADJUSTL( LINE( K+1: ) )
                CASE ( 'METVERS' )
                    MDATA%METVERS     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'CMAQCFG' )
                    MDATA%CMAQCFG     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'MCIPCFG' )
                    MDATA%MCIPCFG     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'EMISCFG' )
                    MDATA%EMISCFG     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'ICBCCFG' )
                    MDATA%ICBCCFG     = ADJUSTL( LINE( K+1: ) )
                CASE ( 'METCFG' )
                    MDATA%METCFG      = ADJUSTL( LINE( K+1: ) )
                CASE ( 'SYNC_TOP' )
                    MDATA%SYNC_TOP    = STR2REAL( LINE( K+1: ) )
                CASE ( 'MAXSYNC' )
                    MDATA%MAXSYNC     = STR2INT( LINE( K+1: ) )
                CASE ( 'MINSYNC' )
                    MDATA%MINSYNC     = STR2INT( LINE( K+1: ) )
                CASE ( 'WB_DUST' )
                    MDATA%WB_DUST     = STR2LOG( LINE( K+1: ) )
                CASE ( 'ERODE_AGLAND' )
                    MDATA%ERODE_AGLAND= STR2LOG( LINE( K+1: ) )
                CASE ( 'WBDUST_BELD' )
                    MDATA%WBDUST_BELD = ADJUSTL( LINE( K+1: ) )
                CASE ( 'LTNG_NO' )
                    MDATA%LTNG_NO     = STR2LOG( LINE( K+1: ) )
                CASE ( 'KZMIN' )
                    MDATA%KZMIN       = STR2LOG( LINE( K+1: ) )
                CASE ( 'ILDEPV' )
                    MDATA%ILDEPV      = STR2LOG( LINE( K+1: ) )
                CASE ( 'MOSAIC' )
                    MDATA%MOSAIC      = STR2LOG( LINE( K+1: ) )
                CASE ( 'ABFLUX' )
                    MDATA%ABFLUX      = STR2LOG( LINE( K+1: ) )
                CASE ( 'HGBIDI' )
                    MDATA%HGBIDI      = STR2LOG( LINE( K+1: ) )
                CASE ( 'SFC_HONO' )
                    MDATA%SFC_HONO    = STR2LOG( LINE( K+1: ) )
                CASE ( 'BIOGEMIS' )
                    MDATA%BIOGEMIS    = STR2LOG( LINE( K+1: ) )
                CASE ( 'PT3DEMIS' )
                    MDATA%PT3DEMIS    = STR2LOG( LINE( K+1: ) )
                CASE ( 'CLD_DIAG' )
                    MDATA%CLD_DIAG    = STR2LOG( LINE( K+1: ) )
                CASE ( 'AERDIAG' )
                    MDATA%AERDIAG     = STR2LOG( LINE( K+1: ) )
                CASE ( 'PHOTDIAG' )
                    MDATA%PHOTDIAG    = STR2LOG( LINE( K+1: ) )
                CASE ( 'SSEMDIAG' )
                    MDATA%SSEMDIAG    = STR2LOG( LINE( K+1: ) )
                CASE ( 'DUSTEM_DIAG')
                    MDATA%DUSTEM_DIAG = STR2LOG( LINE( K+1: ) )
                CASE ( 'LTNGDIAG'   )
                    MDATA%LTNGDIAG    = STR2LOG( LINE( K+1: ) )
                CASE ( 'B3GTS_DIAG' )
                    MDATA%B3GTS_DIAG  = STR2LOG( LINE( K+1: ) )
                CASE ( 'PT3DDIAG'   )
                    MDATA%PT3DDIAG    = STR2LOG( LINE( K+1: ) )
                CASE ( 'PT3DFRAC'   )
                    MDATA%PT3DFRAC    = STR2LOG( LINE( K+1: ) )
                CASE DEFAULT
                    WRITE( MESG, '( 3A, I4 )' ) 'Unrecognized attribute  "', ANAME, '" in "IOAPI_CMAQMETA" at line', L
                    CALL M3MESG( MESG )
            END SELECT

        END DO

999     CONTINUE        !!  EOF-exit from input loop

        CLOSE( MDEV )

        CMAQMETA   = ( .NOT. EFLAG )
        INITCMAQT  = ( .NOT. EFLAG )

        RETURN

    END FUNCTION  INITCMAQT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize MDATA from the environment


    LOGICAL FUNCTION  INITCMAQE( MDATA )

        !!........  Arguments:

        TYPE(CMETA_T) , INTENT( OUT ) :: MDATA

        INTEGER         ISTAT
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!........  body  .........................................

        MDATA%VERSION3     = 1
        MDATA%PROJECT      = BLANK
        MDATA%CASE         = BLANK
        MDATA%EPISODE      = BLANK
        MDATA%SPONSOR      = BLANK
        MDATA%CMAQVERS     = BLANK
        MDATA%MCIPVERS     = BLANK
        MDATA%EMISVERS     = BLANK
        MDATA%METVERS      = BLANK
        MDATA%CMAQCFG      = BLANK
        MDATA%MCIPCFG      = BLANK
        MDATA%EMISCFG      = BLANK
        MDATA%ICBCCFG      = BLANK
        MDATA%METCFG       = BLANK
        MDATA%SYNC_TOP     = BADVAL3
        MDATA%MAXSYNC      = IMISS3
        MDATA%MINSYNC      = IMISS3
        MDATA%WB_DUST      = IMISS3
        MDATA%ERODE_AGLAND = IMISS3
        MDATA%WBDUST_BELD  = BLANK
        MDATA%LTNG_NO      = IMISS3
        MDATA%KZMIN        = IMISS3
        MDATA%ILDEPV       = IMISS3
        MDATA%MOSAIC       = IMISS3
        MDATA%ABFLUX       = IMISS3
        MDATA%HGBIDI       = IMISS3
        MDATA%SFC_HONO     = IMISS3
        MDATA%BIOGEMIS     = IMISS3
        MDATA%PT3DEMIS     = IMISS3
        MDATA%CLD_DIAG     = IMISS3
        MDATA%AERDIAG      = IMISS3
        MDATA%PHOTDIAG     = IMISS3
        MDATA%SSEMDIAG     = IMISS3
        MDATA%DUSTEM_DIAG  = IMISS3
        MDATA%LTNGDIAG     = IMISS3
        MDATA%B3GTS_DIAG   = IMISS3
        MDATA%PT3DDIAG     = IMISS3
        MDATA%PT3DFRAC     = IMISS3

        EFLAG = .FALSE.

        MDATA%VERSION3    = 1
        CALL ENVSTR( 'PROJECT'  , 'Project name', BLANK, MDATA%PROJECT , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "PROJECT"' )
        END IF

        CALL ENVSTR( 'CASE'     , 'Case name'   , BLANK, MDATA%CASE ,     ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "CASE"' )
        END IF

        CALL ENVSTR( 'EPISODE'  , 'Episode'     , BLANK, MDATA%EPISODE ,  ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "EPISODE"' )
        END IF

        CALL ENVSTR( 'CMAQVERS' , 'CMAQ Version', BLANK, MDATA%CMAQVERS , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "CMAQVERS"' )
        END IF

        CALL ENVSTR( 'MCIPVERS' , 'MCIP Version' , BLANK, MDATA%MCIPVERS , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "MCIPVERS"' )
        END IF

        CALL ENVSTR( 'EMISVERS' , 'Emissions model and version', BLANK, MDATA%EMISVERS , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "EMISVERS"' )
        END IF

        CALL ENVSTR( 'METVERS'  , 'Met model and version', BLANK, MDATA%METVERS ,  ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "METVERS"' )
        END IF

        CALL ENVSTR( 'CMAQCFG'  , 'CMAQ configuration', BLANK, MDATA%CMAQCFG ,  ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "CMAQCFG"' )
        END IF

        CALL ENVSTR( 'MCIPCFG'  , 'MCIP configuration', BLANK, MDATA%MCIPCFG ,  ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "MCIPCFG"' )
        END IF

        CALL ENVSTR( 'EMISCFG'  , 'Emissions configuration'    , BLANK, MDATA%EMISCFG , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "EMISCFG"' )
        END IF

        CALL ENVSTR( 'ICBCCFG'  , 'Emissions configuration'    , BLANK, MDATA%ICBCCFG , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "ICBCCFG"' )
        END IF

        CALL ENVSTR( 'METCFG'   , 'Met-model configuration'    , BLANK, MDATA%METCFG , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "METCFG"' )
        END IF

        CALL ENVSTR( 'CTM_WBDUST_BELD', 'landuse database for identifying dust source regions', BLANK, MDATA%WBDUST_BELD , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "WBDUST_BELD"' )
        END IF

        MDATA%SYNC_TOP = ENVREAL( 'SIGMA_SYNC_TOP'  , 'Synch top (sigma)', BADVAL3, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "SIGMA_SYNC_TOP"' )
        END IF

        MDATA%MAXSYNC =  ENVINT( 'CTM_MAXSYNC' , 'Max synch time (sec)' , IMISS3 , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "CTM_MAXSYNC"' )
        END IF

        MDATA%MINSYNC =  ENVINT( 'CTM_MINSYNC' , 'Min synch time (sec)' , IMISS3 , ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            EFLAG = .TRUE.
            CALL M3MESG( 'INITCMAQ:  Bad environment variable "CTM_MINSYNC"' )
        END IF

        MDATA%WB_DUST      = ENVLOG( 'CTM_WB_DUST'     , 'Flag: use in-line wind-borne dust computation?', EFLAG )

        MDATA%ERODE_AGLAND = ENVLOG( 'CTM_ERODE_AGLAND', 'Flag: use in-line wind-borne dust computation?'   , EFLAG )

        MDATA%LTNG_NO      = ENVLOG( 'CTM_LTNG_NO'     , 'Flag: in-line lightning NOx computation?', EFLAG )

        MDATA%KZMIN        = ENVLOG( 'KZMIN'           , 'Flag: use Min Kz option in edyintb?', EFLAG )

        MDATA%ILDEPV       = ENVLOG( 'CTM_ILDEPV'      , 'Flag: in-line deposition velocities?', EFLAG )

        MDATA%MOSAIC       = ENVLOG( 'CTM_MOSAIC'      , 'Flag: use landuse specific deposition velocities?', EFLAG )

        MDATA%ABFLUX       = ENVLOG( 'CTM_ABFLUX'      , 'Flag: Use Ammonia bi-di flux for in-line dep vel?', EFLAG )

        MDATA%HGBIDI       = ENVLOG( 'CTM_HGBIDI'      , 'Flag: Use Mercury bi-di flux for in-line dep vel?', EFLAG )

        MDATA%SFC_HONO     = ENVLOG( 'CTM_SFC_HONO'    , 'Flag: Use Surface HONO interaction for in-line dep vel?', EFLAG )

        MDATA%BIOGEMIS     = ENVLOG( 'CTM_BIOGEMIS'    , 'Flag: in-line biogenic emissions?', EFLAG )

        MDATA%PT3DEMIS     = ENVLOG( 'CTM_PT3DEMIS'    , 'Flag: in-line PT-source plume rise?', EFLAG )

        MDATA%CLD_DIAG     = ENVLOG( 'CLD_DIAG'        , 'Flag: write cloud diagnostics?'  , EFLAG )

        MDATA%AERDIAG      = ENVLOG( 'CTM_AERDIAG'     , 'Flag: write aerosol diagnostics?', EFLAG )

        MDATA%PHOTDIAG     = ENVLOG( 'CTM_PHOTDIAG'    , 'Flag: write PHOT diagnostics?', EFLAG )

        MDATA%SSEMDIAG     = ENVLOG( 'CTM_SSEMDIAG'    , 'Flag: write sea-salt diagnostics?', EFLAG )

        MDATA%DUSTEM_DIAG  = ENVLOG( 'CTM_DUSTEM_DIAG' , 'Flag: write windblown dust emissions diagnostic file?', EFLAG )

        MDATA%LTNGDIAG     = ENVLOG( 'LTNGDIAG'        , 'Flag: write lightning-NOX diagnostics?', EFLAG )

        MDATA%B3GTS_DIAG   = ENVLOG( 'B3GTS_DIAG'      , 'Flag: write beis mass emissions diagnostic file?', EFLAG )

        MDATA%PT3DDIAG     = ENVLOG( 'PT3DDIAG'        , 'Flag: write 3d point source emissions diagnostic file?', EFLAG )

        MDATA%PT3DFRAC     = ENVLOG( 'PT3DFRAC'        , 'Flag: write layer fractions diagnostic?', EFLAG )

        CMAQMETA   = ( .NOT. EFLAG )
        INITCMAQE  = ( .NOT. EFLAG )

        RETURN

    END FUNCTION  INITCMAQE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize CMAQ_MDATA from FNAME file-header


    LOGICAL FUNCTION GETCMAQF( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME

        !!........  body  .........................................

        IF (  GETCMAQT( FNAME, CMAQ_MDATA ) ) THEN
            CMAQMETA = .TRUE.
            GETCMAQF = .TRUE.
        ELSE
            GETCMAQF = .FALSE.
        END IF
        RETURN

    END FUNCTION  GETCMAQF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize MDATA from FNAME file-header


    LOGICAL FUNCTION GETCMAQT( FNAME, MDATA )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        TYPE( CMETA_T )   , INTENT(  OUT) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETCMAQ'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETCMAQT = .FALSE.
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETCMAQT = .FALSE.
        ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
            GETCMAQT = PN_GETCMAQ( FID, MDATA )
        ELSE
            GETCMAQT = GETCMAQ1( FID, MDATA )
        END IF

        RETURN

    END FUNCTION  GETCMAQT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize MDATA from FLIST3(FNUM) file-header


    LOGICAL FUNCTION GETCMAQ1( FNUM, MDATA )

        !!........  Arguments:

        INTEGER        , INTENT(IN   ) :: FNUM       !! I/O API state-subscript
        TYPE( CMETA_T ), INTENT(  OUT) :: MDATA

        !!........  Include files:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETCMAQ'

        !!........  Local Variables:

        INTEGER         IERR
        INTEGER         FID         !!  CDFID3(FNUM)
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = CDFID3( FNUM )
        IF ( FID .LT. 0 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not [P]netCDF' )
            GETCMAQ1 = .FALSE.
            RETURN
        ELSE IF ( FTYPE3( FNUM ) .EQ. MPIGRD3 ) THEN
            GETCMAQ1 = PN_GETCMAQ( FNUM, MDATA )
            RETURN
        END IF


        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'VERSION3', MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "VERSION3" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' PROJECT', MDATA%PROJECT )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PROJECT" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' CASE', MDATA%CASE )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CASE" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' EPISODE', MDATA%EPISODE )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EPISODE" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' SPONSOR', MDATA%SPONSOR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SPONSOR" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' CMAQVERS', MDATA%CMAQVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CMAQVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' MCIPVERS', MDATA%MCIPVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MCIPVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' EMISVERS', MDATA%EMISVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EMISVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' METVERS', MDATA%METVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "METVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' CMAQCFG', MDATA%CMAQCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CMAQCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' MCIPCFG', MDATA%MCIPCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MCIPCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' EMISCFG', MDATA%EMISCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EMISCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' ICBCCFG', MDATA%ICBCCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ICBCCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, ' METCFG', MDATA%METCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "METCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_REAL( FID, NF_GLOBAL, 'SYNC_TOP', MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SYNC_TOP" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'MAXSYNC', MDATA%MAXSYNC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MAXSYNC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'MINSYNC', MDATA%MINSYNC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MINSYNC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'WB_DUST', MDATA%WB_DUST )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "WB_DUST" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'ERODE_AGLAND', MDATA%ERODE_AGLAND )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ERODE_AGLAND" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_TEXT( FID, NF_GLOBAL, 'WBDUST_BELD', MDATA%WBDUST_BELD )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "WBDUST_BELD" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'LTNG_NO', MDATA%LTNG_NO )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "LTNG_NO" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'KZMIN', MDATA%KZMIN )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "KZMIN" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'ILDEPV', MDATA%ILDEPV )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ILDEPV" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'MOSAIC', MDATA%MOSAIC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MOSAIC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'ABFLUX', MDATA%ABFLUX )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ABFLUX" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'HGBIDI', MDATA%HGBIDI )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "HGBIDI" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'SFC_HONO', MDATA%SFC_HONO )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SFC_HONO" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'BIOGEMIS', MDATA%BIOGEMIS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "BIOGEMIS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DEMIS', MDATA%PT3DEMIS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DEMIS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'CLD_DIAG', MDATA%CLD_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CLD_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'AERDIAG', MDATA%AERDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "AERDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'PHOTDIAG', MDATA%PHOTDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PHOTDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'SSEMDIAG', MDATA%SSEMDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SSEMDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'DUSTEM_DIAG', MDATA%DUSTEM_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "DUSTEM_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'LTNGDIAG', MDATA%LTNGDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "LTNGDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'B3GTS_DIAG', MDATA%B3GTS_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "B3GTS_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DDIAG', MDATA%PT3DDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DFRAC', MDATA%PT3DFRAC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DFRAC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Errors putting CMAQ metadata to file "' // TRIM(FLIST3(FNUM)) // '"' )
        END IF          !  ierr nonzero:  operation failed

        GETCMAQ1 = ( .NOT. EFLAG )
        CMAQMETA = ( .NOT. EFLAG )
        RETURN

    END FUNCTION  GETCMAQ1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Initialize MDATA from FLIST3(FNUM) file-header


    LOGICAL FUNCTION PN_GETCMAQ( FNUM, MDATA )

        !!***********************************************************************
        !! Version "$Id: modatts3.F90 358 2016-04-22 15:57:53Z coats $"
        !! EDSS/Models-3 I/O API.
        !! Copyright (C) 2014-2015 UNC Institute for the Environment.
        !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!.........................................................................
        !!  function body starts at line  98
        !!
        !!  FUNCTION:
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       File FLIST3( FID ) already exists.
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!
        !!  REVISION  HISTORY:
        !!      Adapted 8/2015  by CJC from I/O API-3.2 "modatts3.f90" subroutine
        !!      GETCF1()
        !!***********************************************************************

        USE MODPDATA
        USE MODNCFIO

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FNUM       !! I/O API state-subscript
        TYPE( CMETA_T ), INTENT( IN ) :: MDATA

        !!........  Parameters:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/PN_GETCMAQ'


#ifdef  IOAPI_PNCF

        !!........  Include files:

        INCLUDE 'mpif.h'
        INCLUDE 'STATE3.EXT'

        !!........  Local Variables:

        INTEGER         IERR
        INTEGER         FID         !!  CDFID3(FNUM)
        LOGICAL         EFLAG
        CHARACTER*256   MESG


        !!........  body  .........................................

        FID = CDFID3( FNUM )
        IF ( FID .LT. 0 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not [P]netCDF' )
            PN_GETCMAQ = .FALSE.
            RETURN
        ELSE IF ( FTYPE3( FNUM ) .NE. MPIGRD3 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not PnetCDF' )
            PN_GETCMAQ = .FALSE.
            RETURN
        END IF


        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'VERSION3', MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "VERSION3" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' PROJECT', MDATA%PROJECT )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PROJECT" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' CASE', MDATA%CASE )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CASE" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' EPISODE', MDATA%EPISODE )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EPISODE" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' SPONSOR', MDATA%SPONSOR )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SPONSOR" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' CMAQVERS', MDATA%CMAQVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CMAQVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' MCIPVERS', MDATA%MCIPVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MCIPVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' EMISVERS', MDATA%EMISVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EMISVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' METVERS', MDATA%METVERS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "METVERS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' CMAQCFG', MDATA%CMAQCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CMAQCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' MCIPCFG', MDATA%MCIPCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MCIPCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' EMISCFG', MDATA%EMISCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "EMISCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' ICBCCFG', MDATA%ICBCCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ICBCCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, ' METCFG', MDATA%METCFG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "METCFG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_REAL( FID, NF_GLOBAL, 'SYNC_TOP', MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SYNC_TOP" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'MAXSYNC', MDATA%MAXSYNC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MAXSYNC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'MINSYNC', MDATA%MINSYNC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MINSYNC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'WB_DUST', MDATA%WB_DUST )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "WB_DUST" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'ERODE_AGLAND', MDATA%ERODE_AGLAND )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ERODE_AGLAND" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_TEXT( FID, NF_GLOBAL, 'WBDUST_BELD', MDATA%WBDUST_BELD )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "WBDUST_BELD" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'LTNG_NO', MDATA%LTNG_NO )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "LTNG_NO" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'KZMIN', MDATA%KZMIN )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "KZMIN" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'ILDEPV', MDATA%ILDEPV )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ILDEPV" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'MOSAIC', MDATA%MOSAIC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "MOSAIC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'ABFLUX', MDATA%ABFLUX )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "ABFLUX" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'HGBIDI', MDATA%HGBIDI )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "HGBIDI" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'SFC_HONO', MDATA%SFC_HONO )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SFC_HONO" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'BIOGEMIS', MDATA%BIOGEMIS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "BIOGEMIS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DEMIS', MDATA%PT3DEMIS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DEMIS" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'CLD_DIAG', MDATA%CLD_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "CLD_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'AERDIAG', MDATA%AERDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "AERDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'PHOTDIAG', MDATA%PHOTDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PHOTDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'SSEMDIAG', MDATA%SSEMDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "SSEMDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'DUSTEM_DIAG', MDATA%DUSTEM_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "DUSTEM_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'LTNGDIAG', MDATA%LTNGDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "LTNGDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'B3GTS_DIAG', MDATA%B3GTS_DIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "B3GTS_DIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DDIAG', MDATA%PT3DDIAG )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DDIAG" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_GET_ATT_INT( FID, NF_GLOBAL, 'PT3DFRAC', MDATA%PT3DFRAC )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error getting attribute "PT3DFRAC" from "' // TRIM(FLIST3(FNUM)) // '"' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Errors getting CMAQ metadata to file "' // TRIM(FLIST3(FNUM)) // '"' )
        END IF          !  ierr nonzero:  operation failed

        PN_GETCMAQ = ( .NOT.EFLAG )
        CMAQMETA   = ( .NOT. EFLAG )

#endif
#ifndef IOAPI_PNCF

        CALL M3WARN( PNAME, 0, 0, 'Error:  PnetCDF Mode not active.' )
        PN_GETCMAQ = .FALSE.

#endif

        RETURN

    END FUNCTION  PN_GETCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Write formatted CMAQ_MDATA metadata to program log


    LOGICAL FUNCTION LOGCMAQ1( )

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  body  .........................................

        LOGCMAQ1 = LOGCMAQDM( LOGDEV, CMAQ_MDATA )

        RETURN

    END FUNCTION  LOGCMAQ1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Write formatted MDATA metadata to program log


    LOGICAL FUNCTION LOGCMAQM( MDATA )

        !!........  Arguments:

        TYPE( CMETA_T ), INTENT( IN ) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  body  .........................................

        LOGCMAQM = LOGCMAQDM( LOGDEV, MDATA )

        RETURN

    END FUNCTION  LOGCMAQM


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Write formatted CMAQ_MDATA metadata to FNAME


    LOGICAL FUNCTION LOGCMAQF( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  body  .........................................

        LOGCMAQF = LOGCMAQFM( FNAME, CMAQ_MDATA )

        RETURN

    END FUNCTION  LOGCMAQF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Write formatted MDATA metadata to FNAME


    LOGICAL FUNCTION LOGCMAQFM( FNAME, MDATA )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME
        TYPE( CMETA_T )   , INTENT( IN ) :: MDATA

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/LOGCMAQ'

        !!........  Local Variables:

        INTEGER         FDEV

        !!........  body  .........................................

        IF ( .NOT. CMAQMETA ) THEN
            CALL M3WARN( PNAME, 0,0, 'Error:  CMAQ metadata not active' )
            LOGCMAQFM = .FALSE.
            RETURN
        END IF

        FDEV = GETEFILE( FNAME, .FALSE., .TRUE., PNAME )

        IF ( FDEV .LT. 0 ) THEN
            CALL M3WARN( PNAME, 0,0, 'Error:  could not open "' // TRIM( FNAME ) // '"' )
            LOGCMAQFM = .FALSE.
        ELSE
            LOGCMAQFM = LOGCMAQDM( FDEV, MDATA )
        END IF

        RETURN

    END FUNCTION  LOGCMAQFM


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Write formatted MDATA metadata to unit FDEV


    LOGICAL FUNCTION LOGCMAQDM( FDEV, MDATA )

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FDEV
        TYPE( CMETA_T ), INTENT( IN ) :: MDATA

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/LOGCMAQ'

        !!........  Local Variables:

        CHARACTER*512   MESG

        !!........  body  .........................................

        IF ( .NOT. CMAQMETA ) THEN
            CALL M3WARN( PNAME, 0,0, 'Error:  CMAQ metadata not active' )
            LOGCMAQDM = .FALSE.
            RETURN
        END IF

        CALL FILEMESG( FDEV, BAR )

        CALL FILEMESG( FDEV, 'CMAQ Metadata....................................' )

        WRITE( MESG, '(A16, "= ", I5 )' ) 'VERSION3', MDATA%VERSION3
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'PROJECT', MDATA%PROJECT
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'CASE', MDATA%CASE
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'EPISODE', MDATA%EPISODE
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'SPONSOR', MDATA%SPONSOR
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'CMAQVERS', MDATA%CMAQVERS
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'MCIPVERS', MDATA%MCIPVERS
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'EMISVERS', MDATA%EMISVERS
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'METVERS', MDATA%METVERS
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'CMAQCFG', MDATA%CMAQCFG
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'MCIPCFG', MDATA%MCIPCFG
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'EMISCFG', MDATA%EMISCFG
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'ICBCCFG', MDATA%ICBCCFG
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'METCFG', MDATA%METCFG
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", 1PE14.6 )' ) 'SYNC_TOP', MDATA%SYNC_TOP
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", I5 )' ) 'MAXSYNC', MDATA%MAXSYNC
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", I5 )' ) 'MINSYNC', MDATA%MINSYNC
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'WB_DUST', LOGVAL( MDATA%WB_DUST )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'ERODE_AGLAND', LOGVAL( MDATA%ERODE_AGLAND )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'WBDUST_BELD', MDATA%WBDUST_BELD
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'LTNG_NO', LOGVAL( MDATA%LTNG_NO )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'KZMIN', LOGVAL( MDATA%KZMIN )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'ILDEPV', LOGVAL( MDATA%ILDEPV )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'MOSAIC', LOGVAL( MDATA%MOSAIC )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'ABFLUX', LOGVAL( MDATA%ABFLUX )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'HGBIDI', LOGVAL( MDATA%HGBIDI )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'SFC_HONO', LOGVAL( MDATA%SFC_HONO )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'BIOGEMIS', LOGVAL( MDATA%BIOGEMIS )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'PT3DEMIS', LOGVAL( MDATA%PT3DEMIS )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'CLD_DIAG', LOGVAL( MDATA%CLD_DIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'AERDIAG', LOGVAL( MDATA%AERDIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'PHOTDIAG', LOGVAL( MDATA%PHOTDIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'SSEMDIAG', LOGVAL( MDATA%SSEMDIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'DUSTEM_DIAG', LOGVAL( MDATA%DUSTEM_DIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'LTNGDIAG', LOGVAL( MDATA%LTNGDIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'B3GTS_DIAG', LOGVAL( MDATA%B3GTS_DIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'PT3DDIAG', LOGVAL( MDATA%PT3DDIAG )
        CALL FILEMESG( FDEV, MESG )

        WRITE( MESG, '(A16, "= ", A )' ) 'PT3DFRAC', LOGVAL( MDATA%PT3DFRAC )
        CALL FILEMESG( FDEV, MESG )

        CALL FILEMESG( FDEV, BAR )

        LOGCMAQDM = .TRUE.
        RETURN

    END FUNCTION  LOGCMAQDM


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Put CMAQ metadata to FNAME from CMAQ_MDATA


    LOGICAL FUNCTION SETCMAQC( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCMAQ'

        !!........  Local Variables:

        !!........  body  .........................................

        IF ( .NOT. CMAQMETA ) THEN
            CALL M3WARN( PNAME, 0,0, 'Error:  CMAQ metadata not active' )
            SETCMAQC = .FALSE.
        ELSE
            SETCMAQC = SETCMAQT( FNAME, CMAQ_MDATA )
        END IF

        RETURN

    END FUNCTION  SETCMAQC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Put CMAQ metadata to FNAME from MDATA


    LOGICAL FUNCTION SETCMAQT( FNAME, MDATA )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME
        TYPE( CMETA_T )   , INTENT( IN ) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCMAQT'

        !!........  Local Variables:

        INTEGER         FNUM
        CHARACTER*256   MESG

        !!........  body  .........................................

        FNUM = NAME2FID( FNAME )

        IF ( FNUM .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCMAQT = .FALSE.
        ELSE IF ( CDFID3( FNUM ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FNUM) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCMAQT = .FALSE.
        ELSE
            SETCMAQT = SETCMAQ2( FNUM, MDATA )
        END IF

        RETURN

    END FUNCTION  SETCMAQT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Put CMAQ metadata to FLIST3(FNUM) from CMAQ_MDATA


    LOGICAL FUNCTION SETCMAQ1( FNUM )

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FNUM        !!  STATE3 subscript for file

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCMAQ1'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................


        IF ( .NOT. CMAQMETA ) THEN
            CALL M3WARN( PNAME, 0,0, 'Error:  CMAQ metadata not active' )
            SETCMAQ1 = .FALSE.
        ELSE
            SETCMAQ1 = SETCMAQ2( FNUM, CMAQ_MDATA )
        END IF

        RETURN

    END FUNCTION  SETCMAQ1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Put CMAQ metadata to FLIST3(FNUM) from MDATA


    LOGICAL FUNCTION SETCMAQ2( FNUM, MDATA )

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FNUM       !! I/O API state-subscript
        TYPE( CMETA_T ), INTENT( IN ) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCMAQ1'

        !!........  Local Variables:

        INTEGER         IERR
        INTEGER         FID         !!  CDFID3(FNUM)
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = CDFID3( FNUM )
        IF ( FID .LT. 0 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not [P]netCDF' )
            SETCMAQ2 = .FALSE.
            RETURN
        ELSE IF ( FTYPE3( FNUM ) .EQ. MPIGRD3 ) THEN
            SETCMAQ2 = PN_SETCMAQ( FNUM, MDATA )
            RETURN
        END IF

        IERR = NF_REDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error putting file "' // TRIM(FLIST3(FNUM)) // '" into define mode.' )
            SETCMAQ2 = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        EFLAG = .FALSE.

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'VERSION3', NF_INT, 1, CMAQ_MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "VERSION3" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'PROJECT', LEN_TRIM( CMAQ_MDATA%PROJECT ), CMAQ_MDATA%PROJECT )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PROJECT" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CASE', LEN_TRIM( CMAQ_MDATA%CASE ), CMAQ_MDATA%CASE )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CASE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EPISODE', LEN_TRIM( CMAQ_MDATA%EPISODE ), CMAQ_MDATA%EPISODE )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EPISODE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'SPONSOR', LEN_TRIM( CMAQ_MDATA%SPONSOR ), CMAQ_MDATA%SPONSOR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SPONSOR" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CMAQVERS', LEN_TRIM( CMAQ_MDATA%CMAQVERS ), CMAQ_MDATA%CMAQVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CMAQVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'MCIPVERS', LEN_TRIM( CMAQ_MDATA%MCIPVERS ), CMAQ_MDATA%MCIPVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MCIPVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EMISVERS', LEN_TRIM( CMAQ_MDATA%EMISVERS ), CMAQ_MDATA%EMISVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EMISVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'METVERS', LEN_TRIM( CMAQ_MDATA%METVERS ), CMAQ_MDATA%METVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "METVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CMAQCFG', LEN_TRIM( CMAQ_MDATA%CMAQCFG ), CMAQ_MDATA%CMAQCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CMAQCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'MCIPCFG', LEN_TRIM( CMAQ_MDATA%MCIPCFG ), CMAQ_MDATA%MCIPCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MCIPCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EMISCFG', LEN_TRIM( CMAQ_MDATA%EMISCFG ), CMAQ_MDATA%EMISCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EMISCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'ICBCCFG', LEN_TRIM( CMAQ_MDATA%ICBCCFG ), CMAQ_MDATA%ICBCCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ICBCCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'METCFG', LEN_TRIM( CMAQ_MDATA%METCFG ), CMAQ_MDATA%METCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "METCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_REAL( FID, NF_GLOBAL, 'SYNC_TOP', NF_FLOAT, 1, CMAQ_MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SYNC_TOP" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'MAXSYNC', NF_INT, 1, CMAQ_MDATA%MAXSYNC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CASE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'MINSYNC', NF_INT, 1, CMAQ_MDATA%MINSYNC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MINSYNC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'WB_DUST', NF_INT, 1, CMAQ_MDATA%WB_DUST )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "WB_DUST" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'ERODE_AGLAND', NF_INT, 1, CMAQ_MDATA%ERODE_AGLAND )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ERODE_AGLAND" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'WBDUST_BELD', LEN_TRIM( CMAQ_MDATA%WBDUST_BELD ), CMAQ_MDATA%WBDUST_BELD )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "WBDUST_BELD" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'LTNG_NO', NF_INT, 1, CMAQ_MDATA%LTNG_NO )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "LTNG_NO" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'KZMIN', NF_INT, 1, CMAQ_MDATA%KZMIN )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "KZMIN" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'ILDEPV', NF_INT, 1, CMAQ_MDATA%ILDEPV )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ILDEPV" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'MOSAIC', NF_INT, 1, CMAQ_MDATA%MOSAIC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MOSAIC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'ABFLUX', NF_INT, 1, CMAQ_MDATA%ABFLUX )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ABFLUX" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'HGBIDI', NF_INT, 1, CMAQ_MDATA%HGBIDI )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "HGBIDI" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'SFC_HONO', NF_INT, 1, CMAQ_MDATA%SFC_HONO )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SFC_HONO" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'BIOGEMIS', NF_INT, 1, CMAQ_MDATA%BIOGEMIS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "BIOGEMIS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DEMIS', NF_INT, 1, CMAQ_MDATA%PT3DEMIS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DEMIS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'CLD_DIAG', NF_INT, 1, CMAQ_MDATA%CLD_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CLD_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'AERDIAG', NF_INT, 1, CMAQ_MDATA%AERDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "AERDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'PHOTDIAG', NF_INT, 1, CMAQ_MDATA%PHOTDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PHOTDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'SSEMDIAG', NF_INT, 1, CMAQ_MDATA%SSEMDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SSEMDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'DUSTEM_DIAG', NF_INT, 1, CMAQ_MDATA%DUSTEM_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "DUSTEM_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'LTNGDIAG', NF_INT, 1, CMAQ_MDATA%LTNGDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "LTNGDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'B3GTS_DIAG', NF_INT, 1, CMAQ_MDATA%B3GTS_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "B3GTS_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DDIAG', NF_INT, 1, CMAQ_MDATA%PT3DDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NF_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DFRAC', NF_INT, 1, CMAQ_MDATA%PT3DFRAC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DFRAC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Errors putting CMAQ metadata to file "' // TRIM(FLIST3(FNUM)) // '"' )
            SETCMAQ2 = .FALSE.
            RETURN
        END IF          !  if error happened...

        IERR = NF_ENDDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error putting file "' // TRIM(FLIST3(FNUM)) // '" into data mode.' )
        END IF          !  ierr nonzero:  operation failed

        SETCMAQ2 = ( .NOT.EFLAG )

        RETURN

    END FUNCTION  SETCMAQ2


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Put CMAQ metadata to MPIGRD3 file FLIST3(FNUM) from MDATA


    LOGICAL FUNCTION PN_SETCMAQ( FNUM, MDATA )

        !!***********************************************************************
        !! Version "$Id: modatts3.F90 358 2016-04-22 15:57:53Z coats $"
        !! EDSS/Models-3 I/O API.
        !! Copyright (C) 2014-2015 UNC Institute for the Environment.
        !! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
        !! See file "LGPL.txt" for conditions of use.
        !!.........................................................................
        !!  function body starts at line  98
        !!
        !!  FUNCTION:
        !!
        !!  PRECONDITIONS REQUIRED:
        !!       File FLIST3( FID ) already exists.
        !!
        !!  SUBROUTINES AND FUNCTIONS CALLED:
        !!
        !!  REVISION  HISTORY:
        !!      Adapted 8/2015  by CJC from I/O API-3.2 "modatts3.f90" subroutine
        !!      SETCF1()
        !!***********************************************************************

        USE MODPDATA
        USE MODNCFIO

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FNUM
        TYPE( CMETA_T ), INTENT( IN ) :: MDATA

        !!........  Parameters:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/PN_SETCMAQ'


#ifdef  IOAPI_PNCF

        !!........  Include files:

        INCLUDE 'mpif.h'
        INCLUDE 'STATE3.EXT'

        !!........  Local Variables:

        INTEGER         IERR
        INTEGER         FID         !!  CDFID3(FNUM)
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = CDFID3( FNUM )
        IF ( FID .LT. 0 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not [P]netCDF' )
            PN_SETCMAQ = .FALSE.
            RETURN
        ELSE IF ( FTYPE3( FNUM ) .NE. MPIGRD3 ) THEN
            CALL M3MESG( 'Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not PnetCDF' )
            PN_SETCMAQ = .FALSE.
            RETURN
        END IF


        IERR = NFMPI_REDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error putting file "' // TRIM(FLIST3(FNUM)) // '" into define mode.' )
            PN_SETCMAQ = .FALSE.
            RETURN
        END IF          !  ierr nonzero:  operation failed

        EFLAG = .FALSE.

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'VERSION3', NF_INT, 1, CMAQ_MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "VERSION3" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'PROJECT', LEN( CMAQ_MDATA%PROJECT ), CMAQ_MDATA%PROJECT )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PROJECT" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CASE', LEN( CMAQ_MDATA%CASE ), CMAQ_MDATA%CASE )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CASE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EPISODE', LEN( CMAQ_MDATA%EPISODE ), CMAQ_MDATA%EPISODE )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EPISODE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'SPONSOR', LEN( CMAQ_MDATA%SPONSOR ), CMAQ_MDATA%SPONSOR )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SPONSOR" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CMAQVERS', LEN( CMAQ_MDATA%CMAQVERS ), CMAQ_MDATA%CMAQVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CMAQVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'MCIPVERS', LEN( CMAQ_MDATA%MCIPVERS ), CMAQ_MDATA%MCIPVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MCIPVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EMISVERS', LEN( CMAQ_MDATA%EMISVERS ), CMAQ_MDATA%EMISVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EMISVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'METVERS', LEN( CMAQ_MDATA%METVERS ), CMAQ_MDATA%METVERS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "METVERS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'CMAQCFG', LEN( CMAQ_MDATA%CMAQCFG ), CMAQ_MDATA%CMAQCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CMAQCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'MCIPCFG', LEN( CMAQ_MDATA%MCIPCFG ), CMAQ_MDATA%MCIPCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MCIPCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'EMISCFG', LEN( CMAQ_MDATA%EMISCFG ), CMAQ_MDATA%EMISCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "EMISCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'ICBCCFG', LEN( CMAQ_MDATA%ICBCCFG ), CMAQ_MDATA%ICBCCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ICBCCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'METCFG', LEN( CMAQ_MDATA%METCFG ), CMAQ_MDATA%METCFG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "METCFG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_REAL( FID, NF_GLOBAL, 'SYNC_TOP', NF_FLOAT, 1, CMAQ_MDATA%VERSION3 )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SYNC_TOP" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'MAXSYNC', NF_INT, 1, CMAQ_MDATA%MAXSYNC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CASE" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'MINSYNC', NF_INT, 1, CMAQ_MDATA%MINSYNC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MINSYNC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'WB_DUST', NF_INT, 1, CMAQ_MDATA%WB_DUST )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "WB_DUST" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'ERODE_AGLAND', NF_INT, 1, CMAQ_MDATA%ERODE_AGLAND )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ERODE_AGLAND" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'WBDUST_BELD', LEN( CMAQ_MDATA%WBDUST_BELD ), CMAQ_MDATA%WBDUST_BELD )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "WBDUST_BELD" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'LTNG_NO', NF_INT, 1, CMAQ_MDATA%LTNG_NO )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "LTNG_NO" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'KZMIN', NF_INT, 1, CMAQ_MDATA%KZMIN )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "KZMIN" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'ILDEPV', NF_INT, 1, CMAQ_MDATA%ILDEPV )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ILDEPV" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'MOSAIC', NF_INT, 1, CMAQ_MDATA%MOSAIC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "MOSAIC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'ABFLUX', NF_INT, 1, CMAQ_MDATA%ABFLUX )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "ABFLUX" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'HGBIDI', NF_INT, 1, CMAQ_MDATA%HGBIDI )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "HGBIDI" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'SFC_HONO', NF_INT, 1, CMAQ_MDATA%SFC_HONO )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SFC_HONO" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'BIOGEMIS', NF_INT, 1, CMAQ_MDATA%BIOGEMIS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "BIOGEMIS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DEMIS', NF_INT, 1, CMAQ_MDATA%PT3DEMIS )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DEMIS" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'CLD_DIAG', NF_INT, 1, CMAQ_MDATA%CLD_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "CLD_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'AERDIAG', NF_INT, 1, CMAQ_MDATA%AERDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "AERDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'PHOTDIAG', NF_INT, 1, CMAQ_MDATA%PHOTDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PHOTDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'SSEMDIAG', NF_INT, 1, CMAQ_MDATA%SSEMDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "SSEMDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'DUSTEM_DIAG', NF_INT, 1, CMAQ_MDATA%DUSTEM_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "DUSTEM_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'LTNGDIAG', NF_INT, 1, CMAQ_MDATA%LTNGDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "LTNGDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'B3GTS_DIAG', NF_INT, 1, CMAQ_MDATA%B3GTS_DIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "B3GTS_DIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DDIAG', NF_INT, 1, CMAQ_MDATA%PT3DDIAG )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DDIAG" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_INT( FID, NF_GLOBAL, 'PT3DFRAC', NF_INT, 1, CMAQ_MDATA%PT3DFRAC )
        IF ( IERR .NE. 0 ) THEN
            WRITE( MESG, '( 3A, I10 )' ) 'Error putting attribute "PT3DFRAC" to "', TRIM(FLIST3(FNUM)), '" STATUS=', IERR
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IERR = NFMPI_ENDDEF( FID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3MESG( 'Error putting file "' // TRIM(FLIST3(FNUM)) // '" into data mode.' )
            EFLAG = .TRUE.
        END IF          !  ierr nonzero:  operation failed

        IF ( EFLAG ) THEN
            CALL M3MESG( 'Errors putting CMAQ metadata to file "' // TRIM(FLIST3(FNUM)) // '"' )
        END IF          !  ierr nonzero:  operation failed

        PN_SETCMAQ = ( .NOT.EFLAG )

#endif
#ifndef IOAPI_PNCF

        CALL M3WARN( PNAME, 0, 0, 'Error:  PnetCDF Mode not active.' )
        PN_SETCMAQ = .FALSE.

#endif

        RETURN

    END FUNCTION  PN_SETCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDCMAQ()

        CMAQMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDCMAQ


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      SMOKE Attributes
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  INITSMOKEA( )

        INITSMOKEA = INITSMOKET( SMOKE_MDATA )
        RETURN

    END FUNCTION  INITSMOKEA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  INITSMOKET( META )

        !!........  Arguments:

        TYPE( SMETA_T ), INTENT( IN ) :: META

        !!........  body  .........................................

        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        SMOKEMETA  = .FALSE.
        INITSMOKET = .FALSE.
        RETURN

    END FUNCTION  INITSMOKET


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION GETSMOKEF( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME

        !!........  body  .........................................

        IF (  GETSMOKET( FNAME, SMOKE_MDATA ) ) THEN
            SMOKEMETA = .TRUE.
            GETSMOKEF = .TRUE.
        ELSE
            GETSMOKEF = .FALSE.
        END IF
        RETURN

    END FUNCTION  GETSMOKEF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION GETSMOKET( FNAME, MDATA )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT(IN   ) :: FNAME
        TYPE( SMETA_T )   , INTENT(  OUT) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETSMOKE'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETSMOKET = .FALSE.
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETSMOKET = .FALSE.
        ELSE IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
            MESG = 'Unsupported call for PnetCDF file "' // TRIM( FLIST3(FID) ) //'"'
            CALL M3WARN( PNAME, 0, 0, MESG )
            GETSMOKET = .FALSE.
        ELSE
            GETSMOKET = GETSMOKE1( FID, MDATA )
        END IF

        RETURN

    END FUNCTION  GETSMOKET


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION GETSMOKE1( FID, MDATA )

        !!........  Arguments:

        INTEGER        , INTENT(IN   ) :: FID
        TYPE( SMETA_T ), INTENT(  OUT) :: MDATA

        !!........  Include files:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/GETSMOKE'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................

        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        MDATA     = SMOKE_MDATA
        GETSMOKE1 = .FALSE.
        RETURN

    END FUNCTION  GETSMOKE1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKEC( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  body  .........................................

        SETSMOKEC = SETSMOKEA( FNAME, SMOKE_MDATA )

        RETURN

    END FUNCTION  SETSMOKEC


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKEA( FNAME, MDATA )

        !!........  Arguments:

        CHARACTER(LEN=*), INTENT( IN ) :: FNAME
        TYPE( SMETA_T ) , INTENT( IN ) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETSMOKEA'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETSMOKEA = .FALSE.
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETSMOKEA = .FALSE.
        ELSE
            SETSMOKEA = SETSMOKE2( FID, MDATA )
        END IF

        RETURN

    END FUNCTION  SETSMOKEA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKE1( FID )

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FID

        !!........  body  .........................................

        SETSMOKE1 = SETSMOKE2( FID, SMOKE_MDATA )
        RETURN

    END FUNCTION  SETSMOKE1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETSMOKE2( FID, MDATA )

        !!........  Arguments:

        INTEGER        , INTENT( IN ) :: FID
        TYPE( SMETA_T ), INTENT( IN ) :: MDATA

        !!........  Include file:

        INCLUDE 'STATE3.EXT'       !! I/O API state

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETSMOKE1'

        !!........  Local Variables:

        CHARACTER*256   MESG

        !!........  body  .........................................


        CALL M3MESG( 'SMOKE metadata not yet implemented' )
        SETSMOKE2 = .FALSE.
        RETURN

    END FUNCTION  SETSMOKE2


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDSMOKE()

        SMOKEMETA = .FALSE.
        RETURN

    END SUBROUTINE  ENDSMOKE


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      TEXT META Attributes
    !!          INITMTEXT():  initialize TEXT_MDATA from an external file
    !!           SETMTEXT():  Copy TEXT_MDATA to file-header
    !!           ENDMTEXT():  turn off TEXT_META
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  INITMTEXTA( )

        !!........  Local variables:

        INTEGER         MDEV, K, L, N, ISTAT
        CHARACTER*80    LINE
        CHARACTER*256   MESG

        !!........  body  .........................................
        
        IF ( TEXTMETA ) THEN
            INITMTEXTA = .TRUE.
            RETURN
        END IF
        
        MDEV = GETEFILE( 'IOAPI_TEXTMETA', .TRUE., .TRUE., 'MODATTS3/INITMTEXT' )
        IF ( MDEV .LT. 0 ) THEN
            CALL M3MESG( 'MODATTS3/INITMTEXT:  could not open "TEXT_MDATA"' )
            INITMTEXTA = .FALSE.
            RETURN
        END IF

        N = 0
        DO L = 1, 999999999
            READ( MDEV, '(A)', IOSTAT=ISTAT, END=188 ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 2( A, I9, 2X ) )' )     &
                    'MODATTS3/INITMTEXT: STAT=', ISTAT, 'counting "TEXT_MDATA" at line', N
                CALL M3MESG( MESG )
                INITMTEXTA = .FALSE.
                RETURN
            ELSE IF ( LINE .EQ. BLANK ) THEN
                CYCLE
            ELSE IF ( ISCOMMENT( LINE ) ) THEN
                CYCLE
            ELSE
                N = N + 1
            END IF
        END DO
188     CONTINUE

        ALLOCATE( TEXT_MDATA( N ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )                 &
                'MODATTS3/INITMTEXT: Allocation failure.  STAT==', ISTAT
            CALL M3MESG( MESG )
            INITMTEXTA = .FALSE.
            RETURN
        END IF
        
        REWIND( MDEV )
        K = 0
        DO L = 1, 999999999
            READ( MDEV, '(A)', IOSTAT=ISTAT, END=199 ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '( 2( A, I9, 2X ) )' )     &
                    'MODATTS3/INITMTEXT: STAT=', ISTAT, 'reading "TEXT_MDATA" at line', L
                CALL M3MESG( MESG )
                DEALLOCATE( TEXT_MDATA )
                INITMTEXTA = .FALSE.
                RETURN
            ELSE IF ( LINE .EQ. BLANK ) THEN
                CYCLE
            ELSE IF ( ISCOMMENT( LINE ) ) THEN
                CYCLE
            ELSE
                K = K + 1
                TEXT_MDATA(K) = ADJUSTL( LINE )
            END IF
        END DO
199     CONTINUE
        
        TEXT_MLINES = N
        TEXTMETA    = .TRUE.
        INITMTEXTA  = .TRUE.
        CLOSE( MDEV )
        
        RETURN

    END FUNCTION  INITMTEXTA


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  INITMTEXTT( NLINES, LINES )

        !!........  Arguments:

        INTEGER, INTENT(IN   )          :: NLINES
        CHARACTER(LEN=*), INTENT(IN   ) :: LINES(*)

        !!........  Local variables:

        INTEGER         L, ISTAT
        CHARACTER*256   MESG

        !!........  body  .........................................

        
        IF ( TEXTMETA ) THEN

            CALL M3MESG( 'MODATTS3/INITMTEXT: text metadata already active' )
            INITMTEXTT = .FALSE.
            RETURN

        END IF

        ALLOCATE( TEXT_MDATA( NLINES ), STAT = ISTAT )
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )                 &
                'MODATTS3/INITMTEXT: Allocation failure.  STAT==', ISTAT
            CALL M3MESG( MESG )
            INITMTEXTT = .FALSE.
            RETURN
        END IF

        DO L = 1, NLINES
            TEXT_MDATA( L ) = ADJUSTL( LINES( L ) )
        END DO
        
        TEXT_MLINES = NLINES
        TEXTMETA    = .TRUE.
        INITMTEXTT  = .TRUE.

        RETURN

    END FUNCTION  INITMTEXTT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION  SETMTEXT( FNUM )

        !!........  Argument:

        INTEGER, INTENT(IN   ) :: FNUM      !!  subscript in STATE3 arrays

        !!........  Include file:

        INCLUDE 'STATE3.EXT'      !! I/O API internal state

        !!........  Local variables:

        INTEGER         FID, IERR
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = CDFID3( FNUM )
        IF ( .NOT.TEXTMETA ) THEN

            CALL M3MESG( 'MODATTS3/SETMTEXT: text metadata not active' )
            SETMTEXT = .FALSE.
            RETURN

        ELSE IF ( FID .LT. 0 ) THEN

            CALL M3MESG( 'MODATTS3/SETMTEXT: Operation not supported:  file  "' // TRIM(FLIST3(FNUM)) // '" not [P]netCDF' )
            SETMTEXT = .FALSE.
            RETURN

        ELSE IF ( FTYPE3( FNUM ) .NE. MPIGRD3 ) THEN

            IERR = NF_REDEF( FID )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting file "', TRIM( FLIST3(FNUM) ),  &
                     '" into define mode. STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_PUT_ATT_TEXT( FID, NF_GLOBAL, 'TEXT_MDATA', TEXT_MLINES*MXDLEN3, TEXT_MDATA )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting attribute "TEXT_MDATA" to "', TRIM( FLIST3(FNUM) ),  &
                     '" STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed

            IERR = NF_ENDDEF( FID )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting file "', TRIM( FLIST3(FNUM) ),  &
                     '" into data mode. STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed
        
        ELSE

#ifdef  IOAPI_PNCF

            IERR = NFMPI_REDEF( FID )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting file "', TRIM( FLIST3(FNUM) ),  &
                     '" into define mode. STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_TEXT( FID, NF_GLOBAL, 'TEXT_MDATA', TEXT_MLINES*MXDLEN3, TEXT_MDATA )
            IF ( IERR .NE. NF_NOERR ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting attribute "TEXT_MDATA" to "', TRIM( FLIST3(FNUM) ),  &
                     '" STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed

            IERR = NFMPI_ENDDEF( FID )
            IF ( IERR .NE. 0 ) THEN
                WRITE( MESG, '(3A,I10)' )       &
                     'MODATTS3/SETMTEXT: Error putting file "', TRIM( FLIST3(FNUM) ),  &
                     '" into data mode. STATUS=', IERR
                CALL M3MESG( MESG )
                SETMTEXT = .FALSE.
                RETURN
            END IF          !  ierr nonzero:  operation failed

#endif
#ifndef IOAPI_PNCF

            CALL M3MESG( 'MODATTS3/SETMTEXT Error:  PnetCDF Mode not active.' )
            SETMTEXT = .FALSE.
            RETURN

#endif

        END IF      !!  if fid < 0; else if mpigrd3; else...
 
        SETMTEXT = .TRUE.

        RETURN

    END FUNCTION  SETMTEXT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  ENDMTEXT()

        IF ( ALLOCATED( TEXT_MDATA ) ) DEALLOCATE( TEXT_MDATA )
        TEXT_MLINES = 0
        TEXTMETA    = .FALSE.
        RETURN

    END SUBROUTINE  ENDMTEXT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!      CF Attributes
    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  INITCF()

        CFMETA = .TRUE.
        RETURN

    END SUBROUTINE  INITCF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCFF( FNAME )

        !!........  Arguments:

        CHARACTER( LEN=* ), INTENT( IN ) :: FNAME

        !!........  Include file:

        INCLUDE 'STATE3.EXT'

        !!........  Parameter:

        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCFC'

        !!........  Local Variables:

        INTEGER         FID
        CHARACTER*256   MESG

        !!........  body  .........................................

        FID = NAME2FID( FNAME )

        IF ( FID .LE. 0 ) THEN
            MESG = 'File "' // TRIM( FNAME ) //'" not yet open'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCFF = .FALSE.
        ELSE IF ( CDFID3( FID ) .LT. 0 ) THEN
            MESG = 'File "' // TRIM( FLIST3(FID) ) //'" not netCDF'
            CALL M3WARN( PNAME, 0, 0, MESG )
            SETCFF = .FALSE.
        ELSE
            SETCFF = SETCF1( FID )
        END IF

        RETURN

    END FUNCTION  SETCFF


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION SETCF1( FID )

        !!........  Arguments:

        INTEGER, INTENT( IN ) :: FID

        !!........  Include file:

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
        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/SETCF'

        !!........  Local Variables:

        CHARACTER*16    ANAME, XNAME, YNAME, ZNAME
        CHARACTER*16    XUNIT, YUNIT, ZUNIT
        CHARACTER*80    XDESC, YDESC, ZDESC
        CHARACTER*80    XLONG, YLONG, ZLONG
        CHARACTER*80    DSCBUF          !!  scratch buffer for descriptions
        CHARACTER*80    DSCBU2          !!  scratch buffer for descriptions
        CHARACTER*80    MNAME
        CHARACTER*256   MESG            !!  scratch buffer

        INTEGER         DIMS( 5 )       !!  array of dims for NF_DEF_VAR()
        INTEGER         DELS( 5 )       !!  array of dims for NF_DEF_VAR()
        INTEGER         C, R, L
        INTEGER         MID, CID, RID, LID, IERR
        INTEGER         NDIMS, CDIM, RDIM, LDIM, EDIM
        INTEGER         FNUM
        LOGICAL         EFLAG
        REAL*8          DARGS( MXDIM ), X0, XC


        !!........  body  .........................................

        EFLAG = .FALSE.
        IF ( FTYPE3( FID ) .EQ. MPIGRD3 ) THEN
            SETCF1 = PN_SETCF1( FID )
            RETURN
        END IF

        FNUM = CDFID3( FID )

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
        IERR = NF_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'Conventions', L, DSCBUF( 1:L ) )
        IF ( IERR .NE. 0 ) THEN
            DSCBU2 = 'Error creating netCDF file attribute  "Conventions"'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed

        L     = MAX( 0, MIN( TRMGRD3+1, GDTYP3(FID) ) )
        MNAME = PRJNAMES( L )
        IERR  = NF_DEF_VAR( FNUM, MNAME, NF_CHAR, 0, DIMS, MID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR,            &
                'Error creating netCDF variable "Coords"' )
             EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

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
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. LAMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'lambert_conformal_conic'
            L      = LEN_TRIM( DSCBUF )
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'standard_parallel'
            DARGS( 1 ) = P_ALP3(FID)
            DARGS( 2 ) = P_BET3(FID)

            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 2, DARGS )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'latitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_easting'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_northing'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

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
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 6.0D0*P_ALP3D-183.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute  "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'scale_factor_at_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.9996D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'latitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_easting'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1,XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_northing'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. POLGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'polar_stereographic'
            L      = LEN_TRIM( DSCBUF )
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'straight_vertical_longitude_from_pole'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "straight_vertical_longitude_from_pole"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'standard_parallel'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_BET3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'latitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 90.0D0*P_ALP3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_easting'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_northing'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. EQMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'mercator'
            L      = LEN_TRIM( DSCBUF )
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'standard_parallel'
            DARGS( 1 ) =  P_ALP3(FID)
            DARGS( 2 ) = -P_ALP3(FID)

            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 2, DARGS )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'latitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_easting'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_northing'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. TRMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'transverse_mercator'
            L      = LEN_TRIM( DSCBUF )
            IERR = NF_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', L, DSCBUF( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'longitude_of_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'scale_factor_at_central_meridian'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_BET3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'latitude_of_projection_origin'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, P_ALP3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_easting'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DSCBUF = 'false_northing'
            IERR = NF_PUT_ATT_DOUBLE( FNUM, MID, DSCBUF, NF_DOUBLE, 1, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating netCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

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
            IERR = NF_DEF_VAR( FNUM, ZNAME, NF_DOUBLE, 1,DIMS, LID )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating netCDF variable LAY'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'axis', 1, 'Z' )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating att AXIS for vble LVL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

        END IF          !!  if nlays3(FID) > 1

        IF ( NLAYS3(FID) .LE. 1 ) THEN

            CONTINUE        !!  do nothing: no vertical CF metadata

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPH3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPN3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSIGZ3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '
            L     = LEN_TRIM( ZDESC )

            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGPRES3 ) THEN

            ZDESC = 'pres'
            L     = LEN_TRIM( ZDESC )
            ZUNIT = 'Pa'
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            L     = LEN_TRIM( ZUNIT )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'units', L, ZUNIT( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGZVAL3 ) THEN

            ZDESC = 'height_above_terrain'
            ZUNIT = 'm'

            L     = LEN_TRIM( ZDESC )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            L     = LEN_TRIM( ZUNIT )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'units', L, ZUNIT( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGHVAL3 ) THEN

            ZDESC = 'height_above_MSL'
            ZUNIT = 'm'

            L     = LEN_TRIM( ZDESC )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            L     = LEN_TRIM( ZUNIT )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'units', L, ZUNIT( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFEM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '

            L     = LEN_TRIM( ZDESC )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFNM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '

            L     = LEN_TRIM( ZDESC )
            IERR = NF_PUT_ATT_TEXT( FNUM, LID, 'standard_name', L, ZDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating netCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

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
            IERR = NF_DEF_VAR( FNUM, XNAME, NF_DOUBLE, 1, DIMS, CID )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable COL' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

!!          "long_name" attribute already done by CRTFIL3()
!!
!!          L = LEN_TRIM( XDESC )
!!          IERR = NF_PUT_ATT_TEXT( FNUM, CID, 'long_name', L, XDESC( 1:L ) )
!!          IF ( IERR .NE. 0 ) THEN
!!              DSCBUF = 'Error creating attribute LONG_NAME for vble COL'
!!              CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
!!              EFLAG = .TRUE.
!!              GO TO 999
!!          END IF              !!  ierr nonzero:  operation failed

            IERR = NF_PUT_ATT_TEXT( FNUM, CID, 'axis', 1, 'X' )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute AXIS for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( XLONG )
            IERR = NF_PUT_ATT_TEXT( FNUM, CID, 'standard_name', L, XLONG( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating att STANDARD_NAME for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( XUNIT )
            IERR = NF_PUT_ATT_TEXT( FNUM, CID, 'units', L, XUNIT( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute UNITS for variable COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( MNAME )
            IERR = NF_PUT_ATT_TEXT( FNUM, CID, 'grid_mapping', L, MNAME( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error creating netCDF attribute "grid_mapping" for vble COL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed


            NDIMS = 1
            DIMS( 1 ) = RDIM
            IERR = NF_DEF_VAR( FNUM, YNAME, NF_DOUBLE, 1, DIMS, RID )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating netCDF variable ROW' )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            IERR = NF_PUT_ATT_TEXT( FNUM, RID, 'axis', 1, 'Y' )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating attribute AXIS for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( YDESC )
            IERR = NF_PUT_ATT_TEXT( FNUM, RID, 'long_name', L, YDESC( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute LONG_NAME for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( YLONG )
            IERR = NF_PUT_ATT_TEXT( FNUM, RID, 'standard_name', L, YLONG( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating atte STANDARD_NAME for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( YUNIT )
            IERR = NF_PUT_ATT_TEXT( FNUM, RID, 'units', L, YUNIT( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating  attribute UNITS for variable ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            L = LEN_TRIM( MNAME )
            IERR = NF_PUT_ATT_TEXT( FNUM, RID, 'grid_mapping', L, MNAME( 1:L ) )
            IF ( IERR .NE. 0 ) THEN
                MESG = 'Error creating netCDF attribute "grid_mapping" for vble ROW'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

        END IF              !!  if GRIDDED file


        !!...........   Put FNUM back into data mode:  attributes and variables now defined.

        IERR = NF_ENDDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error putting netCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed


        IF ( CFMETA .AND. FTYPE3(FID) .EQ. GRDDED3 ) THEN

            X0 = XORIG3(FID)
            XC = XCELL3(FID)
            DO C = 1, MIN( NCOLS3(FID), MXDIM )
                DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
            END DO

            DIMS( 1 ) = 1
            DELS( 1 ) = MIN( NCOLS3(FID), MXDIM )

            IERR = NF_PUT_VARA_DOUBLE( FNUM, CID, DIMS, DELS, DARGS )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "COL"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            X0 = YORIG3(FID)
            XC = YCELL3(FID)
            DO C = 1, MIN( NROWS3(FID), MXDIM )
                DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
            END DO

            DIMS( 1 ) = 1
            DELS( 1 ) = MIN( NROWS3(FID), MXDIM )

            IERR = NF_PUT_VARA_DOUBLE( FNUM, RID, DIMS, DELS, DARGS )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "ROW"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IF ( NLAYS3(FID) .GT. 1 ) THEN

                DO C = 1, MIN( NLAYS3(FID), MXDIM )
                    DARGS( C ) = 0.5D0 * ( VGLVS3(   L,FID ) + VGLVS3( L+1,FID ) )
                END DO

                DIMS( 1 ) = 1
                DELS( 1 ) = MIN( NLAYS3(FID), MXDIM )

                IERR = NF_PUT_VARA_DOUBLE( FNUM, LID, DIMS, DELS, DARGS )
                IF ( IERR .NE. 0 ) THEN
                    CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "LAY"' )
                    EFLAG = .TRUE.
                    GO TO 999
                END IF          !!  ierr nonzero:  operation failed

            END IF      !!  if nlays3(FID) > 1

        END IF          !!  if cmfeta and gridded...

999     SETCF1 = ( .NOT. EFLAG )
        RETURN

    END FUNCTION  SETCF1


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION PN_SETCF1( FID )

        USE MODPDATA
        USE MODNCFIO

        !!........  Arguments:

        INTEGER, INTENT( IN ) :: FID

#ifdef  IOAPI_PNCF

        !!........  Include files:

        INCLUDE 'mpif.h'
        INCLUDE 'PARMS3.EXT'
        INCLUDE 'STATE3.EXT'

        !!........  Parameters:

        CHARACTER*80, PARAMETER :: PRJNAMES( 0:TRMGRD3+1 ) = &    !!  map-projection names
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
        CHARACTER*24, PARAMETER :: PNAME = 'MODATTS3/PN_SETCF1'

        !!........  Local Variables:

        CHARACTER*16    ANAME, XNAME, YNAME, ZNAME
        CHARACTER*16    XUNIT, YUNIT, ZUNIT
        CHARACTER*80    XDESC, YDESC, ZDESC
        CHARACTER*80    XLONG, YLONG, ZLONG
        CHARACTER*80    DSCBUF          !!  scratch buffer for descriptions
        CHARACTER*80    DSCBU2          !!  scratch buffer for descriptions
        CHARACTER*80    MNAME
        CHARACTER*256   MESG            !!  scratch buffer

        INTEGER         C, R, L
        INTEGER         MID, CID, RID, LID, IERR
        INTEGER         NDIMS, CDIM, RDIM, LDIM, EDIM
        INTEGER         FNUM
        LOGICAL         EFLAG
        REAL*8          DARGS( MXDIM ), X0, XC

        INTEGER( MPI_OFFSET_KIND ) :: DIMS( 5 )       !!  array of dims for NCVDEF()
        INTEGER( MPI_OFFSET_KIND ) :: DELS( 5 )       !!  array of dims for NCVPT()
        INTEGER( MPI_OFFSET_KIND ) :: PNSIZE


        !!........  body  .........................................

        EFLAG = .FALSE.

        IF ( .NOT. PN_IO_PE ) THEN
            PN_SETCF1 = .TRUE.
            RETURN
        END IF

        FNUM = CDFID3( FID )

        IF ( NFMPI_REDEF( FNUM ) .NE. NF_NOERR ) THEN
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

        ANAME = 'CF-1.0'
        IERR = NFMPI_PUT_ATT_TEXT( FNUM, NF_GLOBAL, 'Conventions', NF_CHAR, PN_NAMLEN, ANAME )
        IF ( IERR .NE. 0 ) THEN
            DSCBU2 = 'Error creating PnetCDF file attribute  "Conventions"'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed

        L     = MAX( 0, MIN( TRMGRD3+1, GDTYP3(FID) ) )
        MNAME = PRJNAMES( L )
        IERR = NFMPI_DEF_VAR( FNUM, MNAME, NF_CHAR, 0, DIMS, MID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating PnetCDF variable "Coords"' )
             EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

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
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. LAMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'lambert_conformal_conic'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DARGS( 1 ) = P_ALP3(FID)
            DARGS( 2 ) = P_BET3(FID)

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'standard_parallel', NF_DOUBLE, PN_TWO, DARGS )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID,  'longitude_of_central_meridian', NF_DOUBLE, PN_ONE, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'latitude_of_projection_origin', NF_DOUBLE, PN_ONE, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'longitude_of_projection_origin', NF_DOUBLE, PN_ONE, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_easting', NF_DOUBLE, PN_ONE, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_northing', NF_DOUBLE, PN_ONE, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

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

            CALL M3MESG( 'CF metadata not supported for GENERAL STEREOGRAPHIC' )

        ELSE IF ( GDTYP3(FID) .EQ. UTMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'transverse_mercator'
            IERR   = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'longitude_of_central_meridian', NF_DOUBLE, PN_ONE, 6.0D0*P_ALP3D-183.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute  "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'scale_factor_at_central_meridian', NF_DOUBLE, PN_ONE, 0.9996D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'latitude_of_projection_origin', NF_DOUBLE, PN_ONE, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_easting', NF_DOUBLE, PN_ONE, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_northing', NF_DOUBLE, PN_ONE, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM,DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. POLGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'polar_stereographic'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'straight_vertical_longitude_from_pole', NF_DOUBLE, PN_ONE, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "straight_vertical_longitude_from_pole"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'standard_parallel', NF_DOUBLE, PN_ONE, P_BET3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'latitude_of_projection_origin', NF_DOUBLE, PN_ONE, 90.0D0*P_ALP3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_easting', NF_DOUBLE, PN_ONE, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_northing', NF_DOUBLE, PN_ONE, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. EQMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'mercator'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            DARGS( 1 ) =  P_ALP3(FID)
            DARGS( 2 ) = -P_ALP3(FID)

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'standard_parallel', NF_DOUBLE, PN_TWO, DARGS )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "standard_parallel"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'longitude_of_central_meridian', NF_DOUBLE, PN_ONE, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'latitude_of_projection_origin', NF_DOUBLE, PN_ONE, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'longitude_of_projection_origin', NF_DOUBLE, PN_ONE, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "longitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_easting', NF_DOUBLE, PN_ONE, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_northing', NF_DOUBLE, PN_ONE, 0.0D0 )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( GDTYP3(FID) .EQ. TRMGRD3 ) THEN

            XDESC = 'X coordinate of projection'
            YDESC = 'Y coordinate of projection'
            XUNIT = 'm'
            YUNIT = 'm'

            DSCBUF = 'transverse_mercator'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, MID, 'grid_mapping_name', PN_MXDLEN, DSCBUF )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "grid_mapping_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'longitude_of_central_meridian', NF_DOUBLE, PN_ONE, P_GAM3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "longitude_of_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'scale_factor_at_central_meridian', NF_DOUBLE, PN_ONE, P_BET3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "scale_factor_at_central_meridian"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'latitude_of_projection_origin', NF_DOUBLE, PN_ONE, P_ALP3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "latitude_of_projection_origin"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_easting', NF_DOUBLE, PN_ONE, XCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_easting"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_DOUBLE( FNUM, MID, 'false_northing', NF_DOUBLE, PN_ONE, YCENT3(FID) )
            IF ( IERR .NE. 0 ) THEN
                 DSCBU2 = 'Error creating PnetCDF file attribute "false_northing"'
                 CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                 EFLAG = .TRUE.
                 GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        END IF              !!  if gdtyp3(FID)==latgrd3, lamgrd3, ...


        IF ( NLAYS3(FID) .GT. 1 ) THEN

            IERR = NFMPI_INQ_DIMID( FNUM, 'LAY', LDIM )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error inquiring for dimension "LAY"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF

            DIMS( 1 ) = LDIM
            IERR = NFMPI_DEF_VAR( FNUM, 'LAY', NF_DOUBLE, PN_ONE, DIMS, LID )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating PnetCDF variable LAY'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'axis', PN_ONE, 'Z' )
            IF ( IERR .NE. 0 ) THEN
                DSCBUF = 'Error creating att AXIS for vble LVL'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
                EFLAG = .TRUE.
                GO TO 999
            END IF              !!  ierr nonzero:  operation failed

        END IF          !!  if nlays3(FID) > 1

        IF ( NLAYS3(FID) .LE. 1 ) THEN

            CONTINUE        !!  do nothing: no vertical CF metadata

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPH3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSGPN3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGSIGZ3 ) THEN

            ZDESC = 'atmosphere_sigma_coordinate'
            ZUNIT = ' '

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGPRES3 ) THEN

            ZDESC = 'pres'
            ZUNIT = 'Pa'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGZVAL3 ) THEN

            ZDESC = 'height_above_terrain'
            ZUNIT = 'm'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGHVAL3 ) THEN

            ZDESC = 'height_above_MSL'
            ZUNIT = 'm'
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFEM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        ELSE IF ( VGTYP3(FID) .EQ. VGWRFNM ) THEN

            ZDESC = 'atmosphere_hybrid_coordinate'
            ZUNIT = ' '
            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'standard_name', PN_MXDLEN, ZDESC )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "standard_name"'
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        END IF              !!  if nlays=1, or if vgtyp3(FID)==vgsgph3, ...

        IF ( NLAYS3(FID) .GT. 1 ) THEN

            IERR = NFMPI_PUT_ATT_TEXT( FNUM, LID, 'units', PN_NAMLEN, ZUNIT )
            IF ( IERR .NE. 0 ) THEN
                DSCBU2 = 'Error creating PnetCDF file attribute "units" for ' // ZNAME
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBU2 )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        END IF              !!  if nlays=1, or if vgtyp3(FID)==vgsgph3, ...


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
        IERR = NF_DEF_VAR( FNUM, XNAME, NF_DOUBLE, 1, DIMS, CID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating PnetCDF variable COL' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, CID, 'long_name', PN_NAMLEN, XDESC )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating attribute LONG_NAME for vble COL'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, CID, 'axis', PN_ONE, 'X' )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating attribute AXIS for vble COL'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, CID, 'standard_name', PN_MXDLEN, XLONG )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating att STANDARD_NAME for vble COL'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, CID, 'units', PN_MXDLEN, XUNIT )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating attribute UNITS for variable COL'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, CID, 'grid_mapping', PN_MXDLEN, MNAME )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error creating PnetCDF attribute "grid_mapping" for vble COL'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed


        NDIMS = 1
        DIMS( 1 ) = RDIM
        IERR = NF_DEF_VAR( FNUM, YNAME, NF_DOUBLE, 1, DIMS, RID )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error creating PnetCDF variable ROW' )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, RID, 'axis', PN_ONE, 'Y' )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating attribute AXIS for vble ROW'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, RID, 'long_name', PN_MXDLEN, YDESC )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating  attribute LONG_NAME for vble ROW'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, RID, 'standard_name', PN_MXDLEN, YLONG )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating atte STANDARD_NAME for vble ROW'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, RID, 'units',PN_MXDLEN, YUNIT )
        IF ( IERR .NE. 0 ) THEN
            DSCBUF = 'Error creating  attribute UNITS for variable ROW'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, DSCBUF )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed

        IERR = NFMPI_PUT_ATT_TEXT( FNUM, RID, 'grid_mapping', PN_MXDLEN, MNAME )
        IF ( IERR .NE. 0 ) THEN
            MESG = 'Error creating PnetCDF attribute "grid_mapping" for vble ROW'
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF              !!  ierr nonzero:  operation failed


        !!...........   Put FNUM back into data mode:  attributes and variables now defined.

        IERR = NFMPI_ENDDEF( FNUM )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error putting PnetCDF file into data mode.' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed


        X0 = XORIG3(FID)
        XC = XCELL3(FID)
        DO C = 1, MIN( NCOLS3(FID), MXDIM )
            DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
        END DO

        DIMS( 1 ) = 1
        DELS( 1 ) = MIN( NCOLS3(FID), MXDIM )

        IERR = NFMPI_PUT_VARA_DOUBLE_ALL( FNUM, CID, DIMS, DELS, DARGS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "COL"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed

        X0 = YORIG3(FID)
        XC = YCELL3(FID)
        DO C = 1, MIN( NROWS3(FID), MXDIM )
            DARGS( C ) = X0 + XC * ( DBLE( C ) - 0.5D0 )
        END DO

        DIMS( 1 ) = 1
        DELS( 1 ) = MIN( NROWS3(FID), MXDIM )

        IERR = NFMPI_PUT_VARA_DOUBLE_ALL( FNUM, RID, DIMS, DELS, DARGS )
        IF ( IERR .NE. 0 ) THEN
            CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "ROW"' )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !!  ierr nonzero:  operation failed

        IF ( NLAYS3(FID) .GT. 1 ) THEN

            DO C = 1, MIN( NLAYS3(FID), MXDIM )
                DARGS( C ) = 0.5D0 * ( VGLVS3(   L,FID ) + VGLVS3( L+1,FID ) )
            END DO

            DIMS( 1 ) = 1
            DELS( 1 ) = MIN( NLAYS3(FID), MXDIM )

            IERR = NFMPI_PUT_VARA_DOUBLE_ALL( FNUM, LID, DIMS, DELS, DARGS )
            IF ( IERR .NE. 0 ) THEN
                CALL M3ABORT( FLIST3( FID ), FNUM, IERR, 'Error initializing cfmeta "LAY"' )
                EFLAG = .TRUE.
                GO TO 999
            END IF          !!  ierr nonzero:  operation failed

        END IF      !!  if nlays3(FID) > 1

999     PN_SETCF1 = ( .NOT. EFLAG )

#endif
#ifndef IOAPI_PNCF

        CALL M3MESG( 'PN_SETCF1 error:  PnetCDF Mode not active.' )
        PN_SETCF1 = .FALSE.

#endif

        RETURN

    END FUNCTION  PN_SETCF1


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


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    LOGICAL FUNCTION ISCOMMENT( CBUF )

        CHARACTER(LEN=*), INTENT( IN ) :: CBUF

        CHARACTER*1,  PARAMETER :: BANG   = '!'
        CHARACTER*1,  PARAMETER :: POUND  = '#'
        CHARACTER*1,  PARAMETER :: DOLLAR = '$'
        CHARACTER*2,  PARAMETER :: SLASH2 = '//'

        CHARACTER*1     C1
        CHARACTER*2     C2

        C2 = ADJUSTL( CBUF )
        C1 = C2(1:1)

        ISCOMMENT = ( ( C2 .EQ. SLASH2  ) .OR. ( C1 .EQ. BANG  ) .OR. ( C1 .EQ. POUND ) .OR. ( C1 .EQ. DOLLAR ) )

        RETURN

    END  FUNCTION ISCOMMENT


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    INTEGER FUNCTION STR2LOG( CBUF )

        CHARACTER(LEN=*), INTENT( IN ) :: CBUF

        CHARACTER*1     CH
        INTEGER         K

        CHARACTER*1,  PARAMETER :: PERIOD = '.'

        CHARACTER*1     C1
        CHARACTER*2     C2

        C2 =  ADJUSTL( CBUF )
        IF ( C2 .EQ. PERIOD ) THEN      !!  handle ".TRUE.",  etc.
            C1 = C2( 2:2 )
        ELSE
            C1 = C2( 1:1 )
        END IF

        IF (      ( C1 .EQ. 'Y'  ) .OR. ( C1 .EQ. 'y' ) .OR.        &
                  ( C1 .EQ. 'T'  ) .OR. ( C1 .EQ. 't' ) ) THEN
            STR2LOG = 1
        ELSE IF ( ( C1 .EQ. 'N'  ) .OR. ( C1 .EQ. 'n' ) .OR.        &
                  ( C1 .EQ. 'F'  ) .OR. ( C1 .EQ. 'f' ) ) THEN
            STR2LOG = 0
        ELSE
            STR2LOG = IMISS3
        END IF

        RETURN

    END  FUNCTION STR2LOG


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    INTEGER FUNCTION ENVLOG( NAME, DESC, EFLAG )

        CHARACTER(LEN=*), INTENT(IN   ) :: NAME, DESC
        LOGICAL         , INTENT(INOUT) :: EFLAG

        CHARACTER*256   MESG
        CHARACTER*512   CBUF
        INTEGER         ISTAT

        CALL ENVSTR( NAME, DESC, BLANK, CBUF, ISTAT )
        IF ( ISTAT .GT. 0 ) THEN
            ENVLOG = IMISS3
            EFLAG  = .TRUE.
            MESG   = 'Bad environment variable "' // TRIM( NAME ) // '"'
        ELSE
            ENVLOG = STR2LOG( CBUF )
        END IF

        RETURN

    END  FUNCTION ENVLOG


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    CHARACTER*8 FUNCTION LOGVAL( IVAL )
    
        INTEGER, INTENT( IN ) :: IVAL
        
        IF ( IVAL .EQ. 1 ) THEN
            LOGVAL = 'T'
        ELSE IF ( IVAL .EQ. 0 ) THEN
            LOGVAL = 'F'
        ELSE IF ( IVAL .EQ. IMISS3 ) THEN
            LOGVAL = 'MISSING'
        ELSE
            LOGVAL = 'INVALID'
        END IF

        RETURN

    END  FUNCTION LOGVAL


    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


    SUBROUTINE  FILEMESG( FDEV, MESG )      !!  mimics M3MESG() but to FDEV

        INTEGER,       INTENT( IN ) :: FDEV
        CHARACTER*(*), INTENT( IN ) :: MESG

        WRITE( FDEV, '( 5X, A )' ) TRIM( MESG )
        RETURN

    END SUBROUTINE  FILEMESG

END MODULE MODATTS3

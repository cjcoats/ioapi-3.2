MODULE MODNCFIO

    !!.........................................................................
    !!  Version "$Id: modwrfwndw.f90 238 2023-03-13 16:54:33Z coats $"
    !!  Copyright (c) 2015-2016 UNC Institute for the Environment.
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    !!  See file "LGPL.txt" for conditions of use.
    !!........................................................................
    !!
    !!  DESCRIPTION:
    !!      Utilities for wrfwndw.f90:
    !!      READSTEP*, WRITESTEP* adapted from READNCSTEP*, WRITENCSTEP*
    !!
    !!
    !!  REVISION  HISTORY:
    !!      Prototype  1/2023 by Carlie J. Coats, Jr., UNC IE
    !!
    !!...................................................................................

    USE MODNCFIO
    
    IMPLICIT NONE


    !!--------  Public Routines in this module:  -----------------------

    PUBLIC  :: READNCSTEP, WRITENCSTEP


    !!--------  Generic Interfaces:  -----------------------------------

    INTERFACE READNCSTEP
        MODULE PROCEDURE READFIDSTEP0DR, READFIDSTEP0DI, READFIDSTEP0DS, READFIDSTEP0DB, READFIDSTEP0DD,   &
                         READFIDSTEP1DR, READFIDSTEP1DI, READFIDSTEP1DS, READFIDSTEP1DB, READFIDSTEP1DD,   &
                         READFIDSTEP2DR, READFIDSTEP2DI, READFIDSTEP2DS, READFIDSTEP2DB, READFIDSTEP2DD,   &
                         READFIDSTEP3DR, READFIDSTEP3DI, READFIDSTEP3DS, READFIDSTEP3DB, READFIDSTEP3DD,   &
                         READFIDSTEP4DR, READFIDSTEP4DI, READFIDSTEP4DS, READFIDSTEP4DB, READFIDSTEP4DD
    END INTERFACE READNCVAR


    INTERFACE WRITENCSTEP
        MODULE PROCEDURE WRITEFIDSTEP0DR, WRITEFIDSTEP0DI, WRITEFIDSTEP0DS, WRITEFIDSTEP0DB, WRITEFIDSTEP0DD,   &
                         WRITEFIDSTEP1DR, WRITEFIDSTEP1DI, WRITEFIDSTEP1DS, WRITEFIDSTEP1DB, WRITEFIDSTEP1DD,   &
                         WRITEFIDSTEP2DR, WRITEFIDSTEP2DI, WRITEFIDSTEP2DS, WRITEFIDSTEP2DB, WRITEFIDSTEP2DD,   &
                         WRITEFIDSTEP3DR, WRITEFIDSTEP3DI, WRITEFIDSTEP3DS, WRITEFIDSTEP3DB, WRITEFIDSTEP3DD,   &
                         WRITEFIDSTEP4DR, WRITEFIDSTEP4DI, WRITEFIDSTEP4DS, WRITEFIDSTEP4DB, WRITEFIDSTEP4DD
    END INTERFACE WRITENCVAR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !!      TIME STEPPED FORMS READFIDSTEP*()
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP0DR( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        REAL         , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP0DI( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER      , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP0DS( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER(2)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP0DB( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER(1)   , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP0DD( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        REAL*8       , INTENT(  OUT) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP1DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP1DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP1DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                   !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS            !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP1DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP1DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP2DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP2DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP2DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP2DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP2DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION READFIDSTEP3DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP3DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP3DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP3DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP3DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP4DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP4DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP4DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP4DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION READFIDSTEP4DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(  OUT) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_GET_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_GET_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        READFIDSTEP4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION READFIDSTEP4DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-
    !!      TIME STEPPED FORMS WRITEFIDSTEP*()
    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP0DR( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        REAL         , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP0DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP0DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP0DI( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER      , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP0DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP0DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP0DS( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER(2)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP0DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP0DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP0DB( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        INTEGER(1)   , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP0DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP0DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP0DD( FNAME, CDFID, ISTEP, VNAME, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP
        REAL*8       , INTENT(IN   ) :: GRID

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 1 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(1) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        DIMS(1) = ISTEP
        DELS(1) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP0DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP0DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP1DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP1DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP1DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP1DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP1DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP1DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP1DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                   !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                   !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS            !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP1DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP1DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP1DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP1DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP1DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP1DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 2 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(2) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP1DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP1DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP2DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP2DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP2DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP2DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP2DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP2DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP2DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP2DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP2DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP2DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP2DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP2DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP2DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 3 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(3) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP2DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP2DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-



    LOGICAL FUNCTION WRITEFIDSTEP3DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP3DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP3DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP3DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP3DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP3DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP3DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP3DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP3DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP3DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP3DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP3DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP3DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 4 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(4) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP3DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP3DD


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP4DR( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS    !!  dimensions
        REAL         , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3REAL ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMLEN() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_REAL( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_REAL() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP4DR = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP4DR


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP4DI( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER      , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3INT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP4DI = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP4DI


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP4DS( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(2)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_SHORT ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT2( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP4DS = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP4DS


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP4DB( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        INTEGER(1)   , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. NF_BYTE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_INT1( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_INT() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP4DB = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP4DB


    ! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-=-=-=-=-=-


    LOGICAL FUNCTION WRITEFIDSTEP4DD( FNAME, CDFID, ISTEP, VNAME, NCOLS, NROWS, NLAYS, NSPCS, GRID )
        USE M3UTILIO

        CHARACTER*(*), INTENT(IN   ) :: FNAME                           !!  logical file name
        CHARACTER*(*), INTENT(IN   ) :: VNAME                           !!  variable name
        INTEGER      , INTENT(IN   ) :: CDFID, ISTEP, NCOLS, NROWS, NLAYS, NSPCS      !!  dimensions
        REAL*8       , INTENT(IN   ) :: GRID( NCOLS, NROWS, NLAYS, NSPCS )

        INTEGER         NDIMS, DIMIDS( 7 ), DIMS( 7 ), DELS( 7 )
        INTEGER         FID, VID, XID, YID, TID
        INTEGER         ISTAT, IDIM, ITYPE, NATTS
        LOGICAL         EFLAG
        CHARACTER*512   ANAME, MESG

        !!-----------   function body  -------------------------------

        EFLAG = .FALSE.
        FID   = CDFID

        ISTAT = NF_INQ_UNLIMDIM( FID, TID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for time-dimension in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_UNLIMDIM() failed

        ISTAT = NF_INQ_VARID( FID, VNAME, VID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ID for variable "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VARID() failed

        ISTAT = NF_INQ_VAR( FID, VID, ANAME, ITYPE, NDIMS, DIMIDS, NATTS  )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( NDIMS .NE. 5 ) THEN
            MESG = 'Bad NDIMS for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( DIMIDS(5) .NE. TID ) THEN
            MESG = 'Bad TIME-DIMENSION for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( ITYPE .NE. M3DBLE ) THEN
            MESG = 'Bad TYPE for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_VAR() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(1), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NCOLS ) THEN
            MESG = 'Bad COL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(2), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NROWS ) THEN
            MESG = 'Bad ROW dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(3), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NLAYS ) THEN
            MESG = 'Bad LVL dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        ISTAT = NF_INQ_DIMLEN( FID, DIMIDS(4), IDIM )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        ELSE IF ( IDIM .NE. NSPCS ) THEN
            MESG = 'Bad SPC dimension for  "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_INQ_DIMID() failed

        DIMS(1) = 1
        DIMS(2) = 1
        DIMS(3) = 1
        DIMS(4) = 1
        DIMS(5) = ISTEP
        DELS(1) = NCOLS
        DELS(2) = NROWS
        DELS(3) = NLAYS
        DELS(4) = NSPCS
        DELS(5) = 1
        ISTAT   = NF_PUT_VARA_DOUBLE( FID, VID, DIMS, DELS, GRID )
        IF ( ISTAT .NE. 0 ) THEN
            MESG = 'Error reading "' // TRIM( VNAME ) // '" in "' // TRIM( FNAME ) // '"'
            CALL M3MESG( MESG )
            MESG = NF_STRERROR( ISTAT )
            CALL M3MESG( MESG )
            EFLAG = .TRUE.
            GO TO 999
        END IF          !  ierr nonzero:  NF_PUT_VARA_DOUBLE() failed


999     CONTINUE        !!  close FNAME and return

        WRITEFIDSTEP4DD = ( .NOT. EFLAG )
        RETURN

    END FUNCTION WRITEFIDSTEP4DD



END MODULE MODNCFIO    !! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--==-=-

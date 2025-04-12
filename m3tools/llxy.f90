
SUBROUTINE LLXY( X, Y )

    !!-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
    !!  Version "$Id: llxy.f90 280 2025-04-12 15:34:39Z coats $"
    !!  (C) 2025 Carlie J. Coats, Jr.,
    !!  Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2
    !!  See file "LGPL.txt" for conditions of use.
    !!..................................................................
    !!
    !!  DESCRIPTION:
    !!      Transform the coordinate-pair X,Y or coordinate-arrays pair XX(N),YY(N)
    !!      from Lat-Lon to the coordinate system specified in ENTRY LLXYINIT()
    !!
    !!      Version    4/2025 by CJC for I/O API M3Tools version 4.0
    !!*******************************************************************

    USE M3UTILIO
    USE MODGCTP

    IMPLICIT NONE

    !!......  Arguments:

    REAL,    INTENT( INOUT ) :: X, Y
    REAL,    INTENT( IN    ) :: X1, X2, Y1, Y2
    REAL,    INTENT(   OUT ) :: LON1, LON2, LAT1, LAT2
    INTEGER, INTENT( IN    ) :: GDTYP
    REAL*8,  INTENT( IN    ) :: P_ALP, P_BET, P_GAM, XCENT, YCENT

    REAL*8  XD, YD, LATD, LOND
    REAL*8  XLL, YLL
    REAL*8  DLON1, DLON2, DLON3, DLON4
    REAL*8  DLAT1, DLAT2, DLAT3, DLAT4

    INTEGER, SAVE :: GDTYP1      ! grid type:  1=LAT-LON, 2=UTM, ...
    REAL*8,  SAVE :: P_ALP1      ! first, second, third map
    REAL*8,  SAVE :: P_BET1      ! projection descriptive
    REAL*8,  SAVE :: P_GAM1      ! parameters.
    REAL*8,  SAVE :: XCENT1      ! lon for coord-system X=0
    REAL*8,  SAVE :: YCENT1      ! lat for coord-system Y=0
    LOGICAL, SAVE :: INITFLAG = .FALSE.

    !!....................  body  .............................

    IF ( .NOT.INITFLAG ) THEN
        CALL M3EXIT( 'LLXY', 0, 0, 'Must call INITXY first', 2 )
    ELSE IF ( GDTYP1 .EQ. LATGRD3 )  THEN
        RETURN
    ELSE

        LATD = DBLE( Y )
        LOND = DBLE( X )

        CALL XY2XY( LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LOND, LATD, XD, YD )

        X = REAL( XD )
        Y = REAL( YD )

    END IF

    RETURN


ENTRY XYLL( X, Y )

    IF ( .NOT.INITFLAG ) THEN
        CALL M3EXIT( 'XYLL', 0, 0, 'Must call INITXY first', 2 )
    ELSE IF ( GDTYP1 .EQ. LATGRD3 )  THEN
        RETURN
    ELSE

        XD = DBLE( X )
        YD = DBLE( Y )

        CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    XD, YD, LOND, LATD )

        X = REAL( LOND )
        Y = REAL( LATD )

    END IF

    RETURN


ENTRY LLBOX( X1, X2, Y1, Y2, LON1, LON2, LAT1, LAT2 )

    IF ( .NOT.INITFLAG ) THEN
        CALL M3EXIT( 'LLBOX', 0, 0, 'Must call INITXY first', 2 )
    ELSE IF ( GDTYP1 .EQ. LATGRD3 )  THEN
        RETURN
    ELSE

        XD = DBLE( X1 )     !! ll corner
        YD = DBLE( Y1 )

        CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    XD, YD, DLON1, DLAT1 )

        XD = DBLE( X2 )     !! lr corner
        YD = DBLE( Y1 )

        CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    XD, YD, DLON2, DLAT2 )

        XD = DBLE( X2 )     !!  ur corner
        YD = DBLE( Y2 )

        CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    XD, YD, DLON3, DLAT3 )

        XD = DBLE( X1 )     !!  ul corner
        YD = DBLE( Y1 )

        CALL XY2XY( GDTYP1,  P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,    &
                    LATGRD3, 0.0D0,  0.0D0,  0.0D0,  0.0D0, 0.0D0,      &
                    XD, YD, DLON4, DLAT4 )

        LAT1 = REAL( MIN( DLAT1, DLAT2, DLAT3, DLAT4 ) )
        LAT2 = REAL( MAX( DLAT1, DLAT2, DLAT3, DLAT4 ) )
        LON1 = REAL( MIN( DLON1, DLON2, DLON3, DLON4 ) )
        LON2 = REAL( MAX( DLON1, DLON2, DLON3, DLON4 ) )

    END IF

    RETURN

    !!....................  body of ENTRY LLXYINIT ............

ENTRY INITXY( GDTYP, P_ALP, P_BET, P_GAM, XCENT, YCENT )

    GDTYP1 = GDTYP
    P_ALP1 = P_ALP
    P_BET1 = P_BET
    P_GAM1 = P_GAM
    XCENT1 = XCENT
    YCENT1 = YCENT
    INITFLAG = .TRUE.

    RETURN

END SUBROUTINE LLXY


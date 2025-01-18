
REAL    FUNCTION  POLY (XPT, XPTS, YPTS, NDEG)

    !***********************************************************************
    ! Version "$Id: poly.f90 203 2021-10-14 18:02:11Z coats $"
    ! EDSS/Models-3 I/O API.
    ! Copyright (C) 1992-2002 MCNC and Carlie J. Coats, Jr.,
    ! (C) 2003-2010 by Baron Advanced Meteorological Systems,
    ! (C) 2021 Carlie J. Coats, Jr.
    ! Distributed under the GNU LESSER GENERAL PUBLIC LICENSE version 2.1
    ! See file "LGPL.txt" for conditions of use.
    !.........................................................................
    !  function body starts at line  64
    !
    !  FUNCTION:
    !        Performs arbitrary-degree polynomial interpolation for XPT on
    !        curve determined by XPTS and YPTS using Newton divided-differences.
    !  NOTE:  high-order purely polynomial interpolations have stability
    !         problems.  NDEG <= 5 is recommended. -- CJC
    !
    !  ARGUMENT LIST DESCRIPTION:
    !
    !    Input arguments:
    !        :XPT       point on curve whose value is to be determined
    !        :XPTS      points at which function values are known
    !                   (there are NDEG + 1 values necessary)
    !        :YPTS      function values at each of XPTS
    !        :NDEG      degree of polynomial
    !
    !    Function Value:  POLY      interpolated value at XPT
    !
    !  REVISION HISTORY:
    !
    !       Modified 11/1988 for ROMNET by ??
    !       Modified ??/1990 for ROM 2.2 by CJC:  scalar coefficient arithmetic --
    !            no restrictions on NDEG
    !       Modified 04/1991 by CJC:  optimized initialization
    !       Modified 03/2010 by CJC: F9x changes for I/O API v3.1
    !       Version  10/2021 by CJC:  free ".f90" source format for IOAPI-4.0
    !
    !***********************************************************************

    IMPLICIT NONE

    !...........   ARGUMENTS:

    INTEGER, INTENT(IN   ) :: NDEG
    REAL   , INTENT(IN   ) :: XPT
    REAL   , INTENT(IN   ) :: XPTS ( NDEG + 1 )
    REAL   , INTENT(IN   ) :: YPTS ( NDEG + 1 )


    !...........   LOCAL VARIABLES:

    INTEGER         I, J, K
    REAL            TDIFF, COEFF
    REAL            DSCR, XSCR, YSCR


    !........................................................................
    !.......   begin body of POLY

    !.......   Compute divided differences: denominator is a product for J <> K.
    !.......   Initialization uses unrolled degree I=1 (linear interpolation terms)

    XSCR  =  XPTS ( 1 )
    COEFF =  XPT  -  XSCR
    DSCR  =  COEFF / ( XSCR  -  XPTS( 2 ) )
    YSCR  =  YPTS ( 1 )
    POLY  =  YSCR  +  DSCR * ( YSCR  -  YPTS( 2 ) )


    !.......   Now compute higher order terms using divided differences:
    !.......   denom is a product for J <> K.

    DO  144  I = 2, NDEG

        !.......   Initialization uses unrolled K=1 case.

        XSCR = XPTS ( 1 )
        DSCR = XSCR - XPTS ( 2 )

        DO  100  J = 3 , I + 1
            DSCR = DSCR * ( XSCR - XPTS ( J ) )
100     CONTINUE

        TDIFF  =  YPTS ( 1 ) / DSCR

        DO  133  K = 2, I + 1       !  loop on points K

            XSCR = XPTS ( K )
            DSCR = XSCR - XPTS ( 1 )

            DO  111  J = 2 , K - 1
                DSCR = DSCR * ( XSCR - XPTS ( J ) )
111         CONTINUE                ! end loop: j not k, part 1

            DO  122  J = K + 1 , I + 1
                DSCR = DSCR * ( XSCR - XPTS ( J ) )
122         CONTINUE                ! end loop: j not k, part 2


            !...........   Compute differences term:

            TDIFF  =  TDIFF  +  YPTS ( K ) / DSCR

133     CONTINUE            !  end loop on points K


        !...........   Compute polynomial coefficients:

        COEFF  =  COEFF * ( XPT  -  XPTS ( I ) )

        !...........   Compute interpolated value

        POLY = POLY  +  COEFF * TDIFF

144 CONTINUE        !  end loop on terms of degree I


    RETURN
END FUNCTION POLY


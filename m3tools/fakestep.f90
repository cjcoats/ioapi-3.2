
SUBROUTINE  FAKESTEP( FNAME, JDATE, JTIME, OPS, VAL )

    !!***********************************************************************
    !! Version "$Id: fakestep.f90 101 2015-01-16 16:52:50Z coats $"
    !! EDSS/Models-3 M3TOOLS.
    !! Copyright (C) 1992-2002 MCNC,
    !! (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
    !! (C) 2002-2010 Baron Advanced Meteorological Systems. LLC., and
    !! (C) 2015 UNC Institute for the Environment
    !! Distributed under the GNU GENERAL PUBLIC LICENSE version 2
    !! See file "GPL.txt" for conditions of use.
    !!.........................................................................
    !!  subroutine body starts at line  70
    !!
    !!  FUNCTION:  Perform memory allocation and work for program FAKEFILE
    !!
    !!
    !!  PRECONDITIONS REQUIRED:
    !!       file FNAME already open;
    !!       description of FNAME in FDESC3.EXT valid.
    !!
    !!  SUBROUTINES AND FUNCTIONS CALLED:
    !!       IFILL, RFILL, DFILL
    !!
    !!  REVISION  HISTORY:
    !!      prototype 8/1995 by CJC
    !!      Modified  9/1999 by CJC for enhanced portability
    !!      Modified 11/2005 by CJC:  removed unused vbles
    !!
    !!      Version 02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
    !!      USE M3UTILIO, and related changes.
    !!
    !!      Version  01/2015 by CJC for I/O API v3.2:  F90 free-format source
    !!***********************************************************************

    USE M3UTILIO

    IMPLICIT NONE

    !!...........   ARGUMENTS and their descriptions:

    CHARACTER*16, INTENT(IN   ) :: FNAME
    INTEGER     , INTENT(IN   ) :: JDATE
    INTEGER     , INTENT(IN   ) :: JTIME
    INTEGER     , INTENT(IN   ) :: OPS( NVARS3D )
    REAL        , INTENT(INOUT) :: VAL( NVARS3D )


    !!...........   LOCAL VARIABLES and their descriptions:
    !!...........   NOTE:  the ANSI standard requires the use of SAVE statements
    !!...........   for variables which must retain their values from call to call.

    INTEGER, SAVE :: STEP = 0

    !!...........   SCRATCH LOCAL VARIABLES and their descriptions:

    INTEGER         NCOLS, NROWS, NLAYS
    REAL            X
    REAL            GRID( NCOLS3D, NROWS3D, 2*NLAYS3D )
    CHARACTER*80    MESG
    INTEGER         C, R, L, V
    INTEGER         TYPE, OP, IDEV
    INTEGER         LTYPE, LOP


    !!***********************************************************************
    !!   begin body of subroutine  FAKESTEP

    IF( FTYPE3D .EQ. CUSTOM3 ) THEN
        NCOLS = NCOLS3D
        NROWS = 1
        NLAYS = NLAYS3D
    ELSE IF ( FTYPE3D .EQ. GRDDED3 ) THEN
        NCOLS = NCOLS3D
        NROWS = NROWS3D
        NLAYS = NLAYS3D
    ELSE IF ( FTYPE3D .EQ. BNDARY3 ) THEN
        NCOLS = NTHIK3D
        NROWS = 2 * ( NCOLS3D + NROWS3D + 2 * NTHIK3D )
        NLAYS = NLAYS3D
    ELSE
        WRITE( MESG, '( A, I4, 2X, A )' )  'File type', FTYPE3D, 'not yet supported'
        CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
    END IF

    STEP  = STEP + 1
    LTYPE = -9999
    LOP   = -9999

    DO  V = 1, NVARS3D

        TYPE = VTYPE3D( V )
        OP   = OPS( V )

        IF ( OP .LT. 0 ) THEN

            IDEV = -OP
            DO  L = 1, NLAYS3D
            DO  R = 1, NROWS3D
                READ( IDEV,* ) (GRID( C,R,L ), C = 1, NCOLS3D )
            END DO
            END DO

        ELSE IF ( TYPE .NE. LTYPE  .OR.     &
                  LOP  .NE. OP     .OR.     &
                  TYPE .EQ. 5         ) THEN

            IF ( OP .EQ. 4 ) THEN
                VAL( V ) = FLOAT( STEP )
            END IF
            X = VAL( V )
            IF ( TYPE .EQ. M3INT ) THEN
                CALL IFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL(V) )
            ELSE IF ( TYPE .EQ. M3REAL ) THEN
                CALL RFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL(V) )
            ELSE IF ( TYPE .EQ. M3DBLE ) THEN
                CALL DFILL( GRID, NCOLS, NROWS, NLAYS, OP, VAL(V) )
            END IF
            LTYPE = TYPE
            LOP   = OP

        END IF

        IF( .NOT. WRITE3( FNAME, VNAME3D( V ), JDATE, JTIME, GRID ) ) THEN
            MESG = 'Error writing ' // VNAME3D( V ) // ' to ' // FNAME
            CALL M3EXIT( 'FAKESTEP', JDATE, JTIME, MESG, 2 )
        END IF

    END DO

    RETURN


END SUBROUTINE  FAKESTEP


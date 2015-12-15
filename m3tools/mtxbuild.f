
        PROGRAM MTXBUILD

C***********************************************************************
C Version "$Id: mtxbuild.f 158 2015-02-16 19:51:12Z coats $"
C EDSS/Models-3 M3TOOLS.
C Copyright (C) 1992-2002 MCNC, (C) 1995-2002,2005-2013 Carlie J. Coats, Jr.,
C and (C) 2002-2010 Baron Advanced Meteorological Systems. LLC.
C Distributed under the GNU GENERAL PUBLIC LICENSE version 2
C See file "GPL.txt" for conditions of use.
C.........................................................................
C  program body starts at line  104
C
C  DESCRIPTION:
C       Builds a sparse (grid-to-grid transform) matrix from data contained
C       in a "fractions" file.
C
C  PRECONDITIONS REQUIRED:
C       setenv <logical name> <physical (path) name> for the input,
C       output, and GRIDDESC files.
C       "fractions" file consists of list-formatted (whitespace-delimited)
C       lines with the following fields:
C
C           output grid row number
C           output grid col number
C           input grid row number
C           input grid col number
C           fraction:  AREA(input-cell intersect output-cell)
C              divided by AREA(input-cell)
C
C       These lines should  be sorted into lexicographic order
C       in terms of the first four fields.
C
C  SUBROUTINES AND FUNCTIONS CALLED:
C       I/O API
C
C  REVISION  HISTORY:
C       Prototype 9/2000 by Carlie J. Coats, Jr.,
C       MCNC Environmental Programs
C       Version 11/2001 by CJC for I/O API Version 2.1
C       Version  11/2005 by CJC:  eliminate unused vbles
C       Version   8/2008 by CJC:  USE M3UTILIO, MODATTS3 to put
C       grid-attributes into matrix.
C       Version  02/2010 by CJC for I/O API v3.1:  Fortran-90 only;
C       USE M3UTILIO, and related changes.
C***********************************************************************

      USE M3UTILIO
      USE MODATTS3

      IMPLICIT NONE

C...........   PARAMETERS and their descriptions:

       CHARACTER*16, PARAMETER :: PNAME = 'MTXBUILD'

C...........   LOCAL VARIABLES and their descriptions:

        CHARACTER*16    MNAME   !  input  matrix   file logical name
        CHARACTER*16    CNAME   !  output coordinate system name
        CHARACTER*256   LINE    !  scratch input-line buffer
        LOGICAL         EFLAG
        CHARACTER*256   MESG

        INTEGER         FDEV        !  fractions-file unit number
        INTEGER         LDEV        !  log-device unit number
        INTEGER         ISTAT       !  allocation-status
        INTEGER         NCOL1       !  number of  input-grid rows
        INTEGER         NCOL2       !  number of output-grid rows
        INTEGER         MROWS       !  number of sparse-matrix rows
        INTEGER         MCOEF       !  number of sparse-matrix coeffs
        INTEGER         V, L, N     !  loop counters

        CHARACTER*16    IGRID   !  GRIDDESC name, parameters for  input grid
        INTEGER         NCOLS1
        INTEGER         NROWS1
        INTEGER         NTHIK1
        INTEGER         GDTYP1
        REAL*8          P_ALP1      ! first, second, third map
        REAL*8          P_BET1      ! projection descriptive
        REAL*8          P_GAM1      ! parameters.
        REAL*8          XCENT1      ! lon for coord-system X=0
        REAL*8          YCENT1      ! lat for coord-system Y=0
        REAL*8          XORIG1      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG1      ! Y-coordinate origin of grid
        REAL*8          XCELL1      ! X-coordinate cell dimension
        REAL*8          YCELL1      ! Y-coordinate cell dimension

        CHARACTER*16    OGRID   !  GRIDDESC name, parameter for output grid
        INTEGER         NCOLS2
        INTEGER         NROWS2
        INTEGER         NTHIK2
        INTEGER         GDTYP2
        REAL*8          P_ALP2      ! first, second, third map
        REAL*8          P_BET2      ! projection descriptive
        REAL*8          P_GAM2      ! parameters.
        REAL*8          XCENT2      ! lon for coord-system X=0
        REAL*8          YCENT2      ! lat for coord-system Y=0
        REAL*8          XORIG2      ! X-coordinate origin of grid (map units)
        REAL*8          YORIG2      ! Y-coordinate origin of grid
        REAL*8          XCELL2      ! X-coordinate cell dimension
        REAL*8          YCELL2      ! Y-coordinate cell dimension

        REAL,    ALLOCATABLE::   CBUF( : )


C***********************************************************************
C   begin body of program MTXBUILD

        LDEV  = INIT3()
        EFLAG = .FALSE.
        WRITE( *, '( 5X, A )' )
     &' ',
     &'Program MTXBUILD to build a sparse (grid-to-grid transform)',
     &'file from an ASCII "fractions" file with lines having the',
     &'following fields',
     &' ',
     &'    output grid row number',
     &'    output grid col number',
     &'    input grid row number',
     &'    input grid col number',
     &'    fraction:  AREA(input-cell intersect output-cell)',
     &'             / AREA(input-cell)',
     &'These lines should  be sorted into lexicographic order',
     &'relative to the first four fields.',
     &' ',
     &'See URL',
     &'https://www.cmascenter.org/ioapi/documentation/3.1/html#tools',
     &' ',
     &'Program copyright (C) 1992-2002 MCNC, (C) 1995-2013',
     &'Carlie J. Coats, Jr., and (C) 2002-2010 Baron Advanced',
     &'Meteorological Systems, LLC.  Released under Version 2',
     &'of the GNU General Public License. See enclosed GPL.txt, or',
     &'URL http://www.gnu.org/copyleft/gpl.html',
     &' ',
     &'Comments and questions are welcome and can be sent to',
     &' ',
     &'    Carlie J. Coats, Jr.    cjcoats@email.unc.edu',
     &'    UNC Institute for the Environment',
     &'    100 Europa Dr., Suite 490 Rm 405',
     &'    Campus Box 1105',
     &'    Chapel Hill, NC 27599-1105',
     &' ',
     &'Program version: ',
     &'$Id:: mtxbuild.f 158 2015-02-16 19:51:12Z coats               $',
     &' '

        IF ( .NOT. GETYN( 'Continue with program?', .TRUE. ) ) THEN
            CALL M3EXIT( PNAME, 0, 0,
     &                   'Program terminated at user request', 2 )
        END IF


C...............  Open and "count" input "fractions" file

        MESG = 'Enter logical name for input "fractions" file'
        FDEV = PROMPTFFILE( MESG, .TRUE., .TRUE.,
     &                      'FRACTIONS', PNAME )

        L = 0
        N = 0
        IGRID = ' '
        OGRID = ' '
11      CONTINUE                !  head of "count fractions" loop

            READ( FDEV, '( A )', END = 22, IOSTAT=ISTAT ) LINE
            L = L + 1
            IF ( ISTAT .NE. 0 ) THEN
                WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', ISTAT,
     &              'encountered reading line', N,
     &              'of FRACTIONS file'
                CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
            IF ( LINE( 1:1 ) .NE. '#' ) THEN
                N = N + 1
            ELSE IF ( LINE( 1:7 ) .EQ. '#INGRID'  ) THEN
                V     = 8 + LBLANK( LINE( 8:256 ) )
                IGRID = TRIM( LINE( V:256 ) )
            ELSE IF ( LINE( 1:7 ) .EQ. '#OUTGRID' ) THEN
                V     = 8 + LBLANK( LINE( 8:256 ) )
                OGRID = TRIM( LINE( V:256 ) )
            END IF
            GO TO  11           !  to head of "count fractions" loop

22      CONTINUE                !  exit from "count fractions" loop
        MCOEF = N
        REWIND( FDEV )


C...............  Get output grid description

        IF ( IGRID .EQ. ' ' ) THEN
            CALL GETSTR( 'Enter  input grid name', 'FOO', IGRID )
        ELSE
            MESG = 'Input grid "' // TRIM( IGRID ) // '"'
            CALL M3MSG2( MESG )
        END IF

        IF ( OGRID .EQ. ' ' ) THEN
            CALL GETSTR( 'Enter output grid name', 'BAR', OGRID )
        ELSE
            MESG = 'Output grid "' // TRIM( OGRID ) // '"'
            CALL M3MSG2( MESG )
        END IF

        IF ( .NOT. DSCGRID( IGRID, CNAME, GDTYP1,
     &              P_ALP1, P_BET1,P_GAM1, XCENT1, YCENT1,
     &              XORIG1, YORIG1, XCELL1, YCELL1,
     &              NCOLS1, NROWS1, NTHIK1 ) ) THEN

            MESG   = '"' // TRIM( IGRID ) //
     &               '" not found in GRIDDESC file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        END IF
        NCOL1 = NCOLS3D

        IF ( .NOT. DSCGRID( OGRID, CNAME, GDTYP2,
     &              P_ALP2, P_BET2,P_GAM2, XCENT2, YCENT2,
     &              XORIG2, YORIG2, XCELL2, YCELL2,
     &              NCOLS2, NROWS2, NTHIK2 ) ) THEN

            MESG   = '"' // TRIM( OGRID ) //
     &               '" not found in GRIDDESC file'
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )

        END IF

        NCOL2 = NCOLS3D
        MROWS = NCOLS3D * NROWS3D

        GDNAM3D = OGRID

        ALLOCATE( CBUF( MROWS + 2 * MCOEF + 1 ), STAT = ISTAT )

        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '( A, I10 )' )
     &               'Buffer allocation failed:  STAT=', ISTAT
            CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF


C...............  Build output sparse matrix file

        FTYPE3D = SMATRX3
        GDTYP3D = GDTYP2
        GDNAM3D = OGRID
        P_ALP3D = P_ALP2
        P_BET3D = P_BET2
        P_GAM3D = P_GAM2
        XCENT3D = XCENT2
        YCENT3D = YCENT2
        XORIG3D = XORIG2
        YORIG3D = YORIG2
        XCELL3D = XCELL2
        YCELL3D = YCELL2
        NROWS3D = MROWS
        NCOLS3D = MCOEF
        NLAYS3D = 1
        NTHIK3D = 1
        NVARS3D = 1
        VGTYP3D = IMISS3
        VGTOP3D = BADVAL3
        VGLVS3D( 1 ) = BADVAL3
        VGLVS3D( 2 ) = BADVAL3

        SDATE3D = 0
        STIME3D = 0
        TSTEP3D = 0

        VNAME3D( 1 ) = 'COEF'
        VTYPE3D( 1 ) = M3REAL
        UNITS3D( 1 ) = 'n/a'
        VDESC3D( 1 ) = 'Sparse matrix coefficient'
        FDESC3D( 1 ) = 'Sparse transform matrix'
        FDESC3D( 2 ) = '#INGRID  ' // IGRID
        FDESC3D( 3 ) = '#OUTGRID ' // OGRID
        DO  N = 4, MXDESC3
            FDESC3D( N ) = ' '
        END DO

        MESG = 'Enter name for output SPARSE MATRIX file'
        MNAME = PROMPTMFILE( MESG, FSUNKN3,
     &                       'MATRIX_FILE', PNAME )



C...............  Read "fractions" file and write out the sparse matrix:

        CALL RDFRAC( FDEV, MCOEF, MROWS, NCOL1, NCOL2,
     &               CBUF,                      !  "NX" for sparse matrix
     &               CBUF( MROWS+1 ),           !  "IX"
     &               CBUF( MROWS+MCOEF+1 ) )    !  "CX"

        IF ( .NOT. SETMTXATT( MNAME, 
     &                      IGRID, GDTYP1,
     &                      P_ALP1, P_BET1, P_GAM1, XCENT1, YCENT1,
     &                      XORIG1, YORIG1, XCELL1, YCELL1,
     &                      NCOLS1, NROWS1,
     &                      OGRID, GDTYP2,
     &                      P_ALP2, P_BET2, P_GAM2, XCENT2, YCENT2,
     &                      XORIG2, YORIG2, XCELL2, YCELL2,
     &                      NCOLS2, NROWS2 ) ) THEN
            EFLAG = .TRUE.
            MESG  = 'Could not set grid descriptions for '//MNAME
            CALL M3MESG( MESG )

        ELSE IF ( .NOT.WRITE3( MNAME, 'ALL', 0, 0, CBUF ) ) THEN

            EFLAG = .TRUE.
            MESG  = 'Could not write out sparse matrix'
            CALL M3MESG( MESG )

        END IF


C...............  Shut down program:

        IF ( EFLAG ) THEN
            MESG  = 'Failure in program MTXBUILD'
            ISTAT = 2
        ELSE
            MESG  = 'Success in program MTXBUILD'
            ISTAT = 0
        END IF
        CALL M3EXIT( PNAME, 0, 0, MESG, ISTAT )


        END PROGRAM MTXBUILD


C===========================================================================

        SUBROUTINE  RDFRAC( FDEV, MCOLS, MROWS, NCOL1, NCOL2,
     &                      NX, IX, CX )

        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !!  DESCRIPTION
        !!      Read the input fractions-file with unit number FDEV
        !!      into the indicated sparse matrix.
        !!  PRECONDITIONS
        !!      FDEV already open
        !!      FDEV consists of list-formatted lines sorted into
        !!      lexicographic order by the first four fields
        !!      The fields are:
        !!          input grid row number
        !!          input grid col number
        !!          output grid row number
        !!          output grid col number
        !!          fraction:    AREA(input-cell intersect output-cell)
        !!                 / AREA(input-cell)
        !!
        !!  HISTORY
        !!      Prototype 9/2000 by Carlie J. Coats, Jr., MCNC-EP
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IMPLICIT NONE

        !!  Arguments:

        INTEGER, INTENT(IN) ::   FDEV        !  fractions-file unit number
        INTEGER, INTENT(IN) ::   MCOLS
        INTEGER, INTENT(IN) ::   MROWS
        INTEGER, INTENT(IN) ::   NCOL1
        INTEGER, INTENT(IN) ::   NCOL2
        INTEGER, INTENT(OUT)::   NX( MROWS )
        INTEGER, INTENT(OUT)::   IX( MCOLS )
        REAL,    INTENT(OUT)::   CX( MCOLS )

        CHARACTER*24, PARAMETER :: PNAME = 'MTXBUILD/RDFRAC'

        !!  Local Variables:

        INTEGER         L, J, N, R, ROW
        INTEGER         ISTAT
        INTEGER         ICOL, IROW, JCOL, JROW
        REAL            FRAC
        CHARACTER*256   LINE, MESG

        !!..................  body of RDFRAC  ..............................

        DO R = 1, MROWS
            NX( R ) = 0
        END DO

        DO R = 1, MCOLS
            IX( R ) = 0
            CX( R ) = 0.0
        END DO

        L = 0
        J = 0


111     CONTINUE    !  loop:  skip header lines

            L = L + 1
            READ( FDEV, '( A )', IOSTAT=ISTAT ) LINE
            IF ( ISTAT .NE. 0 ) THEN
                 WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &              'I/O error', ISTAT,
     &              'encountered reading line', L,
     &              'of FRACTIONS file'
                 CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
            END IF
            IF ( LINE( 1:1 ) .EQ. '#' ) GO TO 111

        READ( LINE, *, IOSTAT=ISTAT )
     &          IROW, ICOL, JROW, JCOL, FRAC
        IF ( ISTAT .NE. 0 ) THEN
            WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &          'I/O error', ISTAT,
     &          'encountered parsing line', L,
     &          'of FRACTIONS file'
             CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
        END IF
        ROW = ICOL + ( IROW - 1 )*NCOL2

        DO  R = 1, MROWS    ! loop on matrix rows

            N = 0

122         CONTINUE        !  loop reading matrix coeffs for this row

                IF ( ROW .GT. R ) GO TO 133

                N = N + 1
                J = J + 1
                IX( J ) = JCOL + ( JROW - 1 )*NCOL1
                CX( J ) = FRAC

                L = L + 1
                READ( FDEV, '( A )', END=133, IOSTAT=ISTAT ) LINE
                IF ( ISTAT .NE. 0 ) THEN
                    WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &                  'I/O error', ISTAT,
     &                 'encountered reading line', L,
     &                  'of FRACTIONS file'
                     CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF
                READ( LINE, *, IOSTAT=ISTAT )
     &                  IROW, ICOL, JROW, JCOL, FRAC

                IF ( ISTAT .NE. 0 ) THEN
                    WRITE( MESG, '(A, I9, 2X, A, I9, 2X, A )' )
     &                  'I/O error', ISTAT,
     &                 'encountered parsing line', L,
     &                  'of FRACTIONS file'
                     CALL M3EXIT( PNAME, 0, 0, MESG, 2 )
                END IF
                ROW = ICOL + ( IROW - 1 )*NCOL2

                GO TO 122       !  to head of loop reading matrix coeffs

133         CONTINUE            !  exit from loop reading matrix coeffs

            NX( R ) = N

        END DO          !  end loop on matrix rows

        RETURN

        END SUBROUTINE RDFRAC

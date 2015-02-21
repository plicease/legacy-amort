C
C++******** AMORTORIZATION SCHEDULE **********
C
      PROGRAM AMORT
C
      IMPLICIT NONE
      REAL*4 aloan,int,mint,tpmt,rloan,ppmt,totint,intpmt,resid
      Integer*2 N
C
C  Read amount of loan 
      PRINT *, ' Amount of loan?  '
      PRINT *
      READ *, aloan
99    PRINT *, ' Interest rate?   '
      READ *, int
      mint = int/12.
      PRINT *, ' Monthly payment  '
      READ *, tpmt
C
      rloan = aloan
      OPEN (UNIT=1,FILE='schedule',STATUS='NEW')
      WRITE (1,200)aloan,int,tpmt
200   format ('loan = ',F9.2,'   int. rate = ',F7.4,'   pmt = ',F7.2)
      WRITE (1,*)'    Payment      Interest    Principal  Balance '   
C
      totint = 0
      N = 0
C
10    intpmt = rloan*mint
      totint = totint + intpmt
      ppmt = tpmt - intpmt
      rloan = rloan - ppmt
      N = N + 1
      WRITE (1,100)N,tpmt,intpmt,ppmt,rloan
100   format (I3,2X,F7.2,5X,F7.2,5X,F7.2,5X,F9.2)
      if (rloan.LE.tpmt)go to 20
      go to 10
C
20    intpmt = rloan*mint
      totint = totint + intpmt
      ppmt = rloan + intpmt
      resid = (rloan + intpmt) - tpmt
      N = N + 1
      resid = 0.0
      WRITE (1,100)N,ppmt,intpmt,ppmt,resid
      WRITE (1,*)' total accumulated interest is '
      WRITE (1,300)totint
300   FORMAT (F10.2)
      CLOSE (UNIT=1)

      STOP
      END


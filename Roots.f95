PROGRAM Roots
       INTEGER COUNT, FLAT, ITRATE, LIMIT, MAXITER, N00FIT, SOLVED, STATUS
       REAL DX, F, FDASH, FX, FDX, LASTX, NEWX, OLDX, X0, RESID, ROOT, TINY, TOL
       PARAMETER (TINY=1.E-10)
       PARAMETER (ITRATE = -1, SOLVED = 0, LIMIT = 1, FLAT = 2)
       PRINT*, 'Input the initial estimated root.'
       READ*, X0
       PRINT*, 'Input the maximum number of allowable iterations.'
       READ*, MAXITER
       PRINT*, 'Input the tolerance.'
       READ*, TOL
       NEWX = X0
       STATUS = ITRATE
       DO 10 COUNT = 1, MAXITER
          FDX = FDASH(NEWX)
          IF (ABS(FDX) .LE. TINY) THEN
             STATUS = FLAT
             PRINT*, 'Function is flat in a neighborhood of the root.'
             GO TO 12
          ELSE
             OLDX = NEWX
             FX = F(OLDX)
             DX = -FX/FDX
             NEWX = OLDX + DX
             IF (ABS(DX) .LE. ABS(OLDX)*TOL) STATUS = SOLVED
          END IF
       IF (STATUS .NE. ITRATE) GO TO 11
10    CONTINUE
       STATUS = LIMIT
       PRINT*, 'Convergence was not achieved in the maximum allowed iterations.'
11    IF (STATUS .EQ. SOLVED) THEN
          N00FIT = COUNT
          ROOT = NEWX
          RESID = F(ROOT)
          PRINT*, 'The number of iterations needed was: ', N00FIT, '.'
          PRINT*, 'The root (approximate) is: ', ROOT, '.'
          PRINT*, 'The residual value at the root is: ', RESID, '.'
          PRINT*, 'The tolerance value used for convergence was: ', TOL, '.'
       END IF
12    IF (STATUS .EQ. LIMIT .OR. STATUS .EQ. FLAT) THEN
          LASTX = NEWX
          PRINT*, 'The final approximation to the root was: ', LASTX, '.'
       END IF
       END
       REAL FUNCTION F(Y)
       F = Y - EXP(1/Y)
       RETURN
       END
       REAL FUNCTION FDASH(Y)
       FDASH = 1 + (EXP(1/Y)/(Y**2))
       RETURN
       END

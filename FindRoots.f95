! Written by Royce Korogodsky
! 3/2/15
! Program that uses Newton Raphson method to find roots of a number
SUBROUTINE POW(X,T,Z,Y)
	INTEGER, INTENT(IN) :: X
	REAL, INTENT(IN)    :: T
	REAL, INTENT(OUT)   :: Z,Y
	INTEGER             :: I,ONE
	ONE = 1
	
	I = ONE
	Z = ONE
	
	! Get's X^(n-1)
	DO WHILE (I < X)
		Z = Z * T
		I = I + ONE
	END DO
	!Get's X^n
	Y = Z * T
END SUBROUTINE

SUBROUTINE NEWT(X,G,N,C,FIN)
	REAL, INTENT(IN)     :: G,N
	INTEGER, INTENT(IN)  :: X
	REAL, INTENT(OUT)    :: FIN
	INTEGER, INTENT(OUT) :: C
	REAL                 :: T,TE,Y,Z,LIMIT,V
	LOGICAL              :: CONT
	INTEGER              :: ONE, I
	T = G
	TE = G
	LIMIT = .0005
	ONE = 1
	FIN = 0
	C = 0
	CONT = .TRUE.
	DO WHILE(CONT .eqv. .TRUE.)
		CALL POW(X,T,Z,Y)
		!Newton Raphsons Method for finding roots
		T = (TE-((Y)-N)/(X*Z)) 
		
		!Holds the count for number of itterations
		C = C + ONE
		
		!Gets difference between new guess and old guess
		!If within .0005 end loop and return results
		!If user enters 1 as root return original number
		V = ABS(TE-T)
		IF (V <= LIMIT .or. X == ONE) THEN
			FIN = TE
			CONT = .FALSE.
    	END IF
	
		! Sets next guess
		TE = T
	END DO
END SUBROUTINE NEWT

Program FindRoots
	
	REAL N,G
	INTEGER X,C
	
	! Get number, root, and guess
	PRINT*,"Find a Root of a number using the Newton Raphson method."
	PRINT*,"What number would you like to find the root of? "
	READ*, N
	PRINT*,"What root would you like to find of this number? "
	READ*, X
	PRINT*,"Guess a number close to the desired root... "
	READ*, G
	

	CALL NEWT(X,G,N,C,FIN)
	
	PRINT*, FIN,  "is close to the root of your number with precision of .0005"
	PRINT*, C ,"guesses were used."
END PROGRAM FindRoots
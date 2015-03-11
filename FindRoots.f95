! Written by Royce Korogodsky
! 3/2/15
! Program that uses Newton Raphson method to find roots of a number
Program FindRoots
	
	REAL N,G,T,Y,V,Z,LIMIT
	INTEGER X,C,I,ONE
	LOGICAL CONT
	
	! Get number, root, and guess
	PRINT*,"Find a Root of a number using the Newton Raphson method."
	PRINT*,"What number would you like to find the root of? "
	READ*, N
	PRINT*,"What root would you like to find of this number? "
	READ*, X
	PRINT*,"Guess a number close to the desired root... "
	READ*, G
	
	! Sets the temp value to the same real as first guess
	T = G
	
	LIMIT = .0005
	ONE = 1
	CONT = .TRUE.
	C = 0
	
	DO WHILE(CONT .eqv. .TRUE.)
		I = ONE
		Z = ONE
		
		! Get's X^(n-1)
		DO WHILE (I < X)
			Z = Z * T
			I = I + ONE
		END DO
		
		!Get's X^n
		Y = Z * T
		
		!Newton Raphsons Method for finding roots
		G = (T-((Y)-N)/(X*Z)) 
		
		!Holds the count for number of itterations
		C = C + ONE
		
		!Gets difference between new guess and old guess
		!If within .0005 end loop and return results
		V = ABS(T-G)
		IF (V <= LIMIT) THEN
			PRINT*, G,  "is close to the root of your number with precision of .0005"
			PRINT*, C ,"guesses were used."
			CONT = .FALSE.
    	END IF
	
		!If user enters 1 as root return original number
		IF (X == ONE) THEN
			PRINT*, N, "is the root."
			CONT = .FALSE.
		END IF
	
		! Sets next guess
		T = G
	END DO
END PROGRAM FindRoots
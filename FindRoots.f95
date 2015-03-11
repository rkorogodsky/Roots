! Written by Royce Korogodsky
! 3/2/15
! Program that uses Newton Raphson method to find roots of a number
Program FindRoots
	
	REAL N,G,T,Y,V,Z
	INTEGER X,C,I
	
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
	
	DO WHILE(N >= 0.0)
		I = 1
		Z = 1
		
		! Get's X^(n-1)
		DO WHILE (I < X)
			Z = Z * T
			I = I + 1
		END DO
		
		!Get's X^n
		Y = Z * T
		
		!Newton Raphsons Method for finding roots
		G = (T-((Y)-N)/(X*Z)) 
		
		!Holds the count for number of itterations
		C = C + 1
		
		!Gets difference between new guess and old guess
		!If within .0005 end loop and return results
		V = ABS(T-G)
		IF (V < .0005) THEN
			PRINT*, G,  " is close to the root of your number with precision of .0005"
			PRINT*, C ," guesses were used."
			N = -1
    	END IF
	
		!If user enters 1 as root return original number
		IF (X == 1) THEN
			PRINT*, N, "is the root."
			N = -1
		END IF
	
		! Sets next guess
		T = G
	END DO
END PROGRAM FindRoots
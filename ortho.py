import numpy as np
from scipy.linalg import orth # Construct an orthonormal basis for the range of A using SVD

A = np.array([[2, 0, 0], [0, 5, 0]])  # rank 2 array

#A = np.array([[ 8.79, 9.93, 9.83, 5.45, 3.16],[ 6.11, 6.91, 5.04,-0.27, 7.98],[-9.15,-7.93, 4.86, 4.85, 3.01],[ 9.57, 1.64, 8.83, 0.74, 5.80],[-3.49, 4.02, 9.80,10.00, 4.27],[ 9.84, 0.15,-8.99,-6.02,-5.31]])

#B=orth(A)

#B=orth(A.T)
np.set_printoptions(precision=2)

print("     A   =")
print(A           )

print("\n")
print("orth(A  )=")
print(orth(A)     )

print("\n")
print("orth(A.T)=")
print(orth(A.T)   )

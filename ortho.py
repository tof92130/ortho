import numpy as np
from scipy.linalg import orth # Construct an orthonormal basis for the range of A using SVD

#A = np.array([[2, 0, 0], [0, 5, 0]])  # rank 2 array

A = np.array([[ 8.79, 9.93, 9.83, 5.45, 3.16],
              [ 6.11, 6.91, 5.04,-0.27, 7.98],
              [-9.15,-7.93, 4.86, 4.85, 3.01],
              [ 9.57, 1.64, 8.83, 0.74, 5.80],
              [-3.49, 4.02, 9.80,10.00, 4.27],
              [ 9.84, 0.15,-8.99,-6.02,-5.31]]
            )

#B=orth(A)

#B=orth(A.T)
np.set_printoptions(precision=2)

print("     A   =")
print(A           )


print("\n")
print("orth(A  )=")
print(orth(A)     )

print("\n")
print("orth(A.T).T=")
print(orth(A.T).T)


# U  m=6  n=6
#   -0.59   0.26   0.36   0.31   0.23   0.55
#   -0.40   0.24  -0.22  -0.75  -0.36   0.18
#   -0.03  -0.60  -0.45   0.23  -0.31   0.54
#   -0.43   0.24  -0.69   0.33   0.16  -0.39
#   -0.47  -0.35   0.39   0.16  -0.52  -0.46
#    0.29   0.58  -0.02   0.38  -0.65   0.11

# orth(A  )=
#   -0.59   0.26   0.36   0.31   0.23
#   -0.4    0.24  -0.22  -0.75  -0.36
#   -0.03  -0.6   -0.45   0.23  -0.31
#   -0.43   0.24  -0.69   0.33   0.16
#   -0.47  -0.35   0.39   0.16  -0.52
#    0.29   0.58  -0.02   0.38  -0.65



# Sigma  m=6  n=5
#   27.47   0.00   0.00   0.00   0.00
#    0.00  22.64   0.00   0.00   0.00
#    0.00   0.00   8.56   0.00   0.00
#    0.00   0.00   0.00   5.99   0.00
#    0.00   0.00   0.00   0.00   2.01
#    0.00   0.00   0.00   0.00   0.00

 
# VT  m=5  n=5
#   -0.25  -0.40  -0.69  -0.37  -0.41
#    0.81   0.36  -0.25  -0.37  -0.10
#   -0.26   0.70  -0.22   0.39  -0.49
#    0.40  -0.45   0.25   0.43  -0.62
#   -0.22   0.14   0.59  -0.63  -0.44

#  orth(A.T).T=
#    0.25   0.4    0.69   0.37   0.41
#    0.81   0.36  -0.25  -0.37  -0.10
#   -0.26   0.7   -0.22   0.39  -0.49
#    0.4   -0.45   0.25   0.43  -0.62
#   -0.22   0.14   0.59  -0.63  -0.44
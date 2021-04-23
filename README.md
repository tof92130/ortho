ifort ortho.f90 -mkl -o ortho
gfortran-10 ortho.f90 -framework Accelerate -o  ortho

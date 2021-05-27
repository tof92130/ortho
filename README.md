ifort ortho.f90 -mkl -o ortho
gfortran-10 ortho.f90 -framework Accelerate -o  ortho


orthoComplex avec ifort

ifort orthoComplex.f90  -c  -O3

ifort ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -c -I${LIBMESH_DIR}/include -I${SPACE_BASIS_DIR}/include -I. -O3

ifort spaceIO.o orthoComplex.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -L${SPACE_BASIS_DIR}/lib -lspaceBasis  -mkl -o ortho


orthoComplex avec gfortran

gfortran-10 orthoComplex.f90  -c  -O3

gfortran-10 ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -c -I${LIBMESH_DIR}/include -I${SPACE_BASIS_DIR}/include -I. -O3  -ffree-line-length-256 -framework Accelerate


gfortran-10  spaceIO.o orthoComplex.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -L${SPACE_BASIS_DIR}/lib -lspaceBasis -O3  -ffree-line-length-256 -framework Accelerate -o ortho


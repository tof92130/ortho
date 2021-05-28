ifort ortho.f90 -mkl -o ortho
gfortran-10 ortho.f90 -framework Accelerate -o  ortho


ortho avec ifort


ifort ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -I${LIBMESH_DIR}/include -I${SPACE_BASIS_DIR}/include -I. -c -O3
ifort ortho.f90 -c -O3
ifort main.f90  -c -O3

ifort spaceIO.o ortho.o main.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -L${SPACE_BASIS_DIR}/lib -lspaceBasis  -mkl -o ortho


ortho avec gfortran


gfortran-10 ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -I${LIBMESH_DIR}/include -I${SPACE_BASIS_DIR}/include -ffree-line-length-256 -c -O3
gfortran-10 ortho.f90 -c -O3
gfortran-10 main.f90  -c -O3

gfortran-10  spaceIO.o ortho.o main.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -L${SPACE_BASIS_DIR}/lib -lspaceBasis -O3 -framework Accelerate -o ortho


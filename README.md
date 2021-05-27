ifort ortho.f90 -mkl -o ortho
gfortran-10 ortho.f90 -framework Accelerate -o  ortho


ifort orthoComplex.f90  -c 

ifort ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -c -I${LIBMESH_DIR}/include  -I${SPACE_BASIS_DIR}/include -I. -g -mkl

ifort ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -c -L${LIBMESH_DIR}/lib -lMeshb.7 -I${LIBMESH_DIR}/include -L${SPACE_BASIS_DIR}/lib -lspaceBasis -I${SPACE_BASIS_DIR}/include -I. -g -mkl



ifort spaceIO.o orthoComplex.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -I${LIBMESH_DIR}/include  -L${SPACE_BASIS_DIR}/lib -lspaceBasis -I${SPACE_BASIS_DIR}/include -I. -g -mkl -o ortho



ifort orthoComplex.f90  -c  -O3
ifort ~/Developer/space/tools/spaceIO/src/spaceIO.F90 -c -I${LIBMESH_DIR}/include  -I${SPACE_BASIS_DIR}/include -I. -O3
ifort spaceIO.o orthoComplex.o -L${LIBMESH_DIR}/lib -lMeshbf.7 -L${SPACE_BASIS_DIR}/lib -lspaceBasis  -mkl -o ortho

gfortran-10 ~/Developer/space/tools/spaceIO/src/spaceIO.f90 -c  -L${LIBMESH_DIR}/lib -lMeshbf.7 \
-I${LIBMESH_DIR}/include  -L${SPACE_BASIS_DIR}/lib -lspaceBasis \
-I${SPACE_BASIS_DIR}/include -I. -g  -ffree-line-length-256 -framework Accelerate

gfortran-10 ~/Developer/space/tools/spaceIO/src/spaceIO.f90 -c  -L${LIBMESH_DIR}/lib -lMeshbf.7 \
-I${LIBMESH_DIR}/include  -L${SPACE_BASIS_DIR}/lib -lspaceBasis \
-I${SPACE_BASIS_DIR}/include -I. -g  -ffree-line-length-256 



#gfortran-10 orthoComplex.f90 -framework Accelerate -c 
gfortran-10 orthoComplex.f90  -c 


gfortran-10  spaceIO.o orthoComplex.o -L${LIBMESH_DIR}/lib -lMeshbf.7 \
-I${LIBMESH_DIR}/include  -L${SPACE_BASIS_DIR}/lib -lspaceBasis \
-I${SPACE_BASIS_DIR}/include -I. -g  -ffree-line-length-256 -framework Accelerate -o ortho


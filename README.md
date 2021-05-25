ifort ortho.f90 -mkl -o ortho
gfortran-10 ortho.f90 -framework Accelerate -o  ortho



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

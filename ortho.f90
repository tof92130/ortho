program main

  !> https://fr.wikipedia.org/wiki/Décomposition_en_valeurs_singulières
  !> http://www.netlib.org/lapack/explore-html/d1/d7e/group__double_g_esing_ga84fdf22a62b12ff364621e4713ce02f2.html

  implicit none
  integer            :: n
  integer            :: m
  real(8), pointer   :: A(:,:)
  integer            :: ldA
  real(8), pointer   :: S(:)
  real(8), pointer   :: U(:,:)
  integer            :: ldU
  real(8), pointer   :: VT(:,:)
  integer            :: ldVT
  integer, parameter :: lWMax=1000
  real(8)            :: work(1:lWMax)
  integer            :: lWork
  integer            :: info
  external           :: dgesvd
  external           :: print_matrix
  
  !print '(/"-- Décomposition en valeurs singulières - Propres ou Proper Orthogonal Decomposition (POD) ---"/)'
  print '(/"-- Décomposition Orthogonale aux valeurs Propres - Proper Orthogonal Decomposition (POD) ---"/)'
  
  ! A = U S VT
  !> A \in M_{m,n}
  !> U matrice unitaire \in M_{m,m}  (matrice unitaire: U U* = U* U = I) U* matrice adjointe de U (Transposée de la conjugée)
  !> S matrice \in M_{m,n} seuls les termes diagonaux sont des réels positifs ou nuls
  !> VT matrice unitaire \in M_{n,n} (V* V = V V* = I)
  
  !m=2
  !n=3
  !ldA =m
  !ldU =m
  !ldVT=n
  !allocate( A(1:m,1:n) )
  !A(1,1:3)=[2d0,0d0,0d0]
  !A(2,1:3)=[0d0,5d0,0d0]
  
  !m=4
  !n=5
  !ldA =m
  !ldU =m
  !ldVT=n
  !allocate( A(1:m,1:n) )
  !A(1,1:5)=[1d0,0d0,0d0,0d0,2d0]
  !A(2,1:5)=[0d0,0d0,3d0,0d0,0d0]
  !A(3,1:5)=[0d0,0d0,0d0,0d0,0d0]
  !A(4,1:5)=[0d0,4d0,0d0,0d0,0d0]

  ! https://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mkl_lapack_examples/dgesvd_ex.f.htm
  
  m=6
  n=5
  ldA =m
  ldU =m
  ldVT=n
  allocate( A(1:ldA,1:n) )
  A(1,1:5)=[ 8.79, 9.93, 9.83, 5.45, 3.16]
  A(2,1:5)=[ 6.11, 6.91, 5.04,-0.27, 7.98]
  A(3,1:5)=[-9.15,-7.93, 4.86, 4.85, 3.01]
  A(4,1:5)=[ 9.57, 1.64, 8.83, 0.74, 5.80]
  A(5,1:5)=[-3.49, 4.02, 9.80,10.00, 4.27]
  A(6,1:5)=[ 9.84, 0.15,-8.99,-6.02,-5.31]

  !m=2
  !n=2
  !ldA =m
  !ldU =m
  !ldVT=n
  !allocate( A(1:m,1:n) )
  !A(1,1:2)=[1d0,2d0]
  !A(2,1:2)=[2d0,1d0]


  call print_matrix( 'Matrix A',size(A,1), size(A,2), A)
  
  allocate( s(1:min(m,n)) )  
  allocate(  U(ldu ,m))  
  allocate( VT(ldVT,n))
  
  
  lWork=-1
 !call dgesvd('All','All',m,n,A,lda,S,U,ldu,VT,ldVT,work,lWork,info)
  call dgesvd('All','S'  ,m,n,a,lda,s,u,ldu,vt,ldvt,work,lwork,info)

  LWORK = MIN( LWMAX, INT( WORK(1) ) )

 !call dgesvd('All','All',m,n,A,lda,S,U,ldu,VT,ldVT,work,lWork,info)
  call dgesvd('All','S'  ,m,n,a,lda,s,u,ldu,vt,ldvt,work,lwork,info)
  
  if( .not.info==0 )print '("Problem Info=",i0)',info
  
  print '(/"A = U * S * transpose(V)"/)'
  
  !les coefficients diagonaux de S sont égaux aux valeurs singulières de A.
  ! Les colonnes de U et de V sont, respectivement, vecteur singulier à gauche et à droite pour les valeurs singulières correspondantes.
  
  call print_matrix( 'Valeurs singulières par ordre décroissant', 1, size(S), S)
  
  



  block
    real(8), pointer :: sigma(:,:)
    integer          :: i

    allocate(Sigma(m,n)) ; sigma(:,:)=0d0
    do i=1,size(S)
      sigma(i,i)=s(i)
    enddo

    call print_matrix( 'U', size(U,1), size(U,2), U)
    call print_matrix( 'Sigma', m, n, Sigma)
    call print_matrix( 'VT',size(VT,1), size(VT,2), VT)
  
    call print_matrix( 'Matrice Unitaire UxU^*=Id', m, m, matmul(U ,Transpose(U )))
    call print_matrix( 'Matrice Unitaire VxV^*=Id', n, n, matmul(VT,Transpose(VT)))
    call print_matrix( 'A = U x S x VT',m, n,   matmul(U,matmul(sigma,VT))) 
    deallocate(sigma)
  end block


  deallocate(A)
  deallocate(U)
  deallocate(S)
  deallocate(VT)

end program main


subroutine print_matrix( DESC, M, N, A)
  !>>>
  CHARACTER*(*) :: DESC
  integer       :: m,n
  real(8)       :: A(m,n)
  !>
  integer       :: i,j
  !<<<
  write(*,*)
  print '(a,2x,"m=",i0,2x,"n=",i0)',DESC,m,n
  !write(*,) DESC
  do i=1,m
    write(*,'(*(1x,f6.2))')A(i,1:n)
  enddo
  return
end subroutine print_matrix
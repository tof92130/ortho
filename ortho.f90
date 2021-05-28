subroutine ProperOrthogonalDecomposition(m,n,A,ldA,S,U,ldU,VT,ldVT,info)
  !>>>
  implicit none
  integer            :: n
  integer            :: m
  complex(8)         :: A(1:ldA,1:n)
  integer            :: ldA
  real(8)            :: S( 1:min(m,n) )
  complex(8)         :: U (1:ldU,1:m)
  integer            :: ldU
  complex(8)         :: VT(1:ldVT,1:n)
  integer            :: ldVT
  integer            :: info
  !>
  integer, parameter :: lWMax=1000
  complex(8)         :: work(1:lWMax)
  integer            :: lWork
  real(8), pointer   :: rwork(:)
  external           :: zgesvd
  !<<<
  !>>>
  print '(">>> ProperOrthogonalDecomposition")'
  !<<<
  !>>>
  allocate(rWork(1:5*n))
  !<<<
  !>>>
  lWork=-1
 !call dgesvd('All','All',m,n,A,lda,S,U,ldu,VT,ldVT,work,lWork,rwork,info)
  call zgesvd('All','S'  ,m,n,A(:,:),ldA,S(:),U(:,:),ldU,VT(:,:),ldVT,work,lWork,rWork(:),info)
  !call zgesvd('All','S'  ,m,n,a,lda,s,u,ldu,vt,ldvt,work,lwork,rwork,info)
  
  lwork = min( lWmax, int(work(1)) )
  print '("lWork=",i0)',lWork
  
  !call dgesvd('All','All',m,n,A,lda,S,U,ldu,VT,ldVT,work,lWork,rwork,info)
  !call zgesvd('All','S'  ,m,n,A(:,:),lda,s,u,ldu,vt,ldvt,work,lwork,rwork,info)
  call zgesvd('All','S'  ,m,n,A(:,:),ldA,S(:),U(:,:),ldU,VT(:,:),ldVT,work,lWork,rWork(:),info)
  
  if( .not.info==0 )print '("Problem Info=",i0)',info

  deallocate(rWork)
  !<<<
  !>>>
  print '("<<< ProperOrthogonalDecomposition")'
  !<<<
  return
end subroutine ProperOrthogonalDecomposition


subroutine orthoComplexTest01()
  !>>>
  !  https://fr.wikipedia.org/wiki/Décomposition_en_valeurs_singulières
  !  http://www.netlib.org/lapack/explore-html/d1/d7e/group__double_g_esing_ga84fdf22a62b12ff364621e4713ce02f2.html
  !<<<
  !>>>
  implicit none
  integer            :: n
  integer            :: m
  complex(8), pointer:: A(:,:)
  integer            :: ldA
  real(8)   , pointer:: S(:)
  complex(8), pointer:: U(:,:)
  integer            :: ldU
  complex(8), pointer:: VT(:,:)
  integer            :: ldVT
  integer            :: info
  external           :: print_cmplMatrix
  !<<<
  !>>>
  print '(/"-- Décomposition Orthogonale aux valeurs Propres - Proper Orthogonal Decomposition (POD) ---"/)'
  ! A = U Sigma VT
  !  A \in M_{m,n}
  !  U matrice unitaire \in M_{m,m}  (matrice unitaire: U U* = U* U = I) U* matrice adjointe de U (Transposée de la conjugée)
  !  S matrice \in M_{m,n} seuls les termes diagonaux sont des réels positifs ou nuls
  !  VT matrice unitaire \in M_{n,n} (V* V = V V* = I)
  !<<<
  !>>>
  !> https://www.nag.com/numeric/fl/nagdoc_fl24/pdf/f08/f08kpf.pdf
  m=6
  n=4
  ldA =m
  ldU =m
  ldVT=n
  allocate( A(1:ldA,1:n) )
  A(1,1:n)=[ ( 0.96,-0.81), (-0.03, 0.96), (-0.91, 2.06), (-0.05, 0.41) ]
  A(2,1:n)=[ (-0.98, 1.98), (-1.20, 0.19), (-0.66, 0.42), (-0.81, 0.56) ] 
  A(3,1:n)=[ ( 0.62,-0.46), ( 1.01, 0.02), ( 0.63,-0.17), (-1.11, 0.60) ] 
  A(4,1:n)=[ (-0.37, 0.38), ( 0.19,-0.54), (-0.98,-0.36), ( 0.22,-0.20) ] 
  A(5,1:n)=[ ( 0.83, 0.51), ( 0.20, 0.01), (-0.17,-0.46), ( 1.47, 1.59) ] 
  A(6,1:n)=[ ( 1.08,-0.28), ( 0.20,-0.12), (-0.07, 1.23), ( 0.26, 0.26) ]
  
  call print_cmplMatrix( 'Matrix A',size(A,1), size(A,2), A)
  !<<<
  
  !>>>
  allocate( U (1:ldU ,1:m))  
  allocate( S (1:min(m,n)) )
  allocate( VT(1:ldVT,1:n))
  !<<<
  
  !>>>
  call ProperOrthogonalDecomposition(m,n,A,ldA,S,U,ldU,VT,ldVT,info)  
  !<<<
  
  !>>>  
  print '(/"A = U * Sigma * transpose(V)"/)'
   
  ! les coefficients diagonaux de S sont égaux aux valeurs singulières de A.
  ! Les colonnes de U et de V sont, respectivement, vecteur singulier à gauche et à droite pour les valeurs singulières correspondantes.
  
  !call print_cmplMatrix( 'Valeurs singulières par ordre décroissant', 1, size(S), S)
  print '("Valeurs singulières par ordre décroissan: ",*(f6.2,1x))',S(:)
  
  verif: block
    complex(8), pointer :: sigma(:,:)
    integer             :: i
    
    allocate(Sigma(1:m,1:n)) ; sigma(:,:)=0d0
    do i=1,size(S)
      sigma(i,i)=s(i)
    enddo
    
    call print_cmplMatrix( 'U', size(U,1), size(U,2), U)
    call print_cmplMatrix( 'Sigma', m, n, Sigma)
    call print_cmplMatrix( 'VT',size(VT,1), size(VT,2), VT)
    
    call print_cmplMatrix( 'Matrice Unitaire UxU^*=Id', m, m, matmul(U ,Transpose(conjg(U ))))
    call print_cmplMatrix( 'Matrice Unitaire VxV^*=Id', n, n, matmul(VT,Transpose(conjg(VT))))
    call print_cmplMatrix( 'A = U x Sigma x VT',m, n,   matmul(U,matmul(sigma,VT))) 
    deallocate(sigma)
  end block verif
  !<<<
  !>>>
  deallocate(A)
  deallocate(U)
  deallocate(S)
  deallocate(VT)
  !<<<
end subroutine orthoComplexTest01


subroutine print_cmplMatrix( DESC, M, N, A)
  !>>>
  CHARACTER(*) :: DESC
  integer      :: m,n
  complex(8)   :: A(1:m,1:n)
  !>
  integer      :: i,j
  !<<<
  !>>>
  write(*,*)
  print '(a,2x,"m=",i0,2x,"n=",i0)',DESC,m,n
  !write(*,) DESC
  do i=1,m
    write(*,'(*(3x,(f6.2,1x,f6.2," ,")))')A(i,1:n)
  enddo
  print '("End")'
  !<<<
  return
end subroutine print_cmplMatrix


subroutine orthoRealTest01()
!  !> https://fr.wikipedia.org/wiki/Décomposition_en_valeurs_singulières
!  !> http://www.netlib.org/lapack/explore-html/d1/d7e/group__double_g_esing_ga84fdf22a62b12ff364621e4713ce02f2.html
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
  external           :: print_realMatrix
  
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
  
  call print_realMatrix( 'Matrix A',size(A,1), size(A,2), A)
  
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
  
  call print_realMatrix( 'Valeurs singulières par ordre décroissant', 1, size(S), S)

  block
    real(8), pointer :: sigma(:,:)
    integer          :: i

    allocate(Sigma(m,n)) ; sigma(:,:)=0d0
    do i=1,size(S)
      sigma(i,i)=s(i)
    enddo

    call print_realMatrix( 'U', size(U,1), size(U,2), U)
    call print_realMatrix( 'Sigma', m, n, Sigma)
    call print_realMatrix( 'VT',size(VT,1), size(VT,2), VT)
  
    call print_realMatrix( 'Matrice Unitaire UxU^*=Id', m, m, matmul(U ,Transpose(U )))
    call print_realMatrix( 'Matrice Unitaire VxV^*=Id', n, n, matmul(VT,Transpose(VT)))
    call print_realMatrix( 'A = U x S x VT',m, n,   matmul(U,matmul(sigma,VT))) 
    deallocate(sigma)
  end block
  
  
  deallocate(A)
  deallocate(U)
  deallocate(S)
  deallocate(VT)
  
  return
end subroutine orthoRealTest01


subroutine print_realMatrix( DESC, M, N, A)
  !>>>
  CHARACTER(*) :: DESC
  integer      :: m,n
  real(8)      :: A(m,n)
  !>
  integer      :: i,j
  !<<<
  write(*,*)
  print '(a,2x,"m=",i0,2x,"n=",i0)',DESC,m,n
  !write(*,) DESC
  do i=1,m
    write(*,'(*(1x,f6.2))')A(i,1:n)
  enddo
  return
end subroutine print_realMatrix


subroutine readSnapShots()
  !<<<
  use mesParametres
  use myData
  !>>>
  !<<<
  implicit none
  type(monType) :: ob
  !>
  integer             :: iSnapShot
  integer             :: nSnapShot
  complex(8), pointer :: A(:,:)
  !>>>
  !<<<

  print '(">>> readSnapShots")'  
  nSnapShot=1
  do iSnapShot=1,nSnapShot
    allocate(character(len=128) :: ob%file)
    ob%file="./mode_m01_n01_small_000000340.dat"
    call ob%readRAW
    if( iSnapShot==1 )allocate( A(1:ob%nDeg,1:nSnapShot) )
    !A(:,iSnapShot)=ob%zsol(:,:)
    call ob%display
    call ob%delete
  enddo
  !<<<
  !>>>
  deallocate(A)
  !<<<
  print '("<<< readSnapShots")'  
  return
end subroutine readSnapShots

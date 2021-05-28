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


subroutine orthoComplex()
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
  external           :: print_matrix
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
  
  call print_matrix( 'Matrix A',size(A,1), size(A,2), A)
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
  
  !call print_matrix( 'Valeurs singulières par ordre décroissant', 1, size(S), S)
  print '("Valeurs singulières par ordre décroissan: ",*(f6.2,1x))',S(:)
  
  verif: block
    complex(8), pointer :: sigma(:,:)
    integer             :: i
    
    allocate(Sigma(1:m,1:n)) ; sigma(:,:)=0d0
    do i=1,size(S)
      sigma(i,i)=s(i)
    enddo
    
    call print_matrix( 'U', size(U,1), size(U,2), U)
    call print_matrix( 'Sigma', m, n, Sigma)
    call print_matrix( 'VT',size(VT,1), size(VT,2), VT)
    
    call print_matrix( 'Matrice Unitaire UxU^*=Id', m, m, matmul(U ,Transpose(conjg(U ))))
    call print_matrix( 'Matrice Unitaire VxV^*=Id', n, n, matmul(VT,Transpose(conjg(VT))))
    call print_matrix( 'A = U x Sigma x VT',m, n,   matmul(U,matmul(sigma,VT))) 
    deallocate(sigma)
  end block verif
  !<<<
  !>>>
  deallocate(A)
  deallocate(U)
  deallocate(S)
  deallocate(VT)
  !<<<
end subroutine orthoComplex

subroutine print_matrix( DESC, M, N, A)
  !>>>
  CHARACTER(*) :: DESC
  integer       :: m,n
  complex(8)    :: A(1:m,1:n)
  !>
  integer       :: i,j
  !<<<
  write(*,*)
  print '(a,2x,"m=",i0,2x,"n=",i0)',DESC,m,n
  !write(*,) DESC
  do i=1,m
    write(*,'(*(3x,(f6.2,1x,f6.2," ,")))')A(i,1:n)
  enddo
  print '("End")'
  return
end subroutine print_matrix

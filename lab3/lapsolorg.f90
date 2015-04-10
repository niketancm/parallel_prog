program laplsolv
 use omp_lib
  implicit none
!-----------------------------------------------------------------------
! Serial program for solving the heat conduction problem 
! on a square using the Jacobi method. 
! Written by Fredrik Berntsson (frber@math.liu.se) March 2003
! Modified by Berkant Savas (besav@math.liu.se) April 2006
!-----------------------------------------------------------------------
  integer, parameter                  :: n=6, maxiter=1000
  double precision,parameter          :: tol=1.0E-3
  double precision,dimension(0:n+1,0:n+1) :: T
  double precision,dimension(n)       :: tmp1,tmp2,tmp3
  double precision                    :: error,x,lerror
  real                                :: t0,t1
  integer                             :: i,j,k,first,last
  character(len=20)                   :: str
  
  ! Set boundary conditions and initial values for the unknowns
  T=0.0D0
  T(0:n+1 , 0)     = 1.0D0
  T(0:n+1 , n+1)   = 1.0D0
  T(n+1   , 0:n+1) = 2.0D0
  

  ! Solve the linear system of equations using the Jacobi method
  t0 = omp_get_wtime()

  
  do k=1,maxiter

     error=0.0
     !$omp parallel default(shared) private(tmp1,tmp2,tmp3,j,first,last,lerror)
     first=(n/omp_get_num_threads())*omp_get_thread_num()+1
     last=first+(n/omp_get_num_threads())-1
     if (omp_get_thread_num()==omp_get_num_threads()-1) then
        last=last+mod(n,omp_get_num_threads())
     end if

     tmp1=T(1:n,first-1)
     tmp3=T(1:n,last+1)
     lerror=0.0
     !$omp barrier
    
     
     do j=first,last
        tmp2=T(1:n,j)

        if (j<last) then
           T(1:n,j)=(T(0:n-1,j)+T(2:n+1,j)+T(1:n,j+1)+tmp1)/4.0
        else
           T(1:n,j)=(T(0:n-1,j)+T(2:n+1,j)+tmp3+tmp1)/4.0
        end if

        lerror=max(lerror,maxval(abs(tmp2-T(1:n,j))))
        tmp1=tmp2
     end do

     !$omp critical
     error = max(error,lerror)
     !$omp end critical

     !$omp end parallel
     if (error<tol) then
        exit
     end if
  end do

  t1=omp_get_wtime()

  !    tmp1=T(1:n,0)
  !    error=0.0D0
     
  !    do j=1,n
  !       tmp2=T(1:n,j)
  !       T(1:n,j)=(T(0:n-1,j)+T(2:n+1,j)+T(1:n,j+1)+tmp1)/4.0D0
  !       error=max(error,maxval(abs(tmp2-T(1:n,j))))
  !       tmp1=tmp2
  !    end do
     
  !    if (error<tol) then
  !       exit
  !    end if
     
  ! end do
  
  ! call cpu_time(t1)


  write(unit=*,fmt=*) T

  ! write(unit=*,fmt=*) 'Time:',t1-t0,'Number of Iterations:',k
  ! write(unit=*,fmt=*) 'Temperature of element matrix T  =',T


  ! Uncomment the next part if you want to write the whole solution
  ! to a file. Useful for plotting. 
  
  ! open(unit=7,action='write',file='result.dat',status='unknown')
  ! write(unit=str,fmt='(a,i6,a)') '(',N,'F10.6)'
  ! do i=0,n+1
  !   write (unit=7,fmt=str) T(i,0:n+1)  
  ! end do
  ! close(unit=7)
  
end program laplsolv

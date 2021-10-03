program cluster
  implicit none
  integer i,j,k
  integer, parameter :: n=10
  real*8 c(n,n),cc(n,n),d(n,n),p(n)
  open(unit=5,file='clustergarg',status='old')
  do i=1,n
     read(5,*)(c(i,j),j=1,n)
  end do
10 continue
  d=c
  do i=1,n
     do j=1,n
        cc(i,j)=0.d0
        do k=1,n
           p(k)=min(c(i,k),d(k,j))
           cc(i,j)=maxval(p)
        end do
     end do
  end do
  write(445,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  do i=1,n
     do j=1,n
        write (445, "(F8.4)", advance="NO") cc(i, j)           
     end do
     write(445,*)
  end do
  write(445,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
  do i=1,n
     do j=1,n
        if (cc(i,j).ne.d(i,j))then
           c=cc
           go to 10
        end if
     end do
  end do


     
     
  

end program cluster
           

!This subroutine evaluates the distance between two IFS a and b with weights w.
!Here 'a' is an IFN; 'b' is an IFN.
! 'n' is the number of elements in the IFS 'a' or 'b'
! 'w' is the weight.
! 'distathis' contains the distance between 'a' and 'b'

subroutine rc_this(a,b,n,w,distathis)
  implicit none
  integer i,j,n
  real*8 a(n,2),b(n,2),distathis,w(n)
  real*8 po,pm,term1,term2,term3,term4,dist1(n),dist2(n)
  
  do i=1,n
     do j=1,1
        term1=abs(a(i,j)-b(i,j))+abs(a(i,j+1)-b(i,j+1))
        term2=1.d0+(1.d0+a(i,j))*(1.d0+b(i,j))*(1.d0+a(i,j+1))*(1.d0+b(i,j+1))
        po=abs(max(a(i,j),b(i,j+1))-max(b(i,j),a(i,j+1)))
        pm=abs(min(a(i,j),b(i,j+1))-min(b(i,j),a(i,j+1)))
        term4=po+pm
     end do
     dist1(i)=term1/term2+term4
  end do
  
  distathis=0.d0
  do i=1,n
     distathis=distathis+w(i)*dist1(i)
  end do
  distathis=(5.d0/(12.d0*n))* distathis
end subroutine rc_this

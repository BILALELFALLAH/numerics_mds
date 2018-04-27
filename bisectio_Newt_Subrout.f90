program   NEwton_bisection
implicit none 
real ::x 
integer ::i 


call racine(i,x)
write(*,*)i-1,x,x
 end
subroutine racine(i,x)
implicit none
real*4::x,x1,x0,f,df,eps
integer::i
eps=1.E-5 ; x=1.
x0=0.2
i=1
do while (f(x)>=eps)
 x1=x-f(x)/df(x)
 write(*,*)i,x1             
 x=x1
  i=i+1
end do

end subroutine









real function f(x)
implicit none
real::x
f=cos(x)-x
end function
real function df(x)
implicit none
real::x
df=-sin(x)-1
end function

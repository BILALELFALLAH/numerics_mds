program SECANTE
implicit none 
real :: f ,a,b,eps
real, dimension(40):: x
integer ::i
eps=1E-7
print*,' INTRODUIRE LES VALEURS DE a ET b LES BORNES DE L INTERVALLES '

read*,a,b
if (f(a)*f(b)>=0) then 
print*,'changer les Bornes aucune racine dans cette intervalle '
stop
end if

x(1)=10
do i=1,40
x(i+1)=(x(i))-((b-x(i))*f(x(i)))/(f(b)-f(x(i)))
!if(X(I+1)==x(i)) then
if(abs(f(X(I+1)))<=eps) then 
print*,' ITERATION  LA RACINE   '
print*,i,x(i)
exit
end if
enddo
end program
real function F(x)
implicit none 
real :: x
f=cos(x)-x
end 

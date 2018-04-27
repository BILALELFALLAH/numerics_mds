  program dicho_funct
          implicit none 
          real :: f,epsil,A0,B0,x0
          integer :: I
print*, 'DONNER (a) LE DEBUT DE L INTERVALLE [a,b]='
read*, a0
print*, 'DONNER (b) LA FIN DE L INTERVALLE [a,b]='
read*, b0
!PRINT*, 'DONNER LE NOMBRE DE SUBDIVITIONS N='
!READ*, N
PRINT*, '________________________________________________________'

epsil=1.E-5

IF(F(a0)*F(b0)>=0)then
print*, 'LA RACINE N APPARTIENT PAS A CET INTERVALEE'
print*, 'changer les bornes'
stop
end if

do i=1,40
X0=(b0+a0)/2
IF (F(a0)*F(x0)<0) then
b0=x0
x0=(b0+a0)/2

ELSE
a0=x0
x0=(b0+a0)/2
 if (abs(a0-x0)<epsil) then
write(*,*)i,x0
stop
end if 
end if
end do
end program
       real function f(x)
           implicit none 
           real :: x
           f=x-2
     end function 

! LE PROGRAMME PRICIPALE    !!!!!!!!  !!
!RESOLUTION DE L'EQUATION F(x)=0 PAR:   !!
! 1- L'ALGORITHME DE NEWTON-RAPHSON. !!
! 2- L'ALGORITHME DE LA DICHOTOMIE.    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program Newton_Dichotomie
implicit none 
real ::x0
real, dimension(40) :: x
integer :: i,j
print*,'DANS CE PROGRAMME TOUS LES RESULTATS SERONT EXPRIMER EN RADIAN'
PRINT*,'================================================'
print*,' LA RESOLUTION PAR LA METHODE DE NEWTON RAPHSON '
call Newton(i,X)
PRINT*,'================================================'
print*,' LA RESOLUTION PAR LA METHODE DE DICHOTOMIE'
call Dichotomie(j,x0)
PRINT*,'================================================'
print*,'La difference entre les deux METHODES EST ',abs(X(i+1)-X0)
PRINT*,'LA RESOLUTION PAR LA SECANTE '
call SECANTE(i,x)
 

end program

! LA RESOLUTION PAR LA METHODE DE DICHOTOMIE
subroutine  dichotomie(j,x0)           
implicit none 
real :: f,a,b,x0
integer :: j
print*,'introduire a et  b '
read*,a,b 
 if(f(a)*f(b)>=0) then
    print*,'changer l intervalle '
  else  
     do j=1,1000 
            x0=(a+b)/2
           if(f(a)*f(x0)<0) then 
             b=x0
              x0=(a+b)/2
                   if (abs(b-x0)<=1E-10) then
                         print*,j,x0
                          exit                    
                   end if
             ELSE
                      a=x0
                      x0=(a+b)/2
                    if ((abs(a-x0)<=1E-10)) then
                          print*,'N=',j,'ET X0=',x0
                          exit
                      endif 
                endif
        end do
endif
end subroutine
       !LA RESOLUTION PAR LA METHODE DE NEWTON
subroutine  Newton(i,X)
implicit none 
real :: f,FP
real,dimension(40) :: x
integer :: i 
print*,'intrduire la valeur approximative DE X0 '
read*,x(1)
do i=1,1000
         if (FP(x(i))==0) then
         print*,' le denominateur doit etre non null'
         stop
         endif
 X(i+1)=X(i)-F(X(i))/FP(X(i))        
IF((abs(X(i)-X(i+1)))<=1E-6) THEN 

print*,'N=',i,'ET X0=',X(i+1)
exit
else
X(i)=X(i+1)
end if 
END DO
end subroutine
!LA SECANTEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
subroutine secante(i,x)
implicit none 
real :: f ,a,b
real, dimension(40):: x
integer ::i

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
if(abs(f(X(I+1)))<=1E-6) then 
print*,' ITERATION  LA RACINE   '
print*,i,x(i)
exit
end if
enddo
end subroutine


          ! LA FONCTION COS(X)-X
real function F(x)
implicit none 
real :: x
f=cos(x)-x
end 
       !LA DERIVE DE LA FONCTION
real function FP(x)
real :: x
FP=-sin(x)-1
end 


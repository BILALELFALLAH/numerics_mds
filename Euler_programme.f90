!Ce programme est dans le but de determiner une solution approchee D'UNE EDO   !
! y'=x+y par methode d'Euler en deux formules, simple et amelioree             !
program Euler                                                                  !
implicit none                                                                  !
REAL :: x,h,f,Ys,Ya,Yexact,ErrS,ErrA                                           !
integer :: i                                                                   ! 
h=0.1 ; x=0. ; Ys=1. ; Ya=1.                                                   ! 
write(*,1)'i','x','Yexacte','Ysimple','ErreurSimple','Yameliore','ErreurAmeli' !
do i=1,11                                                                      !                                  
    Ys=Ys+h*f(x,Ys)                             ! formule d'euler simple       ! 
    Ya=Ya+(h/2.)*(f(x,Ya)+f(x+h,Ys))        ! formule d'euler plus precise     !
    x=x+h                                                                      !
    ErrA = abs(Ya-Yexact(x))*100/Yexact(x)                                     !     
    ErrS = abs(Ys-Yexact(x))*100/Yexact(x)                                     !
     write(*,2)i,x,Yexact(x),Ys,ErrS,'%',Ya,ErrA,'%'!Affichage des resultats   !   
end do                                                                         ! 
1 format(2X,A2,2X,A2,2X,A12,2X,A12,2X,A13,3X,A10,2X,A12)                       ! 
2 format(2X,I2,2X,F3.1,2X,F12.8,2X,F12.8,2X,F9.6,1X,A1,2X,F12.8,2X,F9.6,1X,A1) !
end program Euler                                                              ! 
!------------------------------------------------------------------------------!
real function  f(x,y)                                                          !
implicit none                                                                  !
real :: x,y                                                                    !
f = x+y                                                                        !
end function                                                                   !
!-------------------------------------------------------------------------------
real function Yexact(x)                                                        !
implicit none                                                                  !
real :: x                                                                      !
Yexact = -x-1+2*exp(x)                                                         !
end                                                                            !
!______________________________________________________________________________!

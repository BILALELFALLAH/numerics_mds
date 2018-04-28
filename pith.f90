      program foncti_trapezes
              implicit none
           !declaration des variables    
              real :: f,h,T,T1,T2,a,b
              real ::   S,S1,S2,S3,Delta
              integer :: j,n
              
              
              n=40
              a=1
              b=2
              h=(b-a)/n
              

        !l'expression de trapezzzzzzzzzzzzzzzzzz 
        T1=h*(f(a)+f(b))/2
        T2=0
        do j=1,n-1
        T2=T2+h*f(a+j*h)
        end do 
        T=T1+T2
        print*,'par trapeeetzzzz T=',T
       ! par la öethode des sipsonnnnnnnnnnnnnnn
       h=(b-a)/n
        S1=f(a)+f(B)
        S2=0
         do j=2,n-2,2
      S2=S2+f(a+j*h)
       end do
       S3=0
      do j=1,n-1,2
       S3=S3+f(a+h*j)
       end do 
         S=h*(S1+2*S2+4*S3)/3
       print*,'par simpsonnnnnn S=',S  
        Delta=abs(S-T)
      print*,'la difference entre les deux methodes Delta=abs(s-T)=',Delta
        end program
                !la fonction qu'on va estimer
        
        
         real function f(x)
                implicit none 
                real :: x
                f=(x**2+1)*sqrt(x**4+2)*sin(x)


                end function 




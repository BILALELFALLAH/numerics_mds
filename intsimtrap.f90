      program trap__simmp
              implicit none 
              real ::a,b,f,h,T,T1,T2,S,S1,S2,S3
             integer :: j,n

                

           
         print*,'introduire les bornes d integrale a et b '
         read*,a,b
         print*,'introduire la valeur de n '
         read*,n
                  h=(b-a)/n
          !la valeur approche par la methode des  trapezes___
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          T1=h*(f(a)+f(b))/2
        
        T2=0
        do j=1,n-1
        T2=T2+h*f(a+h*j)
        end do

       T=T1+T2
     print*,'TTTTTTRRRRRRRRRRAAAAAAAPPPPPPPPPEEEEEEEEEEZZZZZZZZZZEEEESS' 
     print*,'la valeur aproche par la methode des trapezes est T=',T
        ! la valeur aproche par la methode de  simpsonnnnnnnnnnnnnnn
        S1=f(a)+f(b)

        S2=0
        do j=2,n-2,2
        S2=S2+f(a+h*j)
        end do

        S3=0
        do j=1,n-1,2
        S3=S3+f(a+j*h)
        end do

        S=h*(S1+2*S2+4*S3)/3
        print*,'la valeur approche par la methode de simpson est S=',S

        end program 
       !!!!!!!!!!!!!!!la fonction qu'on va etudier!!!!!!!!!!!!!!!!!!!
       real function f(x)
               implicit none
               real :: x
               f=x**2+1
               end function   



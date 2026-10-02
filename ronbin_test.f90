program name
    use todos
    implicit none
    real(dp), parameter :: rc=100.0_dp, k=-1.80_dp
    character(len=72) :: nombre_archivo_txt,nombre_archivo_txtcomp

    nombre_archivo_txt = "salida/tx_birbin_completo.txt"
    nombre_archivo_txtcomp = "salida/ronbiny_comp.txt"

    call leer_datos
    call birbin_completo(rc,k,nombre_archivo_txt)
    ! call bircos_completo(rc,k,nombre_archivo_txt)
    ! call ronbinx_completo(rc,k,nombre_archivo_txtcomp)
    ! call roncosx_completo(rc,k,nombre_archivo_txt)
    ! call ronbiny_completo(rc,k,nombre_archivo_txt)
    ! call roncosy_completo(rc,k,nombre_archivo_txt)
    ! call birbin_diametro(rc,k,nombre_archivo_txt)
    ! call bircos_diametro(rc,k,nombre_archivo_txt)
    ! call ronbinx_diametro(rc,k,nombre_archivo_txt)
    ! call roncosx_diametro(rc,k,nombre_archivo_txt)
    ! call ronbiny_diametro(rc,k,nombre_archivo_txt)
    ! call roncosy_diametro(rc,k,nombre_archivo_txt)

    ! call grafica_completo_cos(rc,k,nombre_archivo_txt)
    ! call grafica_completo_bin(rc,k,nombre_archivo_txt)
    ! call grafica_fila(rc,k,nombre_archivo_txt)
end program name

module todos
    implicit none
    integer, parameter :: dp = kind(0.0d0)
    real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
    real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
    real(dp) :: di,alfa,beta,gamma,z0,phi
    real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
    integer :: i,j,nlp,np
    
contains
    subroutine leer_datos
        implicit none
        di = 14.0_dp; nlp = 50.0_dp; z0= 99.5_dp
        alfa = 0.0_dp; beta = 0.0_dp; gamma= 99.5_dp
        phi=0.0_dp; np=100
    end subroutine leer_datos

    subroutine birbin_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))!  write(40,*) deno
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp-zy*zy+zx*zx)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno; ty=y+(z0-z)*numy0/deno
            ! write(40,*) x, y, tx, ty
            ! write(40,*) tx, ty
            ! write(40,*) x, y
            ! write(40,*) numx0
            ! write(40,*) numy0
            txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx)/delta; 
            argy=(2.0_dp*pi*ty)/delta
            ! write(40,*) argx, argy

            if(dcos(argx)>0.0_dp.and.dcos(argy)>0.0_dp) then
                write(40,*) txron,tyron
            endif
        enddo   
        enddo
     close(40)
    end subroutine birbin_completo

    subroutine bircos_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno; ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*ty/delta)

            write(40,*) txron, tyron, (cos(argx)+1)/2 + (cos(argy)+1)/2
            ! write(40,*) txron, tyron, ((cos(argx)+1)/2) * ((cos(argy)+1)/2)
        enddo   
        enddo
     close(40)
    end subroutine bircos_completo

    subroutine ronbinx_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta)

            if(dcos(argx)>0.0_dp) then
                write(40,*) txron,tyron
            endif
        enddo   
        enddo
     close(40)
    end subroutine ronbinx_completo

    subroutine roncosx_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta)

            write(40,*) txron, tyron, (cos(argx)+1)/2
            ! write(40,*) txron, tyron, ((cos(argx)+1)/2) * ((cos(argy)+1)/2)
        enddo   
        enddo
     close(40)
    end subroutine roncosx_completo

    subroutine ronbiny_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argy=(2.0_dp*pi*ty/delta)

            if(dcos(argy)>0.0_dp) then
                write(40,*) txron,tyron
            endif
        enddo   
        enddo
     close(40)
    end subroutine ronbiny_completo

    subroutine roncosy_completo(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
        do j=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=dfloat(j)*sdi/dfloat(np)

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argy=(2.0_dp*pi*ty/delta)

            write(40,*) txron, tyron, (cos(argy)+1)/2
            ! write(40,*) txron, tyron, ((cos(argx)+1)/2) * ((cos(argy)+1)/2)
        enddo   
        enddo
     close(40)
    end subroutine roncosy_completo

    subroutine birbin_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno; ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*ty/delta)

            if(dcos(argx)>0.0_dp.and.dcos(argy)>0.0_dp) then
                write(40,*) txron,tyron
            else
                write(40,*) txron,1.0_dp
            endif 
        enddo
     close(40)
    end subroutine birbin_diametro

    subroutine bircos_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno; ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*ty/delta)

            write(40,*) txron, ((cos(argx)+1)/2 + (cos(argy)+1)/2)/2
            ! write(40,*) txron, ((cos(argx)+1)/2) * ((cos(argy)+1)/2)
        enddo   
     close(40)
    end subroutine bircos_diametro

    subroutine ronbinx_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta)

            if(dcos(argx)>0.0_dp) then
                write(40,*) txron,tyron
            else
                write(40,*) txron,1.0_dp
            endif
        enddo   
     close(40)
    end subroutine ronbinx_diametro

    subroutine roncosx_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            x=dfloat(i)*sdi/dfloat(np)
            y=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            tx=x+(z0-z)*numx0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argx=(2.0_dp*pi*tx/delta)

            write(40,*) txron, (cos(argx)+1)/2
        enddo   
     close(40)
    end subroutine roncosx_diametro

    subroutine ronbiny_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            y=dfloat(i)*sdi/dfloat(np)
            x=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argy=(2.0_dp*pi*ty/delta)

            if(dcos(argy)>0.0_dp) then
                write(40,*) txron,tyron
            else
                write(40,*) 1.0_dp,tyron
            endif
        enddo   
     close(40)
    end subroutine ronbiny_diametro

    subroutine roncosy_diametro(rc, k, name_txt)
     implicit none
     character(len=72), intent(in) :: name_txt
     real(dp), intent(in) :: rc, k
    
     delta=2.54_dp/nlp; sdi=di/2.0_dp; c=1.0_dp/rc

     open(40,file=trim(name_txt),status='replace')
        do i=-np,np 
            y=dfloat(i)*sdi/dfloat(np)
            x=0.0_dp

            ra=dsqrt(x**2+y**2)

            if (ra > sdi) cycle

            raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
            z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

            raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
            z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

            deno=(gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-alfa)+zy*(y-beta))
            numx0=(x-alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-beta)+(gamma-z))
            numy0=(y-beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-alfa)+(gamma-z))

            ty=y+(z0-z)*numy0/deno
            txron=x+(z_max-z)*numx0/deno
            tyron=y+(z_max-z)*numy0/deno
            argy=(2.0_dp*pi*ty/delta)

            write(40,*) (cos(argy)+1)/2, tyron
        enddo   
     close(40)
    end subroutine roncosy_diametro

    subroutine grafica_completo_bin(rc,k, name_txt)
        implicit none
        character(len=72), intent(in) :: name_txt
        real(dp), intent(in) :: rc, k
        character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str
        character(len=200) :: texto_titulo
        
        write(Valor_rc_str, '(F12.8)') rc
        write(Valor_k_str,'(F12.8)') k
        write(Valor_z_str, '(F12.8)') z0
        
        texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
        ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

        open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
         write(30,*) 'set term qt'
         write(30,*) 'set terminal qt font "Monaco,12"'
         write(30,*) 'set xlabel "eje x (cm)"'
         write(30,*) 'set ylabel "eje y (cm)"'
         write(30,*) 'set size ratio 1'
         write(30,*) trim(texto_titulo)
         write(30,*) 'set grid'
         write(30,*) 'plot "'//trim(name_txt)//'" ls 0 lc 16 notitle'
        close(30)
        call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
    endsubroutine grafica_completo_bin

    subroutine grafica_completo_cos(rc,k, name_txt)
        implicit none
        character(len=72), intent(in) :: name_txt
        real(dp), intent(in) :: rc, k
        character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str
        character(len=200) :: texto_titulo
        
        write(Valor_rc_str, '(F12.8)') rc
        write(Valor_k_str,'(F12.8)') k
        write(Valor_z_str, '(F12.8)') z0
        
        texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
        ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

        open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
         write(30,*) 'set term qt'
         write(30,*) 'set terminal qt font "Monaco,12"'
         write(30,*) 'set xlabel "eje x (cm)"'
         write(30,*) 'set ylabel "eje y (cm)"'
         write(30,*) 'set size ratio 1'
         write(30,*) trim(texto_titulo)
         write(30,*) 'set grid'
         write(30,*) 'set palette gray'
         write(30,*) 'set cblabel "Intensidad (z)"'
         write(30,*) 'set colorbox'
         write(30,*) 'unset key'
         write(30,*) 'plot "'//trim(name_txt)//'"  using 1:2:3 with points palette pt 7 ps 1 notitle'
        close(30)
        call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
    endsubroutine grafica_completo_cos

    subroutine grafica_fila(rc,k, name_txt)
        implicit none
        character(len=72), intent(in) :: name_txt
        real(dp), intent(in) :: rc, k
        character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str
        character(len=200) :: texto_titulo

        write(Valor_rc_str, '(F14.8)') rc
        write(Valor_k_str, '(F12.8)') k    
        write(Valor_z_str, '(F12.8)') z0

        texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
        ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

        open(unit=30, file='Datos_Grafica_Ronchi_Fila.txt', status='replace')
         write(30,*) 'set term qt'
         write(30,*) 'set terminal qt font "Monaco,12"'
         write(30,*) 'set xlabel "eje pixeles (cm)"'
         write(30,*) 'set ylabel "eje irradiancia (escala de grises)"'
         write(30,*) 'set size ratio 1'
         write(30,*) trim(texto_titulo)
         write(30,*) 'set grid'
         write(30,*) 'unset key'
         write(30,*) 'plot "'//trim(name_txt)//'"'
        close(30)
        call system('gnuplot -p Datos_Grafica_Ronchi_Fila.txt')
    endsubroutine grafica_fila
    
end module todos
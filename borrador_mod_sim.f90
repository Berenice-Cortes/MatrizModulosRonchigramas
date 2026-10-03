program name
    use calculos_ronchigrama
    implicit none
    call simulacion
end program name

module variables
    implicit none
    integer, parameter :: dp = kind(0.0d0) !Aquí lo que se use en cualquier subrutina
    real(dp), parameter :: pi=3.1415926535_dp

    type :: parametros_espejo
        real(dp) :: di, nlp, z0, alfa, beta, gamma, phi, delta
        integer :: np

        real(dp) :: rc, k
    end type parametros_espejo
    type(parametros_espejo) :: datos_esp
contains
end module variables


    
! module calculos_ronchigrama
!     ! use variables
!     implicit none
!     real(dp) ::  x, y, raiz,z,zx,zy,raiz_max,z_max,deno,numx0,numy0,tx,ty,tx_ron,ty_ron
!     real(dp) :: sdi, delta, c
!     character(len=20) :: tipo_if1(2,5)
!     character(len=20) :: tipo_if2(2,8)
!     character(len=3) :: tipo_rejilla
!     integer :: datos_ciclodo_if1(5,4), tipo_num, tipo_numif2
!     real(dp), allocatable :: arr_x(:), arr_y(:)

! contains

!   subroutine simulacion()
!     implicit none
!     integer :: i, j

!     call leer_datos
!     call ctes_sim
!     call datos_if1
!     call asegurar_dimension_arrx
!     call asegurar_dimension_arry(datos_ciclodo_if1(tipo_num,1))
!     call if1
!     call datos_if2

!     open(40,file='puntos_ronchigrama.txt',status='replace')

!     do i = 1, size(arr_x)
!     do j = 1, size(arr_y)
!         if(dist(arr_x(i),arr_y(j)) <= sdi) then 
!             call comun(datos_esp%k,c,dist(arr_x(i),arr_y(j)),arr_x(i),arr_y(j),sdi)
!             call if2(arr_x(i),arr_y(j))
!         end if
!     enddo
!     enddo

!     close(40)

!   end subroutine simulacion

!   subroutine asegurar_dimension_arrx()
!     implicit none
!     integer :: n

!     n=2*datos_esp%np+1

!     if (.not. allocated(arr_x)) then
!       allocate(arr_x(n))
!     else if (size(arr_x) /= n) then
!       ! Si ya existe pero con diferente tamaño, se redimensiona
!       deallocate(arr_x)
!       allocate(arr_x(n))
!     end if
!   end subroutine asegurar_dimension_arrx

!   subroutine asegurar_dimension_arry(n)
!     implicit none
!     integer, intent(in) :: n

!     if (.not. allocated(arr_y)) then
!       allocate(arr_y(n))
!     else if (size(arr_y) /= n) then
!       ! Si ya existe pero con diferente tamaño, se redimensiona
!       deallocate(arr_y)
!       allocate(arr_y(n))
!     end if
!   end subroutine asegurar_dimension_arry

!     function dist(a,b) result(distancia)
!         implicit none
!         real(dp), intent(in) :: a,b
!         real(dp) :: distancia
!         distancia=dsqrt(a**2+b**2)
!     end function dist

!     subroutine comun(k,c,ra,x,y,sdi)
!         implicit none
!         real(dp), intent(in) :: k,c,ra,x,y,sdi
        
!         raiz=dsqrt(1.0_dp-(k+1.0_dp)*c**2*ra**2)
!         z=(c*ra**2)/(1.0_dp+raiz)
!         zx=c*x/(raiz)
!         zy=c*y/(raiz)

!         raiz_max = dsqrt(1.0_dp-(k+1.0_dp)*c**2*sdi**2)
!         z_max = (c*sdi**2)/(1.0_dp+raiz_max)
!         deno = (datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!     end subroutine comun
    
!     function num(vi,vj,pvi,pvj,vli,vlj,vk) result(numerador_i)
!         implicit none
!         real(dp), intent(in):: vi,vj,pvi,pvj,vli,vlj,vk
!         real(dp) :: numerador_i
!         numerador_i = (vi-vli)*(1.0_dp-pvi*pvi+pvj*pvj)-2.0_dp*pvi*(pvj*(vj-vlj)+(datos_esp%gamma-vk))
!     end function num

!     function aberracion_t(vi,vkp,vk,nume,denom) result(abtr)
!         real(dp), intent(in) :: vi,vk,nume,denom,vkp
!         real(dp) :: abtr

!         abtr = vi+(vkp-vk)*(nume/denom)
!     end function aberracion_t

!     subroutine leer_datos
!         implicit none
!         datos_esp%di = 14.0_dp; datos_esp%nlp = 50.0_dp; datos_esp%z0= 99.5_dp
!         datos_esp%alfa = 0.0_dp; datos_esp%beta = 0.0_dp; datos_esp%gamma= 99.5_dp
!         datos_esp%phi=0.0_dp; datos_esp%np= 100; datos_esp%rc=100.0_dp; datos_esp%k =-1.80_dp
!     end subroutine leer_datos

!     subroutine ctes_sim()
!         implicit none
!         sdi=datos_esp%di/2.0_dp; delta=2.54_dp/datos_esp%nlp; c=1.0_dp/datos_esp%rc
!     end subroutine ctes_sim

!     subroutine datos_if1()
!         implicit none
!         character(len=20) :: tipo
!         integer :: tam_compl,tam_fransi,tam_francu,tam_filacu,tam_diam
!         integer :: lim_arr1,lim_abj1,lim_arr2,lim_abj2,lim_arr3,lim_abj3, fila, diam
!         integer :: i, paso_comp, paso_fransi, paso_francu, paso_fila, paso_diam

!         tipo_if1(1,1) = 'completo'; tipo_if1(1,2) = 'franja_simetrico'; tipo_if1(1,3) = 'franja_cualq'
!         tipo_if1(1,4) = 'fila_cualq'; tipo_if1(1,5) = 'diametro'
!         tipo_if1(2,1) = '1'; tipo_if1(2,2) = '2'; tipo_if1(2,3) = '3'; tipo_if1(2,4) = '4'; tipo_if1(2,5) = '5'

!         write(*,*) 'Ellige uno:'
!         print*, tipo_if1(1,:)
!         read(*,*) tipo

!         do i = 1, 5
!             if (tipo_if1(1,i)==tipo) then
!                 read(tipo_if1(2,i),*) tipo_num
!                 exit
!             endif
!         end do

!         print*, 'Numero asociado:', tipo_num
        
!         lim_arr1 = datos_esp%np; lim_abj1 = -datos_esp%np; tam_compl = 2*datos_esp%np+1; paso_comp = 1
!         lim_arr2 = 10; lim_abj2 = -lim_arr2; tam_fransi = 2*lim_arr2+1; paso_fransi = 1
!         lim_arr3 = 10; lim_abj3 = -15; tam_francu = abs(lim_arr3)+abs(lim_abj3); paso_francu = 1
!         tam_filacu = 1; fila = -2; paso_fila = 1
!         tam_diam = 1; diam = 0; paso_diam = 1

!         datos_ciclodo_if1(1,1) = tam_compl; datos_ciclodo_if1(1,2) = lim_arr1; datos_ciclodo_if1(1,3) = lim_abj1; 
!         datos_ciclodo_if1(1,4) = paso_comp;

!         datos_ciclodo_if1(2,1) = tam_fransi; datos_ciclodo_if1(2,2) = lim_arr2; datos_ciclodo_if1(2,3) = lim_abj2; 
!         datos_ciclodo_if1(2,4) = paso_fransi;

!         datos_ciclodo_if1(3,1) = tam_filacu; datos_ciclodo_if1(3,2) = lim_arr3; datos_ciclodo_if1(3,3) = lim_abj3; 
!         datos_ciclodo_if1(3,4) = paso_fransi;

!         datos_ciclodo_if1(4,1) = tam_filacu; datos_ciclodo_if1(4,2) = fila; datos_ciclodo_if1(4,4) = paso_fila;

!         datos_ciclodo_if1(5,1) = tam_diam; datos_ciclodo_if1(5,2) = diam;  datos_ciclodo_if1(5,4) = paso_diam;

!         print*, datos_ciclodo_if1(tipo_num,:)
!     end subroutine datos_if1

!     subroutine datos_if2()
!         implicit none
!         character(len=7) :: tipo
!         integer :: i
!         tipo_if2(1,1) = 'birbin'; tipo_if2(1,2) = 'bircos'; tipo_if2(1,3) = 'ronbinx'
!         tipo_if2(1,4) = 'roncosx'; tipo_if2(1,5) = 'ronbiny'; tipo_if2(1,6) = 'roncosy'
!         tipo_if2(1,7) = 'perbinx'; tipo_if2(1,8) = 'percosy'
!         tipo_if2(2,1) = '1'; tipo_if2(2,2) = '2'; tipo_if2(2,3) = '3'; tipo_if2(2,4) = '4'; 
!         tipo_if2(2,5) = '5'; tipo_if2(2,6) = '6'; tipo_if2(2,7) = '7'; tipo_if2(2,8) = '8'

!         write(*,*) 'Ellige uno:'
!         write(*,*) tipo_if2(1,:)
!         read(*,*) tipo

!         do i = 1, 8
!             if (tipo_if2(1,i)==tipo) then
!                 read(tipo_if2(2,i),*) tipo_numif2
!                 exit
!             endif
!         end do

!         print*, 'Numero asociado:', tipo_numif2
!     end subroutine datos_if2

!     subroutine if1()
!         implicit none
!         integer :: i,j,cont

!         cont=0
!         do i = -datos_esp%np, datos_esp%np
!             cont=cont+1
!             arr_x(cont) = real(i,dp)*sdi/real(datos_esp%np, dp)
!         end do

!         if (tipo_num == 1.or.tipo_num == 2.or.tipo_num == 3) then !más de una fila
!             cont=0
!             do j = datos_ciclodo_if1(tipo_num,2), datos_ciclodo_if1(tipo_num,3),datos_ciclodo_if1(tipo_num,4)
!             cont=cont+1
!             arr_y(cont) = real(j,dp)*sdi/real(datos_esp%np, dp)
!             end do
!         else if (tipo_num == 4.or.tipo_num==5) then !fila cualqu
!             j=datos_ciclodo_if1(tipo_num,2)
!             arr_y(1)=real(j,dp)
!         endif

!         write(*,*) "size(arr_x), size(arr_y)", size(arr_x), size(arr_y)

!     end subroutine if1
    
!     subroutine if2(pupilax,pupilay)
!         real(dp), intent(in) :: pupilax,pupilay
!         real(dp) :: txron,tyron,argx,argy
!         integer, parameter :: arch = 40

!         x=pupilax ; y=pupilay;

!         numx0= num(x,y,zx,zy,datos_esp%alfa,datos_esp%beta,z)
!         numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)

!         if (tipo_numif2==1.or.tipo_numif2==2) then !if ( "bironchigrama" == 'bironchigrama' ) then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)

!             argx = (2.0_dp*pi*tx)/delta
!             argy = (2.0_dp*pi*ty)/delta

!             txron = aberracion_t(x,z_max,z,numx0,deno)
!             tyron = aberracion_t(y,z_max,z,numy0,deno)
            
!             if ( tipo_numif2==1  ) then

!                 if(cos(argx)>=0.and.cos(argy)>=0) then
!                     write(arch,*) txron, tyron
!                 endif

!             else if ( tipo_numif2==2  ) then
!                 write(arch,*) txron,tyron,(cos(argx)+1)/4 + (cos(argy)+1)/4
!             end if  
        
!         else if (tipo_numif2==3.or.tipo_numif2==4 ) then!else if ("ronchigrama vertical x" == 'ronchigrama') then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             argx = (2.0_dp*pi*tx)/delta

!             if ( tipo_numif2==3  ) then
!                 if(cos(argx)>=0.0_dp) then
!                     txron = aberracion_t(x,z_max,z,numx0,deno)
!                     tyron = aberracion_t(y,z_max,z,numy0,deno)
!                     write(arch,*) txron, tyron
!                 endif
!             else if ( tipo_numif2==4  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 tyron = aberracion_t(y,z_max,z,numy0,deno)
!                 write(arch,*) txron, tyron, (cos(argx)+1)/2
!             end if  
!         else if (tipo_numif2==5.or.tipo_numif2==6) then!else if ("ronchigrama horizontal y" == 'ronchigrama') then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)
!             argy = (2.0_dp*pi*ty)/delta

!             if ( tipo_numif2==5  ) then

!                 if(cos(argy)>=0.0_dp) then
!                     txron = aberracion_t(x,z_max,z,numx0,deno)
!                     tyron = aberracion_t(y,z_max,z,numy0,deno)
!                     write(arch,*) txron, tyron
!                 endif

!             else if ( tipo_numif2==6  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 tyron = aberracion_t(y,z_max,z,numy0,deno)
!                 write(arch,*) txron, tyron, (cos(argy)+1)/2
!             end if  
!         else if ((tipo_num==4.or.tipo_num==5).and.(tipo_numif2==7.or.tipo_numif2==8)) then!else if perfil de fila
            
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             argx = (2.0_dp*pi*tx)/delta

!             if ( tipo_numif2==7  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 if(cos(argx)>=0.0_dp) then                    
!                     write(arch,*) txron, 0.0_dp
!                 else
!                     write(arch,*) txron, 1.0_dp
!                 endif
!             else if ( tipo_numif2==8  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 write(arch,*) txron, (cos(argx)+1)/2
!             end if   
!         end if 
!     end subroutine if2
! end module calculos_ronchigrama
module utiles
    implicit none
    integer, parameter :: dp = kind(0.0d0) !Aquí lo que se use en cualquier subrutina
    real(dp), parameter :: pi=3.1415926535_dp

    type :: registro_tiempo
        integer(8) :: count_inicio, count_rate, count_final, valores(8)
        real(dp) :: t_inicial, t_final, tiempo_real, tiempo
    end type registro_tiempo
    type(registro_tiempo) :: reloj

    type :: dowhile
        logical :: ajuste_ok
        character(len=20) :: respuesta
    end type dowhile
    type(dowhile) :: while
    
    type :: url_rutas
        character(len=256) :: carp_plant, carpeta, nombre_imagen
        character(len=350) ::  url_carpeta_caso
        character(len=400) :: arch_param
    end type url_rutas
    type(url_rutas) :: ruta

    type :: datos_image
        character :: arch_imagej, datos, txtimage
        integer :: alto_pixo, ancho_pixo, alto_pixef, ancho_pixef
        integer :: min_pixnx, min_pixny, num_dat, cont_pin, m_dat, fila_evf
        real(dp) :: coord_centrox, coord_centroy, semidiametro, un_pixel
        integer, allocatable :: listcompcoord1(:,:), listcompcoord2(:,:), listcoordbordesp(:,:)
    end type datos_image
    type(datos_image) :: imagej
    
    type, public :: metodo_sim1vsim2
        real(dp), allocatable :: val_sim1(:,:), val_sim2(:,:)
    end type metodo_sim1vsim2
    type(metodo_sim1vsim2) :: simvsim

    type :: parametros_espejo
        real(dp) :: di, nlp, z0, alfa, beta, gamma, phi, delta
        integer :: np

        real(dp) :: rc, k
    end type parametros_espejo
    type(parametros_espejo) :: datos_esp

    type :: param_filtros_ajustes_datos
        real(dp) :: sigma
    end type param_filtros_ajustes_datos
    type(param_filtros_ajustes_datos) :: filt

    type, public :: calc_aberr
        integer :: var
    end type calc_aberr

contains
    subroutine reloj_inicio()
        implicit none
        call system_clock(reloj%count_inicio, reloj%count_rate)
        call cpu_time(reloj%t_inicial)
        call date_and_time(values=reloj%valores)

        ! open(unit=51, file='salida/registro.txt', status='unknown', position='append') 
            write(*, '(A, I4.4, "/", I2.2, "/", I2.2, " ", I2.2, ":", I2.2, ":", I2.2)') &
            "Registro realizado el: ",&
            &reloj%valores(1),reloj%valores(2),reloj%valores(3),reloj%valores(5),reloj%valores(6),reloj%valores(7)
        ! close(51)
    end subroutine reloj_inicio

    subroutine reloj_fin()
        implicit none
        call cpu_time(reloj%t_final)
        call system_clock(reloj%count_final)
        reloj%tiempo = reloj%t_final - reloj%t_inicial
        reloj%tiempo_real = real(reloj%count_final - reloj%count_inicio, dp) / real(reloj%count_rate, dp)

        ! open(unit=51, file='salida/registro.txt', status='unknown', position='append') 
        write(*,'(2X,A,F18.10,A)') "Tiempo cpu de ejecucion: ", reloj%tiempo, " segundos"
        write(*,'(2X,A,F18.10,A)') "Tiempo real de reloj:", reloj%tiempo_real, "segundos"
        call date_and_time(values=reloj%valores)
        write(*, '(A, I4.4, "/", I2.2, "/", I2.2, " ", I2.2, ":", I2.2, ":", I2.2)') &
        "Registro terminado el: ",&
        &reloj%valores(1),reloj%valores(2),reloj%valores(3),reloj%valores(5),reloj%valores(6),reloj%valores(7)
        ! WRITE(51,'(A)') "------------------------------------------------------------------------------------------------------------------"
        ! WRITE(51,'(A)') "------------------------------------------------------------------------------------------------------------------"
        ! close(51)

        print*, "PROCESO FINALIZADO CON ÉXITO"
    end subroutine reloj_fin

    function afirmativo(resp) result(comprob)
        implicit none
        character(len=20) :: resp
        logical :: comprob
        
        comprob=(resp=='s'.or.resp=='S'.or.resp=='Sí'.or.resp=='sí')
    end function afirmativo

    subroutine crear_directorios_IO()
        !Sirve para Fortran 2008 en adelante
        implicit none
        integer :: cmd_status

        ! Crear la carpeta en el sistema operativo
        ! En Linux / macOS / Unix:
        call execute_command_line('mkdir -p "Entrada"', exitstat=cmd_status)

        ! En Windows (cmd):
        ! call execute_command_line('mkdir "' // trim(carp_plant) // '"', exitstat=cmd_status)

        ! Verificar si se creó correctamente
        if (cmd_status /= 0) then
            write(*,*) 'Error: No se pudo crear el directorio: ', "Entrada"
        else
            write(*,*) 'Directorio listo: ', "Entrada"
        end if

        call execute_command_line('mkdir -p "Salida"', exitstat=cmd_status)

        ! En Windows (cmd):
        ! call execute_command_line('mkdir "' // trim(carp_plant) // '"', exitstat=cmd_status)

        ! Verificar si se creó correctamente
        if (cmd_status /= 0) then
            write(*,*) 'Error: No se pudo crear el directorio: ', "Salida"
        else
            write(*,*) 'Directorio listo: ', "Salida"
        end if
    end subroutine crear_directorios_IO

    subroutine introducir_rutasynombres_desdeterminal()
        ruta%carp_plant= '/Users/berenicecortes/matriz_mod_tesis/Entrada'
        ruta%carpeta = 'carpeta5'; ruta%nombre_imagen = 'rc9840z09340ke'
        write(*,'(A)') 'Introduce la ruta de la carpeta matriz de plantillas (De donde se va a sacar la información): '
        ! read(*,*) ruta%carp_plant
        ! write(*,'(A)') 'Nombre que se le dara a la carpeta de caso: '
        ! read(*,*) ruta%carpeta
        ! write(*,'(A)') 'Nombre que se le dará a la imagen: '
        ! read(*,*) ruta%nombre_imagen

        open(30,file="Entrada/ruta_carpetas_extraccion.txt",status='replace',action='write')
            write(30,'(A,2X,A)') 'URL de carpeta matriz de plantillas:', ruta%carp_plant
            write(30,'(A,2X,A)') 'Nombre carpeta | (zn_z0##):', ruta%carpeta
            write(30,'(A,2X,A)') 'Nombre imagen | (z0####rc####k####):', ruta%nombre_imagen
        close(30)

        open(30,file='Salida/url_donde_guardara_archivos_generados.txt',status='replace',action='write')
            write(30,'(A)') 'URL archivos generados:', ruta%carp_plant
        close(30)
    end subroutine introducir_rutasynombres_desdeterminal

    subroutine editarnanourl()
        implicit none
        character(len=500) :: comando_nano
        integer :: ierr

        comando_nano = 'nano "Entrada/#espejo_a_simular.txt"'
        call execute_command_line(trim(comando_nano), wait=.true., exitstat=ierr)
    end subroutine editarnanourl

    function leer_valor(unit_num) result(val)
        integer, intent(in) :: unit_num
        character(len=256)  :: val, linea
        read(unit_num, '(A)') linea
        val = adjustl(linea(index(linea, ':') + 1 :))
    end function leer_valor

    subroutine leer_primigenios()
        implicit none

        open(30,file='Entrada/ruta_carpetas_extraccion.txt',status='old',action='read')
            ruta%carp_plant = leer_valor(30)
            ruta%carpeta = leer_valor(30)
            ruta%nombre_imagen = leer_valor(30)
        close(30)

        print *, "Carpeta matriz de parametros:   ", trim(ruta%carp_plant)
        print *, "Nombre de carpeta de ronchigram:   ", trim(ruta%carpeta)
        print *, "Nombre de imagen (con datos) de ronchigram:   ", trim(ruta%nombre_imagen)
    end subroutine leer_primigenios

    subroutine trim_carpeta_caso()
        implicit none
        ruta%url_carpeta_caso = trim(trim(ruta%carp_plant)//'/'//trim(ruta%carpeta))
        print *, "Carpeta del caso:   ", ruta%url_carpeta_caso
    end subroutine trim_carpeta_caso

    subroutine crear_directorio_carpetacaso()
        !Sirve para Fortran 2008 en adelante
        implicit none
        integer :: cmd_status

        ! Crear la carpeta en el sistema operativo
        ! En Linux / macOS / Unix:
        call execute_command_line('mkdir -p "'//trim(ruta%url_carpeta_caso)//'"', exitstat=cmd_status)

        ! En Windows (cmd):
        ! call execute_command_line('mkdir "' // trim(carp_plant) // '"', exitstat=cmd_status)

        ! Verificar si se creó correctamente
        if (cmd_status /= 0) then
            write(*,*) 'Error: No se pudo crear el directorio: ', trim(ruta%url_carpeta_caso)
        else
            write(*,*) 'Directorio listo: ', trim(ruta%url_carpeta_caso)
        end if
    end subroutine crear_directorio_carpetacaso

    subroutine asignar_rutas()
        implicit none
        ! ruta%url_param = trim(ruta%carp_plant)//trim('/#espejo_a_simular.txt')
        ! ruta%url_param = trim(ruta%carp_plant)//'/'//trim(ruta%nombre_imagen)
        ruta%arch_param = trim(ruta%url_carpeta_caso)//'/datos_'//trim(ruta%nombre_imagen)//'.txt'
        print('(A,X,A)'), 'Archivo de datos:', trim(ruta%arch_param)
    end subroutine asignar_rutas

 ! ==================================================================================================================================
    subroutine crear_fichero_datos_prueba()
        implicit none
        open(30,file=trim(ruta%url_carpeta_caso)//'/prueba_'//trim(ruta%nombre_imagen)//'.txt',status='replace',action='write')
            write(30,'(A)') 'Di,nlp,z0,alfa,beta,gamma,np,phi'
            write(30,'(F15.8,F15.8,F15.8,F15.8,F15.8,F15.8,I10,F15.8)') 7.0,50.0,93.4,0.0,0.0,93.4,50,0.018
        close(30)
    end subroutine crear_fichero_datos_prueba

    subroutine leer_datos_pruebas()
        implicit none
        open(30,file=trim(ruta%url_carpeta_caso)//'/prueba_'//trim(ruta%nombre_imagen)//'.txt',status='old',action='read')
            read(30,*)
            read(30,*) datos_esp%di,datos_esp%nlp,datos_esp%z0,datos_esp%alfa,datos_esp%beta,datos_esp%gamma,&
            &datos_esp%np,datos_esp%phi
        close(30)
    end subroutine leer_datos_pruebas

    subroutine crear_fichero_datosparam()
        implicit none
        open(30,file=trim(ruta%arch_param),status='replace',action='write')
            write(30,'(A,X,F20.8)') 'Diametro:'
            write(30,'(A,X,I4)') 'Nlp:'
            write(30,'(A,X,F20.8)') 'z0:'
            write(30,'(A,X,F20.8)') 'alfa:'
            write(30,'(A,X,F20.8)') 'beta:'
            write(30,'(A,X,F20.8)') 'gamma:'
            write(30,'(A,X,I8)') 'np:'
            write(30,'(A,X,F15.12)') 'phi:'
        close(30)
    end subroutine crear_fichero_datosparam

    subroutine pedir_datosparam_terminal()
        implicit none
        write(*,'(A,X)') 'Diametro:'
        read(*,*) datos_esp%di
        write(*,'(A,X)') 'Nlp:'
        read(*,*) datos_esp%nlp
        write(*,'(A,X)') 'z0:'
        read(*,*) datos_esp%z0
        write(*,'(A,X)') 'alfa:'
        read(*,*) datos_esp%alfa
        write(*,'(A,X)') 'beta:'
        read(*,*) datos_esp%beta
        write(*,'(A,X)') 'gamma:'
        read(*,*) datos_esp%gamma
        write(*,'(A,X)') 'np:'
        read(*,*) datos_esp%np
        write(*,'(A,X)') 'phi:'
        read(*,*) datos_esp%phi
    end subroutine pedir_datosparam_terminal

    subroutine vaciar_determinal_datosparam()
        implicit none
        open(30,file=trim(ruta%arch_param),status='replace',action='write')
            write(30,'(A,X,F20.8)') 'Diametro:', datos_esp%di
            write(30,'(A,6X,F20.8)') 'Nlp:', datos_esp%nlp
            write(30,'(A,7X,F20.8)') 'z0:', datos_esp%z0
            write(30,'(A,5X,F20.8)') 'alfa:', datos_esp%alfa
            write(30,'(A,5X,F20.8)') 'beta:', datos_esp%beta
            write(30,'(A,4X,F20.8)') 'gamma:', datos_esp%gamma
            write(30,'(A,7X,I10)') 'np:', datos_esp%np
            write(30,'(A,6X,F20.8)') 'phi:', datos_esp%phi
        close(30)
    end subroutine vaciar_determinal_datosparam

    subroutine recibirterycrear_dat_fichero_datosparam()
        implicit none
        write(*,'(A,X)') 'Diametro:'
        read(*,*) datos_esp%di
        write(*,'(A,X)') 'Nlp:'
        read(*,*) datos_esp%nlp
        write(*,'(A,X)') 'z0:'
        read(*,*) datos_esp%z0
        write(*,'(A,X)') 'alfa:'
        read(*,*) datos_esp%alfa
        write(*,'(A,X)') 'beta:'
        read(*,*) datos_esp%beta
        write(*,'(A,X)') 'gamma:'
        read(*,*) datos_esp%gamma
        write(*,'(A,X)') 'np:'
        read(*,*) datos_esp%np
        write(*,'(A,X)') 'phi:'
        read(*,*) datos_esp%phi

        open(30,file=trim(ruta%arch_param),status='replace',action='write')
            write(30,'(A,X,F20.8)') 'Diametro:', datos_esp%di
            write(30,'(A,X,I4)') 'Nlp:', datos_esp%nlp
            write(30,'(A,X,F20.8)') 'z0:', datos_esp%z0
            write(30,'(A,X,F20.8)') 'alfa:', datos_esp%alfa
            write(30,'(A,X,F20.8)') 'beta:', datos_esp%beta
            write(30,'(A,X,F20.8)') 'gamma:', datos_esp%gamma
            write(30,'(A,X,I8)') 'np:', datos_esp%np
            write(30,'(A,X,F15.12)') 'phi:', datos_esp%phi
        close(30)
    end subroutine recibirterycrear_dat_fichero_datosparam

    subroutine editarnano_datosparam()
        implicit none
        character(len=500) :: comando_nano
        integer :: ierr

        comando_nano = 'nano "'//trim(ruta%arch_param)//'"'
        call execute_command_line(trim(comando_nano), wait=.true., exitstat=ierr)
    end subroutine editarnano_datosparam

    subroutine leer_archivo() !creo que no es necesario porque se asigna en la lectura. (en reiniciar no)
        implicit none
        character(len=256) :: A
        real(dp) :: numbero
        integer :: I
        open(30,file=trim(ruta%arch_param),status='old',action='read')
            A=leer_valor(30); read(A,*) numbero; datos_esp%di=numbero!leer como string y cambiar a tipo de varible
            A=leer_valor(30); read(A,*) numbero; datos_esp%nlp=numbero
            A=leer_valor(30); read(A,*) numbero; datos_esp%z0=numbero
            A=leer_valor(30); read(A,*) numbero; datos_esp%alfa=numbero
            A=leer_valor(30); read(A,*) numbero; datos_esp%beta=numbero
            A=leer_valor(30); read(A,*) numbero; datos_esp%gamma=numbero
            A=leer_valor(30); read(A,*) I; datos_esp%np=I
            A=leer_valor(30); read(A,*) numbero; datos_esp%phi=numbero

            ! print*, "Leer valor sin trim: ", A
            ! print*, 'Ahora numero', numbero
            ! read(30,'(A,X,F20.8)') A,datos_esp%di !leer como string y cambiar a tipo de varible
            ! read(30,'(A,X,I4)') A,A,datos_esp%nlp
            ! read(30,'(A,X,F20.8)') A, datos_esp%z0
            ! read(30,'(A,X,F20.8)') A, datos_esp%alfa
            ! read(30,'(A,X,F20.8)') A, datos_esp%beta
            ! read(30,'(A,X,F20.8)') A, datos_esp%gamma
            ! read(30,'(A,X,I8)') A, datos_esp%np
            ! write(30,'(A,X,F15.12)') A, datos_esp%phi
        close(30)
    end subroutine leer_archivo
 ! ==================================================================================================================================

    function func_param_azar(cantidad_decim, min_val, max_val) result(paramet)
        implicit none
        ! Recibe el factor (10.0, 100.0, etc.), el mínimo y el máximo
        integer, intent(in) :: cantidad_decim
        real(dp), intent(in) :: min_val, max_val
        real(dp) :: factor_decim
        real(dp)             :: paramet
        integer :: i
        
        ! 1. Genera el número aleatorio base en [0.0, 1.0)
        call random_number(paramet)
        
        ! 2. Escala al rango físico real
        paramet = min_val + paramet * (max_val - min_val)
        
        ! 3. Redondea usando tu factor basado en potencias de 10
        factor_decim=1.0_dp
        do i = 1, cantidad_decim
            factor_decim=factor_decim*10 
        end do
        paramet = anint(paramet * factor_decim) / factor_decim
    end function func_param_azar

end module utiles
!Sin definir bien ruta de salida y ruta de entrada. Hacerlo un modulo

module inicios
    implicit none
    
contains
    subroutine inicio()
        use utiles
        implicit none

        write(*,*) '¿Quieres iniciar/reiniciar las carpetas? (Afirmatuvo: Sí,s)'
        read(*,*) while%respuesta
        while%ajuste_ok = afirmativo(while%respuesta)

        if ( while%ajuste_ok ) call inicializar

        call editar
    end subroutine inicio

    subroutine inicializar()
        use utiles
        implicit none
        call reloj_inicio()
        call crear_directorios_IO()
        call introducir_rutasynombres_desdeterminal()
        call leer_primigenios()
        call trim_carpeta_caso()
        call crear_directorio_carpetacaso()
        call asignar_rutas()
        call crear_fichero_datos_prueba()
        call leer_datos_pruebas()
        call vaciar_determinal_datosparam()
    end subroutine inicializar

    subroutine editar()
        use utiles
        implicit none
        
        call reloj_inicio()
        call leer_primigenios()
        call trim_carpeta_caso()
        call asignar_rutas()

        write(*,*) '¿Quieres cambiar los parametros?'
        read(*,*) while%respuesta

        while%ajuste_ok = afirmativo(while%respuesta)

        if ( while%ajuste_ok ) then
            call editarnano_datosparam
        end if

        call leer_archivo()
    end subroutine editar
    
end module inicios

! module datos_compartidos
!     use utiles
!   implicit none
!   real(dp), allocatable :: arr_x(:), arr_y(:),arr_xy(:,:)
!   integer :: tam_pupila

! contains

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

!   subroutine asegurar_dimension_arry_y(n)
!     implicit none
!     integer, intent(in) :: n

!     if (.not. allocated(arr_y)) then
!       allocate(arr_y(n))
!     else if (size(arr_y) /= n) then
!       ! Si ya existe pero con diferente tamaño, se redimensiona
!       deallocate(arr_y)
!       allocate(arr_y(n))
!     end if
!   end subroutine asegurar_dimension_arry_y

!   subroutine asegurar_dimension_arry_xy(n)
!     implicit none
!     integer, intent(in) :: n

!     if (.not. allocated(arr_xy)) then
!       allocate(arr_xy(2,n))
!     else if (size(arr_xy) /= 2*n) then
!       ! Si ya existe pero con diferente tamaño, se redimensiona
!       deallocate(arr_xy)
!       allocate(arr_xy(2,n))
!     end if
!   end subroutine asegurar_dimension_arry_xy

! end module datos_compartidos

! module calculos_sagita
!     use utiles
!     implicit none
!     real(dp) :: sdi, delta, ra, c, x, y, raiz,z,zx,zy,raiz_max,z_max,deno,numx0,numy0,tx,ty,tx_ron,ty_ron

! contains

!     function dist(a,b) result(c)
!         implicit none
!         real(dp), intent(in) :: a,b
!         real(dp) :: c
!         c=dsqrt(a**2+b**2)
!     end function dist

!     subroutine comun(k,c,ra,x,y,sdi)
!         implicit none
!         real(dp), intent(in) :: k,c,ra,x,y,sdi
!         ! real(dp), intent(out) :: raiz,z,zx,zy,raiz_max,z_max,deno
        
!         raiz=dsqrt(1.0_dp-(k+1.0_dp)*c**2*ra**2)
!         z=(c*ra**2)/(1.0_dp+raiz)
!         zx=c*x/(raiz)
!         zy=c*y/(raiz)

!         raiz_max=dsqrt(1.0_dp-(k+1.0_dp)*c**2*sdi**2)
!         z_max=(c*sdi**2)/(1.0_dp+raiz_max)
!         deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
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

!     subroutine rejilla(tipo,aberr_tx,tx_esp,ty_esp)
!         implicit none
!         character(len=3), intent(in) :: tipo
!         real(dp), intent(in) :: aberr_tx,tx_esp,ty_esp
!         real(dp)::argx,x_visib,y_visib

!         argx=(2.0_dp*pi*aberr_tx/delta)

!         if(tipo=='bin') then
!             write(40,*) tx_esp,ty_esp,(dcos(argx)+1)/2
!         else if(tipo=='cos') then
!             if(dcos(argx)>0.0_dp) then
!                 x_visib=tx_esp; y_visib=ty_esp
!                 write(40,*) x_visib,y_visib
!             endif
!         endif

!     end subroutine rejilla

!     subroutine rejilla2(tipo,aberr_tx,aberr_ty)
!         implicit none
!         character(len=3), intent(in) :: tipo
!         real(dp), intent(in) :: aberr_tx,aberr_ty
!         real(dp)::argx,argy

!         if(tipo=='bin2') then
!         ! write(40,*) txron,tyron,(dcos(argx)+1)/2
!         ! if(dcos(argx)>0.0_dp) then
!         !         write(40,*) txron,tyron
!         !     endif
!         else if(tipo=='cos2') then

!         endif

!         argx=(2.0_dp*pi*aberr_tx/delta); argy=(2.0_dp*pi*aberr_ty/delta)
!     end subroutine rejilla2
! end module calculos_sagita

! module simulador
!     use utiles; use datos_compartidos
!     use calculos_sagita
!     implicit none
!     character(len=20) :: tipo_if1(2,5)
!     character(len=20) :: tipo_if2(2,8)
!     character(len=3) :: tipo_rejilla
!     integer :: datos_ciclo_do(5,4), tipo_num
    
! contains
!     subroutine leer_datos
!         implicit none
!         datos_esp%di = 14.0_dp; datos_esp%nlp = 50.0_dp; datos_esp%z0= 99.5_dp
!         datos_esp%alfa = 0.0_dp; datos_esp%beta = 0.0_dp; datos_esp%gamma= 99.5_dp
!         datos_esp%phi=0.0_dp; datos_esp%np=100
!     end subroutine leer_datos

!     subroutine ctes_sim()
!         implicit none
!         sdi=datos_esp%di/2.0_dp; delta=2.54_dp/datos_esp%nlp; c=1.0_dp/datos_esp%rc
!     end subroutine ctes_sim

!     !datos ciclo do
!     subroutine datos_para_ciclodo_if1()
!         implicit none
!         character(len=20) :: tipo
!         integer :: i,tam, tam1, tam2, fila

!         write(*,*) 'Ellige uno: completo, franja_simetrico, franja_cualq, fila_cualq, diametro'
!         read(*,*) tipo

!         tipo_if1(1,1) = 'completo'; tipo_if1(1,2) = 'franja_simetrico'; tipo_if1(1,3) = 'franja_cualq'
!         tipo_if1(1,4) = 'fila_cualq'; tipo_if1(1,5) = 'diametro'
!         tipo_if1(2,1) = '1'; tipo_if1(2,2) = '2'; tipo_if1(2,3) = '3'; tipo_if1(2,4) = '4'; tipo_if1(2,5) = '5'

!         print*, tipo_if1

!         do i = 1, 6
!             if (tipo_if1(2,i)==tipo) then
!                 ! read(tipo_if1(2,i),*) tipo_num
!                 print*, 'Numero i', i
!                 tipo_num = i
!                 exit
!             endif

!         end do

!         print*, 'Numero asociado:', tipo_num
!         tam=20
!         tam1=-45; tam2=15
!         fila=3

!         datos_ciclo_do(1,1) = 2*datos_esp%np+1; datos_ciclo_do(1,2) = -datos_esp%np; 
!         datos_ciclo_do(1,3) = datos_esp%np; datos_ciclo_do(1,4) = 1;
!         datos_ciclo_do(2,1) = 2*tam+1; datos_ciclo_do(2,2) = -tam; datos_ciclo_do(2,3) = tam; datos_ciclo_do(2,4) = 1;
!         datos_ciclo_do(3,1) = abs(tam1)+abs(tam2); datos_ciclo_do(3,2) = tam1; datos_ciclo_do(3,3) = tam2; datos_ciclo_do(3,4) = 1;
!         datos_ciclo_do(4,1) = 1; datos_ciclo_do(4,2) = fila!; datos_ciclo_do(4,3) = 0; datos_ciclo_do(4,4) = 1;
!         datos_ciclo_do(5,1) = 1; datos_ciclo_do(5,2) = 0!; datos_ciclo_do(5,3) = 0; datos_ciclo_do(5,4) = 1;
!     end subroutine datos_para_ciclodo_if1

!     subroutine datos_para_ciclodo_if2()
!         implicit none
!         character(len=7) :: tipo
!         integer :: i,tam, tam1, tam2, fila

!         write(*,*) 'Ellige uno: birbin, bircos, ronbinx,roncosx,ronbiny,roncosy'
!         read(*,*) tipo

!         tipo_if2(1,1) = 'birbin'; tipo_if2(1,2) = 'bircos'; tipo_if2(1,3) = 'ronbinx'
!         tipo_if2(1,4) = 'roncosx'; tipo_if2(1,5) = 'ronbiny'; tipo_if2(1,6) = 'roncosy'
!         tipo_if2(1,7) = 'perbinx'; tipo_if2(1,8) = 'percosy'
!         tipo_if2(2,1) = '1'; tipo_if2(2,2) = '2'; tipo_if2(2,3) = '3'; tipo_if2(2,4) = '4'; 
!         tipo_if2(2,5) = '5'; tipo_if2(2,6) = '6'; tipo_if2(2,7) = '7'; tipo_if2(2,8) = '8'

!         print*, tipo_if2

!         do i = 1, 8
!             if (tipo_if2(2,i)==tipo) then
!                 ! read(tipo_if2(2,i),*) tipo_num
!                 print*, 'Numero i', i
!                 tipo_num = i
!                 exit
!             endif

!         end do

!         print*, 'Numero asociado:', tipo_num
!         tam=20
!         tam1=-3; tam2=5
!         fila=3
        
!     end subroutine datos_para_ciclodo_if2

!     subroutine ciclo_do()
!         implicit none
!         integer :: i,j,cont
!         ! real(dp) :: ra

!         cont=0
!         do i = -datos_esp%np, datos_esp%np
!             cont=cont+1
!             arr_x(cont) = real(i,dp)*sdi/real(datos_esp%np, dp)
!         end do

!         print*, 'tipo_num', tipo_num

!         print*, 'Datos ciclos do:', datos_ciclo_do(tipo_num,2), datos_ciclo_do(tipo_num,3)


!         if (tipo_num == 1.or.tipo_num == 2.or.tipo_num == 3) then !completo
!             cont=0
!             do j = datos_ciclo_do(tipo_num,2), datos_ciclo_do(tipo_num,3),datos_ciclo_do(tipo_num,4)
!             cont=cont+1
!             arr_y(cont) = real(j,dp)*sdi/real(datos_esp%np, dp)
!             end do
!         else if (tipo_num == 4.or.tipo_num==5) then !fila cualqu
!             j=datos_ciclo_do(tipo_num,2)
!             arr_y(1)=real(j,dp)
!         endif

!         tam_pupila=0

!         ! write(10,*) "size(arr_x), size(arr_y)", size(arr_x), size(arr_y)

!         do i = 1, size(arr_x)
!         do j = 1, size(arr_y)
!             if(dist(arr_x(i),arr_y(j))<=sdi) then 
!                 tam_pupila = tam_pupila+1
!             endif
!         enddo
!         enddo

!         print*, 'tamaño pupila', tam_pupila

!     end subroutine ciclo_do

!     !filtrar pupila, nuevo tamaño
!     subroutine arreglo_pupila()
!         implicit none
!         ! type1, intent(in) :: arg1
!         ! type2, intent(out) ::  arg2
!         integer :: i,j, cont

!         cont=0

!         do i = 1, size(arr_x)
!         do j = 1, size(arr_y)
!             if(dist(arr_x(i),arr_y(j))<=sdi) then
!                 cont=cont+1
!                 arr_xy(1,cont) = arr_x(i)
!                 arr_xy(2,cont) = arr_y(j)
!             endif
!         enddo
!         enddo
        
!     end subroutine arreglo_pupila
    
!     !comun calculos

!     subroutine calculos_comunes(pupilax,pupilay)
!         real(dp), intent(in) :: pupilax,pupilay
!         real(dp) :: txron,tyron,argx,argy
!         integer, parameter :: arch = 40

!         x=pupilax ; y=pupilay;

!         numx0= num(x,y,zx,zy,datos_esp%alfa,datos_esp%beta,z)
!         numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)

!         if ( tipo_num==1.or.tipo_num==2 ) then !if ( "bironchigrama" == 'bironchigrama' ) then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)

!             argx = (2.0_dp*pi*tx)/delta
!             argy = (2.0_dp*pi*ty)/delta

!             txron = aberracion_t(x,z_max,z,numx0,deno)
!             tyron = aberracion_t(y,z_max,z,numy0,deno)
            
!             if ( tipo_num==1  ) then

!                 if(cos(argx)>=0.and.cos(argy)>=0) then
!                     write(arch,*) txron, tyron
!                 endif

!             else if ( tipo_num==2  ) then
!                 write(arch,*) txron,tyron,(cos(argx)+1)/4 + (cos(argy)+1)/4
!             end if  
        
!         else if (tipo_num==3.or.tipo_num==4 ) then!else if ("ronchigrama vertical x" == 'ronchigrama') then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             argx = (2.0_dp*pi*tx)/delta

!             if ( tipo_num==3  ) then
!                 if(cos(argx)>=0.0_dp) then
!                     txron = aberracion_t(x,z_max,z,numx0,deno)
!                     tyron = aberracion_t(y,z_max,z,numy0,deno)
!                     write(arch,*) txron, tyron
!                 endif
!             else if ( tipo_num==4  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 tyron = aberracion_t(y,z_max,z,numy0,deno)
!                 write(arch,*) txron, tyron, (cos(argx)+1)/2
!             end if  
!         else if (tipo_num==5.or.tipo_num==6) then!else if ("ronchigrama horizontal y" == 'ronchigrama') then
!             numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
!             ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)
!             argy = (2.0_dp*pi*ty)/delta

!             if ( tipo_num==5  ) then

!                 if(cos(argy)>=0.0_dp) then
!                     txron = aberracion_t(x,z_max,z,numx0,deno)
!                     tyron = aberracion_t(y,z_max,z,numy0,deno)
!                     write(arch,*) txron, tyron
!                 endif

!             else if ( tipo_num==6  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 tyron = aberracion_t(y,z_max,z,numy0,deno)
!                 write(arch,*) txron, tyron, (cos(argy)+1)/2
!             end if  
!         else if (tipo_num==7.or.tipo_num==8) then!else if perfil de fila
            
!             tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
!             argx = (2.0_dp*pi*tx)/delta

!             if ( tipo_num==7  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 if(cos(argx)>=0.0_dp) then                    
!                     write(arch,*) txron, 0.0_dp
!                 else
!                     write(arch,*) txron, 1.0_dp
!                 endif
!             else if ( tipo_num==8  ) then
!                 txron = aberracion_t(x,z_max,z,numx0,deno)
!                 write(arch,*) txron, (cos(argx)+1)/2
!             end if   
!         end if 
!     end subroutine calculos_comunes
! end module simulador
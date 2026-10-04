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

module calculos_ronchigrama
    use utiles
    implicit none
    real(dp) ::  x, y, raiz,z,zx,zy,raiz_max,z_max,deno,numx0,numy0,tx,ty,tx_ron,ty_ron
    real(dp) :: sdi, delta, c
    character(len=20) :: tipo_if1(2,5)
    character(len=20) :: tipo_if2(2,8)
    character(len=3) :: tipo_rejilla
    integer :: datos_ciclodo_if1(5,4), tipo_num, tipo_numif2
    real(dp), allocatable :: arr_x(:), arr_y(:)

contains

    subroutine simulacion_png(nombre_puntos_ronchigrama)
        implicit none
        character(len=72), intent(in) :: nombre_puntos_ronchigrama
        integer :: i, j

        call leer_datos
        call ctes_sim
        call datos_if1
        call asegurar_dimension_arrx
        call asegurar_dimension_arry(datos_ciclodo_if1(tipo_num,1))
        call if1
        call datos_if2

        open(40,file=nombre_puntos_ronchigrama,status='replace')
            do i = 1, size(arr_x)
            do j = 1, size(arr_y)
                if(dist(arr_x(i),arr_y(j)) <= sdi) then 
                    call comun(datos_esp%k,c,dist(arr_x(i),arr_y(j)),arr_x(i),arr_y(j),sdi)
                    call if2(arr_x(i),arr_y(j))
                end if
            enddo
            enddo
        close(40)

    end subroutine simulacion_png

    subroutine simulacion_array
        implicit none
        integer :: cont!,i,j

        !Subrutina, tamaño fila pupila
        cont=0

        ! do i = 1, size(arr_x)
        ! do j = 1, size(arr_y)
        !     if(dist(arr_x(i),arr_y(j))<=sdi) then
        !         cont=cont+1
        !         arr_xy(1,cont) = arr_x(i)
        !         arr_xy(2,cont) = arr_y(j)
        !     endif
        ! enddo
        ! enddo
    end subroutine simulacion_array

    subroutine asegurar_dimension_arrx()
        implicit none
        integer :: n

        n=2*datos_esp%np+1

        if (.not. allocated(arr_x)) then
        allocate(arr_x(n))
        else if (size(arr_x) /= n) then
        ! Si ya existe pero con diferente tamaño, se redimensiona
        deallocate(arr_x)
        allocate(arr_x(n))
        end if
    end subroutine asegurar_dimension_arrx

    subroutine asegurar_dimension_arry(n)
     implicit none
     integer, intent(in) :: n

        if (.not. allocated(arr_y)) then
        allocate(arr_y(n))
        else if (size(arr_y) /= n) then
        ! Si ya existe pero con diferente tamaño, se redimensiona
        deallocate(arr_y)
        allocate(arr_y(n))
        end if
    end subroutine asegurar_dimension_arry

    function dist(a,b) result(distancia)
        implicit none
        real(dp), intent(in) :: a,b
        real(dp) :: distancia
        distancia=dsqrt(a**2+b**2)
    end function dist

    subroutine comun(k,c,ra,x,y,sdi)
        implicit none
        real(dp), intent(in) :: k,c,ra,x,y,sdi
        
        raiz=dsqrt(1.0_dp-(k+1.0_dp)*c**2*ra**2)
        z=(c*ra**2)/(1.0_dp+raiz)
        zx=c*x/(raiz)
        zy=c*y/(raiz)

        raiz_max = dsqrt(1.0_dp-(k+1.0_dp)*c**2*sdi**2)
        z_max = (c*sdi**2)/(1.0_dp+raiz_max)
        deno = (datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
    end subroutine comun
    
    function num(vi,vj,pvi,pvj,vli,vlj,vk) result(numerador_i)
        implicit none
        real(dp), intent(in):: vi,vj,pvi,pvj,vli,vlj,vk
        real(dp) :: numerador_i
        numerador_i = (vi-vli)*(1.0_dp-pvi*pvi+pvj*pvj)-2.0_dp*pvi*(pvj*(vj-vlj)+(datos_esp%gamma-vk))
    end function num

    function aberracion_t(vi,vkp,vk,nume,denom) result(abtr)
        real(dp), intent(in) :: vi,vk,nume,denom,vkp
        real(dp) :: abtr

        abtr = vi+(vkp-vk)*(nume/denom)
    end function aberracion_t

    subroutine leer_datos
        implicit none
        datos_esp%di = 14.0_dp; datos_esp%nlp = 50.0_dp; datos_esp%z0= 99.5_dp
        datos_esp%alfa = 0.0_dp; datos_esp%beta = 0.0_dp; datos_esp%gamma= 99.5_dp
        datos_esp%phi=0.0_dp; datos_esp%np= 100; datos_esp%rc=100.0_dp; datos_esp%k =-1.80_dp
    end subroutine leer_datos

    subroutine ctes_sim()
        implicit none
        sdi=datos_esp%di/2.0_dp; delta=2.54_dp/datos_esp%nlp; c=1.0_dp/datos_esp%rc
    end subroutine ctes_sim

    subroutine datos_if1()
        implicit none
        character(len=20) :: tipo
        integer :: tam_compl,tam_fransi,tam_francu,tam_filacu,tam_diam
        integer :: lim_arr1,lim_abj1,lim_arr2,lim_abj2,lim_arr3,lim_abj3, fila, diam
        integer :: i, paso_comp, paso_fransi, paso_francu, paso_fila, paso_diam

        tipo_if1(1,1) = 'completo'; tipo_if1(1,2) = 'franja_simetrico'; tipo_if1(1,3) = 'franja_cualq'
        tipo_if1(1,4) = 'fila_cualq'; tipo_if1(1,5) = 'diametro'
        tipo_if1(2,1) = '1'; tipo_if1(2,2) = '2'; tipo_if1(2,3) = '3'; tipo_if1(2,4) = '4'; tipo_if1(2,5) = '5'

        write(*,*) 'Ellige uno:'
        print*, tipo_if1(1,:)
        read(*,*) tipo

        do i = 1, 5
            if (tipo_if1(1,i)==tipo) then
                read(tipo_if1(2,i),*) tipo_num
                exit
            endif
        end do

        print*, 'Numero asociado:', tipo_num
        
        lim_arr1 = datos_esp%np; lim_abj1 = -datos_esp%np; tam_compl = 2*datos_esp%np+1; paso_comp = 1
        lim_arr2 = 10; lim_abj2 = -lim_arr2; tam_fransi = 2*lim_arr2+1; paso_fransi = 1
        lim_arr3 = 10; lim_abj3 = -15; tam_francu = abs(lim_arr3)+abs(lim_abj3); paso_francu = 1
        tam_filacu = 1; fila = -2; paso_fila = 1
        tam_diam = 1; diam = 0; paso_diam = 1

        datos_ciclodo_if1(1,1) = tam_compl; datos_ciclodo_if1(1,2) = lim_arr1; datos_ciclodo_if1(1,3) = lim_abj1; 
        datos_ciclodo_if1(1,4) = paso_comp;

        datos_ciclodo_if1(2,1) = tam_fransi; datos_ciclodo_if1(2,2) = lim_arr2; datos_ciclodo_if1(2,3) = lim_abj2; 
        datos_ciclodo_if1(2,4) = paso_fransi;

        datos_ciclodo_if1(3,1) = tam_filacu; datos_ciclodo_if1(3,2) = lim_arr3; datos_ciclodo_if1(3,3) = lim_abj3; 
        datos_ciclodo_if1(3,4) = paso_fransi;

        datos_ciclodo_if1(4,1) = tam_filacu; datos_ciclodo_if1(4,2) = fila; datos_ciclodo_if1(4,4) = paso_fila;

        datos_ciclodo_if1(5,1) = tam_diam; datos_ciclodo_if1(5,2) = diam;  datos_ciclodo_if1(5,4) = paso_diam;

        print*, datos_ciclodo_if1(tipo_num,:)
    end subroutine datos_if1

    subroutine datos_if2()
        implicit none
        character(len=7) :: tipo
        integer :: i
        tipo_if2(1,1) = 'birbin'; tipo_if2(1,2) = 'bircos'; tipo_if2(1,3) = 'ronbinx'
        tipo_if2(1,4) = 'roncosx'; tipo_if2(1,5) = 'ronbiny'; tipo_if2(1,6) = 'roncosy'
        tipo_if2(1,7) = 'perbinx'; tipo_if2(1,8) = 'percosy'
        tipo_if2(2,1) = '1'; tipo_if2(2,2) = '2'; tipo_if2(2,3) = '3'; tipo_if2(2,4) = '4'; 
        tipo_if2(2,5) = '5'; tipo_if2(2,6) = '6'; tipo_if2(2,7) = '7'; tipo_if2(2,8) = '8'

        write(*,*) 'Ellige uno:'
        write(*,*) tipo_if2(1,:)
        read(*,*) tipo

        do i = 1, 8
            if (tipo_if2(1,i)==tipo) then
                read(tipo_if2(2,i),*) tipo_numif2
                exit
            endif
        end do

        print*, 'Numero asociado:', tipo_numif2
    end subroutine datos_if2

    subroutine if1()
        implicit none
        integer :: i,j,cont

        cont=0
        do i = -datos_esp%np, datos_esp%np
            cont=cont+1
            arr_x(cont) = real(i,dp)*sdi/real(datos_esp%np, dp)
        end do

        if (tipo_num == 1.or.tipo_num == 2.or.tipo_num == 3) then !más de una fila
            cont=0
            do j = datos_ciclodo_if1(tipo_num,2), datos_ciclodo_if1(tipo_num,3),datos_ciclodo_if1(tipo_num,4)
            cont=cont+1
            arr_y(cont) = real(j,dp)*sdi/real(datos_esp%np, dp)
            end do
        else if (tipo_num == 4.or.tipo_num==5) then !fila cualqu
            j=datos_ciclodo_if1(tipo_num,2)
            arr_y(1)=real(j,dp)
        endif

        write(*,*) "size(arr_x), size(arr_y)", size(arr_x), size(arr_y)

    end subroutine if1
    
    subroutine if2(pupilax,pupilay)
        real(dp), intent(in) :: pupilax,pupilay
        real(dp) :: txron,tyron,argx,argy
        integer, parameter :: arch = 40

        x=pupilax ; y=pupilay;

        numx0= num(x,y,zx,zy,datos_esp%alfa,datos_esp%beta,z)
        numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)

        if (tipo_numif2==1.or.tipo_numif2==2) then !if ( "bironchigrama" == 'bironchigrama' ) then
            numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
            tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
            ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)

            argx = (2.0_dp*pi*tx)/delta
            argy = (2.0_dp*pi*ty)/delta

            txron = aberracion_t(x,z_max,z,numx0,deno)
            tyron = aberracion_t(y,z_max,z,numy0,deno)
            
            if ( tipo_numif2==1  ) then

                if(cos(argx)>=0.and.cos(argy)>=0) then
                    write(arch,*) txron, tyron
                endif

            else if ( tipo_numif2==2  ) then
                write(arch,*) txron,tyron,(cos(argx)+1)/4 + (cos(argy)+1)/4
            end if  
        
        else if (tipo_numif2==3.or.tipo_numif2==4 ) then!else if ("ronchigrama vertical x" == 'ronchigrama') then
            numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
            tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
            argx = (2.0_dp*pi*tx)/delta

            if ( tipo_numif2==3  ) then
                if(cos(argx)>=0.0_dp) then
                    txron = aberracion_t(x,z_max,z,numx0,deno)
                    tyron = aberracion_t(y,z_max,z,numy0,deno)
                    write(arch,*) txron, tyron
                endif
            else if ( tipo_numif2==4  ) then
                txron = aberracion_t(x,z_max,z,numx0,deno)
                tyron = aberracion_t(y,z_max,z,numy0,deno)
                write(arch,*) txron, tyron, (cos(argx)+1)/2
            end if  
        else if (tipo_numif2==5.or.tipo_numif2==6) then!else if ("ronchigrama horizontal y" == 'ronchigrama') then
            numy0= num(y,x,zy,zx,datos_esp%beta,datos_esp%alfa,z)
            ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)
            argy = (2.0_dp*pi*ty)/delta

            if ( tipo_numif2==5  ) then

                if(cos(argy)>=0.0_dp) then
                    txron = aberracion_t(x,z_max,z,numx0,deno)
                    tyron = aberracion_t(y,z_max,z,numy0,deno)
                    write(arch,*) txron, tyron
                endif

            else if ( tipo_numif2==6  ) then
                txron = aberracion_t(x,z_max,z,numx0,deno)
                tyron = aberracion_t(y,z_max,z,numy0,deno)
                write(arch,*) txron, tyron, (cos(argy)+1)/2
            end if  
        else if ((tipo_num==4.or.tipo_num==5).and.(tipo_numif2==7.or.tipo_numif2==8)) then!else if perfil de fila
            
            tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
            argx = (2.0_dp*pi*tx)/delta

            if ( tipo_numif2==7  ) then
                txron = aberracion_t(x,z_max,z,numx0,deno)
                if(cos(argx)>=0.0_dp) then                    
                    write(arch,*) txron, 0.0_dp
                else
                    write(arch,*) txron, 1.0_dp
                endif
            else if ( tipo_numif2==8  ) then
                txron = aberracion_t(x,z_max,z,numx0,deno)
                write(arch,*) txron, (cos(argx)+1)/2
            end if   
        end if 
    end subroutine if2
end module calculos_ronchigrama
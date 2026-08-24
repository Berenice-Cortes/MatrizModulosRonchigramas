! piezas varias ====================================================================================================================
! ==================================================================================================================================
! =========================================utiles o inicialización=====================================================================
! ==================================================================================================================================
module utiles
    implicit none
    integer, parameter :: dp = kind(0.0d0) !Aquí lo que se use en cualquier subrutina

    type :: registro_tiempo
        integer(8) :: count_inicio, count_rate, count_final, valores(8)
        real(dp) :: t_inicial, t_final, tiempo_real, tiempo
    end type registro_tiempo
    type(registro_tiempo) :: reloj
    
    type :: url_rutas
        character(len=150) :: carp_plant, carpeta, nombre_imagen
        character(len=250) :: rutaruta
        character(len=130) :: rutap
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
    end type parametros_espejo
    type(parametros_espejo) :: datos_esp

    type :: param_filtros_ajustes_datos
        real(dp) :: sigma
    end type param_filtros_ajustes_datos
    type(param_filtros_ajustes_datos) :: filt

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
        close(51)
    end subroutine reloj_fin

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

     ! =======================================================================================================================

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

    subroutine rutas_carpetas()
        implicit none

        open(30,file='Salida/url_ubicacion_guardar_archivos_generados.txt',status='old',action='read')
            read(30,'(A)')
            read(30,'(A)') ruta%carp_plant
        close(30)

        print*, 'la ruta que saca es:', trim(ruta%carp_plant)//'/#espejo_a_simular.txt'
        ruta%rutap= trim(ruta%carp_plant)//'/#espejo_a_simular.txt'

        open(30,file= trim(ruta%rutap),status='old',action='read')
        ! open(30,file=trim(ruta%carp_plant)//trim('/#espejo_a_simular.txt'),status='old',action='read')
            read(30,'(A)') ruta%nombre_imagen
        close(30)
    endsubroutine rutas_carpetas

    subroutine ficheros_IO()
        implicit none

        open(unit=40, file='Entrada' // '/datos_.txt', status='replace', action='write')
            write(40, *) 'Di,nlp,z0,alfa,beta,gamma,np,phi'
        close(40)

        open(unit=40, file='Salida' // '/url_ubicacion_guardar_archivos_generados.txt', status='replace', action='write')
            write(40, *) 'Aquí va la ubicación de la carpeta donde se guardara: png-'
            write(40, '(A)') '/Users/berenicecortes/Desktop/carpeta_de_carpetas_plantillas'
        close(40)
    end subroutine ficheros_IO

    subroutine escribirnano()
        implicit none
        character(len=500) :: comando_nano

        print('(A)'), trim(ruta%rutaruta)//'/datos_'//trim(ruta%nombre_imagen)//'.txt'
        comando_nano = 'nano "' // trim(ruta%rutaruta) // '/datos_' // trim(ruta%nombre_imagen) // '.txt"'
    end subroutine escribirnano

    ! subroutine leer_parametros_simulacion()
    !     implicit none
    !     ! datos_esp%di = 14.0_dp; datos_esp%nlp = 50.0_dp; datos_esp%z0= 99.5_dp
    !     ! datos_esp%alfa = 0.0_dp; datos_esp%beta = 0.0_dp; datos_esp%gamma= 99.5_dp
    !     ! datos_esp%np=50; datos_esp%phi=0.0_dp
    !     open(30,file= trim(ruta%rutaruta) // '/datos_' // trim(ruta%nombre_imagen) // '.txt',status='old',action='read') !trim(rutas%ruta_datos)
    !     read(30,*)
    !     read(30,*) datos_esp%di,datos_esp%nlp,datos_esp%z0,datos_esp%alfa,datos_esp%beta,datos_esp%gamma,&
    !     &datos_esp%np,datos_esp%phi
    !     close(30)
    ! end subroutine leer_parametros_simulacion

    ! ! subroutine numero_azar(v1,v2)
    !  !     !Ver si es mejor subrutina o una función, para dar rango y paso. 
    !  !     implicit none
    !  !     real(dp), intent(in):: val_in,num_decim,rango
    !  !     real(dp), intent(out):: v1,v2
    !  !     real(dp) :: a,b

    !  !     call random_number(a); call random_number(b); 
    !  !     write(*,'(A,F20.16)') 'Sin redondear Valor de a =', a
    !  !     write(*,'(A,F20.17)') 'Sin redondear Valor de b =', b
    !  !     ! Redondea a 4 decimales
    !  !     a = anint(a * 10.0_dp) / 10.0_dp
    !  !     b = anint(b * 10.0_dp) / 10.0_dp
    !  !     write(*,'(A,F20.16)') 'Redondeado Valor de a =', a
    !  !     write(*,'(A,F20.17)') 'Redondeado Valor de b =', b

    !  !     v1 = 100.0_dp + a
    !  !     v2 = 0.0_dp + b
    ! ! endsubroutine numero_azar

    ! subroutine decimal_azar(num_decim,d1,d2)
    !     !Ver si es mejor subrutina o una función, para dar rango y paso. 
    !     implicit none
    !     real(dp), intent(in):: num_decim
    !     real(dp), intent(out):: d1,d2

    !     call random_number(d1); call random_number(d2); 
    !     write(*,'(A,F20.16)') 'Sin redondear Valor de a =', d1
    !     write(*,'(A,F20.17)') 'Sin redondear Valor de b =', d2
    !     ! Redondea a 4 decimales
    !     d1 = anint(d1 * num_decim) / num_decim
    !     d2 = anint(d2 * num_decim) / num_decim
    ! endsubroutine decimal_azar

    ! subroutine parameter_azar(num_decim,min_val,max_val,paramet)
    !     !Ver si es mejor subrutina o una función, para dar rango y paso. 
    !     implicit none
    !     real(dp), intent(in):: num_decim,min_val,max_val
    !     real(dp), intent(out):: paramet


    !     call random_number(paramet);

    !     paramet=min_val + paramet * (max_val - min_val)
    !     write(*,'(A,F20.16)') 'Sin redondear Valor de a =', paramet

    !     paramet=anint(paramet*num_decim)/num_decim
    ! endsubroutine parameter_azar

    ! function func_param_azar(cantidad_decim, min_val, max_val) result(paramet)
    !     implicit none
    !     ! Recibe el factor (10.0, 100.0, etc.), el mínimo y el máximo
    !     integer, intent(in) :: cantidad_decim
    !     real(dp), intent(in) :: min_val, max_val
    !     real(dp) :: factor_decim
    !     real(dp)             :: paramet
    !     integer :: i
        
    !     ! 1. Genera el número aleatorio base en [0.0, 1.0)
    !     call random_number(paramet)
        
    !     ! 2. Escala al rango físico real
    !     paramet = min_val + paramet * (max_val - min_val)
        
    !     ! 3. Redondea usando tu factor basado en potencias de 10
    !     factor_decim=1.0_dp
    !     do i = 1, cantidad_decim
    !         factor_decim=factor_decim*10 
    !     end do
    !     paramet = anint(paramet * factor_decim) / factor_decim
    ! end function func_param_azar

    ! subroutine agregar_dat_exp()
    !     integer :: i, posicion
    !     real(8) :: coorinfl(cont_pin+2, 3), coor_x_max
    !     open(11, file=trim(carpeta)//'maximini.txt', status='old')
    !     read(11,*)
    !     do i = 1, cont_pin + 2
    !         read(11,*) posicion, coorinfl(i,2), coorinfl(i,3)
    !     end do
    !     close(11)
    !     coor_x_max = coorinfl((cont_pin/2) + 2, 2)
    !     open(unit=10, file=datos, status="unknown", position="append")
    !     write(10,*) m_dat, (coord_centrox - coor_x_max) * un_pixel
    !     close(10)
    ! end subroutine agregar_dat_exp 
end module utiles

! module calculos_varios
!     use utiles
!     implicit none
! contains
!      subroutine normalizar()
!         integer :: i, j, a, b
!         real(8), allocatable :: pos_pix(:), irrad(:), dummy(:)
!         real(8) :: punt_inf(cont_pin+2, 4)
!         allocate(pos_pix(num_dat), irrad(num_dat), dummy(num_dat))
!         open(10, file=trim(carpeta)//'gaussian_filtered.txt', status='old')
!         do i = 1, num_dat
!             read(10,*) pos_pix(i), dummy(i), irrad(i)
!         end do
!         close(10)
!         open(38, file=trim(carpeta)//'maximini.txt', status='old')
!         read(38,*)
!         do i = 1, cont_pin + 2
!             read(38,*) punt_inf(i,1), punt_inf(i,2), punt_inf(i,3)
!         end do
!         close(38)
!         do i = 1, cont_pin + 2, 2
!             punt_inf(i,4) = punt_inf(i,3)
!         end do
!         do i = 2, cont_pin + 2, 2
!             punt_inf(i,4) = punt_inf(i+1,3)
!         end do
!         write(*,'(a,i2,a)') 'rango de grafica (1,', cont_pin+2, ')'
!         read(*,*) a, b
!         m_dat = 0
!         open(27, file=trim(carpeta)//'normalizado_cort.txt', status='replace')
!         do i = 1, num_dat
!             do j = a, b - 1
!                 if (i >= int(punt_inf(j,1)) .and. i < int(punt_inf(j+1,1))) then
!                     write(27,*) pos_pix(i), (irrad(i)-punt_inf(j,4))/abs(punt_inf(j+1,3)-punt_inf(j,3))
!                     m_dat = m_dat + 1
!                 end if
!             end do
!         end do
!         i = int(punt_inf(b,1))
!         write(27,*) pos_pix(i), (irrad(i)-punt_inf(b-1,4))/abs(punt_inf(b,3)-punt_inf(b-1,3))
!         m_dat = m_dat + 1
!         close(27)
!         deallocate(pos_pix, irrad, dummy)
!      end subroutine normalizar

!      subroutine reescalar_coord()
!         integer :: i
!         real(8), allocatable :: x(:), y(:)
!         allocate(x(m_dat), y(m_dat))
!         open(10, file=trim(carpeta)//'normalizado_cort.txt', status='old')
!         do i = 1, m_dat
!             read(10,*) x(i), y(i)
!         end do
!         close(10)
!         open(10, file=trim(carpeta)//'normalizado_cort_cm.txt', status='replace')
!         do i = 1, m_dat
!             write(10,*) (x(i) - coord_centrox) * un_pixel, y(i)
!         end do
!         close(10)
!         deallocate(x, y)
!      end subroutine reescalar_coord

!      subroutine reordenar_trenza(arreglo)
!         implicit none
!         real(dp), intent(inout) :: arreglo(:,:)
!         real(dp), allocatable :: nuevo(:,:)
!         integer :: n, centro, i, desplazamiento, signo

!         n = size(arreglo)/2
!         centro = (n + 1)/2

!         allocate(nuevo(n,2))

!         ! 1. La primera posición del nuevo es el centro del arreglo
!         nuevo(1,1) = arreglo(centro,1)
!         nuevo(1,2) = arreglo(centro,2)

!         ! 2. Llenamos el resto alternando derecha e izquierda
!         signo = 1            ! Empieza hacia la derecha (+1)
!         desplazamiento = 1    ! Distancia al centro

!         do i = 2, n
!             ! Calculamos el índice del arreglo que queremos tomar
!             ! Si i es par, vamos a la derecha. Si es impar, a la izquierda.
!             if (mod(i, 2) == 0) then
!                 nuevo(i,1) = arreglo(centro + desplazamiento,1)
!                 nuevo(i,2) = arreglo(centro + desplazamiento,2)
!             else
!                 nuevo(i,1) = arreglo(centro - desplazamiento,1)
!                 nuevo(i,2) = arreglo(centro - desplazamiento,2)
!                 desplazamiento = desplazamiento + 1 ! Aumentamos la distancia tras un par Izq/Der
!             end if
!         end do

!         !  open(10,file="ver_trenza_mod.txt",status="replace",action="write")
!         !  do i = 1, 2*datos_esp%np+1
!         !     write(10,'(F20.16,F20.16)') nuevo(i,1), nuevo(i,2)
!         !  end do
!         ! close(10)

!         arreglo=nuevo
!         deallocate(nuevo)
!      end subroutine reordenar_trenza

! end module calculos_varios

! module calculos_para_optimizacion
!     use utiles
!     implicit none
! contains
!     subroutine dif_cuadrados(arreglo_base,arreglo_iterado,sumatoria)
!         implicit none
!         real(dp), intent(in) :: arreglo_base(:,:), arreglo_iterado(:,:)
!         real(dp), intent(out) ::  sumatoria
!         real(dp) :: y1,y2
!         integer :: i

!         sumatoria=0.0_dp
!         !open(40,file="Sim1.txt",status='old',action='read')
!         !open(41,file="Sim2.txt",status='old',action='read')
!         do i = 1, 2*datos_esp%np+1
!             y1= arreglo_base(i,2); y2 = arreglo_iterado(i,2)
!             sumatoria = sumatoria + (y2-y1)**2
!         end do
!         !close(40)
!         !close(41)
        
!     end subroutine dif_cuadrados
    
!     subroutine arreglo_minman()
!         implicit none
!         real(8),allocatable :: minman(:,:)
!         integer :: i, num_min, num_max, j
!         real(8), allocatable :: pos_pix(:), irrad(:), dummy(:)
!         logical, allocatable :: es_max(:), es_min(:)

!         allocate(pos_pix(num_dat), irrad(num_dat), dummy(num_dat), es_max(num_dat), es_min(num_dat))

!          open(10, file=trim(carpeta)//'/gaussian_filtered.txt', status='old')
!           do i = 1, num_dat
!             read(10,*) pos_pix(i), dummy(i), irrad(i)
!           end do
!          close(10)

!          es_min = .false.; es_max = .false.; num_min = 0; num_max = 0

!          do i = 2, num_dat - 1
!             if (irrad(i) < irrad(i-1) .and. irrad(i) < irrad(i+1)) then
!                 es_min(i) = .true.; num_min = num_min + 1
!             end if
!             if (irrad(i) > irrad(i-1) .and. irrad(i) > irrad(i+1)) then
!                 es_max(i) = .true.; num_max = num_max + 1
!             end if
!          end do

!          cont_pin = num_min + num_max; j=0

!          allocate(minman(cont_pin,3))

!          do i = 2, num_dat - 1
!             if (es_min(i) .or. es_max(i)) then ; j=j+1
!                 minman(j,1)=i; 
!                 minman(j,2)=pos_pix(i); 
!                 minman(j,3)=irrad(i); 
!             endif
!          end do

!          print'(A)', "Posición en lista"
!          print*, int(minman(:,1))
!          print'(A)', "Posición pixel en x"
!          print*, int(minman(:,2))
!          print'(A)', "Irradiancia de pixel"
!          print*, minman(:,3)
        
!         deallocate(pos_pix, irrad, dummy, es_max, es_min,minman)
!     end subroutine arreglo_minman

!     subroutine puntos_intermedios()
!         implicit none
!         real(8),allocatable :: minman(:,:)
!         integer :: i, num_min, num_max, j
!         real(8), allocatable :: pos_pix(:), irrad(:), dummy(:)
!         logical, allocatable :: es_max(:), es_min(:)

!         allocate(pos_pix(num_dat), irrad(num_dat), dummy(num_dat), es_max(num_dat), es_min(num_dat))

!          open(10, file=trim(carpeta)//'/gaussian_filtered.txt', status='old')
!           do i = 1, num_dat
!             read(10,*) pos_pix(i), dummy(i), irrad(i)
!           end do
!          close(10)

!          es_min = .false.; es_max = .false.; num_min = 0; num_max = 0

!          do i = 2, num_dat - 1
!             if (irrad(i) < irrad(i-1) .and. irrad(i) < irrad(i+1)) then
!                 es_min(i) = .true.; num_min = num_min + 1
!             end if
!             if (irrad(i) > irrad(i-1) .and. irrad(i) > irrad(i+1)) then
!                 es_max(i) = .true.; num_max = num_max + 1
!             end if
!          end do

!          cont_pin = num_min + num_max; j=0

!          !  print*, "Total de maximos y minimos", cont_pin

!          allocate(minman(cont_pin,3))

!          do i = 2, num_dat - 1
!             if (es_min(i) .or. es_max(i)) then ; j=j+1
!                 minman(j,1)=i; 
!                 minman(j,2)=pos_pix(i); 
!                 minman(j,3)=irrad(i); 
!             endif
!          end do

!          !  print'(A)', "Posición en lista"
!          !  print*, int(minman(:,1))
!          !  print'(A)', "Posición pixel en x"
!          !  print*, int(minman(:,2))
!          !  print'(A)', "Irradiancia de pixel"
!          !  print*, minman(:,3)
!          !  print'(A)',
!          !  print'(A)', "Valor intermedio"
!          !  print*, (pos_pix(170)+pos_pix(108))/2

!          !  open(85,file='puntos_binarizar.txt',action='write',position='append')
!          do i = 1, cont_pin-1
!             ! print*, int(minman(i+1,1)), int(minman(i,1))
!             ! print*, int(pos_pix(int(minman(i,1)))),int(pos_pix(int(minman(i+1,1))))
!             ! print*, int(pos_pix(int(minman(i,1)))+pos_pix(int(minman(i+1,1))))
!             ! print*, int(pos_pix(int(minman(i+1,1)))+pos_pix(int(minman(i,1))))/2
!             if(mod(i,2)/=0) then
!                 write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1)))+pos_pix(int(minman(i,1))))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+10)+pos_pix(int(minman(i,1))+10))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+25)+pos_pix(int(minman(i,1))+25))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+30)+pos_pix(int(minman(i,1))+30))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+45)+pos_pix(int(minman(i,1))+45))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+50)+pos_pix(int(minman(i,1))+50))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+55)+pos_pix(int(minman(i,1))+55))/2, FILA_EVF
!                 ! write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1))+58)+pos_pix(int(minman(i,1))+58))/2, FILA_EVF
!             endif
!             write(85,'(I0,3X,I0)') int(pos_pix(int(minman(i+1,1)))+pos_pix(int(minman(i,1))))/2, FILA_EVF
!             ! print*,"----------------------------------------------------"
!          end do
!          !  close(85)
        
!         deallocate(pos_pix, irrad, dummy, es_max, es_min,minman)
!     end subroutine puntos_intermedios

!     subroutine puntos_maxmin()
!         implicit none
!         real(8),allocatable :: minman(:,:)
!         integer :: i, num_min, num_max, j
!         real(8), allocatable :: pos_pix(:), irrad(:), dummy(:)
!         logical, allocatable :: es_max(:), es_min(:)

!         allocate(pos_pix(num_dat), irrad(num_dat), dummy(num_dat), es_max(num_dat), es_min(num_dat))

!          open(10, file=trim(carpeta)//'/gaussian_filtered.txt', status='old')
!           do i = 1, num_dat
!             read(10,*) pos_pix(i), dummy(i), irrad(i)
!           end do
!          close(10)

!          es_min = .false.; es_max = .false.; num_min = 0; num_max = 0

!          do i = 2, num_dat - 1
!             if (irrad(i) < irrad(i-1) .and. irrad(i) < irrad(i+1)) then
!                 es_min(i) = .true.; num_min = num_min + 1
!             end if
!             if (irrad(i) > irrad(i-1) .and. irrad(i) > irrad(i+1)) then
!                 es_max(i) = .true.; num_max = num_max + 1
!             end if
!          end do

!          cont_pin = num_min + num_max; j=0

!          allocate(minman(cont_pin,3))

!          do i = 2, num_dat - 1
!             if (es_min(i) .or. es_max(i)) then ; j=j+1
!                 write(85,'(F18.12,3X,I0)') pos_pix(i), FILA_EVF
!             endif
!          end do
        
!         deallocate(pos_pix, irrad, dummy, es_max, es_min,minman)
!     end subroutine puntos_maxmin

!     subroutine txt_lista_minman()
!         implicit none
!         integer :: i, num_min, num_max
!         real(8), allocatable :: pos_pix(:), irrad(:), dummy(:)
!         logical, allocatable :: es_max(:), es_min(:)

!         allocate(pos_pix(num_dat), irrad(num_dat), dummy(num_dat), es_max(num_dat), es_min(num_dat))

!         open(10, file=trim(carpeta)//'/gaussian_filtered.txt', status='old')
!          do i = 1, num_dat
!             read(10,*) pos_pix(i), dummy(i), irrad(i)
!          end do
!         close(10)

!         es_min = .false.; es_max = .false.; num_min = 0; num_max = 0

!         do i = 2, num_dat - 1
!             if (irrad(i) < irrad(i-1) .and. irrad(i) < irrad(i+1)) then
!                 es_min(i) = .true.; num_min = num_min + 1
!             end if
!             if (irrad(i) > irrad(i-1) .and. irrad(i) > irrad(i+1)) then
!                 es_max(i) = .true.; num_max = num_max + 1
!             end if
!         end do

!         cont_pin = num_min + num_max

!         open(38, file=trim(carpeta)//'/maximini.txt', status='replace')
!          write(38,'(A,4X,A,10X,A)') "numdato", "pospix", "irrad"
!          write(38,'(i4,3x,f12.6,3x,f12.6)') 1, pos_pix(1), irrad(1)

!          do i = 2, num_dat - 1
!             if (es_min(i) .or. es_max(i)) write(38,'(i4,3x,f12.6,3x,f12.6)') i, pos_pix(i), irrad(i)
!          end do

!          write(38,'(i4,3x,f12.6,3x,f12.6)') num_dat, pos_pix(num_dat), irrad(num_dat)
!         close(38)
        
!         deallocate(pos_pix, irrad, dummy, es_max, es_min)
!     end subroutine txt_lista_minman
 
! end module calculos_para_optimizacion

! ! fin de piezas ====================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================


! ! ARMADO ===========================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================
! module simulacion
!     use utiles
!     implicit none 
! contains
!     subroutine ronchigrama_simulado_cos(rc, k)
!         real(dp), intent(in) :: rc, k
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file="salida/ronchigrama_comp.txt",status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=-datos_esp%np,datos_esp%np 
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,tyron,(dcos(argx)+1)/2
!         enddo   
!         enddo
!         close(40)
!     end subroutine ronchigrama_simulado_cos

!     subroutine ronchigrama_simulado_bin(rc, k)
!         real(dp), intent(in) :: rc, k
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file="salida/ronchigrama_comp.txt",status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=-datos_esp%np,datos_esp%np 
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             if(dcos(argx)>0.0_dp) then
!                 write(40,*) txron,tyron
!             endif
!         enddo   
!         enddo
!         close(40)
!     end subroutine ronchigrama_simulado_bin

!     subroutine franja_ronchigrama_simulado_cos(rc,k,valor_y1,valor_y2,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y1,valor_y2
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=valor_y1,valor_y2
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2
!         enddo   
!         enddo
!         close(40)
!     end subroutine franja_ronchigrama_simulado_cos

!     subroutine franja_ronchigrama_simulado_bin(rc,k,valor_y1,valor_y2,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y1,valor_y2
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=valor_y1,valor_y2
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             if(dcos(argx)>0.0_dp) then
!                 write(40,*) txron,tyron
!             endif
!         enddo   
!         enddo
!         close(40)
!     end subroutine franja_ronchigrama_simulado_bin

!     subroutine franja_simetrico_ronchigrama_simulado_cos(rc,k,valor_y,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=-valor_y,valor_y
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2
!         enddo   
!         enddo
!         close(40)
!     end subroutine franja_simetrico_ronchigrama_simulado_cos

!     subroutine franja_simetrico_ronchigrama_simulado_bin(rc,k,valor_y,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!         do j=-valor_y,valor_y
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             if(dcos(argx)>0.0_dp) then
!                 write(40,*) txron,tyron
!             endif
!         enddo   
!         enddo
!         close(40)
!     end subroutine franja_simetrico_ronchigrama_simulado_bin

!     subroutine fila_ronchigrama_simulado_cos(rc,k,valor_y,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!            j=valor_y
!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2   
!         enddo
!         close(40)
!     end subroutine fila_ronchigrama_simulado_cos

!     subroutine fila_ronchigrama_simulado_bin(rc,k,valor_y,NomArc)
!         real(dp), intent(in) :: rc, k
!         integer, intent(in) :: valor_y
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!            j=valor_y

!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2   
!         enddo
!         close(40)
!     end subroutine fila_ronchigrama_simulado_bin

!     subroutine fila_diam_ronchigrama_simulado_cos_array(rc,k,val_sim)
!         real(dp), intent(in) :: rc, k
!         real(dp), intent(inout) :: val_sim(:,:)
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j,cont
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc
!         cont=0

!         !open(40,file=trim(NomArc),status='replace')
!            j=0; cont=0
!         do i=-datos_esp%np,datos_esp%np 
!             cont=cont+1

!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             !write(40,*) txron,(dcos(argx)+1)/2  
!             val_sim(cont,1)= txron; val_sim(cont,2)= (dcos(argx)+1)/2  
!         enddo
!         !close(40)
!     end subroutine fila_diam_ronchigrama_simulado_cos_array

!     subroutine fila_diam_ronchigrama_simulado_cos(rc,k,NomArc)
!         real(dp), intent(in) :: rc, k
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j,cont
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc
!         cont=0

!         open(40,file=trim(NomArc),status='replace')
!            j=0; cont=0
!         do i=-datos_esp%np,datos_esp%np 
!             cont=cont+1

!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2  
!         enddo
!         close(40)
!     end subroutine fila_diam_ronchigrama_simulado_cos

!     subroutine fila_diam_ronchigrama_simulado_bin(rc,k,NomArc)
!         real(dp), intent(in) :: rc, k
!         character(len=20), intent(in) :: NomArc
!         real(dp) :: delta,sdi,c,x,y,ra,raiz,z,zx,zy,raiz_max,z_max
!         real(dp) :: deno,numx0,numy0,tx,ty,txron,tyron,argx,argy
!         real(dp), parameter :: pi=3.1415926535_dp !Buscar si estoy siendo redundante
!         integer :: i,j
    
!         delta=2.54_dp/datos_esp%nlp; sdi=datos_esp%di/2.0_dp; c=1.0_dp/rc

!         open(40,file=trim(NomArc),status='replace')
!         do i=-datos_esp%np,datos_esp%np 
!            j=0

!             x=dfloat(i)*sdi/dfloat(datos_esp%np)
!             y=dfloat(j)*sdi/dfloat(datos_esp%np)

!             ra=dsqrt(x**2+y**2)

!             if (ra > sdi) cycle

!             raiz=sqrt(1.0_dp-(k+1.0_dp)*c*c*ra*ra)
!             z=(c*ra*ra/(1.0_dp+raiz)); zx=c*x/raiz; zy=c*y/raiz

!             raiz_max = sqrt(1.0_dp-(k+1.0_dp)*c*c*sdi*sdi)
!             z_max = (c*sdi*sdi)/(1.0_dp+raiz_max)

!             deno=(datos_esp%gamma-z)*(1.0_dp-zx*zx-zy*zy)+2.0_dp*(zx*(x-datos_esp%alfa)+zy*(y-datos_esp%beta))
!             numx0=(x-datos_esp%alfa)*(1.0_dp-zx*zx+zy*zy)-2.0_dp*zx*(zy*(y-datos_esp%beta)+(datos_esp%gamma-z))
!             numy0=(y-datos_esp%beta)*(1.0_dp+zx*zx-zy*zy)-2.0_dp*zy*(zx*(x-datos_esp%alfa)+(datos_esp%gamma-z))

!             tx=x+(datos_esp%z0-z)*numx0/deno; ty=y+(datos_esp%z0-z)*numy0/deno
!             txron=x+(z_max-z)*numx0/deno; tyron=y+(z_max-z)*numy0/deno
!             argx=(2.0_dp*pi*tx/delta); argy=(2.0_dp*pi*tx/delta)

!             write(40,*) txron,(dcos(argx)+1)/2   
!         enddo
!         close(40)
!     end subroutine fila_diam_ronchigrama_simulado_bin
! end module simulacion

! module tratam_dat_sintexp
!     use utiles
!     implicit none
! contains
!     subroutine datos_imagen()
!         implicit none
!         open(unit=20, file=trim(carpeta)//'/datos_imagen.txt', status='old')
!             read(20,*) ancho_pixo, alto_pixo
!             read(20,'(A)') txtimage
!         close(20)

!         arch_imagej = trim(carpeta)//'/'//trim(txtimage)//'.txt'
!         datos = trim(carpeta)//'/datos_'//trim(txtimage)//'.txt'
!     end subroutine datos_imagen

!     subroutine datos_matriz_foto()
!         implicit none
!         integer :: i, j, pix_esp
!         real(4), allocatable :: foto(:,:)
!         integer, allocatable :: listcantpixesp(:)

!         allocate(foto(alto_pixo, ancho_pixo))
!         open(10, file=arch_imagej, status='old')
!         do i = 1, alto_pixo
!             read(10, *) (foto(i,j), j = 1, ancho_pixo)
!         end do
!         close(10)

!         allocate(listcantpixesp(alto_pixo))
!         alto_pixef = 0
!         do i = 1, alto_pixo
!             do j = 1, ancho_pixo
!                 if (foto(i,j) /= 0) then
!                     alto_pixef = alto_pixef + 1; exit
!                 end if
!             end do
!             listcompcoord1(i,1) = j-1; listcompcoord1(i,2) = i 
!         end do

!         do i = 1, alto_pixo
!             if (listcompcoord1(i,1) /= ancho_pixo) then
!                 min_pixny = i; exit
!             end if
!         end do

!         do i = 2, alto_pixo
!             if (listcompcoord1(i,1) < listcompcoord1(i-1,1)) then
!                 min_pixnx = listcompcoord1(i,1)
!             elseif (listcompcoord1(i,1) > listcompcoord1(i-1,1)) then
!                 exit
!             end if
!         end do

!         do i = 1, alto_pixo
!             pix_esp = 0
!             do j = 1, ancho_pixo
!                 if (foto(i,j) > 0) pix_esp = pix_esp + 1
!             end do
!             listcantpixesp(i) = pix_esp
!         end do

!         ancho_pixef = 0
!         do i = 2, alto_pixo
!             if (listcantpixesp(i) > listcantpixesp(i-1)) then
!                 ancho_pixef = listcantpixesp(i)
!             elseif (listcantpixesp(i) < listcantpixesp(i-1)) then
!                 exit
!             end if
!         end do

!         do i = 1, alto_pixo
!             listcompcoord2(i,1) = listcompcoord1(i,1) + listcantpixesp(i)
!             listcompcoord2(i,2) = i
!         end do
!         deallocate(foto, listcantpixesp)
!     end subroutine datos_matriz_foto

!     subroutine coord_borde_esp()
!         implicit none
!         integer :: i, j
!         j = 0
!         do i = 1, alto_pixo
!             if (ancho_pixo /= listcompcoord1(i,1)) then
!                 j = j + 1
!                 listcoordbordesp(j,1) = listcompcoord1(i,1)
!                 listcoordbordesp(j,2) = listcompcoord1(i,2)
!             end if
!         end do
!         j = alto_pixef
!         do i = 1, alto_pixo
!             if (ancho_pixo /= listcompcoord2(i,1)) then
!                 j = j + 1
!                 listcoordbordesp(j,1) = listcompcoord2(i,1)
!                 listcoordbordesp(j,2) = listcompcoord2(i,2)
!             end if
!         end do
!     end subroutine coord_borde_esp

!     subroutine tres_puntos(x1,y1,x2,y2,x3,y3)
!         implicit none
!         integer, intent(out) :: x1,y1,x2,y2,x3,y3
!         integer :: rees, indice_azar
!         real :: r
!         call random_number(r)
!         indice_azar = 1 + int(alto_pixef * r)
!         x1 = listcoordbordesp(indice_azar,1)
!         y1 = listcoordbordesp(indice_azar,2)

!         if ((indice_azar + alto_pixef/3) <= alto_pixef) then
!             x2 = listcoordbordesp(indice_azar + alto_pixef/3, 1)
!             y2 = listcoordbordesp(indice_azar + alto_pixef/3, 2)
!         else
!             rees = (indice_azar + alto_pixef/3) - alto_pixef
!             x2 = listcoordbordesp(rees, 1)
!             y2 = listcoordbordesp(rees, 2)
!         end if

!         if (indice_azar + 2*alto_pixef/3 <= alto_pixef) then
!             x3 = listcoordbordesp(indice_azar + 2*alto_pixef/3, 1)
!             y3 = listcoordbordesp(indice_azar + 2*alto_pixef/3, 2)
!         else
!             rees = (indice_azar + 2*alto_pixef/3) - alto_pixef
!             x3 = listcoordbordesp(rees, 1)
!             y3 = listcoordbordesp(rees, 2)
!         end if
!     end subroutine tres_puntos

!     subroutine calcular_centros(x1,y1,x2,y2,x3,y3,oxe,oye,sdipixe)
!         implicit none
!         integer, intent(in) :: x1,y1,x2,y2,x3,y3
!         integer, intent(out) :: oxe,oye,sdipixe
!         real(8) :: m1,m2,mp1,mp2,pmed1x,pmed1y,pmed2x,pmed2y
!         real(8) :: a1,a2,b1,b2,c1,c2,det,detx,dety,ox,oy,sdipix
!         integer :: tx1,tx2,tx3,ty1,ty2,ty3

!         ty1=-y1; ty2=-y2; ty3=-y3; tx1=x1; tx2=x2; tx3=x3
!         m1 = real(ty2-ty1)/real(tx2-tx1); m2 = real(ty3-ty2)/real(tx3-tx2)
!         mp1 = -1.0d0/m1; mp2 = -1.0d0/m2
!         pmed1x = 0.5d0*(tx1+tx2); pmed1y = 0.5d0*(ty1+ty2)
!         pmed2x = 0.5d0*(tx3+tx2); pmed2y = 0.5d0*(ty3+ty2)
!         a1=-mp1; a2=-mp2; b1=1.0d0; b2=1.0d0
!         c1=-mp1*pmed1x + pmed1y; c2=-mp2*pmed2x + pmed2y
!         det = a1*b2 - a2*b1; detx = c1*b2 - c2*b1; dety = a1*c2 - c1*a2
!         ox = detx/det; oy = dety/det; sdipix = sqrt((tx2-ox)**2+(ty2-oy)**2)
!         oxe = int(ox); oye = -int(oy); sdipixe = int(sdipix)
!     end subroutine calcular_centros
    
!     !Hay que hacer la matriz espejo rectangular y la matriz espejo pirámide.
    
!     subroutine matriz_espejo_efec_rect()
!         implicit none
!         integer :: i, j, max_pixnx, max_pixny
!         real(4), allocatable :: foto(:,:)

!         allocate(foto(alto_pixo, ancho_pixo))

!         open(10, file=arch_imagej, status='old')
!             do i = 1, alto_pixo
!                 read(10, *) (foto(i,j), j = 1, ancho_pixo)
!             end do
!         close(10)

!         max_pixny = min_pixny + alto_pixef; max_pixnx = min_pixnx + ancho_pixef

!         open(11, file=trim(carpeta)//'/ronchi_exp_rect.txt', status='replace')
!             do i = min_pixny, max_pixny
!                 write(11,*) (foto(i,j), j = min_pixnx, max_pixnx)
!             end do
!         close(11)

!         deallocate(foto)
!     end subroutine matriz_espejo_efec_rect
    
! end module tratam_dat_sintexp

! module filtros_ajustes_dat
!     use utiles
!     implicit none
! contains

!     subroutine extraer_fila_efec()
!         implicit none
!         integer :: i, j, fila_eve
!         real(4) :: espejo(alto_pixef+1, ancho_pixef+1)
!         real(4), allocatable :: coordxirrad(:,:)

!         open(11, file=trim(carpeta)//'/ronchi_exp_rect.txt', status='old')
!             do i = 1, alto_pixef + 1
!                 read(11, *) (espejo(i,j), j = 1, ancho_pixef + 1)
!             end do
!         close(11)

!         fila_eve = fila_evf - min_pixny

!         allocate(coordxirrad(ancho_pixef+1, 2))
!          do i = 1, ancho_pixef + 1
!             coordxirrad(i,1) = real(i + min_pixnx)
!             coordxirrad(i,2) = espejo(fila_eve, i)
!          end do

!          num_dat = 0
!          open(25, file=trim(carpeta)//'/fila_exp.txt', status='replace')
!             do i = 1, ancho_pixef + 1
!                 if (coordxirrad(i,2) > 0.0) then
!                     num_dat = num_dat + 1
!                     write(25,'(f14.7,f14.7)') coordxirrad(i,1), coordxirrad(i,2)
!                 end if
!             end do
!          close(25)
!         deallocate(coordxirrad)

!     end subroutine extraer_fila_efec

!     subroutine gaussian_filtered()
!         implicit none
!         integer :: i, j, half_window
!         real(8), allocatable :: x(:), y(:), y_smoothed(:), weights(:)
!         real(8) :: weight_sum

!         half_window = int(3.0d0 * sigma)

!         allocate(x(num_dat), y(num_dat), y_smoothed(num_dat), weights(-half_window:half_window))
        
!         open(15, file=trim(carpeta)//'/fila_exp.txt', status='old')
!          do i = 1, num_dat
!             read(15,*) x(i), y(i)
!          end do
!         close(15)

!         weight_sum = 0.0d0

!         do j = -half_window, half_window
!             weights(j) = exp(-real(j)**2/(2.0d0*sigma**2))
!             weight_sum = weight_sum + weights(j)
!         end do

!         weights = weights / weight_sum

!         do i = 1, num_dat
!             y_smoothed(i) = 0.0d0
!             do j = -half_window, half_window
!                 if (i + j >= 1 .and. i + j <= num_dat) then
!                     y_smoothed(i) = y_smoothed(i) + weights(j) * y(i + j)
!                 end if
!             end do
!         end do

!         open(10, file=trim(carpeta)//'/gaussian_filtered.txt', status='replace')
!          do i = 1, num_dat
!             write(10,'(f12.6,1x,f12.6,1x,f18.6)') x(i), y(i), y_smoothed(i)
!          end do
!         close(10)

!         deallocate(x, y, y_smoothed, weights)
!     end subroutine gaussian_filtered
! end module filtros_ajustes_dat

! module graficos
!     use utiles
!     implicit none
! contains
!     !Graficadores

!     subroutine grafica_compar(rc,k)
!         implicit none
!         real(dp), intent(in) :: rc, k
!         character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str

!         write(Valor_rc_str, '(F14.8)') rc; write(Valor_k_str, '(F12.8)') k    
!         write(Valor_z_str, '(F12.8)') datos_esp%z0

!         open(unit=30, file='salida/grafica_comp.txt', status='replace')
!          write(30,*) 'set xlabel "eje pixeles (cm)"'
!          write(30,*) 'set ylabel "eje irradiancia (escala de grises)"'
!          write(30,*) 'set size ratio 1'
!          write(30,*) 'set title "Comparación z_0='//trim(Valor_z_str)//': sim1 desc, sim2 k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
!          write(30,*) 'set grid'
!          write(30,*) 'set grid'
!          write(30,*) 'plot "salida/sim1.txt" using 1:2 with lines title "sim1","salida/sim2.txt" using 1:2 with lines'
!         close(30)
!       call system('gnuplot -p salida/grafica_comp.txt')
!     endsubroutine grafica_compar

!     subroutine grafica_fila(rc,k)
!         implicit none
!         real(dp), intent(in) :: rc, k
!         character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str

!         write(Valor_rc_str, '(F14.8)') rc; write(Valor_k_str, '(F12.8)') k    
!         write(Valor_z_str, '(F12.8)') datos_esp%z0

!         open(unit=30, file='Datos_Grafica_Ronchi_Fila.txt', status='replace')
!          write(30,*) 'set xlabel "eje pixeles (cm)"'
!          write(30,*) 'set ylabel "eje irradiancia (escala de grises)"'
!          write(30,*) 'set title "z_0='//trim(Valor_z_str)//', k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
!          write(30,*) 'set grid'
!          write(30,*) 'plot "ronchigrama_fila.txt"'
!         close(30)
!         call system('gnuplot -p Datos_Grafica_Ronchi_Fila.txt')
!     endsubroutine grafica_fila

!     subroutine grafica_completo_bin(Valor_rc_str,Valor_k_str)
!         implicit none
!         character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
!         character(len=72) :: Valor_z_str
!         character(len=200) :: texto_titulo
        
!         write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
!         texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
!         ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

!         open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
!          write(30,*) 'set term qt'
!          write(30,*) 'set terminal qt font "Monaco,12"'
!          write(30,*) 'set xlabel "eje x (cm)"'
!          write(30,*) 'set ylabel "eje y (cm)"'
!          write(30,*) 'set size ratio 1'
!          write(30,*) trim(texto_titulo)
!          write(30,*) 'set grid'
!          write(30,*) 'plot "salida/ronchigrama_comp.txt" ls 0 lc 16 notitle'
!         close(30)
!         call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
!     endsubroutine grafica_completo_bin

!     subroutine grafica_gaussian()
!         implicit none
!         character(len=260) :: tit, comando, arch

!         arch = trim(carpeta)//'/gaussian_filtered.txt'
!         print*,"ARCHIVO", arch
!         write(tit,'(a,f12.6)') 'filtro gaussiano sigma=', sigma
        
!         open(30, file=trim(carpeta)//'/graf_g.txt', status='replace')
!          write(30,*) 'set title "'//trim(tit)//'"'
!          write(30,*) 'set grid'
!          write(30,*) 'plot "'//trim(arch)//'" u 1:2 w l t "orig", "'//trim(arch)//'" u 1:3 w l t "filt"'
!         close(30)

!         comando = 'gnuplot -p ' // trim(carpeta) // '/graf_g.txt'
!         call system(comando)
!     end subroutine grafica_gaussian

!     subroutine png_bin(Valor_rc_str,Valor_k_str)
!         implicit none
!         character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
!         character(len=72) :: Valor_z_str
!         character(len=200) :: texto_titulo,url_plantilla,archivo_datosgnuplot,comando_sistema
        
!         write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
!         texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
!         ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

!         url_plantilla = '/Users/berenicecortes/Desktop/carpeta_de_carpetas_plantillas/espejoresumen8rc9581k-098'

!         archivo_datosgnuplot= trim(url_plantilla)//'/Datos_PNG_RonchiComp.txt'

!         open(unit=30, file=trim(archivo_datosgnuplot), status='replace')
!             write(30,*) 'set terminal pngcairo size 800,800 background "white"'
!             write(30,*) 'set output "'//trim(url_plantilla)//'/imagen_ronchi_simu.png"'
!             write(30,*) 'set size ratio 1'
!             write(30,*) trim(texto_titulo)
!             ! Ocultar todos los elementos decorativos
!             write(30,*) 'unset key'         ! Oculta la leyenda
!             write(30,*) 'unset tics'        ! Oculta las marcas de graduación/números de los ejes
!             write(30,*) 'unset border'      ! Oculta el recuadro exterior
!             write(30,*) 'unset colorbox'    ! Oculta la barra de escala de color (paleta de grises)
!             write(30,*) 'set palette gray'
!             ! write(30,*) 'set lmargin 0'
!             ! write(30,*) 'set rmargin 0'
!             ! write(30,*) 'set tmargin 0'
!             ! write(30,*) 'set bmargin 0'
!             write(30,*) 'plot "salida/ronchigrama_comp.txt" ls 0 lc 16 notitle'
!             write(30,*) 'set output'
!         close(30)

!         comando_sistema = 'gnuplot -p "' // trim(archivo_datosgnuplot) // '"'
!         call system(trim(comando_sistema))
!     endsubroutine png_bin

!     subroutine grafica_completo_cos(Valor_rc_str,Valor_k_str)
!         implicit none
!         character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
!         character(len=72) :: Valor_z_str
!         character(len=200) :: texto_titulo
        
!         write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
!         texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
!         ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

!         open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
!          write(30,*) 'set term qt'
!          write(30,*) 'set terminal qt font "Monaco,12"'
!          write(30,*) 'set xlabel "eje x (cm)"'
!          write(30,*) 'set ylabel "eje y (cm)"'
!          write(30,*) 'set size ratio 1'
!          write(30,*) trim(texto_titulo)
!          write(30,*) 'set grid'
!          write(30,*) 'set palette gray'
!          write(30,*) 'set cblabel "Intensidad (z)"'
!          write(30,*) 'set colorbox'
!          write(30,*) 'unset key'
!          write(30,*) 'plot "salida/ronchigrama_comp.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'
!         close(30)
!         call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
!     endsubroutine grafica_completo_cos

!     subroutine png_cos(Valor_rc_str,Valor_k_str)
!         implicit none
!         character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
!         character(len=72) :: Valor_z_str, name_image
!         character(len=200) :: texto_titulo,archivo_datosgnuplot,comando_sistema
        
!         write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
!         texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
!         ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

!         ! url_plantilla = '/Users/berenicecortes/Desktop/carpeta_de_carpetas_plantillas/espejoresumen8rc9581k-098'
        
!         open(unit=30, file=trim(ruta%rutaruta)//'/ruta.txt', status='old', action='read')
!          read(30,'(A)') name_image
!         close(30)

!         archivo_datosgnuplot= trim(ruta%rutaruta)//'/Datos_PNG_RonchiComp.txt'

!         open(unit=30, file=trim(archivo_datosgnuplot), status='replace')
!             write(30,*) 'set terminal pngcairo size 800,800 background "white"'
!             write(30,*) 'set output "'//trim(ruta%rutaruta)//'/'//trim(name_image)//'.png"'
!             write(30,*) 'set size ratio 1'
!             write(30,*) trim(texto_titulo)
!             ! Ocultar todos los elementos decorativos
!             write(30,*) 'unset key'         ! Oculta la leyenda
!             write(30,*) 'unset tics'        ! Oculta las marcas de graduación/números de los ejes
!             write(30,*) 'unset border'      ! Oculta el recuadro exterior
!             write(30,*) 'unset colorbox'    ! Oculta la barra de escala de color (paleta de grises)
!             write(30,*) 'set palette gray'
!             ! write(30,*) 'set lmargin 0'
!             ! write(30,*) 'set rmargin 0'
!             ! write(30,*) 'set tmargin 0'
!             ! write(30,*) 'set bmargin 0'
!             write(30,*) 'plot "salida/ronchigrama_comp.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'
!             write(30,*) 'set output'
!         close(30)

!         comando_sistema = 'gnuplot -p "' // trim(archivo_datosgnuplot) // '"'
!         call system(trim(comando_sistema))
!     endsubroutine png_cos    
! end module graficos

! ! FIN ARMADO =======================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================
! ! ==================================================================================================================================
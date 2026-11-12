program name
    use modulo_borrador
    implicit none
    character(len=72) :: nombre_txt

    nombre_txt = 'Salida/'

    call simulacion_grafica(nombre_txt)
    print*, datos_limites_x
    print*, datos_limites_y
end program name

module borr_graficos
    use utiles
    implicit none

contains

    ! =========================================================================
    ! SUBRUTINA 1: GENERACIÓN Y EXPORTACIÓN DE PNG EN CARPETA
    ! =========================================================================
    subroutine borr_generar_png(tipo_png, nombre_archivo, ruta_carpeta)
        implicit none
        integer, intent(in)                     :: tipo_png         ! 1: binario, 2: continuo cos
        character(len=72), intent(in)           :: nombre_archivo   ! Nombre sin extensión (ej. 'ronchi_out')
        character(len=*), intent(in), optional  :: ruta_carpeta     ! Carpeta destino (ej. 'salida' o 'plantillas')
        character(len=72)                       :: Valor_rc_str, Valor_k_str, Valor_z_str
        character(len=300)                      :: carpeta_dest, archivo_salida_png, archivo_script, linea_plot, titulo
        character(len=80)                       :: xlabel, ylabel
        character(len=76)                       :: nombre_con_extension
        logical                                 :: usar_ratio

        usar_ratio = .true.

        ! Carpeta y nombre por defecto
        if (present(ruta_carpeta)) then
            carpeta_dest = trim(ruta_carpeta)
        else
            carpeta_dest = 'salida'
        end if

        archivo_salida_png = trim(carpeta_dest) // '/' // trim(nombre_archivo) // '.png'
        archivo_script = trim(carpeta_dest) // '/script_export_png.gp'
        nombre_con_extension = trim(nombre_archivo) // '.txt'

        ! Formateo numérico
        write(Valor_rc_str, '(F14.8)') datos_esp%rc
        write(Valor_k_str,  '(F12.8)') datos_esp%k    
        write(Valor_z_str,  '(F12.8)') datos_esp%z0

        titulo = '"z_0='//trim(adjustl(Valor_z_str))//', k='//trim(adjustl(Valor_k_str))// &
                 ' y rc='//trim(adjustl(Valor_rc_str))//'"'

        select case (tipo_png)
        case (1) ! ==== PNG COMPLETO BINARIO ====
            linea_plot = 'plot "'//trim(adjustl(nombre_con_extension))//'" ls 0 lc 16 notitle'

        case (2) ! ==== PNG COMPLETO COS ====
            linea_plot = 'plot "'//trim(adjustl(nombre_con_extension))//'" using 1:2:3 with points palette pt 7 ps 1 notitle'

        case (3) ! ==== FILA PNG ====
            xlabel     = '"eje pixeles (cm)"'
            ylabel     = '"eje irradiancia (escala de grises)"'
            linea_plot = 'plot "'//trim(adjustl(nombre_con_extension))//'" with lines notitle'
            usar_ratio = .false.

        case (4) ! ==== COMPARACIÓN ====
            xlabel     = '"eje pixeles (cm)"'
            ylabel     = '"eje irradiancia (escala de grises)"'
            linea_plot = 'plot "sim1.txt" using 1:2 with lines title "sim1", "sim2.txt" using 1:2 with lines title "sim2"'
            usar_ratio = .false.

        case default
            print *, "Error: Tipo de PNG no válido."
            return
        end select

        ! Escritura del script de renderizado PNG
        open(unit=30, file=trim(archivo_script), status='replace')
            write(30,*) 'set terminal pngcairo size 800,800 background "white"'
            write(30,*) 'set output "' // trim(archivo_salida_png) // '"'
            write(30,*) 'set size ratio 1'
            write(30,*) 'unset key'
            write(30,*) 'unset tics'
            write(30,*) 'unset border'
            write(30,*) 'unset colorbox'
            write(30,*) 'set palette gray'
            write(30,*) 'set title ', trim(titulo)
            write(30,*) trim(linea_plot)
            ! Cierre del archivo de imagen para asegurar escritura en disco
            write(30,*) 'set output'
        close(30)

        ! No requiere '-p' ya que no abre ventana interactiva
        call system('gnuplot "' // trim(archivo_script) // '"')
    end subroutine borr_generar_png

    ! =========================================================================
    ! SUBRUTINA 2: VISUALIZACIÓN INTERACTIVA EN PANTALLA
    ! =========================================================================
    subroutine borr_graficas_interactivas(tipo_grafica,nombre, sigma)
        implicit none
        integer, intent(in)            :: tipo_grafica  ! 1: comparacion, 2: fila, 3: comp_bin, 4: comp_cos, 5: gaussiano
        character(len=72), intent(in)  :: nombre
        real(dp), intent(in), optional :: sigma
        character(len=72)              :: Valor_rc_str, Valor_k_str, Valor_z_str, sigma_str
        character(len=260)             :: archivo_script, linea_plot, titulo
        character(len=80)              :: xlabel, ylabel
        logical                        :: usar_ratio

        usar_ratio = .true.
        archivo_script = 'script_interactivo.gp'

        ! Formateo numérico
        write(Valor_rc_str, '(F14.8)') datos_esp%rc
        write(Valor_k_str,  '(F12.8)') datos_esp%k    
        write(Valor_z_str,  '(F12.8)') datos_esp%z0

        select case (tipo_grafica)
        case (1) ! ==== COMPLETO BIN ==== 
            xlabel     = '"eje x (cm)"'
            ylabel     = '"eje y (cm)"'
            titulo     = '"z_0='//trim(adjustl(Valor_z_str))//', k='//trim(adjustl(Valor_k_str))// &
                         ' y rc='//trim(adjustl(Valor_rc_str))//'"'
            linea_plot = 'plot "'//trim(nombre)//'.txt" ls 0 lc 16 notitle'

        case (2) ! ==== COMPLETO COS ====
            xlabel     = '"eje x (cm)"'
            ylabel     = '"eje y (cm)"'
            titulo     = '"z_0='//trim(adjustl(Valor_z_str))//', k='//trim(adjustl(Valor_k_str))// &
                         ' y rc='//trim(adjustl(Valor_rc_str))//'"'
            linea_plot = 'plot "'//trim(nombre)//'.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'

        case (3) ! ==== FILA ====
            xlabel     = '"eje pixeles (cm)"'
            ylabel     = '"eje irradiancia (escala de grises)"'
            titulo     = '"z_0='//trim(adjustl(Valor_z_str))//', k='//trim(adjustl(Valor_k_str))// &
                         ' y rc='//trim(adjustl(Valor_rc_str))//'"'
            linea_plot = 'plot "'//trim(nombre)//'.txt" with lines notitle'
            usar_ratio = .false.

        case (4) ! ==== COMPARACIÓN ====
            xlabel     = '"eje pixeles (cm)"'
            ylabel     = '"eje irradiancia (escala de grises)"'
            usar_ratio = .false.
            titulo     = '"Comparación z_0='//trim(adjustl(Valor_z_str))//': sim1 desc, sim2 k='// &
                         trim(adjustl(Valor_k_str))//' y rc='//trim(adjustl(Valor_rc_str))//'"'
            linea_plot = 'plot "sim1.txt" using 1:2 with lines title "sim1", "sim2.txt" using 1:2 with lines title "sim2"'

        case (5) ! ==== FILTRO GAUSSIANO ====
            xlabel     = '"eje x"'
            ylabel     = '"irradiancia"'
            usar_ratio = .false.
            if (present(sigma)) then
                write(sigma_str, '(F12.6)') sigma
            else
                sigma_str = 'N/A'
            end if
            titulo     = '"Filtro gaussiano sigma=' // trim(adjustl(sigma_str)) // '"'
            linea_plot = 'plot "gaussian_filtered.txt" u 1:2 w l t "orig", "gaussian_filtered.txt" u 1:3 w l t "filt"'

        case default
            print *, "Error: Tipo de gráfica interactiva no válido."
            return
        end select

        ! Escritura del script gnuplot
        open(unit=30, file=trim(archivo_script), status='replace')
            write(30,*) 'set term qt font "Sans,10"'
            write(30,*) 'set xlabel ', trim(xlabel)
            write(30,*) 'set ylabel ', trim(ylabel)
            write(30,*) 'set grid'
            if (usar_ratio) write(30,*) 'set size ratio 1'
            write(30,*) 'set title ', trim(titulo)
            write(30,*) trim(linea_plot)
        close(30)

        call system('gnuplot -p ' // trim(archivo_script))
    end subroutine borr_graficas_interactivas
end module borr_graficos

module modulo_borrador
    use calculos_ronchigrama
    use borr_graficos
    implicit none
    
contains

    subroutine simulacion_grafica_V1(nombre_archivo_puntos_ronchigrama)
        implicit none
        character(len=72), intent(in) :: nombre_archivo_puntos_ronchigrama
        ! character(len=7), intent(in) :: tipo_grafica
        character(len=76) :: nombre_con_extension
        integer :: i, j

        call leer_datos
        call ctes_sim
        call datos_if1
        call asegurar_dimension_arrx(2*datos_esp%np+1)
        call asegurar_dimension_arry(datos_ciclodo_if1(tipo_num,1))
        call if1
        call datos_if2

        nombre_con_extension = trim(nombre_archivo_puntos_ronchigrama)//'.txt'

        open(40,file=nombre_con_extension,status='replace')
            do i = 1, size(arr_x)
            do j = 1, size(arr_y)
                if(dist(arr_x(i),arr_y(j)) <= sdi) then 
                    call comun(datos_esp%k,c,dist(arr_x(i),arr_y(j)),arr_x(i),arr_y(j),sdi)
                    call if2(arr_x(i),arr_y(j))
                end if
            enddo
            enddo
        close(40)
    end subroutine simulacion_grafica_V1

    subroutine simulacion_grafica(nombre_archivo_puntos_ronchigrama)
        implicit none
        character(len=72), intent(in) :: nombre_archivo_puntos_ronchigrama
        ! character(len=7), intent(in) :: tipo_grafica
        character(len=76) :: nombre_con_extension
        integer :: i, j

        call leer_datos
        call ctes_sim
        call DATOS_CANTIDAD_CALCULOS
        call asegurar_dimension_arrx(datos_limites_x(5))
        call asegurar_dimension_arry(datos_limites_y(5))
        call CANTIDAD_CALCULOS
        call DATOS_TIPO_PATRON

        nombre_con_extension = trim(nombre_archivo_puntos_ronchigrama)//'.txt'

        open(40,file=nombre_con_extension,status='replace')
            do i = 1, size(arr_x)
            do j = 1, size(arr_y)
                if(dist(arr_x(i),arr_y(j)) <= sdi) then 
                    call comun(datos_esp%k,c,dist(arr_x(i),arr_y(j)),arr_x(i),arr_y(j),sdi)
                    call TIPO_PATRON(arr_x(i),arr_y(j))
                end if
            enddo
            enddo
        close(40)
    end subroutine simulacion_grafica

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

end module modulo_borrador
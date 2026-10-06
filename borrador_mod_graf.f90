program name
    use graficos
    implicit none

    call graficas(4)
end program name

module graficos
    use utiles
    implicit none
    
contains

    subroutine graficas(tipo_grafica,sigma)
        implicit none
        integer, intent(in)  :: tipo_grafica ! 1: comparacion, 2: fila, 3: completo, 4: gaussiano
        character(len=72)  :: Valor_rc_str, Valor_k_str, Valor_z_str, sigma_str
        real(dp), intent(in), optional :: sigma ! Obligatorio solo si tipo_grafica == 4
        character(len=150) :: archivo_script, linea_plot, titulo
        character(len=50)  :: xlabel, ylabel
        logical            :: usar_ratio

        ! Por defecto, la mayoría usa ratio 1, lo desactivamos si es necesario
        usar_ratio = .true.

        !1. Formatear los números a cadenas de texto de forma común
        write(Valor_rc_str, '(F14.8)') datos_esp%rc
        write(Valor_k_str, '(F12.8)') datos_esp%k    
        write(Valor_z_str, '(F12.8)') datos_esp%z0

        ! 2. Configurar las variables específicas según el tipo seleccionado
        archivo_script = 'datos_gnuplot.txt' !trim(setup_global%rutas%base) //
        select case (tipo_grafica)
        case (1) ! ==== COMPARACION ====
            xlabel         = '"eje pixeles (cm)"'
            ylabel         = '"eje irradiancia (escala de grises)"'
            titulo         = '"Comparación z_0='//trim(Valor_z_str)//': sim1 desc, sim2 k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
            linea_plot     = 'plot "sim1.txt" using 1:2 with lines title "sim1","sim2.txt" using 1:2 with lines'

        case (2) ! ==== FILA ====
            xlabel         = '"eje pixeles (cm)"'
            ylabel         = '"eje irradiancia (escala de grises)"'
            titulo         = '"z_0='//trim(Valor_z_str)//', k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
            linea_plot     = 'plot "ronchigrama_fila.txt"'
            usar_ratio     = .false.

        case (3) ! ==== COMPLETO BIN ====
            xlabel         = '"eje x (cm)"'
            ylabel         = '"eje y (cm)"'
            titulo         = '"z_0='//trim(Valor_z_str)//', k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
            linea_plot     = 'plot "ronchigrama_comp_bin.txt" ls 0 lc 16 notitle'

        case(4) ! ==== PNG COMPLETO BIN ====
            titulo         = '"z_0='//trim(Valor_z_str)//', k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
            ! linea_plot     = 'plot "Salida/birbin_comp.txt" ls 0 lc 16 notitle'
            linea_plot     = 'plot "Salida/roncosy_comp.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'


        case (5) ! ==== FILTRO GAUSSIANO ====
            ! Nota: Se asume que usas las rutas unificadas de la estructura para los archivos,
            ! si no, puedes cambiar trim(setup_global%rutas%base) por tu variable de carpeta antigua.
            xlabel         = '"eje x"'
            ylabel         = '"irradiancia"'
            usar_ratio     = .false.
            
            ! Validar si se proporcionó el parámetro sigma de manera segura
            if (present(sigma)) then
                write(sigma_str, '(F12.6)') sigma
            else
                sigma_str = 'N/A'
            end if
            titulo         = '"Filtro gaussiano sigma=' // trim(sigma_str) // '"'
            
            ! linea_plot     = 'plot "' // trim(setup_global%rutas%base) // 'gaussian_filtered.txt" u 1:2 w l t "orig", ' // &
                            !  '"' // trim(setup_global%rutas%base) // 'gaussian_filtered.txt" u 1:3 w l t "filt"'

        case default
            print *, "Error: Tipo de gráfica no válido en generar_grafica."
            return
        end select

        ! 3. Escritura del archivo Gnuplot genérico
        open(unit=30, file=trim(archivo_script), status='replace')

            ! Terminal y salida
            if (tipo_grafica == 3) write(30,*) 'set term qt'
            if (tipo_grafica == 4) then
                write(30,*) 'set terminal pngcairo size 800,800 background "white"'
                write(30,*) 'set output "imagen_ronchi_simu.png"'
                write(30,*) 'unset key'
                write(30,*) 'unset tics'
                write(30,*) 'unset border'
                write(30,*) 'unset colorbox'
                write(30,*) 'set palette gray'
            end if

            ! Configuración común de ejes y apariencia
            if (tipo_grafica /= 4) then
                write(30,*) 'set xlabel ', trim(xlabel)
                write(30,*) 'set ylabel ', trim(ylabel)
                write(30,*) 'set grid'
            end if

            if (usar_ratio) write(30,*) 'set size ratio 1'
            write(30,*) 'set title ', trim(titulo)

            ! Renderizar el gráfico
            write(30,*) trim(linea_plot)

            ! Cerrar y vaciar el buffer del archivo PNG hacia el disco
            if (tipo_grafica == 4) write(30,*) 'set output'

        close(30)

        ! 4. Ejecución del script
        call system('gnuplot -p ' // trim(archivo_script))

    !     open(unit=30, file=trim(archivo_datosgnuplot), status='replace')
    !         write(30,*) 'set terminal pngcairo size 800,800 background "white"'
    !         write(30,*) 'set output "'//trim(url_plantilla)//'/imagen_ronchi_simu.png"'
    !         write(30,*) 'set size ratio 1'
    !         write(30,*) trim(texto_titulo)
    !         ! Ocultar todos los elementos decorativos
    !         write(30,*) 'unset key'         ! Oculta la leyenda
    !         write(30,*) 'unset tics'        ! Oculta las marcas de graduación/números de los ejes
    !         write(30,*) 'unset border'      ! Oculta el recuadro exterior
    !         write(30,*) 'unset colorbox'    ! Oculta la barra de escala de color (paleta de grises)
    !         write(30,*) 'set palette gray'
    !         ! write(30,*) 'set lmargin 0'
    !         ! write(30,*) 'set rmargin 0'
    !         ! write(30,*) 'set tmargin 0'
    !         ! write(30,*) 'set bmargin 0'
    !         write(30,*) 'plot "salida/ronchigrama_comp.txt" ls 0 lc 16 notitle'
    !         write(30,*) 'set output'
    !     close(30)

    end subroutine graficas
    ! subroutine grafica_compar(rc,k)
    !     implicit none
    !     real(dp), intent(in) :: rc, k
    !     character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str

    !     write(Valor_rc_str, '(F14.8)') rc; write(Valor_k_str, '(F12.8)') k    
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0

    !     open(unit=30, file='salida/grafica_comp.txt', status='replace')
    !      write(30,*) 'set xlabel "eje pixeles (cm)"'
    !      write(30,*) 'set ylabel "eje irradiancia (escala de grises)"'
    !      write(30,*) 'set size ratio 1'
    !      write(30,*) 'set title "Comparación z_0='//trim(Valor_z_str)//': sim1 desc, sim2 k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
    !      write(30,*) 'set grid'
    !      write(30,*) 'set grid'
    !      write(30,*) 'plot "salida/sim1.txt" using 1:2 with lines title "sim1","salida/sim2.txt" using 1:2 with lines'
    !     close(30)
    !   call system('gnuplot -p salida/grafica_comp.txt')
    ! endsubroutine grafica_compar

    ! subroutine grafica_fila(rc,k)
    !     implicit none
    !     real(dp), intent(in) :: rc, k
    !     character(len=72) :: Valor_rc_str, Valor_k_str, Valor_z_str

    !     write(Valor_rc_str, '(F14.8)') rc; write(Valor_k_str, '(F12.8)') k    
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0

    !     open(unit=30, file='Datos_Grafica_Ronchi_Fila.txt', status='replace')
    !      write(30,*) 'set xlabel "eje pixeles (cm)"'
    !      write(30,*) 'set ylabel "eje irradiancia (escala de grises)"'
    !      write(30,*) 'set title "z_0='//trim(Valor_z_str)//', k='//trim(Valor_k_str)//' y rc='//trim(Valor_rc_str)//'"'
    !      write(30,*) 'set grid'
    !      write(30,*) 'plot "ronchigrama_fila.txt"'
    !     close(30)
    !     call system('gnuplot -p Datos_Grafica_Ronchi_Fila.txt')
    ! endsubroutine grafica_fila

    ! subroutine grafica_completo_bin(Valor_rc_str,Valor_k_str)
    !     implicit none
    !     character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
    !     character(len=72) :: Valor_z_str
    !     character(len=200) :: texto_titulo
        
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
    !     texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
    !     ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

    !     open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
    !      write(30,*) 'set term qt'
    !      write(30,*) 'set terminal qt font "Monaco,12"'
    !      write(30,*) 'set xlabel "eje x (cm)"'
    !      write(30,*) 'set ylabel "eje y (cm)"'
    !      write(30,*) 'set size ratio 1'
    !      write(30,*) trim(texto_titulo)
    !      write(30,*) 'set grid'
    !      write(30,*) 'plot "salida/ronchigrama_comp.txt" ls 0 lc 16 notitle'
    !     close(30)
    !     call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
    ! endsubroutine grafica_completo_bin

    ! subroutine grafica_gaussian()
    !     implicit none
    !     character(len=260) :: tit, comando, arch

    !     arch = trim(carpeta)//'/gaussian_filtered.txt'
    !     print*,"ARCHIVO", arch
    !     write(tit,'(a,f12.6)') 'filtro gaussiano sigma=', sigma
        
    !     open(30, file=trim(carpeta)//'/graf_g.txt', status='replace')
    !      write(30,*) 'set title "'//trim(tit)//'"'
    !      write(30,*) 'set grid'
    !      write(30,*) 'plot "'//trim(arch)//'" u 1:2 w l t "orig", "'//trim(arch)//'" u 1:3 w l t "filt"'
    !     close(30)

    !     comando = 'gnuplot -p ' // trim(carpeta) // '/graf_g.txt'
    !     call system(comando)
    ! end subroutine grafica_gaussian

    ! subroutine png_bin(Valor_rc_str,Valor_k_str)
    !     implicit none
    !     character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
    !     character(len=72) :: Valor_z_str
    !     character(len=200) :: texto_titulo,url_plantilla,archivo_datosgnuplot,comando_sistema
        
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
    !     texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
    !     ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

    !     url_plantilla = '/Users/berenicecortes/Desktop/carpeta_de_carpetas_plantillas/espejoresumen8rc9581k-098'

    !     archivo_datosgnuplot= trim(url_plantilla)//'/Datos_PNG_RonchiComp.txt'

    !     open(unit=30, file=trim(archivo_datosgnuplot), status='replace')
    !         write(30,*) 'set terminal pngcairo size 800,800 background "white"'
    !         write(30,*) 'set output "'//trim(url_plantilla)//'/imagen_ronchi_simu.png"'
    !         write(30,*) 'set size ratio 1'
    !         write(30,*) trim(texto_titulo)
    !         ! Ocultar todos los elementos decorativos
    !         write(30,*) 'unset key'         ! Oculta la leyenda
    !         write(30,*) 'unset tics'        ! Oculta las marcas de graduación/números de los ejes
    !         write(30,*) 'unset border'      ! Oculta el recuadro exterior
    !         write(30,*) 'unset colorbox'    ! Oculta la barra de escala de color (paleta de grises)
    !         write(30,*) 'set palette gray'
    !         ! write(30,*) 'set lmargin 0'
    !         ! write(30,*) 'set rmargin 0'
    !         ! write(30,*) 'set tmargin 0'
    !         ! write(30,*) 'set bmargin 0'
    !         write(30,*) 'plot "salida/ronchigrama_comp.txt" ls 0 lc 16 notitle'
    !         write(30,*) 'set output'
    !     close(30)

    !     comando_sistema = 'gnuplot -p "' // trim(archivo_datosgnuplot) // '"'
    !     call system(trim(comando_sistema))
    ! endsubroutine png_bin

    ! subroutine grafica_completo_cos(Valor_rc_str,Valor_k_str)
    !     implicit none
    !     character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
    !     character(len=72) :: Valor_z_str
    !     character(len=200) :: texto_titulo
        
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
    !     texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
    !     ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

    !     open(unit=30, file='salida/Datos_Grafica_RonchiComp.txt', status='replace')
    !      write(30,*) 'set term qt'
    !      write(30,*) 'set terminal qt font "Monaco,12"'
    !      write(30,*) 'set xlabel "eje x (cm)"'
    !      write(30,*) 'set ylabel "eje y (cm)"'
    !      write(30,*) 'set size ratio 1'
    !      write(30,*) trim(texto_titulo)
    !      write(30,*) 'set grid'
    !      write(30,*) 'set palette gray'
    !      write(30,*) 'set cblabel "Intensidad (z)"'
    !      write(30,*) 'set colorbox'
    !      write(30,*) 'unset key'
    !      write(30,*) 'plot "salida/ronchigrama_comp.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'
    !     close(30)
    !     call system('gnuplot -p salida/Datos_Grafica_RonchiComp.txt')
    ! endsubroutine grafica_completo_cos

    ! subroutine png_cos(Valor_rc_str,Valor_k_str)
    !     implicit none
    !     character(len=72), intent(in) :: Valor_rc_str, Valor_k_str
    !     character(len=72) :: Valor_z_str, name_image
    !     character(len=200) :: texto_titulo,archivo_datosgnuplot,comando_sistema
        
    !     write(Valor_z_str, '(F12.8)') datos_esp%z0  
        
    !     texto_titulo = 'set title "z_0=' // trim(adjustl(Valor_z_str)) //', k=' // trim(adjustl(Valor_k_str)) // &
    !     ' y rc=' // trim(adjustl(Valor_rc_str)) // '"'

    !     ! url_plantilla = '/Users/berenicecortes/Desktop/carpeta_de_carpetas_plantillas/espejoresumen8rc9581k-098'
        
    !     open(unit=30, file=trim(ruta%rutaruta)//'/ruta.txt', status='old', action='read')
    !      read(30,'(A)') name_image
    !     close(30)

    !     archivo_datosgnuplot= trim(ruta%rutaruta)//'/Datos_PNG_RonchiComp.txt'

    !     open(unit=30, file=trim(archivo_datosgnuplot), status='replace')
    !         write(30,*) 'set terminal pngcairo size 800,800 background "white"'
    !         write(30,*) 'set output "'//trim(ruta%rutaruta)//'/'//trim(name_image)//'.png"'
    !         write(30,*) 'set size ratio 1'
    !         write(30,*) trim(texto_titulo)
    !         ! Ocultar todos los elementos decorativos
    !         write(30,*) 'unset key'         ! Oculta la leyenda
    !         write(30,*) 'unset tics'        ! Oculta las marcas de graduación/números de los ejes
    !         write(30,*) 'unset border'      ! Oculta el recuadro exterior
    !         write(30,*) 'unset colorbox'    ! Oculta la barra de escala de color (paleta de grises)
    !         write(30,*) 'set palette gray'
    !         ! write(30,*) 'set lmargin 0'
    !         ! write(30,*) 'set rmargin 0'
    !         ! write(30,*) 'set tmargin 0'
    !         ! write(30,*) 'set bmargin 0'
    !         write(30,*) 'plot "salida/ronchigrama_comp.txt" using 1:2:3 with points palette pt 7 ps 1 notitle'
    !         write(30,*) 'set output'
    !     close(30)

    !     comando_sistema = 'gnuplot -p "' // trim(archivo_datosgnuplot) // '"'
    !     call system(trim(comando_sistema))
    ! endsubroutine png_cos 
end module graficos
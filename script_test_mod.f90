program pruebas_modulos
    use utiles
    implicit none
    
    call reloj_inicio()
    call crear_directorios_ficheros_IO()
    call editarnano()
    call leer_primigenios()
    call reloj_fin()
end program pruebas_modulos
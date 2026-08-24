program pruebas_modulos
    use utiles
    implicit none
    
    call reloj_inicio()

    call crear_directorios_IO()


    print*, ruta%carpeta

    call reloj_fin()
end program pruebas_modulos
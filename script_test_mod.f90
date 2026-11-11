program pruebas_modulos
    use calculos_ronchigrama
    implicit none

    call DATOS_CANTIDAD_CALCULOS
    
    print*, datos_limites_x
    print*, datos_limites_y

    call DATOS_TIPO_PATRON

    print*, tipo_rejilla
    print*, tipo_brillo
    print*, tipo_analisis
end program pruebas_modulos
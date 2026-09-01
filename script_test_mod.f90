program pruebas_modulos
    use utiles; use inicios; use calculos_sagita
    implicit none

    call inicio()

    print*, datos_esp%np
    
    datos_esp%rc=100.0_DP; datos_esp%k=1.0_DP

    print*, datos_esp%rc
    print*, datos_esp%k

    tipo_rejilla = 'cos'

    print*, tipo_rejilla
    call puntos_txt(tipo_rejilla)

    call reloj_fin()
end program pruebas_modulos
program pruebas_modulos
    use utiles; use datos_compartidos; use calculos_sagita; use simulador
    implicit none
    integer :: i
    ! real(dp) :: txron,tyron,argx,argy

    call leer_datos
    datos_esp%rc=100.0_dp; datos_esp%k =-1.80_dp
    call ctes_sim
    call datos_para_ciclodo_if1
    call asegurar_dimension_arrx
    call asegurar_dimension_arry_y(datos_ciclo_do(tipo_num,1))
    call ciclo_do()
    call asegurar_dimension_arry_xy(tam_pupila)
    call arreglo_pupila
    call datos_para_ciclodo_if2

    do i = 1, size(arr_xy(1,:))
        ! write(11, '(2F20.16)') arr_xy(1,i),arr_xy(2,i)
    end do

    ! write(11,*) '============================'

    ! do i = 1, size(arr_x)
    !     write(11, '(2F20.16)') arr_xy(2,i)
    ! end do

    open(40,file='datos_grafica.txt',status='replace')
    do i = 1, size(arr_xy(1,:))
        call comun(datos_esp%k,c,dist(arr_xy(1,i),arr_xy(2,i)),arr_xy(1,i),arr_xy(2,i),sdi)
        ! write(9,*) 'tx',tx,'ty',ty,'x,y',x, y,'z0', datos_esp%z0,'z',z,'numx0',numx0,'deno',deno
        ! write(9,*) 'numx0',numx0,'deno',deno
        ! print*, 'valores call comun',datos_esp%k,c,dist(arr_x(15),arr_y(20)),arr_x(1),arr_y(2),sdi,raiz,z,zx,zy,raiz_max,z_max,deno
        call calculos_comunes(arr_xy(1,i),arr_xy(2,i))
    enddo
    close(40)

    ! if ( "bironchigrama" == 'bironchigrama' ) then
    !     tx = x + (datos_esp%z0-z)*(numx0/deno)
    !     ty = y + (datos_esp%z0-z)*(numy0/deno)
    !     tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
    !     ty = aberracion_t(y,datos_esp%z0,z,numy0,deno)
    !     argx = (2*pi*tx)/deno
    !     argy = (2*pi*ty)/deno
    !     if ( "bin" == 'bin'  ) then

    !         if(cos(argx)<=0.and.cos(argy)<=0) then
    !             txron = x + (z_max-z)*(numx0/deno)
    !             tyron = y + (z_max-z)*(numy0/deno)
    !             write(*,*) txron, tyron
    !         endif

    !     else if ( "cos" == 'cos'  ) then

    !         txron = x + (z_max-z)*(numx0/deno)
    !         tyron = y + (z_max-z)*(numy0/deno)
    !         txron = aberracion_t(x,z_max,z,numx0,deno)
    !         tyron = aberracion_t(y,z_max,z,numy0,deno)

    !         write(*,*) txron, tyron, (cos(argx)+1)/2
    !     end if  

    ! else if ("ronchigrama vertical x" == 'ronchigrama') then
    !     tx = x + (datos_esp%z0-z)*(numx0/deno)
    !     tx = aberracion_t(x,datos_esp%z0,z,numx0,deno)
    !     argx = (2*pi*tx)/deno
    !     if ( "bin" == 'bin'  ) then
    !         if(cos(argx)<=0) then
    !             txron = x + (z_max-z)*(numx0/deno)
    !             tyron = y + (z_max-z)*(numy0/deno)
    !             write(*,*) txron, tyron
    !         endif
    !     else if ( "cos" == 'cos'  ) then
    !         txron = x + (z_max-z)*(numx0/deno)
    !         tyron = y + (z_max-z)*(numy0/deno)
    !         write(*,*) txron, tyron, (cos(argx)+1)/2
    !     end if  

    ! else if ("ronchigrama horizontal y" == 'ronchigrama') then
    !     ty = y + (datos_esp%z0-z)*(numy0/deno)
    !     argy = (2*pi*ty)/deno
    !     if ( "bin" == 'bin'  ) then

    !         if(cos(argy)<=0) then
    !             txron = x + (z_max-z)*(numx0/deno)
    !             tyron = y + (z_max-z)*(numy0/deno)
    !             write(*,*) txron, tyron
    !         endif

    !     else if ( "cos" == 'cos'  ) then

    !         txron = x + (z_max-z)*(numx0/deno)
    !         tyron = y + (z_max-z)*(numy0/deno)

    !         write(*,*) txron, tyron, (cos(argy)+1)/2
    !     end if  

    ! end if

    !tipo de dibujo

end program pruebas_modulos
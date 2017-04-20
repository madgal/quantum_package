
use bitmasks
 
BEGIN_PROVIDER [ integer, N_det_generators_restart ]
 implicit none
 BEGIN_DOC
 ! Read the wave function 
 END_DOC
 integer :: i
 integer, save :: ifirst = 0
 double precision :: norm
 print*, ' Providing N_det_generators_restart'
 if(ifirst == 0)then
  call ezfio_get_determinants_n_det(N_det_generators_restart)
  ifirst = 1
 else
  print*,'PB in generators_restart restart !!!'
 endif
 call write_int(output_determinants,N_det_generators_restart,'Number of generators_restart')
END_PROVIDER


 BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators_restart, (N_int,2,N_det_generators_restart) ]
&BEGIN_PROVIDER [ integer(bit_kind), ref_generators_restart, (N_int,2,N_states) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators_restart, (N_det_generators_restart,N_states) ]
 implicit none
 BEGIN_DOC
 ! read wf
 ! 
 END_DOC
 integer                        :: i, k,j
 integer, save :: ifirst = 0
 double precision, allocatable  :: psi_coef_read(:,:)
 print*, ' Providing psi_det_generators_restart'
 if(ifirst == 0)then
  call read_dets(psi_det_generators_restart,N_int,N_det_generators_restart)
   allocate (psi_coef_read(N_det_generators_restart,N_states))
   call ezfio_get_determinants_psi_coef(psi_coef_read)
   do k = 1, N_states
    do i = 1, N_det_generators_restart
     psi_coef_generators_restart(i,k) = psi_coef_read(i,k)
    enddo
   enddo
   do k = 1, N_states
    do i = 1, N_det_generators_restart
     if(dabs(psi_coef_generators_restart(i,k)).gt.0.5d0)then
      do j = 1, N_int
       ref_generators_restart(j,1,k) = psi_det_generators_restart(j,1,i)
       ref_generators_restart(j,2,k) = psi_det_generators_restart(j,2,i)
      enddo
      exit
     endif
    enddo
    call debug_det(ref_generators_restart(1,1,k),N_int)
   enddo
  ifirst = 1
  deallocate(psi_coef_read)
 else 
  print*,'PB in generators_restart restart !!!'
 endif

END_PROVIDER

BEGIN_PROVIDER [ integer, size_select_max]
 implicit none
 BEGIN_DOC
 ! Size of the select_max array
 END_DOC
 size_select_max = 10000
END_PROVIDER

BEGIN_PROVIDER [ double precision, select_max, (size_select_max) ]
 implicit none
 BEGIN_DOC
 ! Memo to skip useless selectors
 END_DOC
 select_max = huge(1.d0)
END_PROVIDER

 BEGIN_PROVIDER [ integer, N_det_generators ]
&BEGIN_PROVIDER [ integer(bit_kind), psi_det_generators, (N_int,2,10000) ]
&BEGIN_PROVIDER [ double precision, psi_coef_generators, (10000,N_states) ]

END_PROVIDER

subroutine update_generators_restart_coef
 implicit none 
 call set_generators_to_generators_restart
 call set_psi_det_to_generators
 call diagonalize_CI
 integer :: i,j,k,l
 do i = 1, N_det_generators_restart
   do j = 1, N_states
    psi_coef_generators_restart(i,j) = psi_coef(i,j)
   enddo
 enddo
 soft_touch psi_coef_generators_restart
 provide one_body_dm_mo_alpha_generators_restart
end

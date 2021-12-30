    
    Module Stuff
      
      Character(300) stuff_prefix
      
    CONTAINS
      
      Subroutine stuff_write_array( fn, data1d, data2d, data2d4, data3d)
        Implicit None
        Character(*), Intent(In) :: fn
        Real(Kind=8), Optional, Intent(In) :: data1d (:)
        Real(Kind=8), Optional, Intent(In) :: data2d (:,:)
        Real(Kind=4), Optional, Intent(In) :: data2d4(:,:)
        Real(Kind=8), Optional, Intent(In) :: data3d (:,:,:)
        Integer(Kind=4) i, fnid
        
        fnid = 88
        
        open(fnid, file = 'zz_'//trim(fn)//'_'//trim(stuff_prefix)//'.txt')
        
        if (present(data1d)) then
          do i = 1,Size(data1d,1)
            write(fnid,'(i6,3x,e20.12)') i,data1d(i)
          end do
        end if
        
        close(fnid)
      End Subroutine
            
    End Module

      module module1
        real*8 :: array1(7, 2)     
      end

      module module2
        real*8 :: array2(13:28, 21:77)     
      end
      
      program main
      use module1
      use module2
      Use mod_parser
      implicit none
      Type(parser) p
      Real(Kind=8) array10(100:200), array20(100:200)
      Integer(Kind=4) i
      
      print *,'aaaa'
      
      !CALL print1(array = array1, filename = 'array1.dat')
      !CALL print1(array2, 'array2.dat')
      
      array1 = 1._8
      
      call p % push(array1, 'array1.dat')
      
      do i = 100,200
        array10(i) = Real(i,8)
        array20(i) = Real(i*2,8)
      end do
      call test(array10, array20)
      
      end program main
      
      
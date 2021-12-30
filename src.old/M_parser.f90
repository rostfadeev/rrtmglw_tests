
      Module mod_parser
        
        Type parser
          Private
        Contains
          Generic,   Public :: push       => push_d14, push_d18, push_d24, push_d28
          Procedure         :: push_d14   => parser_push_d14
          Procedure         :: push_d18   => parser_push_d18
          Procedure         :: push_d24   => parser_push_d24
          Procedure         :: push_d28   => parser_push_d28
          Procedure         :: push_all   => parser_push_all
        End Type
        
      Contains
        
        Subroutine parser_push_d14(this, data, filename)
          Class(parser), Intent(InOut) :: this
          Real(Kind=4), Target, Intent(In) :: data(:)
          Character(*),         Intent(In) :: filename
          
          call this % push_all(d14 = data, filename = filename)
        End Subroutine
        
        Subroutine parser_push_d18(this, data, filename)
          Class(parser), Intent(InOut) :: this
          Real(Kind=8), Target, Intent(In) :: data(:)
          Character(*),         Intent(In) :: filename
          
          call this % push_all(d18 = data, filename = filename)
        End Subroutine
        
        Subroutine parser_push_d24(this, data, filename)
          Class(parser), Intent(InOut) :: this
          Real(Kind=4), Target, Intent(In) :: data(:,:)
          Character(*),         Intent(In) :: filename
          
          call this % push_all(d24 = data, filename = filename)
        End Subroutine
        
        Subroutine parser_push_d28(this, data, filename)
          Class(parser), Intent(InOut) :: this
          Real(Kind=8), Target, Intent(In) :: data(:,:)
          Character(*),         Intent(In) :: filename
          
          call this % push_all(d28 = data, filename = filename)
        End Subroutine
        
        Subroutine parser_push_all(this, d14, d18, d24, d28, filename)
          Class(parser), Intent(InOut) :: this
          Real(Kind=4), Target, Intent(In), Optional :: d14(:)
          Real(Kind=8), Target, Intent(In), Optional :: d18(:)
          Real(Kind=4), Target, Intent(In), Optional :: d24(:,:)
          Real(Kind=8), Target, Intent(In), Optional :: d28(:,:)
          Character(*),         Intent(In) :: filename
          
          Integer(Kind=4) n1,n2, i,j
          Character(1000) str
          Character(1) comma
          
          print *,'in parser'
          
          if (present(d28)) then
            n1 = Size(d28,1)
            n2 = Size(d28,2)
            
            write(*,'(a)') '{'
            call get_string(n1, str)
            do j = 1,n2
              if (j /= n2) then
                comma = ','
              else
                comma = ''
              end if
              
              write(*,trim(str)) '{',(d28(i,j),',',i=1,n1-1),d28(n1,j),'}'//comma
            end do
            write(*,'(a)') '}'
          end if
          
        End Subroutine
        
        Subroutine get_string(n, str)
          Implicit None
          Integer(Kind=4), Intent(In)    :: n
          Character(*),    Intent(InOut) :: str
          
          str = ''
          write(str,'(i8)') n
          str = '(a,'//trim(str)//'(1x,e10.2,a),a)'
          
        End Subroutine
        
        Subroutine test(array1, array2)
          Implicit None
          Real(Kind=8), Intent(In) :: array1(:)
          Real(Kind=8), Intent(In) :: array2(10:20)
          Integer(Kind=4) i
          
          print *,array1
          print *
          write(*,'(200(f10.1))') (array1(i),i=1,Size(array1))
          print *
          write(*,'(200(f10.1))') (array2(i),i=10,20)
          print *,'done'
          
        End Subroutine

        
      End Module
      
      
      
      
      
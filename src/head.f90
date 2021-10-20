

      Program rrtmglw_test
        Use YOMPHY3
        Use rrtmg_lw_rad
        USE rrtmg_lw_init
        Implicit None
        Integer(Kind=4), Parameter :: KST  = 2
        Integer(Kind=4), Parameter :: KEND = 401
        Integer(Kind=4), Parameter :: KLEV = 96
        Integer(Kind=4), Parameter :: KLON = 403
        !
        Integer(Kind=4)            :: ICLD = 2
        !
        Real(Kind=8) PAPRSF_play   (KLEV)
        Real(Kind=8) PAPRS_plev    (KLEV+1)
        Real(Kind=8) PT_tlay       (KLEV)
        Real(Kind=8) ZTH_tlev      (KLEV+1)
        Real(Kind=8) PTS_tsfc      (1)
        Real(Kind=8) ZQ_h2ovmr     (KLEV)
        Real(Kind=8) PQO3_o3vmr   (KLEV)
        Real(Kind=8) PEMIS_emis    (1)
        Real(Kind=8) ZEMIW_emiw    (1)
        Real(Kind=8) ZCLDSW_cldfr  (KLEV)
        Real(Kind=8) ZTAUCLD_taucld(KLEV,16)
        Real(Kind=8) ZFIWP_cicewp  (KLEV)
        Real(Kind=8) ZFLWP_cliqwp  (KLEV)
        Real(Kind=8) ZRADIP_reice  (KLEV)
        Real(Kind=8) ZRADLP_reliq  (KLEV)
        Real(Kind=8) Ztauaer_tauaer(5,KLEV)
        !
        Real(Kind=8) UFLX          (KLEV+1)
        Real(Kind=8) DFLX          (KLEV+1)
        Real(Kind=4) UFLXC         (KLEV+1)
        Real(Kind=4) DFLXC         (KLEV+1)
        !
        Real(Kind=8) UFLX_ref      (KLEV+1)
        Real(Kind=8) DFLX_ref      (KLEV+1)
        Real(Kind=4) UFLXC_ref     (KLEV+1)
        Real(Kind=4) DFLXC_ref     (KLEV+1)
        !
        Integer(Kind=4) NZK,NZKJ, fnid, KSTEP, i,i2, lon,k,k2
        Character(100) fn
        Character(3) c3
        Character(100) prefix
        
        Real(Kind=8) R,RMD,RMV,RMO3,RD,RV,RCPD,RNAVO,RKBOL
        
        fnid = 60
        
        prefix = '../data/'
        
        call ExtMods_init()
        
        
        RKBOL=1.380658E-23_8
        RNAVO=6.0221367E+23_8
        
        R=RNAVO*RKBOL
        RMD=28.9644_8
        RMV=18.0153_8
        RMO3=47.9942_8
        RD=1000._8*R/RMD
        RV=1000._8*R/RMV
        RCPD=3.5_8*RD
        
        CALL rrtmg_lw_ini(RCPD)
        
        c3 = '050'
        
        do lon = KST, KEND
          !call IntToChar(lon, c3)
          
          call read_rrtmg_array( data2d = PAPRSF_play (:)  , vn = 'PAPRSF' ,vnr = 'play', suffix = 'to')
          call read_rrtmg_array( data2d = PAPRS_plev  (:)  , vn = 'PAPRS'  ,vnr = 'plev', suffix = 'to')
          call read_rrtmg_array( data2d = PT_tlay     (:)  , vn = 'PT'     ,vnr = 'tlay', suffix = 'to')
          call read_rrtmg_array( data2d = ZTH_tlev    (:)  , vn = 'ZTH'    ,vnr = 'tlev', suffix = 'to')
          call read_rrtmg_array( data1d = PTS_tsfc         , vn = 'PTS'    ,vnr = 'tsfc', suffix = 'to')
          call read_rrtmg_array( data2d = ZQ_h2ovmr     (:)  , vn = 'ZQ'     ,vnr = 'h2ovmr', suffix = 'to')
          call read_rrtmg_array( data2d = PQO3_o3vmr   (:)  , vn = 'PQO3'   ,vnr = 'o3vmr', suffix = 'to')
          call read_rrtmg_array( data1d = PEMIS_emis       , vn = 'PEMIS'  ,vnr = 'emis', suffix = 'to')
          call read_rrtmg_array( data1d = ZEMIW_emiw       , vn = 'ZEMIW'  ,vnr = 'emiw', suffix = 'to')
          call read_rrtmg_array( data2d = ZCLDSW_cldfr (:)  , vn = 'ZCLDSW' ,vnr = 'cldfr', suffix = 'to')
          call read_rrtmg_array( data3d = ZTAUCLD_taucld(:,:), vn = 'ZTAUCLD',vnr = 'taucld', suffix = 'to')
          call read_rrtmg_array( data2d = ZFIWP_cicewp  (:)  , vn = 'ZFIWP'  ,vnr = 'cicewp', suffix = 'to')
          call read_rrtmg_array( data2d = ZFLWP_cliqwp  (:)  , vn = 'ZFLWP'  ,vnr = 'cliqwp', suffix = 'to')
          call read_rrtmg_array( data2d = ZRADIP_reice (:)  , vn = 'ZRADIP' ,vnr = 'reice', suffix = 'to')
          call read_rrtmg_array( data2d = ZRADLP_reliq (:)  , vn = 'ZRADLP' ,vnr = 'reliq', suffix = 'to')
          call read_rrtmg_array( data3d = Ztauaer_tauaer(:,:), vn = 'Ztauaer',vnr = 'tauaer', suffix = 'to')
        
          !-----------------------------
          
          CALL rrtmg_lw &
              (lon,lon,1,KLEV, ICLD    ,0    , &
              PAPRSF_play    , PAPRS_plev    ,PT_tlay    , ZTH_tlev    ,PTS_tsfc    , &
              ZQ_h2ovmr  ,PQO3_o3vmr   , RCARDI,& !co2vmr  ,  ch4vmr  ,n2ovmr  ,o2vmr, &
  !             cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
  !             PEMIS,Zemiw, 0 ,2,1, ZCLDSW   , &
  !             PEMIS,Zemiw, 2 ,3,1, ZCLDSW   , &
              PEMIS_emis,ZEMIW_emiw, 2 ,2,1, ZCLDSW_cldfr   , &
              ZTAUCLD_taucld  ,ZFIWP_cicewp  ,ZFLWP_cliqwp  , ZRADIP_reice   ,ZRADLP_reliq   , &
              tauaer = Ztauaer_tauaer, &
              uflx = uflx, dflx = dflx, uflxc = uflxc, dflxc = dflxc)
          
          !-----------------------------
          
          call read_rrtmg_array( data2d  = UFLX_ref (:), vn = 'UFLX' ,vnr = 'uflx' , suffix = 'output')
          call read_rrtmg_array( data2d  = DFLX_ref (:), vn = 'DFLX' ,vnr = 'dflx' , suffix = 'output')
          call read_rrtmg_array( data2d4 = UFLXC_ref(:), vn = 'UFLXC',vnr = 'uflxc', suffix = 'output')
          call read_rrtmg_array( data2d4 = DFLXC_ref(:), vn = 'DFLXC',vnr = 'dflxc', suffix = 'output')
          
          
          if (.False.) then
            do k = 1,KLEV+1
              write(*,'(i8,100(e20.12))') k,\
                  UFLX (k),UFLX (k)-UFLX_ref (k), \
                  DFLX (k),DFLX (k)-DFLX_ref (k), \
                  UFLXC(k),UFLXC(k)-UFLXC_ref(k), \
                  DFLXC(k),DFLXC(k)-DFLXC_ref(k)
            end do
          end if
          write(*,'(a,1(i6),10(e20.12))') c3,lon, maxval(abs(UFLX -UFLX_ref )), \
           maxval(abs(DFLX -DFLX_ref )),maxval(abs(UFLXC-UFLXC_ref)),maxval(abs(DFLXC-DFLXC_ref))
        end do
        
        print *,'done'
  
    Contains
    
      
      Subroutine write_rrtmg_array( data1d, data2d, data2d4, data3d, vn, vnr, suffix)
        Implicit None
        Character(*), Intent(In) :: vn, vnr, suffix
        Real(Kind=8), Optional, Intent(In) :: data1d (:)
        Real(Kind=8), Optional, Intent(In) :: data2d (:,:)
        Real(Kind=4), Optional, Intent(In) :: data2d4(:,:)
        Real(Kind=8), Optional, Intent(In) :: data3d (:,:,:)
        Integer(Kind=4) i2
        
        fn = 'zz_'//c3//'_'//vn//'_'//vnr//'_'//suffix//'_rrtmglw.txt'
        open(fnid, file = trim(fn), position = 'append')
        
        if (present(data1d)) then
          print *,vn,'  ',Size(data1d,1)
          do i = KST,KEND,40
            i2 = i-KST+1
            write(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZKJ,i,data1d(i2)
          end do
        end if
        
        if (present(data2d)) then
          print *,vn,'  ',Size(data2d,1)
          do i = KST,KEND,40
            i2 = i-KST+1
            write(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZKJ,i,(data2d(i2,k),k=1,Size(data2d,2))
          end do
        end if
        if (present(data2d4)) then
          print *,vn,'  ',Size(data2d4,1)
          do i = KST,KEND,40
            i2 = i-KST+1
            write(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZKJ,i,(data2d4(i2,k),k=1,Size(data2d4,2))
          end do
        end if
        
        if (present(data3d)) then
          print *,vn,'  ',Size(data3d,1)
          if (vn == 'ZTAUCLD') then
            do i = KST,KEND,40
              do k = 1,KLEV
                i2 = i-KST+1
                write(fnid,'(4(i8),1000(2x,e20.12))') KSTEP,NZKJ,i,k,data3d(i2,k,1:16)
              end do
            end do
          end if
          if (vn == 'Ztauaer') then
            do i = KST,KEND,40
              do k = 1,KLEV
                i2 = i-KST+1
                write(fnid,'(4(i8),1000(2x,e20.12))') KSTEP,NZKJ,i,k,data3d(i2,1:5,k)
              end do
            end do
          end if
          
          close(fnid)
        end if
      End Subroutine
            
      
      Subroutine read_rrtmg_array( data1d, data2d, data2d4, data3d, vn, vnr, suffix)
        Implicit None
        Character(*),           Intent(In)    :: vn, vnr, suffix
        Real(Kind=8), Optional, Intent(InOut) :: data1d (1)
        Real(Kind=8), Optional, Intent(InOut) :: data2d (:)
        Real(Kind=4), Optional, Intent(InOut) :: data2d4(:)
        Real(Kind=8), Optional, Intent(InOut) :: data3d (:,:)
        
        fn = trim(prefix)//'zz_'//c3//'_'//vn//'_'//vnr//'_'//suffix//'_rrtmglw.txt'
        open(fnid, file = trim(fn), action = 'read')
        
        if (present(data1d)) then
          do i = KST,KEND,1
            read(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZK,i2,data1d(1)
            if (i2 == lon) exit
          end do
        end if
        
        if (present(data2d)) then
          do i = KST,KEND,1
            read(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZK,i2,(data2d(k),k=1,Size(data2d,1))
            if (i2 == lon) exit
          end do
        end if
        if (present(data2d4)) then
          do i = KST,KEND,1
            read(fnid,'(3(i8),1000(2x,e20.12))') KSTEP,NZK,i2,(data2d4(k),k=1,Size(data2d4,1))
            if (i2 == lon) exit
          end do
        end if
        
        if (present(data3d)) then
          if (vn == 'ZTAUCLD') then
            do i = KST,KEND,1
              do k = 1,KLEV
                read(fnid,'(4(i8),1000(2x,e20.12))') KSTEP,NZK,i2,k2,data3d(k,1:16)
              end do
              if (i2 == lon) exit
            end do
          end if
          if (vn == 'Ztauaer') then
            do i = KST,KEND,1
              do k = 1,KLEV
                read(fnid,'(4(i8),1000(2x,e20.12))') KSTEP,NZK,i2,k2,data3d(1:5,k)
              end do
              if (i2 == lon) exit
            end do
          end if
          
          if (i > KEND) then
            print *,'lon not found! ',lon
            STOP
          end if
          
          close(fnid)
        end if
      End Subroutine
      
          
          Subroutine IntToChar(i,c,ca)
            Integer(Kind=4),               Intent(In)              :: i
            Character*(*),                 Intent(InOut), Optional :: c
            Character(Len=:), Allocatable, Intent(InOut), Optional :: ca
            Character(100) fmt, str
            Integer(Kind=4) n,k,m
            
            if (present(c)) then
              n = len(c)
              do k = 1,n-1
                write(c(k:k+1),'(i1)') 0
              end do
            else if (present(ca)) then
              n = 20
            else
              print *,'argument missed in "IntToChar"'
              STOP
            end if
            
            do k = 1,n
              if (i < 10**k) exit
            end do
            
            if(k > n) k = n
            do m = 1,999
              if (k < 10**m) then
                if (m < 10) then
                  write(str,'(a,i1,a)') '(a,i',m,',a)'
                else if (m < 100) then
                  write(str,'(a,i2,a)') '(a,i',m,',a)'
                end if
                exit
              end if
            end do
            write(fmt,str) '(i',k,')'
            
            if (present(c)) then
              write(c(n-k+1:n),fmt) i
            else if (present(ca)) then
              Allocate(Character(Len=k) :: ca)
              write(ca(n-k+1:n),fmt) i
            end if
          End Subroutine
        
      End Program
      
      

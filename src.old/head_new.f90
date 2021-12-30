

      Program rrtmglw_test
        Use YOMPHY3
        Use rrtmg_lw_rad
        USE rrtmg_lw_init
        Use Stuff
        Implicit None
        Integer(Kind=4), Parameter :: KST  = 2
        Integer(Kind=4), Parameter :: KEND = 2
        Integer(Kind=4), Parameter :: KLEV = 96
        Integer(Kind=4), Parameter :: KLON = 403
        !
        Integer(Kind=4)            :: ICLD = 2
        !
        Real(Kind=8) PAPRSF_play   (KST:KEND,KLEV)
        Real(Kind=8) PAPRS_plev    (KST:KEND,KLEV+1)
        Real(Kind=8) PT_tlay       (KST:KEND,KLEV)
        Real(Kind=8) ZTH_tlev      (KST:KEND,KLEV+1)
        Real(Kind=8) PTS_tsfc      (KST:KEND)
        Real(Kind=8) ZQ_h2ovmr     (KST:KEND,KLEV)
        Real(Kind=8) PQO3_o3vmr    (KST:KEND,KLEV)
        Real(Kind=8) emis          (KST:KEND,nbndlw)
        Real(Kind=8) PEMIS_emis    (KST:KEND)
        Real(Kind=8) ZEMIW_emiw    (KST:KEND)
        Real(Kind=8) ZCLDSW_cldfr  (KST:KEND,KLEV)
        Real(Kind=8) ZTAUCLD_taucld(KST:KEND,KLEV,16)
        Real(Kind=8) taucld        (16,KST:KEND,KLEV)
        Real(Kind=8) ZFIWP_cicewp  (KST:KEND,KLEV)
        Real(Kind=8) ZFLWP_cliqwp  (KST:KEND,KLEV)
        Real(Kind=8) ZRADIP_reice  (KST:KEND,KLEV)
        Real(Kind=8) ZRADLP_reliq  (KST:KEND,KLEV)
        Real(Kind=8) Ztauaer_tauaer(KST:KEND,5,KLEV)
        Real(Kind=8) tauaer        (KST:KEND,KLEV,16)
        
        Real(Kind=8) play  (KST:KEND,KLEV)
        Real(Kind=8) plev  (KST:KEND,KLEV+1)
        Real(Kind=8) tlay  (KST:KEND,KLEV)
        Real(Kind=8) tlev  (KST:KEND,KLEV+1)
        
        Real(Kind=8) h2ovmr(KST:KEND,KLEV)
        Real(Kind=8) co2vmr(KST:KEND,KLEV)
        Real(Kind=8) o3vmr (KST:KEND,KLEV)
        Real(Kind=8) n2ovmr(KST:KEND,KLEV)
        Real(Kind=8) ch4vmr(KST:KEND,KLEV)
        Real(Kind=8) o2vmr (KST:KEND,KLEV)
        
        Real(Kind=8) cfc11vmr(KST:KEND,KLEV)
        Real(Kind=8) cfc12vmr(KST:KEND,KLEV)
        Real(Kind=8) cfc22vmr(KST:KEND,KLEV)
        Real(Kind=8) ccl4vmr (KST:KEND,KLEV)
        !
        Real(Kind=8) UFLX          (KST:KEND,KLEV+1)
        Real(Kind=8) DFLX          (KST:KEND,KLEV+1)
        Real(Kind=8) UFLXC         (KST:KEND,KLEV+1)
        Real(Kind=8) DFLXC         (KST:KEND,KLEV+1)
        Real(Kind=8) hr            (KST:KEND,KLEV)
        Real(Kind=8) hrc           (KST:KEND,KLEV)
        !
        Real(Kind=8) UFLX_ref      (KST:KEND,KLEV+1)
        Real(Kind=8) DFLX_ref      (KST:KEND,KLEV+1)
        Real(Kind=4) UFLXC_ref     (KST:KEND,KLEV+1)
        Real(Kind=4) DFLXC_ref     (KST:KEND,KLEV+1)
        !
        Integer(Kind=4) NZK,NZKJ, fnid, KSTEP, i,i2, lon,k,k2, n, nlay, l
        Character(100) fn
        Character(3) c3
        Character(100) prefix
        
        Real(Kind=8) R,RMD,RMV,RMO3,RD,RV,RCPD,RNAVO,RKBOL
        
        real(kind=8), parameter :: amdw = 1.607793_rb  ! Molecular weight of dry air / water vapor
        real(kind=8), parameter :: amdc = 0.658114_rb  ! Molecular weight of dry air / carbon dioxide
        real(kind=8), parameter :: amdo = 0.603428_rb  ! Molecular weight of dry air / ozone
        real(kind=8), parameter :: amdm = 1.805423_rb  ! Molecular weight of dry air / methane
        real(kind=8), parameter :: amdn = 0.658090_rb  ! Molecular weight of dry air / nitrous oxide
        real(kind=8), parameter :: amdo2 = 0.905140_rb ! Molecular weight of dry air / oxygen
        real(kind=8), parameter :: amdc1 = 0.210852_rb ! Molecular weight of dry air / CFC11
        real(kind=8), parameter :: amdc2 = 0.239546_rb ! Molecular weight of dry air / CFC12
        
        
        fnid = 60
        
        prefix = '../data/'
        stuff_prefix = 'new'
        
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
        
        nlay = KLEV
        
        do lon = KST, KEND
          !call IntToChar(lon, c3)
          
          call read_rrtmg_array( data2d = PAPRSF_play (lon,:)  , vn = 'PAPRSF' ,vnr = 'play', suffix = 'to')
          call read_rrtmg_array( data2d = PAPRS_plev  (lon,:)  , vn = 'PAPRS'  ,vnr = 'plev', suffix = 'to')
          call read_rrtmg_array( data2d = PT_tlay     (lon,:)  , vn = 'PT'     ,vnr = 'tlay', suffix = 'to')
          call read_rrtmg_array( data2d = ZTH_tlev    (lon,:)  , vn = 'ZTH'    ,vnr = 'tlev', suffix = 'to')
          call read_rrtmg_array( data1d = PTS_tsfc    (:)    , vn = 'PTS'    ,vnr = 'tsfc', suffix = 'to')
          call read_rrtmg_array( data2d = ZQ_h2ovmr   (lon,:)  , vn = 'ZQ'     ,vnr = 'h2ovmr', suffix = 'to')
          call read_rrtmg_array( data2d = PQO3_o3vmr  (lon,:)  , vn = 'PQO3'   ,vnr = 'o3vmr', suffix = 'to')
          call read_rrtmg_array( data1d = PEMIS_emis  (:)    , vn = 'PEMIS'  ,vnr = 'emis', suffix = 'to')
          call read_rrtmg_array( data1d = ZEMIW_emiw  (:)    , vn = 'ZEMIW'  ,vnr = 'emiw', suffix = 'to')
          call read_rrtmg_array( data2d = ZCLDSW_cldfr(lon,:)  , vn = 'ZCLDSW' ,vnr = 'cldfr', suffix = 'to')
          call read_rrtmg_array( data3d = ZTAUCLD_taucld(lon,:,:), vn = 'ZTAUCLD',vnr = 'taucld', suffix = 'to')
          call read_rrtmg_array( data2d = ZFIWP_cicewp  (lon,:)  , vn = 'ZFIWP'  ,vnr = 'cicewp', suffix = 'to')
          call read_rrtmg_array( data2d = ZFLWP_cliqwp  (lon,:)  , vn = 'ZFLWP'  ,vnr = 'cliqwp', suffix = 'to')
          call read_rrtmg_array( data2d = ZRADIP_reice (lon,:)  , vn = 'ZRADIP' ,vnr = 'reice', suffix = 'to')
          call read_rrtmg_array( data2d = ZRADLP_reliq (lon,:)  , vn = 'ZRADLP' ,vnr = 'reliq', suffix = 'to')
          call read_rrtmg_array( data3d = Ztauaer_tauaer(lon,:,:), vn = 'Ztauaer',vnr = 'tauaer', suffix = 'to')
        
          !-----------------------------
          
          !USE YOMPHY3, ONLY :  RCH4     ,RN2O    ,RCFC11  ,RCFC12,RO2, RLWUH
          co2vmr(lon,:) = RCARDI*amdc         ! 2
          n2ovmr(lon,:) = RN2O*amdn           ! 4
          ch4vmr(lon,:) = RCH4*amdm           ! 6
          o2vmr (lon,:) = RO2*amdo2           ! 7
          
          cfc11vmr(lon,:) = rcfc11*amdc1
          cfc12vmr(lon,:) = rcfc12*amdc2
          cfc22vmr(lon,:) = 0._8
          ccl4vmr (lon,:) = 0._8
          
          do l = 1,KLEV
            h2ovmr(lon,l) = ZQ_h2ovmr (lon,nlay-l+1)*amdw  ! 1
            o3vmr (lon,l) = PQO3_o3vmr(lon,nlay-l+1)*amdo  ! 3
          end do
          
          do n=1,nbndlw
            if (n < 6 .or. n > 8) then
              emis(lon,n) = PEMIS_emis(lon)
            else
              emis(lon,n) = ZEMIW_emiw(lon)
            endif
          enddo
          
          do l = 1,KLEV
            taucld(1:16, lon, l) = ZTAUCLD_taucld(lon, l, 1:16)
          end do
          
          do l = 1,KLEV
            play(lon, l) = PAPRSF_play(lon, nlay-l+1)/100._8
            tlay(lon, l) = PT_tlay    (lon, nlay-l+1)
          end do
          
          do l = 1,KLEV+1
            plev(lon, l) = PAPRS_plev (lon, nlay+1-l+1)/100._8
            tlev(lon, l) = ZTH_tlev   (lon, nlay+1-l+1)
          end do
          
          do l = 1,KLEV
            tauaer(lon, l, 1) = Ztauaer_tauaer(lon,1,nlay+1-l)
            tauaer(lon, l, 2) = Ztauaer_tauaer(lon,1,nlay+1-l)
            tauaer(lon, l, 3) = Ztauaer_tauaer(lon,2,nlay+1-l)                        
            tauaer(lon, l, 4) = Ztauaer_tauaer(lon,2,nlay+1-l)
            tauaer(lon, l, 5) = Ztauaer_tauaer(lon,2,nlay+1-l)
            tauaer(lon, l, 6) = Ztauaer_tauaer(lon,3,nlay+1-l)
            tauaer(lon, l, 8) = Ztauaer_tauaer(lon,3,nlay+1-l)
            tauaer(lon, l, 9) = Ztauaer_tauaer(lon,3,nlay+1-l)  
            tauaer(lon, l, 7) = Ztauaer_tauaer(lon,4,nlay+1-l)
            tauaer(lon, l,10) = Ztauaer_tauaer(lon,5,nlay+1-l)
            tauaer(lon, l,11) = Ztauaer_tauaer(lon,5,nlay+1-l)
            tauaer(lon, l,12) = Ztauaer_tauaer(lon,5,nlay+1-l)
            tauaer(lon, l,13) = Ztauaer_tauaer(lon,5,nlay+1-l)
            tauaer(lon, l,14) = Ztauaer_tauaer(lon,5,nlay+1-l)                 
            tauaer(lon, l,15) = Ztauaer_tauaer(lon,5,nlay+1-l)
            tauaer(lon, l,16) = Ztauaer_tauaer(lon,5,nlay+1-l)
          end do
          
          !ZCLDSW_cldfr(lon,1) = 0._8
          
          !print *,ZCLDSW_cldfr
          
          CALL rrtmg_lw &
              (ncol = 1, nlay = KLEV, icld = ICLD, idrv = 0, &
              play = play, plev = plev, tlay = tlay, tlev = tlev, tsfc = PTS_tsfc, &
              h2ovmr = h2ovmr, o3vmr = o3vmr, co2vmr = co2vmr, ch4vmr = ch4vmr, n2ovmr = n2ovmr, o2vmr = o2vmr, &
              cfc11vmr = cfc11vmr, cfc12vmr = cfc12vmr, cfc22vmr = cfc22vmr, ccl4vmr = ccl4vmr, &
              emis = emis, &
              inflglw = 2, iceflglw = 2, liqflglw = 1, cldfr = ZCLDSW_cldfr, &
              taucld = taucld, cicewp = ZFIWP_cicewp, cliqwp = ZFLWP_cliqwp, &
              reice = ZRADIP_reice, reliq = ZRADLP_reliq   , &
              tauaer = tauaer, &
              uflx = uflx, dflx = dflx, hr = hr, uflxc = uflxc, dflxc = dflxc, hrc = hrc)
              
!             (ncol    ,nlay    ,icld    ,idrv    , &
!              play    ,plev    ,tlay    ,tlev    ,tsfc    , &
!              h2ovmr  ,o3vmr   ,co2vmr  ,ch4vmr  ,n2ovmr  ,o2vmr, &
!              cfc11vmr,cfc12vmr,cfc22vmr,ccl4vmr ,emis    , &
!              inflglw ,iceflglw,liqflglw,cldfr   , &
!              taucld  ,cicewp  ,cliqwp  ,reice   ,reliq   , &
!              tauaer  , &
!              uflx    ,dflx    ,hr      ,uflxc   ,dflxc,  hrc, &
!              duflx_dt,duflxc_dt )
          
          
          !-----------------------------
          
          call read_rrtmg_array( data2d  = UFLX_ref (lon,:), vn = 'UFLX' ,vnr = 'uflx' , suffix = 'output')
          call read_rrtmg_array( data2d  = DFLX_ref (lon,:), vn = 'DFLX' ,vnr = 'dflx' , suffix = 'output')
          call read_rrtmg_array( data2d4 = UFLXC_ref(lon,:), vn = 'UFLXC',vnr = 'uflxc', suffix = 'output')
          call read_rrtmg_array( data2d4 = DFLXC_ref(lon,:), vn = 'DFLXC',vnr = 'dflxc', suffix = 'output')
          
          
          if (.True.) then
            do k = 1,KLEV+1
              write(*,'(i8,4(e15.5),a,4(e15.5),a,100(e15.5))') k,\
                  UFLX (lon,k),UFLX_ref (lon,k),UFLX (lon,k)-UFLX_ref (lon,k),(UFLX (lon,k)-UFLX_ref (lon,k))/max(1.E-10,UFLX_ref (lon,k)), \
                  ' - ',\
                  DFLX (lon,k),DFLX_ref (lon,k),DFLX (lon,k)-DFLX_ref (lon,k),(DFLX (lon,k)-DFLX_ref (lon,k))/max(1.E-10,DFLX_ref (lon,k)), \
                  ' - ',\
                  UFLXC(lon,k),UFLXC(lon,k)-UFLXC_ref(lon,k), \
                  DFLXC(lon,k),DFLXC(lon,k)-DFLXC_ref(lon,k)
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
      
      

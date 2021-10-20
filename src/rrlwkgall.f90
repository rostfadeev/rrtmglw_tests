      module rrlw_kg01

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 1
! band 1:  10-250 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mn2 : real     
! kbo_mn2 : real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no1  = 16

      real(kind=rb) :: fracrefao(no1)  , fracrefbo(no1)
      real(kind=rb) :: kao(5,13,no1)
      real(kind=rb) :: kbo(5,13:59,no1)
      real(kind=rb) :: kao_mn2(19,no1) , kbo_mn2(19,no1)
      real(kind=rb) :: selfrefo(10,no1), forrefo(4,no1)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 1
! band 1:  10-250 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! ka_mn2  : real     
! kb_mn2  : real     
! selfref : real     
! forref  : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng1  = 10

      real(kind=rb) :: fracrefa(ng1)  , fracrefb(ng1)
      real(kind=rb) :: ka(5,13,ng1)   , absa(65,ng1)
      real(kind=rb) :: kb(5,13:59,ng1), absb(235,ng1)
      real(kind=rb) :: ka_mn2(19,ng1) , kb_mn2(19,ng1)
      real(kind=rb) :: selfref(10,ng1), forref(4,ng1)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrlw_kg01




      module rrlw_kg02

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 2
! band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no2  = 16

      real(kind=rb) :: fracrefao(no2)   , fracrefbo(no2)
      real(kind=rb) :: kao(5,13,no2)
      real(kind=rb) :: kbo(5,13:59,no2)
      real(kind=rb) :: selfrefo(10,no2) , forrefo(4,no2)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 2
! band 2:  250-500 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! absa    : real
! absb    : real
! selfref : real     
! forref  : real
!
! refparam: real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng2  = 12

      real(kind=rb) :: fracrefa(ng2)  , fracrefb(ng2)
      real(kind=rb) :: ka(5,13,ng2)   , absa(65,ng2)
      real(kind=rb) :: kb(5,13:59,ng2), absb(235,ng2)
      real(kind=rb) :: selfref(10,ng2), forref(4,ng2)

      real(kind=rb) :: refparam(13)

      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg02


      module rrlw_kg03

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 3
! band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mn2o: real     
! kbo_mn2o: real     
! selfrefo: real     
! forrefo : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no3  = 16

      real(kind=rb) :: fracrefao(no3,9) ,fracrefbo(no3,5)
      real(kind=rb) :: kao(9,5,13,no3)
      real(kind=rb) :: kbo(5,5,13:59,no3)
      real(kind=rb) :: kao_mn2o(9,19,no3), kbo_mn2o(5,19,no3)
      real(kind=rb) :: selfrefo(10,no3)
      real(kind=rb) :: forrefo(4,no3)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 3
! band 3:  500-630 cm-1 (low - h2o,co2; high - h2o,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! ka_mn2o : real     
! kb_mn2o : real     
! selfref : real     
! forref  : real
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng3  = 16

      real(kind=rb) :: fracrefa(ng3,9) ,fracrefb(ng3,5)
      real(kind=rb) :: ka(9,5,13,ng3)  ,absa(585,ng3)
      real(kind=rb) :: kb(5,5,13:59,ng3),absb(1175,ng3)
      real(kind=rb) :: ka_mn2o(9,19,ng3), kb_mn2o(5,19,ng3)
      real(kind=rb) :: selfref(10,ng3)
      real(kind=rb) :: forref(4,ng3)

      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))

      end module rrlw_kg03


      module rrlw_kg04

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 4
! band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no4  = 16

      real(kind=rb) :: fracrefao(no4,9)  ,fracrefbo(no4,5)
      real(kind=rb) :: kao(9,5,13,no4)
      real(kind=rb) :: kbo(5,5,13:59,no4)
      real(kind=rb) :: selfrefo(10,no4)  ,forrefo(4,no4)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 4
! band 4:  630-700 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
! absa    : real
! absb    : real
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng4  = 14

      real(kind=rb) :: fracrefa(ng4,9)  ,fracrefb(ng4,5)
      real(kind=rb) :: ka(9,5,13,ng4)   ,absa(585,ng4)
      real(kind=rb) :: kb(5,5,13:59,ng4),absb(1175,ng4)
      real(kind=rb) :: selfref(10,ng4)  ,forref(4,ng4)

      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))

      end module rrlw_kg04
      module rrlw_kg05

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 5
! band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real
! kao     : real     
! kbo     : real     
! kao_mo3 : real     
! selfrefo: real     
! forrefo : real     
! ccl4o   : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no5  = 16

      real(kind=rb) :: fracrefao(no5,9) ,fracrefbo(no5,5)
      real(kind=rb) :: kao(9,5,13,no5)
      real(kind=rb) :: kbo(5,5,13:59,no5)
      real(kind=rb) :: kao_mo3(9,19,no5)
      real(kind=rb) :: selfrefo(10,no5)
      real(kind=rb) :: forrefo(4,no5)
      real(kind=rb) :: ccl4o(no5)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 5
! band 5:  700-820 cm-1 (low - h2o,co2; high - o3,co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real
! ka      : real     
! kb      : real     
! ka_mo3  : real     
! selfref : real     
! forref  : real     
! ccl4    : real
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng5  = 16

      real(kind=rb) :: fracrefa(ng5,9) ,fracrefb(ng5,5)
      real(kind=rb) :: ka(9,5,13,ng5)   ,absa(585,ng5)
      real(kind=rb) :: kb(5,5,13:59,ng5),absb(1175,ng5)
      real(kind=rb) :: ka_mo3(9,19,ng5)
      real(kind=rb) :: selfref(10,ng5)
      real(kind=rb) :: forref(4,ng5)
      real(kind=rb) :: ccl4(ng5)
      
      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,1,13,1),absb(1,1))

      end module rrlw_kg05

      module rrlw_kg06

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 6
! band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mco2: real     
! selfrefo: real     
! forrefo : real     
!cfc11adjo: real
! cfc12o  : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no6  = 16

      real(kind=rb) , dimension(no6) :: fracrefao
      real(kind=rb) :: kao(5,13,no6)
      real(kind=rb) :: kao_mco2(19,no6)
      real(kind=rb) :: selfrefo(10,no6)
      real(kind=rb) :: forrefo(4,no6)

      real(kind=rb) , dimension(no6) :: cfc11adjo
      real(kind=rb) , dimension(no6) :: cfc12o

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 6
! band 6:  820-980 cm-1 (low - h2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mco2 : real     
! selfref : real     
! forref  : real     
!cfc11adj : real
! cfc12   : real
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng6  = 8

      real(kind=rb) , dimension(ng6) :: fracrefa
      real(kind=rb) :: ka(5,13,ng6),absa(65,ng6)
      real(kind=rb) :: ka_mco2(19,ng6)
      real(kind=rb) :: selfref(10,ng6)
      real(kind=rb) :: forref(4,ng6)

      real(kind=rb) , dimension(ng6) :: cfc11adj
      real(kind=rb) , dimension(ng6) :: cfc12

      equivalence (ka(1,1,1),absa(1,1))

      end module rrlw_kg06
      module rrlw_kg07

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 7
! band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mco2: real     
! kbo_mco2: real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no7  = 16

      real(kind=rb) , dimension(no7) :: fracrefbo
      real(kind=rb) :: fracrefao(no7,9)
      real(kind=rb) :: kao(9,5,13,no7)
      real(kind=rb) :: kbo(5,13:59,no7)
      real(kind=rb) :: kao_mco2(9,19,no7)
      real(kind=rb) :: kbo_mco2(19,no7)
      real(kind=rb) :: selfrefo(10,no7)
      real(kind=rb) :: forrefo(4,no7)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 7
! band 7:  980-1080 cm-1 (low - h2o,o3; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mco2 : real     
! kb_mco2 : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng7  = 12

      real(kind=rb) , dimension(ng7) :: fracrefb
      real(kind=rb) :: fracrefa(ng7,9)
      real(kind=rb) :: ka(9,5,13,ng7) ,absa(585,ng7)
      real(kind=rb) :: kb(5,13:59,ng7),absb(235,ng7)
      real(kind=rb) :: ka_mco2(9,19,ng7)
      real(kind=rb) :: kb_mco2(19,ng7)
      real(kind=rb) :: selfref(10,ng7)
      real(kind=rb) :: forref(4,ng7)

      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg07
      module rrlw_kg08

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 8
! band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mco2: real     
! kbo_mco2: real     
! kao_mn2o: real     
! kbo_mn2o: real     
! kao_mo3 : real     
! selfrefo: real     
! forrefo : real     
! cfc12o  : real     
!cfc22adjo: real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no8  = 16

      real(kind=rb) , dimension(no8) :: fracrefao
      real(kind=rb) , dimension(no8) :: fracrefbo
      real(kind=rb) , dimension(no8) :: cfc12o
      real(kind=rb) , dimension(no8) :: cfc22adjo

      real(kind=rb) :: kao(5,13,no8)
      real(kind=rb) :: kao_mco2(19,no8)
      real(kind=rb) :: kao_mn2o(19,no8)
      real(kind=rb) :: kao_mo3(19,no8)
      real(kind=rb) :: kbo(5,13:59,no8)
      real(kind=rb) :: kbo_mco2(19,no8)
      real(kind=rb) :: kbo_mn2o(19,no8)
      real(kind=rb) :: selfrefo(10,no8)
      real(kind=rb) :: forrefo(4,no8)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 8
! band 8:  1080-1180 cm-1 (low (i.e.>~300mb) - h2o; high - o3)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mco2 : real     
! kb_mco2 : real     
! ka_mn2o : real     
! kb_mn2o : real     
! ka_mo3  : real     
! selfref : real     
! forref  : real     
! cfc12   : real     
! cfc22adj: real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng8  = 8

      real(kind=rb) , dimension(ng8) :: fracrefa
      real(kind=rb) , dimension(ng8) :: fracrefb
      real(kind=rb) , dimension(ng8) :: cfc12
      real(kind=rb) , dimension(ng8) :: cfc22adj

      real(kind=rb) :: ka(5,13,ng8)    ,absa(65,ng8)
      real(kind=rb) :: kb(5,13:59,ng8) ,absb(235,ng8)
      real(kind=rb) :: ka_mco2(19,ng8)
      real(kind=rb) :: ka_mn2o(19,ng8)
      real(kind=rb) :: ka_mo3(19,ng8)
      real(kind=rb) :: kb_mco2(19,ng8)
      real(kind=rb) :: kb_mn2o(19,ng8)
      real(kind=rb) :: selfref(10,ng8)
      real(kind=rb) :: forref(4,ng8)

      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg08

      module rrlw_kg09

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 9
! band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mn2o: real     
! kbo_mn2o: real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no9  = 16

      real(kind=rb) , dimension(no9) :: fracrefbo

      real(kind=rb) :: fracrefao(no9,9)
      real(kind=rb) :: kao(9,5,13,no9)
      real(kind=rb) :: kbo(5,13:59,no9)
      real(kind=rb) :: kao_mn2o(9,19,no9)
      real(kind=rb) :: kbo_mn2o(19,no9)
      real(kind=rb) :: selfrefo(10,no9)
      real(kind=rb) :: forrefo(4,no9)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 9
! band 9:  1180-1390 cm-1 (low - h2o,ch4; high - ch4)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mn2o : real     
! kb_mn2o : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng9  = 12

      real(kind=rb) , dimension(ng9) :: fracrefb
      real(kind=rb) :: fracrefa(ng9,9)
      real(kind=rb) :: ka(9,5,13,ng9) ,absa(585,ng9)
      real(kind=rb) :: kb(5,13:59,ng9) ,absb(235,ng9)
      real(kind=rb) :: ka_mn2o(9,19,ng9)
      real(kind=rb) :: kb_mn2o(19,ng9)
      real(kind=rb) :: selfref(10,ng9)
      real(kind=rb) :: forref(4,ng9)

      equivalence (ka(1,1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg09
      module rrlw_kg10

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 10
! band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no10 = 16

      real(kind=rb) , dimension(no10) :: fracrefao
      real(kind=rb) , dimension(no10) :: fracrefbo

      real(kind=rb) :: kao(5,13,no10)
      real(kind=rb) :: kbo(5,13:59,no10)
      real(kind=rb) :: selfrefo(10,no10)
      real(kind=rb) :: forrefo(4,no10)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 10
! band 10:  1390-1480 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng10 = 6

      real(kind=rb) , dimension(ng10) :: fracrefa
      real(kind=rb) , dimension(ng10) :: fracrefb

      real(kind=rb) :: ka(5,13,ng10)   , absa(65,ng10)
      real(kind=rb) :: kb(5,13:59,ng10), absb(235,ng10)
      real(kind=rb) :: selfref(10,ng10)
      real(kind=rb) :: forref(4,ng10)

      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg10
      module rrlw_kg11

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 11
! band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! kao_mo2 : real     
! kbo_mo2 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no11 = 16

      real(kind=rb) , dimension(no11) :: fracrefao
      real(kind=rb) , dimension(no11) :: fracrefbo

      real(kind=rb) :: kao(5,13,no11)
      real(kind=rb) :: kbo(5,13:59,no11)
      real(kind=rb) :: kao_mo2(19,no11)
      real(kind=rb) :: kbo_mo2(19,no11)
      real(kind=rb) :: selfrefo(10,no11)
      real(kind=rb) :: forrefo(4,no11)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 11
! band 11:  1480-1800 cm-1 (low - h2o; high - h2o)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! ka_mo2  : real     
! kb_mo2  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng11 = 8

      real(kind=rb) , dimension(ng11) :: fracrefa
      real(kind=rb) , dimension(ng11) :: fracrefb

      real(kind=rb) :: ka(5,13,ng11)   , absa(65,ng11)
      real(kind=rb) :: kb(5,13:59,ng11), absb(235,ng11)
      real(kind=rb) :: ka_mo2(19,ng11)
      real(kind=rb) :: kb_mo2(19,ng11)
      real(kind=rb) :: selfref(10,ng11)
      real(kind=rb) :: forref(4,ng11)

      equivalence (ka(1,1,1),absa(1,1)),(kb(1,13,1),absb(1,1))

      end module rrlw_kg11
      module rrlw_kg12

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 12
! band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no12 = 16

      real(kind=rb) :: fracrefao(no12,9)
      real(kind=rb) :: kao(9,5,13,no12)
      real(kind=rb) :: selfrefo(10,no12)
      real(kind=rb) :: forrefo(4,no12)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 12
! band 12:  1800-2080 cm-1 (low - h2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng12 = 8

      real(kind=rb) :: fracrefa(ng12,9)
      real(kind=rb) :: ka(9,5,13,ng12) ,absa(585,ng12)
      real(kind=rb) :: selfref(10,ng12)
      real(kind=rb) :: forref(4,ng12)

      equivalence (ka(1,1,1,1),absa(1,1))

      end module rrlw_kg12
      module rrlw_kg13

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 13
! band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mco2: real     
! kao_mco : real     
! kbo_mo3 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no13 = 16

      real(kind=rb) , dimension(no13) :: fracrefbo

      real(kind=rb) :: fracrefao(no13,9)
      real(kind=rb) :: kao(9,5,13,no13)
      real(kind=rb) :: kao_mco2(9,19,no13)
      real(kind=rb) :: kao_mco(9,19,no13)
      real(kind=rb) :: kbo_mo3(19,no13)
      real(kind=rb) :: selfrefo(10,no13)
      real(kind=rb) :: forrefo(4,no13)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 13
! band 13:  2080-2250 cm-1 (low - h2o,n2o; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mco2 : real     
! ka_mco  : real     
! kb_mo3  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng13 = 4

      real(kind=rb) , dimension(ng13) :: fracrefb

      real(kind=rb) :: fracrefa(ng13,9)
      real(kind=rb) :: ka(9,5,13,ng13) ,absa(585,ng13)
      real(kind=rb) :: ka_mco2(9,19,ng13)
      real(kind=rb) :: ka_mco(9,19,ng13)
      real(kind=rb) :: kb_mo3(19,ng13)
      real(kind=rb) :: selfref(10,ng13)
      real(kind=rb) :: forref(4,ng13)

      equivalence (ka(1,1,1,1),absa(1,1))

      end module rrlw_kg13
      module rrlw_kg14

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 14
! band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
!fracrefbo: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no14 = 16

      real(kind=rb) , dimension(no14) :: fracrefao
      real(kind=rb) , dimension(no14) :: fracrefbo

      real(kind=rb) :: kao(5,13,no14)
      real(kind=rb) :: kbo(5,13:59,no14)
      real(kind=rb) :: selfrefo(10,no14)
      real(kind=rb) :: forrefo(4,no14)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 14
! band 14:  2250-2380 cm-1 (low - co2; high - co2)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
!fracrefb : real    
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng14 = 2

      real(kind=rb) , dimension(ng14) :: fracrefa
      real(kind=rb) , dimension(ng14) :: fracrefb

      real(kind=rb) :: ka(5,13,ng14)   ,absa(65,ng14)
      real(kind=rb) :: kb(5,13:59,ng14),absb(235,ng14)
      real(kind=rb) :: selfref(10,ng14)
      real(kind=rb) :: forref(4,ng14)

      equivalence (ka(1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrlw_kg14
      module rrlw_kg15

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 15
! band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kao_mn2 : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no15 = 16

      real(kind=rb) :: fracrefao(no15,9)
      real(kind=rb) :: kao(9,5,13,no15)
      real(kind=rb) :: kao_mn2(9,19,no15)
      real(kind=rb) :: selfrefo(10,no15)
      real(kind=rb) :: forrefo(4,no15)


!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 15
! band 15:  2380-2600 cm-1 (low - n2o,co2; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! ka_mn2  : real     
! selfref : real     
! forref  : real     
!
! absa    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng15 = 2

      real(kind=rb) :: fracrefa(ng15,9)
      real(kind=rb) :: ka(9,5,13,ng15) ,absa(585,ng15)
      real(kind=rb) :: ka_mn2(9,19,ng15)
      real(kind=rb) :: selfref(10,ng15)
      real(kind=rb) :: forref(4,ng15)

      equivalence (ka(1,1,1,1),absa(1,1))

      end module rrlw_kg15
      module rrlw_kg16

      use parkind ,only : im => kind_im, rb => kind_rb

      implicit none
      save

!-----------------------------------------------------------------
! rrtmg_lw ORIGINAL abs. coefficients for interval 16
! band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefao: real    
! kao     : real     
! kbo     : real     
! selfrefo: real     
! forrefo : real     
!-----------------------------------------------------------------

      integer(kind=im), parameter :: no16 = 16

      real(kind=rb) , dimension(no16) :: fracrefbo

      real(kind=rb) :: fracrefao(no16,9)
      real(kind=rb) :: kao(9,5,13,no16)
      real(kind=rb) :: kbo(5,13:59,no16)
      real(kind=rb) :: selfrefo(10,no16)
      real(kind=rb) :: forrefo(4,no16)

!-----------------------------------------------------------------
! rrtmg_lw COMBINED abs. coefficients for interval 16
! band 16:  2600-3000 cm-1 (low - h2o,ch4; high - nothing)
!
! Initial version:  JJMorcrette, ECMWF, jul1998
! Revised: MJIacono, AER, jun2006
! Revised: MJIacono, AER, aug2008
!-----------------------------------------------------------------
!
!  name     type     purpose
!  ----   : ----   : ---------------------------------------------
!fracrefa : real    
! ka      : real     
! kb      : real     
! selfref : real     
! forref  : real     
!
! absa    : real
! absb    : real
!-----------------------------------------------------------------

      integer(kind=im), parameter :: ng16 = 2

      real(kind=rb) , dimension(ng16) :: fracrefb

      real(kind=rb) :: fracrefa(ng16,9)
      real(kind=rb) :: ka(9,5,13,ng16) ,absa(585,ng16)
      real(kind=rb) :: kb(5,13:59,ng16), absb(235,ng16)
      real(kind=rb) :: selfref(10,ng16)
      real(kind=rb) :: forref(4,ng16)

      equivalence (ka(1,1,1,1),absa(1,1)), (kb(1,13,1),absb(1,1))

      end module rrlw_kg16


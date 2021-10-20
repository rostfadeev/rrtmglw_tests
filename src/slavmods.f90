      Module YOMCST
        Implicit None
        
        Real(Kind=8) :: RPI
        
      End Module
      
      Module YOMPHY3
        Implicit None
        
        Real(Kind=8) :: RCH4
        Real(Kind=8) :: RN2O
        Real(Kind=8) :: RCFC11
        Real(Kind=8) :: RCFC12
        Real(Kind=8) :: RO2
        Real(Kind=8) :: RLWUH
        Real(Kind=8) :: RCARDI
        
      End Module
      
      
      Subroutine ExtMods_init()
        Use YOMCST
        Use YOMPHY3
        Implicit None
        Real(Kind=8) ZAIRMWG,ZCO2MWG,ZCH4MWG,ZN2OMWG
        Real(Kind=8) ZO3MWG,ZC11MWG,ZC12MWG
        
        RPI = ASIN(1._8)*2._8
        
        ZAIRMWG = 28.970_8
        ZCO2MWG = 44.011_8
        ZCH4MWG = 16.043_8
        ZN2OMWG = 44.013_8
        ZO3MWG  = 47.9982_8
        ZC11MWG = 137.3686_8
        ZC12MWG = 120.9140_8

        RCARDI  = 401.E-06_8*ZCO2MWG/ZAIRMWG
        RCH4    = 1.72E-06_8*ZCH4MWG/ZAIRMWG
        RN2O    = 310.E-09_8*ZN2OMWG/ZAIRMWG
        !RO3     =   1.E-06_8*ZO3MWG /ZAIRMWG
        RCFC11  = 280.E-12_8*ZC11MWG/ZAIRMWG
        RCFC12  = 484.E-12_8*ZC12MWG/ZAIRMWG
        RO2=1.E-14
        
        RLWUH = 0.84
        
        !REPH2O=5.E-10_8
        
      End Subroutine
      

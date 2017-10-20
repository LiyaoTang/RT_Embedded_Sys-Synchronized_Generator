--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx Analog to digital converter

package STM32F4.ADC is

   -- General ADC definitions

   type ADC_No is range 1 .. 3;

   -- ADC_CR1 Control 1

   subtype AWDCH_Range is Bits_5 range 0 .. 18;

   type RES_Options is (Bits_12_ADCCLK_cycles_15,
                        Bits_10_ADCCLK_cycles_13,
                        Bits_8_ADCCLK_cycles_11,
                        Bits_6_ADCCLK_cycles_9) with Size => 2;

   -- Hardwired connections

   VRef_Channel : constant AWDCH_Range := 17;
   VBat_Channel : constant AWDCH_Range := 18;

   -- Ranks

   subtype Regular_Channel_Rank  is Natural range 1 .. 16;
   subtype Injected_Channel_Rank is Natural range 1 .. 4;

   -- ADC_CR2 Control 2

   type CONT_Options is (Single,
                         Continuous)      with Size => 1;

   type ALIGN_Options is (Right,
                          Left)      with Size => 1;

   type JEXTSEL_Options is
     (Timer_1_CC4_event,
      Timer_1_TRGO_event,
      Timer_2_CC1_event,
      Timer_2_TRGO_event,
      Timer_3_CC2_event,
      Timer_3_CC4_event,
      Timer_4_CC1_event,
      Timer_4_CC2_event,
      Timer_4_CC3_event,
      Timer_4_TRGO_event,
      Timer_5_CC4_event,
      Timer_5_TRGO_event,
      Timer_8_CC2_event,
      Timer_8_CC3_event,
      Timer_8_CC4_event,
      EXTI_line15) with Size => 4;

   type EXTSEL_Options is
     (Timer_1_CC1_event,
      Timer_1_CC2_event,
      Timer_1_CC3_event,
      Timer_2_CC2_event,
      Timer_2_CC3_event,
      Timer_2_CC4_event,
      Timer_2_TRGO_event,
      Timer_3_CC1_event,
      Timer_3_TRGO_event,
      Timer_4_CC4_event,
      Timer_5_CC1_event,
      Timer_5_CC2_event,
      Timer_5_CC3_event,
      Timer_8_CC1_event,
      Timer_8_TRGO_event,
      EXTI_line11) with Size => 4;

   type EXTEN_Options is (Disabled,
                          Rising_edge,
                          Falling_edge,
                          Both_edges) with Size => 2;

   -- ADC_SMPR1 Sample time 1

   type SMP_Options is
     (Cycles_3,
      Cycles_15,
      Cycles_28,
      Cycles_56,
      Cycles_84,
      Cycles_112,
      Cycles_144,
      Cycles_480) with Size => 3;

   -- ADC_CCR Common control

   type MULTI_Options is
     (Independent,
      Dual_regular_simultaneous_injected_simultaneous,
      Dual_regular_simultaneous_alternate_trigger,
      Dual_injected_simultaneous_only,
      Dual_regular_simultaneous_only,
      Dual_interleaved_only,
      Dual_alternate_trigger_only,
      Triple_regular_simultaneous_injected_simultaneous,
      Triple_regular_simultaneous_alternate_trigger,
      Triple_injected_simultaneous_only,
      Triple_regular_simultaneous_only,
      Triple_interleaved_only,
      Triple_alternate_trigger_only) with Size => 5;

   type DMA_Options is
     (Disabled,
      Mode_1,
      Mode_2,
      Mode_3) with Size => 2;

   type ADCPRE_Options is
     (Divided_by_2,
      Divided_by_4,
      Divided_by_6,
      Divided_by_8) with Size => 2;

private

   -- ADC_SR Status

   type ADC_SR is record
      AWD      : Occurance; -- Analog watchdog flag (read & clear on write)
      EOC      : Completed; -- Regular channel end of conversion (read & clear on write)
      JEOC     : Completed; -- Injected channel end of conversion (read & clear on write)
      JSTRT    : Starter;   -- Injected channel start flag (read & clear on write)
      STRT     : Starter;   -- Regular channel start flag (read & clear on write)
      OVR      : Occurance; -- Overrun (read & clear on write)
      Reserved : Bits_26;
   end record with Volatile, Size => Word'Size;

   for ADC_SR use record
      AWD      at 0 range  0 ..  0; -- Analog watchdog flag (read & clear on write)
      EOC      at 0 range  1 ..  1; -- Regular channel end of conversion (read & clear on write)
      JEOC     at 0 range  2 ..  2; -- Injected channel end of conversion (read & clear on write)
      JSTRT    at 0 range  3 ..  3; -- Injected channel start flag (read & clear on write)
      STRT     at 0 range  4 ..  4; -- Regular channel start flag (read & clear on write)
      OVR      at 0 range  5 ..  5; -- Overrun (read & clear on write)
      Reserved at 0 range  6 .. 31;
   end record;

   ADC_SR_Reset : constant ADC_SR :=
     (AWD      => Not_Occured,  -- Analog watchdog flag (read & clear on write)
      EOC      => Not_Complete, -- Regular channel end of conversion (read & clear on write)
      JEOC     => Not_Complete, -- Injected channel end of conversion (read & clear on write)
      JSTRT    => Not_Started,  -- Injected channel start flag (read & clear on write)
      STRT     => Not_Started,  -- Regular channel start flag (read & clear on write)
      OVR      => Not_Occured,  -- Overrun (read & clear on write)
      Reserved => 0);

   -- ADC_CR1 Control 1

   for RES_Options use (Bits_12_ADCCLK_cycles_15 => 0,
                        Bits_10_ADCCLK_cycles_13 => 1,
                        Bits_8_ADCCLK_cycles_11  => 2,
                        Bits_6_ADCCLK_cycles_9   => 3);

   type ADC_CR1 is record
      AWDCH      : AWDCH_Range; -- Analog watchdog channel select bits (read & write)
      EOCIE      : Enabler;     -- Interrupt enable for EOC (read & write)
      AWDIE      : Enabler;     -- Analog watchdog interrupt enable (read & write)
      JEOCIE     : Enabler;     -- Interrupt enable for injected channels (read & write)
      SCAN       : Enabler;     -- Scan mode (read & write)
      AWDSGL     : Enabler;     -- Enable the watchdog on a single channel in scan mode (read & write)
      JAUTO      : Enabler;     -- Automatic injected group conversion (read & write)
      DISCEN     : Enabler;     -- Discontinuous mode on regular channels (read & write)
      JDISCEN    : Enabler;     -- Discontinuous mode on injected channels (read & write)
      DISCNUM    : Bits_3;      -- Discontinuous mode channel count (read & write)
      Reserved_1 : Bits_6;
      JAWDEN     : Enabler;     -- Analog watchdog enable on injected channels (read & write)
      AWDEN      : Enabler;     -- Analog watchdog enable on regular channels (read & write)
      RES        : RES_Options; -- Resolution
      OVRIE      : Enabler;     -- Overrun interrupt enable (read & write)
      Reserved_2 : Bits_5;
   end record with Volatile, Size => Word'Size;

   for ADC_CR1 use record
      AWDCH      at 0 range  0 ..  4; -- Analog watchdog channel select bits (read & write)
      EOCIE      at 0 range  5 ..  5; -- Interrupt enable for EOC (read & write)
      AWDIE      at 0 range  6 ..  6; -- Analog watchdog interrupt enable (read & write)
      JEOCIE     at 0 range  7 ..  7; -- Interrupt enable for injected channels (read & write)
      SCAN       at 0 range  8 ..  8; -- Scan mode (read & write)
      AWDSGL     at 0 range  9 ..  9; -- Enable the watchdog on a single channel in scan mode (read & write)
      JAUTO      at 0 range 10 .. 10; -- Automatic injected group conversion (read & write)
      DISCEN     at 0 range 11 .. 11; -- Discontinuous mode on regular channels (read & write)
      JDISCEN    at 0 range 12 .. 12; -- Discontinuous mode on injected channels (read & write)
      DISCNUM    at 0 range 13 .. 15; -- Discontinuous mode channel count (read & write)
      Reserved_1 at 0 range 16 .. 21;
      JAWDEN     at 0 range 22 .. 22; -- Analog watchdog enable on injected channels (read & write)
      AWDEN      at 0 range 23 .. 23; -- Analog watchdog enable on regular channels (read & write)
      RES        at 0 range 24 .. 25; -- Resolution
      OVRIE      at 0 range 26 .. 26; -- Overrun interrupt enable (read & write)
      Reserved_2 at 0 range 27 .. 31;
   end record;

   ADC_CR1_Reset : constant ADC_CR1 :=
     (AWDCH      => 0,                        -- Analog watchdog channel select bits (read & write)
      EOCIE      => Disable,                  -- Interrupt enable for EOC (read & write)
      AWDIE      => Disable,                  -- Analog watchdog interrupt enable (read & write)
      JEOCIE     => Disable,                  -- Interrupt enable for injected channels (read & write)
      SCAN       => Disable,                  -- Scan mode (read & write)
      AWDSGL     => Disable,                  -- Enable the watchdog on a single channel in scan mode (read & write)
      JAUTO      => Disable,                  -- Automatic injected group conversion (read & write)
      DISCEN     => Disable,                  -- Discontinuous mode on regular channels (read & write)
      JDISCEN    => Disable,                  -- Discontinuous mode on injected channels (read & write)
      DISCNUM    => 0,                        -- Discontinuous mode channel count (read & write)
      Reserved_1 => 0,
      JAWDEN     => Disable,                  -- Analog watchdog enable on injected channels (read & write)
      AWDEN      => Disable,                  -- Analog watchdog enable on regular channels (read & write)
      RES        => Bits_12_ADCCLK_cycles_15, -- Resolution
      OVRIE      => Disable,                  -- Overrun interrupt enable (read & write)
      Reserved_2 => 0);

   -- ADC_CR2 Control 2

   for CONT_Options use (Single => 0, Continuous => 1);

   for ALIGN_Options use (Right => 0, Left => 1);

   for JEXTSEL_Options use
     (Timer_1_CC4_event  =>  0,
      Timer_1_TRGO_event =>  1,
      Timer_2_CC1_event  =>  2,
      Timer_2_TRGO_event =>  3,
      Timer_3_CC2_event  =>  4,
      Timer_3_CC4_event  =>  5,
      Timer_4_CC1_event  =>  6,
      Timer_4_CC2_event  =>  7,
      Timer_4_CC3_event  =>  8,
      Timer_4_TRGO_event =>  9,
      Timer_5_CC4_event  => 10,
      Timer_5_TRGO_event => 11,
      Timer_8_CC2_event  => 12,
      Timer_8_CC3_event  => 13,
      Timer_8_CC4_event  => 14,
      EXTI_line15        => 15);

   for EXTSEL_Options use
     (Timer_1_CC1_event  =>  0,
      Timer_1_CC2_event  =>  1,
      Timer_1_CC3_event  =>  2,
      Timer_2_CC2_event  =>  3,
      Timer_2_CC3_event  =>  4,
      Timer_2_CC4_event  =>  5,
      Timer_2_TRGO_event =>  6,
      Timer_3_CC1_event  =>  7,
      Timer_3_TRGO_event =>  8,
      Timer_4_CC4_event  =>  9,
      Timer_5_CC1_event  => 10,
      Timer_5_CC2_event  => 11,
      Timer_5_CC3_event  => 12,
      Timer_8_CC1_event  => 13,
      Timer_8_TRGO_event => 14,
      EXTI_line11        => 15);

   for EXTEN_Options use (Disabled     => 0,
                          Rising_edge  => 1,
                          Falling_edge => 2,
                          Both_edges   => 3);

   type ADC_CR2 is record
      ADON       : Enabler;         -- A/D Converter ON / OFF (read & write)
      CONT       : CONT_Options;    -- Continuous conversion (read & write)
      Reserved_1 : Bits_6;
      DMA        : Enabler;         -- Direct memory access mode (for single ADC mode) (read & write)
      DDS        : Enabler;         -- DMA disable selection (for single ADC mode) (read & write)
      EOCS       : Enabler;         -- End of conversion selectionat (read & write)
      ALIGN      : ALIGN_Options;   -- Data alignment (read & write)
      Reserved_2 : Bits_4;
      JEXTSEL    : JEXTSEL_Options; -- External event select for injected group (read & write)
      JEXTEN     : EXTEN_Options;   -- External trigger enable for injected channels (read & write)
      JSWSTART   : Enabler;         -- Start conversion of injected channels (read & write)
      Reserved_3 : Bit;
      EXTSEL     : EXTSEL_Options;  -- External event select for regular group (read & write)
      EXTEN      : EXTEN_Options;   -- External trigger enable for regular channels (read & write)
      SWSTART    : Enabler;         -- Start conversion of regular channels (read & write)
      Reserved_4 : Bit;
   end record with Volatile, Size => Word'Size;

   for ADC_CR2 use record
      ADON       at 0 range  0 ..  0; -- A/D Converter ON / OFF (read & write)
      CONT       at 0 range  1 ..  1; -- Continuous conversion (read & write)
      Reserved_1 at 0 range  2 ..  7;
      DMA        at 0 range  8 ..  8; -- Direct memory access mode (for single ADC mode) (read & write)
      DDS        at 0 range  9 ..  9; -- DMA disable selection (for single ADC mode) (read & write)
      EOCS       at 0 range 10 .. 10; -- End of conversion selection (read & write)
      ALIGN      at 0 range 11 .. 11; -- Data alignment (read & write)
      Reserved_2 at 0 range 12 .. 15;
      JEXTSEL    at 0 range 16 .. 19; -- External event select for injected group (read & write)
      JEXTEN     at 0 range 20 .. 21; -- External trigger enable for injected channels (read & write)
      JSWSTART   at 0 range 22 .. 22; -- Start conversion of injected channels (read & write)
      Reserved_3 at 0 range 23 .. 23;
      EXTSEL     at 0 range 24 .. 27; -- External event select for regular group (read & write)
      EXTEN      at 0 range 28 .. 29; -- External trigger enable for regular channels (read & write)
      SWSTART    at 0 range 30 .. 30; -- Start conversion of regular channels (read & write)
      Reserved_4 at 0 range 31 .. 31;
   end record;

   ADC_CR2_Reset : constant ADC_CR2 :=
     (ADON       => Disable,           -- A/D Converter ON / OFF (read & write)
      CONT       => Single,            -- Continuous conversion (read & write)
      Reserved_1 => 0,
      DMA        => Disable,           -- Direct memory access mode (for single ADC mode) (read & write)
      DDS        => Disable,           -- DMA disable selection (for single ADC mode) (read & write)
      EOCS       => Disable,           -- End of conversion selectionat (read & write)
      ALIGN      => Right,             -- Data alignment (read & write)
      Reserved_2 => 0,
      JEXTSEL    => Timer_1_CC4_event, -- External event select for injected group (read & write)
      JEXTEN     => Disabled,          -- External trigger enable for injected channels (read & write)
      JSWSTART   => Disable,           -- Start conversion of injected channels (read & write)
      Reserved_3 => 0,
      EXTSEL     => Timer_1_CC1_event, -- External event select for regular group (read & write)
      EXTEN      => Disabled,          -- External trigger enable for regular channels (read & write)
      SWSTART    => Disable,           -- Start conversion of regular channels (read & write)
      Reserved_4 => 0);

   -- ADC_SMPR1 Sample time 1

   for SMP_Options use
     (Cycles_3   => 0,
      Cycles_15  => 1,
      Cycles_28  => 2,
      Cycles_56  => 3,
      Cycles_84  => 4,
      Cycles_112 => 5,
      Cycles_144 => 6,
      Cycles_480 => 7);

   type ADC_SMPR1 is array (AWDCH_Range range 10 .. 18) of SMP_Options with Pack, Size => 27;

   -- ADC_SMPR2 Sample time 2

   type ADC_SMPR2 is array (AWDCH_Range range 0 .. 9) of SMP_Options with Pack, Size => 30;

   -- ADC_JOFRx Injected channel data offset

   type ADC_JOFRx is record
      JOFFSETx : Bits_12; -- Data offset for injected channel x (read & write)
      Reserved : Bits_20;
   end record with Volatile, Size => Word'Size;

   for ADC_JOFRx use record
      JOFFSETx at 0 range  0 .. 11; -- Data offset for injected channel x (read & write)
      Reserved at 0 range 12 .. 31;
   end record;

   type ADC_JOFR is array (1 .. 4) of ADC_JOFRx with Pack, Size => 4 * Word'Size;

   -- ADC_SQR1 Regular sequence 1

   type SQ1 is array (Regular_Channel_Rank range 13 .. 16) of AWDCH_Range with Component_Size => 5, Size => 20;

   type ADC_SQR1 is record
      SQ       : SQ1;    -- xth conversion in regular sequence (read & write)
      L        : Bits_4; -- Regular channel sequence length - 1 (read & write)
      Reserved : Bits_8;
   end record with Volatile, Size => Word'Size;

   for ADC_SQR1 use record
      SQ       at 0 range  0 .. 19; -- xth conversion in regular sequence (read & write)
      L        at 0 range 20 .. 23; -- Regular channel sequence length - 1 (read & write)
      Reserved at 0 range 24 .. 31;
   end record;

   -- ADC_SQR2 Regular sequence 2

   type SQ2 is array (Regular_Channel_Rank range 7 .. 12) of AWDCH_Range with Component_Size => 5, Size => 30;

   type ADC_SQR2 is record
      SQ       : SQ2;    -- xth conversion in regular sequence (read & write)
      Reserved : Bits_2;
   end record with Volatile, Size => Word'Size;

   for ADC_SQR2 use record
      SQ       at 0 range  0 .. 29; -- xth conversion in regular sequence (read & write)
      Reserved at 0 range 30 .. 31;
   end record;

   -- ADC_SQR3 Regular sequence 3

   type SQ3 is array (Regular_Channel_Rank range 1 .. 6) of AWDCH_Range with Component_Size => 5, Size => 30;

   type ADC_SQR3 is record
      SQ       : SQ3;    -- xth conversion in regular sequence (read & write)
      Reserved : Bits_2;
   end record with Volatile, Size => Word'Size;

   for ADC_SQR3 use record
      SQ       at 0 range  0 .. 29; -- xth conversion in regular sequence (read & write)
      Reserved at 0 range 30 .. 31;
   end record;

   -- ADC_JSQR Injected sequence register

   type JSQ1 is array (1 .. 4) of AWDCH_Range with Pack, Size => 20;

   type ADC_JSQR is record
      JSQ      : JSQ1;    -- xth conversion in regular sequence (read & write)
      L        : Bits_2; -- Regular channel sequence length - 1 (read & write)
      Reserved : Bits_10;
   end record with Volatile, Size => Word'Size;

   for ADC_JSQR use record
      JSQ      at 0 range  0 .. 19; -- xth conversion in regular sequence (read & write)
      L        at 0 range 20 .. 21; -- Regular channel sequence length - 1 (read & write)
      Reserved at 0 range 22 .. 31;
   end record;

   -- ADC_JDRx Injected data

   type ADC_JDRx is record
      JDATAx   : Bits_16; -- Injected data (read only)
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for ADC_JDRx use record
      JDATAx   at 0 range  0 .. 15; -- Injected data (read only)
      Reserved at 0 range 16 .. 31;
   end record;

   type ADC_JDR is array (1 .. 4) of ADC_JDRx with Pack, Size => 4 * Word'Size;

   -- ADC Register

   type ADC_Pad_Array is array (1 .. 173) of Bits_8 with Pack;

   type ADC_Register is record
      SR         : ADC_SR;    -- Status
      CR1        : ADC_CR1;   -- Control 1
      CR2        : ADC_CR2;   -- Control 2
      SMPR1      : ADC_SMPR1; -- Sample time 1
      Reserved_1 : Bits_5;
      SMPR2      : ADC_SMPR2; -- Sample time 2
      Reserved_2 : Bits_2;
      JOFR       : ADC_JOFR;  -- Data offsets for injected channels
      HTR        : Bits_12;   -- Watchdog higher threshold
      Reserved_3 : Bits_20;
      LTR        : Bits_12;   -- Watchdog lower threshold
      Reserved_4 : Bits_20;
      SQR1       : ADC_SQR1;  -- Regular sequence 1
      SQR2       : ADC_SQR2;  -- Regular sequence 2
      SQR3       : ADC_SQR3;  -- Regular sequence 3
      JSQR       : ADC_JSQR;  -- Injected sequence register
      JDR        : ADC_JDR;   -- Injected data
      DR         : Bits_16;   -- Regular data (read only)
      Reserved_5 : Bits_16;
      Reserved_6 : ADC_Pad_Array;
   end record with Volatile, Size => 16#100# * Byte'Size;

   for ADC_Register use record
      SR         at 16#00# range  0 ..   31; -- Status
      CR1        at 16#04# range  0 ..   31; -- Control 1
      CR2        at 16#08# range  0 ..   31; -- Control 2
      SMPR1      at 16#0C# range  0 ..   26; -- Sample time 1
      Reserved_1 at 16#0C# range 27 ..   31;
      SMPR2      at 16#10# range  0 ..   29; -- Sample time 1
      Reserved_2 at 16#10# range 30 ..   31;
      JOFR       at 16#14# range  0 ..  127; -- Data offsets for injected channels
      HTR        at 16#24# range  0 ..   11; -- Watchdog higher threshold
      Reserved_3 at 16#24# range 12 ..   31;
      LTR        at 16#28# range  0 ..   11; -- Watchdog lower threshold
      Reserved_4 at 16#28# range 12 ..   31;
      SQR1       at 16#2C# range  0 ..   31; -- Regular sequence 1
      SQR2       at 16#30# range  0 ..   31; -- Regular sequence 2
      SQR3       at 16#34# range  0 ..   31; -- Regular sequence 3
      JSQR       at 16#38# range  0 ..   31; -- Injected sequence register
      JDR        at 16#3C# range  0 ..  127; -- Injected data
      DR         at 16#4C# range  0 ..   15; -- Regular data (read only)
      Reserved_5 at 16#4C# range 16 ..   31;
      Reserved_6 at 16#50# range  0 .. 1383;
   end record;

   -- ADCs array

   type ADCs is array (ADC_No) of ADC_Register with Pack, Size => 3 * 16#100# * Byte'Size;

   -- ADC_CSR Common status

   type ADC_CSR_Module is record
      AWD      : Occurance; -- Analog watchdog flag (read only)
      EOC      : Completed; -- Regular channel end of conversion (read only)
      JEOC     : Completed; -- Injected channel end of conversion (read only)
      JSTRT    : Starter;   -- Injected channel start flag (read only)
      STRT     : Starter;   -- Regular channel start flag (read only)
      OVR      : Occurance; -- Overrun (read only)
      Reserved : Bits_2;
   end record with Volatile, Size => Byte'Size;

   for ADC_CSR_Module use record
      AWD      at 0 range  0 ..  0; -- Analog watchdog flag (read only)
      EOC      at 0 range  1 ..  1; -- Regular channel end of conversion (read only)
      JEOC     at 0 range  2 ..  2; -- Injected channel end of conversion (read only)
      JSTRT    at 0 range  3 ..  3; -- Injected channel start flag (read only)
      STRT     at 0 range  4 ..  4; -- Regular channel start flag (read only)
      OVR      at 0 range  5 ..  5; -- Overrun (read only)
      Reserved at 0 range  6 ..  7;
   end record;

   type ADC_CSR_Modules is array (ADC_No) of ADC_CSR_Module;

   type ADC_CSR is record
      ADC      : ADC_CSR_Modules;
      Reserved : Bits_8;
   end record with Volatile, Size => Word'Size;

   for ADC_CSR use record
      ADC      at 0 range  0 .. 23;
      Reserved at 0 range 24 .. 31;
   end record;

   -- ADC_CCR Common control

   for MULTI_Options use
     (Independent                                       => 2#00000#,
      Dual_regular_simultaneous_injected_simultaneous   => 2#00001#,
      Dual_regular_simultaneous_alternate_trigger       => 2#00010#,
      Dual_injected_simultaneous_only                   => 2#00101#,
      Dual_regular_simultaneous_only                    => 2#00110#,
      Dual_interleaved_only                             => 2#00111#,
      Dual_alternate_trigger_only                       => 2#01001#,
      Triple_regular_simultaneous_injected_simultaneous => 2#10001#,
      Triple_regular_simultaneous_alternate_trigger     => 2#10010#,
      Triple_injected_simultaneous_only                 => 2#10101#,
      Triple_regular_simultaneous_only                  => 2#10110#,
      Triple_interleaved_only                           => 2#10111#,
      Triple_alternate_trigger_only                     => 2#11001#);

   for DMA_Options use
     (Disabled => 0,
      Mode_1   => 1,
      Mode_2   => 2,
      Mode_3   => 3);

   for ADCPRE_Options use
     (Divided_by_2 => 0,
      Divided_by_4 => 1,
      Divided_by_6 => 2,
      Divided_by_8 => 3);

   type ADC_CCR is record
      MULTI      : MULTI_Options;  -- Multi ADC mode selection (read & write)
      Reserved_1 : Bits_3;
      DELAYADC   : Bits_4;         -- Delay between 2 sampling phases - 5 (read & write)
      Reserved_2 : Bit;
      DDS        : Disabler;       -- DMA disable selection (for multi-ADC mode) (read & write)
      DMA        : DMA_Options;    -- Direct memory access mode for multi ADC mode (read & write)
      ADCPRE     : ADCPRE_Options; -- ADC prescaler (read & write)
      Reserved_3 : Bits_4;
      VBATE      : Enabler;        -- VBAT enable (read & write)
      TSVREFE    : Enabler;        -- Temperature sensor and VREFINT enable (read & write)
      Reserved_4 : Bits_8;
   end record with Volatile, Size => Word'Size;

   for ADC_CCR use record
      MULTI      at 0 range  0 ..  4; -- Multi ADC mode selection (read & write)
      Reserved_1 at 0 range  5 ..  7;
      DELAYADC   at 0 range  8 .. 11; -- Delay between 2 sampling phases - 5 (read & write)
      Reserved_2 at 0 range 12 .. 12;
      DDS        at 0 range 13 .. 13; -- DMA disable selection (for multi-ADC mode) (read & write)
      DMA        at 0 range 14 .. 15; -- Direct memory access mode for multi ADC mode (read & write)
      ADCPRE     at 0 range 16 .. 17; -- ADC prescaler (read & write)
      Reserved_3 at 0 range 18 .. 21;
      VBATE      at 0 range 22 .. 22; -- VBAT enable (read & write)
      TSVREFE    at 0 range 23 .. 23; -- Temperature sensor and VREFINT enable (read & write)
      Reserved_4 at 0 range 24 .. 31;
   end record;

   -- ADC_CDR Common regular data for dual and triple modes

   type ADC_CDR is record
      DATA1 : Bits_16; -- 1st data item of a pair of regular conversions (read only)
      DATA2 : Bits_16; -- 2nd data item of a pair of regular conversions (read only)
   end record with Volatile, Size => Word'Size;

   for ADC_CDR use record
      DATA1 at 0 range  0 .. 15; -- 1st data item of a pair of regular conversions (read only)
      DATA2 at 0 range 16 .. 31; -- 2nd data item of a pair of regular conversions (read only)
   end record;

   -- ADC123 Register

   type ADC123_Register is record
      ADC : ADCs;    -- ADCs
      CSR : ADC_CSR; -- Common status
      CCR : ADC_CCR; -- Common control
      CDR : ADC_CDR; -- Common regular data for dual and triple modes
   end record with Volatile, Size => (3 * 16#100# + 16#0C#) * Byte'Size;

   for ADC123_Register use record
      ADC at 16#000# range  0 .. 6143;
      CSR at 16#300# range  0 ..   31;
      CCR at 16#304# range  0 ..   31;
      CDR at 16#308# range  0 ..   31;
   end record;

   -- Set register addresse

   ADC123 : ADC123_Register with
     Volatile, Address => System'To_Address (Base_ADC123), Import;

end STM32F4.ADC;

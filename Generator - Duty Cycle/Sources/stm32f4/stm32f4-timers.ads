--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx Timers

package STM32F4.Timers is

   -- General timer definitions

   type Timer_No is range 1 .. 14;

   -- TIM_1_8_TIMx_CR1

   type UDIS_Options is (UEV_enabled,
                         UEV_disabled)     with Size => 1;

   type CMS_Options is (Edge_aligned_mode,
                        Center_aligned_mode_1,
                        Center_aligned_mode_2,
                        Center_aligned_mode_3) with Size => 2;

   type CKD_Options is (tCK_Int,
                        tCK_Int_2,
                        tCK_Int_4) with Size => 2;

   -- TIM_1_8_TIMx_CR2

   type MMS_Options is (Reset,
                        Enable,
                        Update,
                        Compare_Pulse,
                        Compare_OC1REF,
                        Compare_OC2REF,
                        Compare_OC3REF,
                        Compare_OC4REF) with Size => 3;

   -- TIM_1_8_TIMx_SMCR

   type SMS_Options is (Disabled,
                        Encoder_mode_1,
                        Encoder_mode_2,
                        Encoder_mode_3,
                        Reset_Mode,
                        Gated_Mode,
                        Trigger_Mode,
                        External_Clock_Mode_1) with Size => 3;

   type TS_Options is (Internal_Trigger_0,
                       Internal_Trigger_1,
                       Internal_Trigger_2,
                       Internal_Trigger_3,
                       TI1_Edge_Detector,
                       Filtered_Timer_Input_1,
                       Filtered_Timer_Input_2,
                       External_Trigger_input) with Size => 3;

   type ETF_Options is (fCK_INT_N2,
                        fCK_INT_N4,
                        fCK_INT_N8,
                        fDTS_by_2_N6,
                        fDTS_by_2_N8,
                        fDTS_by_4_N6,
                        fDTS_by_4_N8,
                        fDTS_by_8_N6,
                        fDTS_by_8_N8,
                        fDTS_by_16_N5,
                        fDTS_by_16_N6,
                        fDTS_by_16_N8,
                        fDTS_by_32_N5,
                        fDTS_by_32_N6,
                        fDTS_by_32_N8) with Size => 4;

   type ETPS_Options is (Off,
                         divided_by_2,
                         divided_by_4,
                         divided_by_8) with Size => 2;

   -- TIM_1_8_TIMx_CCMR1

   type CC1S_Options is (Output,
                         Input_TI1,
                         Input_TI2,
                         Input_TRC) with Size => 2;

   type IC1PSC_Options is (No_Prescaler,
                           every_2,
                           every_4,
                           every_8) with Size => 2;

   type OCM_Options is (Frozen,
                        Chan_1_Active,
                        Chan_1_Inactive,
                        Toggle,
                        Force_Inactive,
                        Force_Active,
                        PWM_Mode_1,
                        PWM_Mode_2) with Size => 3;

   -- TIM_1_8_TIMx_BDTR

   type LOCK_Options is (Off,
                         Level_1,
                         Level_2,
                         Level_3) with Size => 2;

private

   --------------------
   -- Timers 1 and 8 --
   --------------------

   -- TIM_1_8_TIMx_CR1

   for UDIS_Options use (UEV_enabled  => 0, UEV_disabled => 1);

   for CMS_Options use (Edge_aligned_mode     => 0,
                        Center_aligned_mode_1 => 1,
                        Center_aligned_mode_2 => 2,
                        Center_aligned_mode_3 => 3);

   for CKD_Options use (tCK_Int   => 0,
                        tCK_Int_2 => 1,
                        tCK_Int_4 => 2);

   type TIM_1_8_TIMx_CR1 is record
      CEN      : Enabler;      -- Counter enable (read & write)
      UDIS     : UDIS_Options; -- Update disable (read & write)
      URS      : Bit;          -- Update request source (read & write)
      OPM      : Bit;          -- One pulse mode (read & write)
      DIR      : Direction;    -- Direction (read & write)
      CMS      : CMS_Options;  -- Center-aligned mode selection (read & write)
      ARPE     : Buffering;    -- Auto-reload preload enable (read & write)
      CKD      : CKD_Options;  -- Clock division (read & write)
      Reserved : Bits_22;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CR1 use record
      CEN      at 0 range  0 ..  0; -- (read & write)
      UDIS     at 0 range  1 ..  1; -- (read & write)
      URS      at 0 range  2 ..  2; -- (read & write)
      OPM      at 0 range  3 ..  3; -- (read & write)
      DIR      at 0 range  4 ..  4; -- (read & write)
      CMS      at 0 range  5 ..  6; -- (read & write)
      ARPE     at 0 range  7 ..  7; -- (read & write)
      CKD      at 0 range  8 ..  9; -- (read & write)
      Reserved at 0 range 10 .. 31;
   end record;

   -- TIM_1_8_TIMx_CR2

   for MMS_Options use (Reset          => 0,
                        Enable         => 1,
                        Update         => 2,
                        Compare_Pulse  => 3,
                        Compare_OC1REF => 4,
                        Compare_OC2REF => 5,
                        Compare_OC3REF => 6,
                        Compare_OC4REF => 7);

   type TIM_1_8_TIMx_CR2 is record
      CCPC       : Bit;         -- Capture/compare preloaded control (read & write)
      Reserved_1 : Bit;
      CCUS       : Bit;         -- Capture/compare control update selection (read & write)
      CCDS       : Bit;         -- Capture/compare DMA selection (read & write)
      MMS        : MMS_Options; -- Master mode selection (read & write)
      TI1S       : Bit;         -- TI1 selection (read & write)
      OIS1       : Bit;         -- Output Idle state 1 (OC1 output) (read & write)
      OIS1N      : Bit;         -- Output Idle state 1 (OC1N output) (read & write)
      OIS2       : Bit;         -- Output Idle state 2 (OC2 output) (read & write)
      OIS2N      : Bit;         -- Output Idle state 2 (OC2N output) (read & write)
      OIS3       : Bit;         -- Output Idle state 3 (OC3 output) (read & write)
      OIS3N      : Bit;         -- Output Idle state 4 (OC3N output) (read & write)
      OIS4       : Bit;         -- Output Idle state 1 (OC4 output) (read & write)
      Reserved_2 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CR2 use record
      CCPC       at 0 range  0 ..  0; -- (read & write)
      Reserved_1 at 0 range  1 ..  1;
      CCUS       at 0 range  2 ..  2; -- (read & write)
      CCDS       at 0 range  3 ..  3; -- (read & write)
      MMS        at 0 range  4 ..  6; -- (read & write)
      TI1S       at 0 range  7 ..  7; -- (read & write)
      OIS1       at 0 range  8 ..  8; -- (read & write)
      OIS1N      at 0 range  9 ..  9; -- (read & write)
      OIS2       at 0 range 10 .. 10; -- (read & write)
      OIS2N      at 0 range 11 .. 11; -- (read & write)
      OIS3       at 0 range 12 .. 12; -- (read & write)
      OIS3N      at 0 range 13 .. 13; -- (read & write)
      OIS4       at 0 range 14 .. 14; -- (read & write)
      Reserved_2 at 0 range 15 .. 31;
   end record;

   -- TIM_1_8_TIMx_SMCR

   for SMS_Options use (Disabled              => 0,
                        Encoder_mode_1        => 1,
                        Encoder_mode_2        => 2,
                        Encoder_mode_3        => 3,
                        Reset_Mode            => 4,
                        Gated_Mode            => 5,
                        Trigger_Mode          => 6,
                        External_Clock_Mode_1 => 7);

   for TS_Options use (Internal_Trigger_0     => 0,
                       Internal_Trigger_1     => 1,
                       Internal_Trigger_2     => 2,
                       Internal_Trigger_3     => 3,
                       TI1_Edge_Detector      => 4,
                       Filtered_Timer_Input_1 => 5,
                       Filtered_Timer_Input_2 => 6,
                       External_Trigger_input => 7);

   for ETF_Options use (fCK_INT_N2    =>  0,
                        fCK_INT_N4    =>  2,
                        fCK_INT_N8    =>  3,
                        fDTS_by_2_N6  =>  4,
                        fDTS_by_2_N8  =>  5,
                        fDTS_by_4_N6  =>  6,
                        fDTS_by_4_N8  =>  7,
                        fDTS_by_8_N6  =>  8,
                        fDTS_by_8_N8  =>  9,
                        fDTS_by_16_N5 => 10,
                        fDTS_by_16_N6 => 11,
                        fDTS_by_16_N8 => 12,
                        fDTS_by_32_N5 => 13,
                        fDTS_by_32_N6 => 14,
                        fDTS_by_32_N8 => 15);

   for ETPS_Options use (Off => 0,
                         divided_by_2 => 1,
                         divided_by_4 => 2,
                         divided_by_8 => 3);

   type TIM_1_8_TIMx_SMCR is record
      SMS        : SMS_Options;  -- Slave mode selection (read & write)
      Reserved_1 : Bit;
      TS         : TS_Options;   -- Trigger selection (read & write)
      MSM        : Bit;          -- Master/slave mode (read & write)
      ETF        : ETF_Options;  -- External trigger filter (read & write)
      ETPS       : ETPS_Options; -- External trigger prescaler (read & write)
      ECE        : Enabler;      -- External clock enable (read & write)
      ETP        : Inverter;     -- External trigger polarity (read & write)
      Reserved_2 : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_SMCR use record
      SMS        at 0 range  0 ..  2; -- Slave mode selection (read & write)
      Reserved_1 at 0 range  3 ..  3;
      TS         at 0 range  4 ..  6; -- Trigger selection (read & write)
      MSM        at 0 range  7 ..  7; -- Master/slave mode (read & write)
      ETF        at 0 range  8 .. 11; -- External trigger filter (read & write)
      ETPS       at 0 range 12 .. 13; -- External trigger prescaler (read & write)
      ECE        at 0 range 14 .. 14; -- External clock enable (read & write)
      ETP        at 0 range 15 .. 15; -- External trigger polarity (read & write)
      Reserved_2 at 0 range 16 .. 31;
   end record;

   -- TIM_1_8_TIMx_DIER

   type TIM_1_8_TIMx_DIER is record
      UIE      : Enabler; -- Update interrupt enable (read & write)
      CC1IE    : Enabler; -- Capture/Compare 1 interrupt enable (read & write)
      CC2IE    : Enabler; -- Capture/Compare 2 interrupt enable (read & write)
      CC3IE    : Enabler; -- Capture/Compare 3 interrupt enable (read & write)
      CC4IE    : Enabler; -- Capture/Compare 4 interrupt enable (read & write)
      COMIE    : Enabler; -- COM interrupt enable (read & write)
      TIE      : Enabler; -- Trigger interrupt enable (read & write)
      BIE      : Enabler; -- Break interrupt enable (read & write)
      UDE      : Enabler; -- Update DMA request enable (read & write)
      CC1DE    : Enabler; -- Capture/Compare 1 DMA request enable (read & write)
      CC2DE    : Enabler; -- Capture/Compare 2 DMA request enable (read & write)
      CC3DE    : Enabler; -- Capture/Compare 3 DMA request enable (read & write)
      CC4DE    : Enabler; -- Capture/Compare 4 DMA request enable (read & write)
      COMDE    : Enabler; -- COM DMA request enable (read & write)
      TDE      : Enabler; -- Trigger DMA request enable (read & write)
      Reserved : Bits_17;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_DIER use record
      UIE      at 0 range  0 ..  0; -- Update interrupt enable (read & write)
      CC1IE    at 0 range  1 ..  1; -- Capture/Compare 1 interrupt enable (read & write)
      CC2IE    at 0 range  2 ..  2; -- Capture/Compare 2 interrupt enable (read & write)
      CC3IE    at 0 range  3 ..  3; -- Capture/Compare 3 interrupt enable (read & write)
      CC4IE    at 0 range  4 ..  4; -- Capture/Compare 4 interrupt enable (read & write)
      COMIE    at 0 range  5 ..  5; -- COM interrupt enable (read & write)
      TIE      at 0 range  6 ..  6; -- Trigger interrupt enable (read & write)
      BIE      at 0 range  7 ..  7; -- Break interrupt enable (read & write)
      UDE      at 0 range  8 ..  8; -- Update DMA request enable (read & write)
      CC1DE    at 0 range  9 ..  9; -- Capture/Compare 1 DMA request enable (read & write)
      CC2DE    at 0 range 10 .. 10; -- Capture/Compare 2 DMA request enable (read & write)
      CC3DE    at 0 range 11 .. 11; -- Capture/Compare 3 DMA request enable (read & write)
      CC4DE    at 0 range 12 .. 12; -- Capture/Compare 4 DMA request enable (read & write)
      COMDE    at 0 range 13 .. 13; -- COM DMA request enable (read & write)
      TDE      at 0 range 14 .. 14; -- Trigger DMA request enable (read & write)
      Reserved at 0 range 15 .. 31;
   end record;

   -- TIM_1_8_TIMx_SR

   type TIM_1_8_TIMx_SR is record
      UIF        : Occurance; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      : Occurance; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      : Occurance; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      CC3IF      : Occurance; -- Capture/Compare 3 interrupt flag (read & clear on write 'Not_Occured')
      CC4IF      : Occurance; -- Capture/Compare 4 interrupt flag (read & clear on write 'Not_Occured')
      COMIF      : Occurance; -- COM interrupt flag (read & clear on write 'Not_Occured')
      TIF        : Occurance; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      BIF        : Occurance; -- Break interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 : Bit;
      CC10F      : Occurance; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      : Occurance; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      CC30F      : Occurance; -- Capture/Compare 3 overcapture flag (read & clear on write 'Not_Occured')
      CC40F      : Occurance; -- Capture/Compare 4 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_2 : Bits_19;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_SR use record
      UIF        at 0 range  0 ..  0; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      at 0 range  1 ..  1; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      at 0 range  2 ..  2; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      CC3IF      at 0 range  3 ..  3; -- Capture/Compare 3 interrupt flag (read & clear on write 'Not_Occured')
      CC4IF      at 0 range  4 ..  4; -- Capture/Compare 4 interrupt flag (read & clear on write 'Not_Occured')
      COMIF      at 0 range  5 ..  5; -- COM interrupt flag (read & clear on write 'Not_Occured')
      TIF        at 0 range  6 ..  6; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      BIF        at 0 range  7 ..  7; -- Break interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 at 0 range  8 ..  8;
      CC10F      at 0 range  9 ..  9; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      at 0 range 10 .. 10; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      CC30F      at 0 range 11 .. 11; -- Capture/Compare 3 overcapture flag (read & clear on write 'Not_Occured')
      CC40F      at 0 range 12 .. 12; -- Capture/Compare 4 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_2 at 0 range 13 .. 31;
   end record;

   -- TIM_1_8_TIMx_EGR

   type TIM_1_8_TIMx_EGR is record
      UG       : Generator; -- Update generation (write only)
      CC1G     : Generator; -- Capture/Compare 1 generation (write only)
      CC2G     : Generator; -- Capture/Compare 2 generation (write only)
      CC3G     : Generator; -- Capture/Compare 3 generation (write only)
      CC4G     : Generator; -- Capture/Compare 4 generation (write only)
      COMG     : Generator; -- Capture/Compare control update generation (write only)
      TG       : Generator; -- Trigger generation (write only)
      BG       : Generator; -- Break generation (write only)
      Reserved : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_EGR use record
      UG       at 0 range  0 ..  0; -- Update generation (write only)
      CC1G     at 0 range  1 ..  1; -- Capture/Compare 1 generation (write only)
      CC2G     at 0 range  2 ..  2; -- Capture/Compare 2 generation (write only)
      CC3G     at 0 range  3 ..  3; -- Capture/Compare 3 generation (write only)
      CC4G     at 0 range  4 ..  4; -- Capture/Compare 4 generation (write only)
      COMG     at 0 range  5 ..  5; -- Capture/Compare control update generation (write only)
      TG       at 0 range  6 ..  6; -- Trigger generation (write only)
      BG       at 0 range  7 ..  7; -- Break generation (write only)
      Reserved at 0 range  8 .. 31;
   end record;

   -- TIM_1_8_TIMx_CCMR1

   for CC1S_Options use (Output    => 0,
                         Input_TI1 => 1,
                         Input_TI2 => 2,
                         Input_TRC => 3);

   for IC1PSC_Options use (No_Prescaler => 0,
                           every_2      => 1,
                           every_4      => 2,
                           every_8      => 3);

   for OCM_Options use (Frozen          => 0,
                        Chan_1_Active   => 1,
                        Chan_1_Inactive => 2,
                        Toggle          => 3,
                        Force_Inactive  => 4,
                        Force_Active    => 5,
                        PWM_Mode_1      => 6,
                        PWM_Mode_2      => 7);

   type Capture_Compare is (Capture, Compare);

   type TIM_1_8_TIMx_CCMR1_Capture is record
      CC1S_Ca     : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      IC1PSC      : IC1PSC_Options;  -- Input capture 1 prescaler (read & write)
      IC1F        : ETF_Options;     -- Input capture 1 filter (read & write)
      CC2S_Ca     : CC1S_Options;    -- Capture/Compare 2 Selection (read & write)
      IC2PSC      : IC1PSC_Options;  -- Input capture 2 prescaler (read & write)
      IC2F        : ETF_Options;     -- Input capture 2 filter (read & write)
      Reserved_Ca : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CCMR1_Capture use record
      CC1S_Ca     at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      IC1PSC      at 0 range  2 ..  3; -- Input capture 1 prescaler (read & write)
      IC1F        at 0 range  4 ..  7; -- Input capture 1 filter (read & write)
      CC2S_Ca     at 0 range  8 ..  9; -- Capture/Compare 2 Selection (read & write)
      IC2PSC      at 0 range 10 .. 11; -- Input capture 2 prescaler (read & write)
      IC2F        at 0 range 12 .. 15; -- Input capture 2 filter (read & write)
      Reserved_Ca at 0 range 16 .. 31;
   end record;

   -- TIM_1_8_TIMx_CCMR1_Compare

   type TIM_1_8_TIMx_CCMR1_Compare is record
      CC1S_Co     : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      OC1FE       : Enabler;         -- Output Compare 1 fast enable (read & write)
      OC1PE       : Enabler;         -- Output Compare 1 preload enable (read & write)
      OC1M        : OCM_Options;     -- Output Compare 1 mode (read & write)
      OC1CE       : Enabler;         -- Output Compare 1 mode (read & write)
      CC2S_Co     : CC1S_Options;    -- Capture/Compare 2 Selection (read & write)
      OC2FE       : Enabler;         -- Output Compare 2 fast enable (read & write)
      OC2PE       : Enabler;         -- Output Compare 2 preload enable (read & write)
      OC2M        : OCM_Options;     -- Output Compare 2 mode (read & write)
      OC2CE       : Enabler;         -- Output Compare 2 mode (read & write)
      Reserved_Co : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CCMR1_Compare use record
      CC1S_Co     at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      OC1FE       at 0 range  2 ..  2; -- Output Compare 1 fast enable (read & write)
      OC1PE       at 0 range  3 ..  3; -- Output Compare 1 preload enable (read & write)
      OC1M        at 0 range  4 ..  6; -- Output Compare 1 mode (read & write)
      OC1CE       at 0 range  7 ..  7; -- Output Compare 1 mode (read & write)
      CC2S_Co     at 0 range  8 ..  9; -- Capture/Compare 2 Selection (read & write)
      OC2FE       at 0 range 10 .. 10; -- Output Compare 2 fast enable (read & write)
      OC2PE       at 0 range 11 .. 11; -- Output Compare 2 preload enable (read & write)
      OC2M        at 0 range 12 .. 14; -- Output Compare 2 mode (read & write)
      OC2CE       at 0 range 15 .. 15; -- Output Compare 2 mode (read & write)
      Reserved_Co at 0 range 16 .. 31;
   end record;

   type TIM_1_8_TIMx_CCMR1 (State : Capture_Compare := Capture) is record
      case State is
         when Capture => CAP : TIM_1_8_TIMx_CCMR1_Capture;
         when Compare => CPR : TIM_1_8_TIMx_CCMR1_Compare;
      end case;
   end record with Volatile, Unchecked_Union, Size => Word'Size;

   -- TIM_1_8_TIMx_CCMR2

   type TIM_1_8_TIMx_CCMR2_Capture is record
      CC3S        : CC1S_Options;    -- Capture/Compare 3 Selection (read & write)
      IC3PSC      : IC1PSC_Options;  -- Input capture 3 prescaler (read & write)
      IC3F        : ETF_Options;     -- Input capture 3 filter (read & write)
      CC4S        : CC1S_Options;    -- Capture/Compare 4 Selection (read & write)
      IC4PSC      : IC1PSC_Options;  -- Input capture 4 prescaler (read & write)
      IC4F        : ETF_Options;     -- Input capture 4 filter (read & write)
      Reserved    : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CCMR2_Capture use record
      CC3S        at 0 range  0 ..  1; -- Capture/Compare 3 Selection (read & write)
      IC3PSC      at 0 range  2 ..  3; -- Input capture 3 prescaler (read & write)
      IC3F        at 0 range  4 ..  7; -- Input capture 3 filter (read & write)
      CC4S        at 0 range  8 ..  9; -- Capture/Compare 4 Selection (read & write)
      IC4PSC      at 0 range 10 .. 11; -- Input capture 4 prescaler (read & write)
      IC4F        at 0 range 12 .. 15; -- Input capture 4 filter (read & write)
      Reserved    at 0 range 16 .. 31;
   end record;

   type TIM_1_8_TIMx_CCMR2_Compare is record
      CC3S        : CC1S_Options;    -- Capture/Compare 3 Selection (read & write)
      OC3FE       : Enabler;         -- Output Compare 3 fast enable (read & write)
      OC3PE       : Enabler;         -- Output Compare 3 preload enable (read & write)
      OC3M        : OCM_Options;     -- Output Compare 3 mode (read & write)
      OC3CE       : Enabler;         -- Output Compare 3 mode (read & write)
      CC4S        : CC1S_Options;    -- Capture/Compare 4 Selection (read & write)
      OC4FE       : Enabler;         -- Output Compare 4 fast enable (read & write)
      OC4PE       : Enabler;         -- Output Compare 4 preload enable (read & write)
      OC4M        : OCM_Options;     -- Output Compare 4 mode (read & write)
      OC4CE       : Enabler;         -- Output Compare 4 mode (read & write)
      Reserved    : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CCMR2_Compare use record
      CC3S        at 0 range  0 ..  1; -- Capture/Compare 3 Selection (read & write)
      OC3FE       at 0 range  2 ..  2; -- Output Compare 3 fast enable (read & write)
      OC3PE       at 0 range  3 ..  3; -- Output Compare 3 preload enable (read & write)
      OC3M        at 0 range  4 ..  6; -- Output Compare 3 mode (read & write)
      OC3CE       at 0 range  7 ..  7; -- Output Compare 3 mode (read & write)
      CC4S        at 0 range  8 ..  9; -- Capture/Compare 4 Selection (read & write)
      OC4FE       at 0 range 10 .. 10; -- Output Compare 4 fast enable (read & write)
      OC4PE       at 0 range 11 .. 11; -- Output Compare 4 preload enable (read & write)
      OC4M        at 0 range 12 .. 14; -- Output Compare 4 mode (read & write)
      OC4CE       at 0 range 15 .. 15; -- Output Compare 4 mode (read & write)
      Reserved    at 0 range 16 .. 31;
   end record;

   type TIM_1_8_TIMx_CCMR2 (State : Capture_Compare := Capture) is record
      case State is
         when Capture => CAP : TIM_1_8_TIMx_CCMR2_Capture;
         when Compare => CPR : TIM_1_8_TIMx_CCMR2_Compare;
      end case;
   end record with Volatile, Unchecked_Union, Size => Word'Size;

   -- TIM_1_8_TIMx_CCER

   type TIM_1_8_TIMx_CCER is record
      CC1E     : Enabler; -- Capture/Compare 1 output enable (read & write)
      CC1P     : Bit;     -- Capture/Compare 1 output polarity (read & write)
      CC1NE    : Enabler; -- Capture/Compare 1 complementary output enable (read & write)
      CC1NP    : Bit;     -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E     : Enabler; -- Capture/Compare 2 output enable (read & write)
      CC2P     : Bit;     -- Capture/Compare 2 output polarity (read & write)
      CC2NE    : Enabler; -- Capture/Compare 2 complementary output enable (read & write)
      CC2NP    : Bit;     -- Capture/Compare 2 complementary output polarity (read & write)
      CC3E     : Enabler; -- Capture/Compare 3 output enable (read & write)
      CC3P     : Bit;     -- Capture/Compare 3 output polarity(read & write)
      CC3NE    : Enabler; -- Capture/Compare 3 complementary output enable (read & write)
      CC3NP    : Bit;     -- Capture/Compare 3 complementary output polarity (read & write)
      CC4E     : Enabler; -- Capture/Compare 4 output enable (read & write)
      CC4P     : Bit;     -- Capture/Compare 4 output polarity (read & write)
      Reserved : Bits_18;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_CCER use record
      CC1E     at 0 range  0 ..  0; -- Capture/Compare 1 output enable (read & write)
      CC1P     at 0 range  1 ..  1; -- Capture/Compare 1 output polarity (read & write)
      CC1NE    at 0 range  2 ..  2; -- Capture/Compare 1 complementary output enable (read & write)
      CC1NP    at 0 range  3 ..  3; -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E     at 0 range  4 ..  4; -- Capture/Compare 2 output enable (read & write)
      CC2P     at 0 range  5 ..  5; -- Capture/Compare 2 output polarity (read & write)
      CC2NE    at 0 range  6 ..  6; -- Capture/Compare 2 complementary output enable (read & write)
      CC2NP    at 0 range  7 ..  7; -- Capture/Compare 2 complementary output polarity (read & write)
      CC3E     at 0 range  8 ..  8; -- Capture/Compare 3 output enable (read & write)
      CC3P     at 0 range  9 ..  9; -- Capture/Compare 3 output polarity(read & write)
      CC3NE    at 0 range 10 .. 10; -- Capture/Compare 3 complementary output enable (read & write)
      CC3NP    at 0 range 11 .. 11; -- Capture/Compare 3 complementary output polarity (read & write)
      CC4E     at 0 range 12 .. 12; -- Capture/Compare 4 output enable (read & write)
      CC4P     at 0 range 13 .. 13; -- Capture/Compare 4 output polarity (read & write)
      Reserved at 0 range 14 .. 31;
   end record;

   -- TIM_1_8_TIMx_BDTR

   for LOCK_Options use (Off     => 0,
                         Level_1 => 1,
                         Level_2 => 2,
                         Level_3 => 3);

   type TIM_1_8_TIMx_BDTR is record
      DTG      : Bits_8;       -- Dead-time generator setup (read & write)
      LOCK     : LOCK_Options; -- Lock configuration (read & write)
      OSSI     : Bit;          -- Off-state selection for Idle mode (read & write)
      OSSR     : Bit;          -- Off-state selection for Run mode (read & write)
      BKE      : Enabler;      -- Break enable (read & write)
      BKP      : Polarity;     -- Break polarity (read & write)
      AOE      : Enabler;      -- Automatic output enable (read & write)
      MOE      : Enabler;      -- Main output enable (read & write)
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_BDTR use record
      DTG      at 0 range  0 ..  7; -- Dead-time generator setup (read & write)
      LOCK     at 0 range  8 ..  9; -- Lock configuration (read & write)
      OSSI     at 0 range 10 .. 10; -- Off-state selection for Idle mode (read & write)
      OSSR     at 0 range 11 .. 11; -- Off-state selection for Run mode (read & write)
      BKE      at 0 range 12 .. 12; -- Break enable (read & write)
      BKP      at 0 range 13 .. 13; -- Break polarity (read & write)
      AOE      at 0 range 14 .. 14; -- Automatic output enable (read & write)
      MOE      at 0 range 15 .. 15; -- Main output enable (read & write)
      Reserved at 0 range 16 .. 31;
   end record;

   -- TIM_1_8_TIMx_DCR

   type TIM_1_8_TIMx_DCR is record
      DBA        : Bits_5;  -- DMA base address (read & write)
      Reserved_1 : Bits_3;
      DBL        : Bits_5;  -- DMA burst length (read & write)
      Reserved_2 : Bits_19;
   end record with Volatile, Size => Word'Size;

   for TIM_1_8_TIMx_DCR use record
      DBA        at 0 range  0 ..  4; -- DMA base address (read & write)
      Reserved_1 at 0 range  5 ..  7;
      DBL        at 0 range  8 .. 12; -- DMA burst length (read & write)
      Reserved_2 at 0 range 13 .. 31;
   end record;

   -- TIM_1_8_Register

   type TIM_1_8_Register is record
      CR1        : TIM_1_8_TIMx_CR1;   -- Control register 1
      CR2        : TIM_1_8_TIMx_CR2;   -- Control register 2
      SMCR       : TIM_1_8_TIMx_SMCR;  -- Slave mode control register
      DIER       : TIM_1_8_TIMx_DIER;  -- DMA/interrupt enable register
      SR         : TIM_1_8_TIMx_SR;    -- Status register
      EGR        : TIM_1_8_TIMx_EGR;   -- Event generation register
      CCMR1      : TIM_1_8_TIMx_CCMR1; -- Capture/compare mode register 1
      CCMR2      : TIM_1_8_TIMx_CCMR2; -- Capture/compare mode register 2
      CCER       : TIM_1_8_TIMx_CCER;  -- Capture/compare enable register
      CNT        : Bits_16;            -- Counter
      Reserved_1 : Bits_16;
      PSC        : Bits_16;            -- Prescaler
      Reserved_2 : Bits_16;
      ARR        : Bits_16;            -- Auto_reload register
      Reserved_3 : Bits_16;
      RCR        : Bits_8;             -- Repetition counter register
      Reserved_4 : Bits_24;
      CCR1       : Bits_16;            -- Capture/compare register 1
      Reserved_5 : Bits_16;
      CCR2       : Bits_16;            -- Capture/compare register 2
      Reserved_6 : Bits_16;
      CCR3       : Bits_16;            -- Capture/compare register 3
      Reserved_7 : Bits_16;
      CCR4       : Bits_16;            -- Capture/compare register 4
      Reserved_8 : Bits_16;
      BDTR       : TIM_1_8_TIMx_BDTR;  -- Break and dead-time register
      DCR        : TIM_1_8_TIMx_DCR;   -- DMA control register
      DMAR       : Bits_16;               -- DMA address for full transfer
      Reserved_9 : Bits_16;
   end record with Volatile, Size => Byte'Size * 16#50#;

   for TIM_1_8_Register use record
      CR1        at 16#00# range  0 .. 31; -- Control register 1
      CR2        at 16#04# range  0 .. 31; -- Control register 2
      SMCR       at 16#08# range  0 .. 31; -- Slave mode control register
      DIER       at 16#0C# range  0 .. 31; -- DMA/interrupt enable register
      SR         at 16#10# range  0 .. 31; -- Status register
      EGR        at 16#14# range  0 .. 31; -- Event generation register
      CCMR1      at 16#18# range  0 .. 31; -- Capture/compare mode register 1
      CCMR2      at 16#1C# range  0 .. 31; -- Capture/compare mode register 2
      CCER       at 16#20# range  0 .. 31; -- Capture/compare enable register
      CNT        at 16#24# range  0 .. 15; -- Counter
      Reserved_1 at 16#24# range 16 .. 31;
      PSC        at 16#28# range  0 .. 15; -- Prescaler
      Reserved_2 at 16#28# range 16 .. 31;
      ARR        at 16#2C# range  0 .. 15; -- Auto_reload register
      Reserved_3 at 16#2C# range 16 .. 31;
      RCR        at 16#30# range  0 ..  7; -- Repetition counter register
      Reserved_4 at 16#30# range  8 .. 31;
      CCR1       at 16#34# range  0 .. 15; -- Capture/compare register 1
      Reserved_5 at 16#34# range 16 .. 31;
      CCR2       at 16#38# range  0 .. 15; -- Capture/compare register 2
      Reserved_6 at 16#38# range 16 .. 31;
      CCR3       at 16#3C# range  0 .. 15; -- Capture/compare register 3
      Reserved_7 at 16#3C# range 16 .. 31;
      CCR4       at 16#40# range  0 .. 15; -- Capture/compare register 4
      Reserved_8 at 16#40# range 16 .. 31;
      BDTR       at 16#44# range  0 .. 31; -- Break and dead-time register
      DCR        at 16#48# range  0 .. 31; -- DMA control register
      DMAR       at 16#4C# range  0 .. 15; -- DMA address for full transfer
      Reserved_9 at 16#4C# range 16 .. 31;
   end record;

   -------------------
   -- Timers 2 to 5 --
   -------------------

   -- TIM_2_to_5_TIMx_CR1

   type TIM_2_to_5_TIMx_CR1 is record
      Reserved_1 : Bits_3;
      CCDS       : Bit;         -- Capture/compare DMA selection (read & write)
      MMS        : MMS_Options; -- Master mode selection (read & write)
      TI1S       : Bit;         -- TI1 selection (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_2_to_5_TIMx_CR1 use record
      Reserved_1 at 0 range  0 ..  2;
      CCDS       at 0 range  3 ..  3; -- (read & write)
      MMS        at 0 range  4 ..  6; -- (read & write)
      TI1S       at 0 range  7 ..  7; -- (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- TIM_2_to_5_TIMx_DIER

   type TIM_2_to_5_TIMx_DIER is record
      UIE        : Enabler; -- Update interrupt enable (read & write)
      CC1IE      : Enabler; -- Capture/Compare 1 interrupt enable (read & write)
      CC2IE      : Enabler; -- Capture/Compare 2 interrupt enable (read & write)
      CC3IE      : Enabler; -- Capture/Compare 3 interrupt enable (read & write)
      CC4IE      : Enabler; -- Capture/Compare 4 interrupt enable (read & write)
      Reserved_1 : Bit;
      TIE        : Enabler; -- Trigger interrupt enable (read & write)
      Reserved_2 : Bit;
      UDE        : Enabler; -- Update DMA request enable (read & write)
      CC1DE      : Enabler; -- Capture/Compare 1 DMA request enable (read & write)
      CC2DE      : Enabler; -- Capture/Compare 2 DMA request enable (read & write)
      CC3DE      : Enabler; -- Capture/Compare 3 DMA request enable (read & write)
      CC4DE      : Enabler; -- Capture/Compare 4 DMA request enable (read & write)
      Reserved_3 : Bit;
      TDE        : Enabler; -- Trigger DMA request enable (read & write)
      Reserved_4 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for TIM_2_to_5_TIMx_DIER use record
      UIE        at 0 range  0 ..  0; -- Update interrupt enable (read & write)
      CC1IE      at 0 range  1 ..  1; -- Capture/Compare 1 interrupt enable (read & write)
      CC2IE      at 0 range  2 ..  2; -- Capture/Compare 2 interrupt enable (read & write)
      CC3IE      at 0 range  3 ..  3; -- Capture/Compare 3 interrupt enable (read & write)
      CC4IE      at 0 range  4 ..  4; -- Capture/Compare 4 interrupt enable (read & write)
      Reserved_1 at 0 range  5 ..  5;
      TIE        at 0 range  6 ..  6; -- Trigger interrupt enable (read & write)
      Reserved_2 at 0 range  7 ..  7;
      UDE        at 0 range  8 ..  8; -- Update DMA request enable (read & write)
      CC1DE      at 0 range  9 ..  9; -- Capture/Compare 1 DMA request enable (read & write)
      CC2DE      at 0 range 10 .. 10; -- Capture/Compare 2 DMA request enable (read & write)
      CC3DE      at 0 range 11 .. 11; -- Capture/Compare 3 DMA request enable (read & write)
      CC4DE      at 0 range 12 .. 12; -- Capture/Compare 4 DMA request enable (read & write)
      Reserved_3 at 0 range 13 .. 13;
      TDE        at 0 range 14 .. 14; -- Trigger DMA request enable (read & write)
      Reserved_4 at 0 range 15 .. 31;
   end record;

   -- TIM_2_to_5_TIMx_SR

   type TIM_2_to_5_TIMx_SR is record
      UIF        : Occurance; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      : Occurance; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      : Occurance; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      CC3IF      : Occurance; -- Capture/Compare 3 interrupt flag (read & clear on write 'Not_Occured')
      CC4IF      : Occurance; -- Capture/Compare 4 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 : Bit;
      TIF        : Occurance; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      Reserved_2 : Bits_2;
      CC10F      : Occurance; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      : Occurance; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      CC30F      : Occurance; -- Capture/Compare 3 overcapture flag (read & clear on write 'Not_Occured')
      CC40F      : Occurance; -- Capture/Compare 4 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_3 : Bits_19;
   end record with Volatile, Size => Word'Size;

   for TIM_2_to_5_TIMx_SR use record
      UIF        at 0 range  0 ..  0; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      at 0 range  1 ..  1; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      at 0 range  2 ..  2; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      CC3IF      at 0 range  3 ..  3; -- Capture/Compare 3 interrupt flag (read & clear on write 'Not_Occured')
      CC4IF      at 0 range  4 ..  4; -- Capture/Compare 4 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 at 0 range  5 ..  5;
      TIF        at 0 range  6 ..  6; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      Reserved_2 at 0 range  7 ..  8;
      CC10F      at 0 range  9 ..  9; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      at 0 range 10 .. 10; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      CC30F      at 0 range 11 .. 11; -- Capture/Compare 3 overcapture flag (read & clear on write 'Not_Occured')
      CC40F      at 0 range 12 .. 12; -- Capture/Compare 4 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_3 at 0 range 13 .. 31;
   end record;

   -- TIM_2_to_5_TIMx_EGR

   type TIM_2_to_5_TIMx_EGR is record
      UG         : Generator; -- Update generation (write only)
      CC1G       : Generator; -- Capture/Compare 1 generation (write only)
      CC2G       : Generator; -- Capture/Compare 2 generation (write only)
      CC3G       : Generator; -- Capture/Compare 3 generation (write only)
      CC4G       : Generator; -- Capture/Compare 4 generation (write only)
      Reserved_1 : Bit;
      TG         : Generator; -- Trigger generation (write only)
      Reserved_2 : Bits_25;
   end record with Volatile, Size => Word'Size;

   for TIM_2_to_5_TIMx_EGR use record
      UG         at 0 range  0 ..  0; -- Update generation (write only)
      CC1G       at 0 range  1 ..  1; -- Capture/Compare 1 generation (write only)
      CC2G       at 0 range  2 ..  2; -- Capture/Compare 2 generation (write only)
      CC3G       at 0 range  3 ..  3; -- Capture/Compare 3 generation (write only)
      CC4G       at 0 range  4 ..  4; -- Capture/Compare 4 generation (write only)
      Reserved_1 at 0 range  5 ..  5;
      TG         at 0 range  6 ..  6; -- Trigger generation (write only)
      Reserved_2 at 0 range  7 .. 31;
   end record;

   -- TIM_2_to_5_TIMx_CCER

   type TIM_2_to_5_TIMx_CCER is record
      CC1E       : Enabler; -- Capture/Compare 1 output enable (read & write)
      CC1P       : Bit;     -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 : Bit;
      CC1NP      : Bit;     -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E       : Enabler; -- Capture/Compare 2 output enable (read & write)
      CC2P       : Bit;     -- Capture/Compare 2 output polarity (read & write)
      Reserved_2 : Bit;
      CC2NP      : Bit;     -- Capture/Compare 2 complementary output polarity (read & write)
      CC3E       : Enabler; -- Capture/Compare 3 output enable (read & write)
      CC3P       : Bit;     -- Capture/Compare 3 output polarity(read & write)
      Reserved_3 : Bit;
      CC3NP      : Bit;     -- Capture/Compare 3 complementary output polarity (read & write)
      CC4E       : Enabler; -- Capture/Compare 4 output enable (read & write)
      CC4P       : Bit;     -- Capture/Compare 4 output polarity (read & write)
      Reserved_4 : Bit;
      CC4NP      : Bit;     -- Capture/Compare 4 complementary output polarity (read & write)
      Reserved_5 : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_2_to_5_TIMx_CCER use record
      CC1E       at 0 range  0 ..  0; -- Capture/Compare 1 output enable (read & write)
      CC1P       at 0 range  1 ..  1; -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 at 0 range  2 ..  2;
      CC1NP      at 0 range  3 ..  3; -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E       at 0 range  4 ..  4; -- Capture/Compare 2 output enable (read & write)
      CC2P       at 0 range  5 ..  5; -- Capture/Compare 2 output polarity (read & write)
      Reserved_2 at 0 range  6 ..  6;
      CC2NP      at 0 range  7 ..  7; -- Capture/Compare 2 complementary output polarity (read & write)
      CC3E       at 0 range  8 ..  8; -- Capture/Compare 3 output enable (read & write)
      CC3P       at 0 range  9 ..  9; -- Capture/Compare 3 output polarity(read & write)
      Reserved_3 at 0 range 10 .. 10;
      CC3NP      at 0 range 11 .. 11; -- Capture/Compare 3 complementary output polarity (read & write)
      CC4E       at 0 range 12 .. 12; -- Capture/Compare 4 output enable (read & write)
      CC4P       at 0 range 13 .. 13; -- Capture/Compare 4 output polarity (read & write)
      Reserved_4 at 0 range 14 .. 14;
      CC4NP      at 0 range 15 .. 15; -- Capture/Compare 4 complementary output polarity (read & write)
      Reserved_5 at 0 range 16 .. 31;
   end record;

   -- TIM_2_to_5_Register

   type TIM_2_to_5_Register is record
      CR1        : TIM_1_8_TIMx_CR1;     -- Control register 1
      CR2        : TIM_2_to_5_TIMx_CR1;  -- Control register 2
      SMCR       : TIM_1_8_TIMx_SMCR;    -- Slave mode control register
      DIER       : TIM_2_to_5_TIMx_DIER; -- DMA/Interrupt enable register
      SR         : TIM_2_to_5_TIMx_SR;   -- Status register
      EGR        : TIM_2_to_5_TIMx_EGR;  -- Event generation register
      CCMR1      : TIM_1_8_TIMx_CCMR1;   -- Capture/compare mode register 1
      CCMR2      : TIM_1_8_TIMx_CCMR2;   -- Capture/compare mode register 2
      CCER       : TIM_2_to_5_TIMx_CCER; -- Capture/compare enable register
      CNT        : Bits_32;              -- Counter;
      PSC        : Bits_16;              -- Prescaler
      Reserved_1 : Bits_16;
      ARR        : Bits_32;              -- Auto-reload register
      Reserved_2 : Bits_32;
      CCR1       : Bits_32;              -- Capture/compare register 1
      CCR2       : Bits_32;              -- Capture/compare register 2
      CCR3       : Bits_32;              -- Capture/compare register 3
      CCR4       : Bits_32;              -- Capture/compare register 4
      Reserved_3 : Bits_32;
      DCR        : TIM_1_8_TIMx_DCR;     -- DMA control register
      DMAR       : Bits_16;              -- DMA address for full transfer
      Reserved_4 : Bits_16;
   end record with Volatile, Size => Byte'Size * 16#50#;

   for TIM_2_to_5_Register use record
      CR1        at 16#00# range  0 .. 31; -- Control register 1
      CR2        at 16#04# range  0 .. 31; -- Control register 2
      SMCR       at 16#08# range  0 .. 31; -- Slave mode control register
      DIER       at 16#0C# range  0 .. 31; -- DMA/Interrupt enable register
      SR         at 16#10# range  0 .. 31; -- Status register
      EGR        at 16#14# range  0 .. 31; -- Event generation register
      CCMR1      at 16#18# range  0 .. 31; -- Capture/compare mode register 1
      CCMR2      at 16#1C# range  0 .. 31; -- Capture/compare mode register 2
      CCER       at 16#20# range  0 .. 31; -- Capture/compare enable register
      CNT        at 16#24# range  0 .. 31; -- Counter;
      PSC        at 16#28# range  0 .. 15; -- Prescaler
      Reserved_1 at 16#28# range 16 .. 31;
      ARR        at 16#2C# range  0 .. 31; -- Auto-reload register
      Reserved_2 at 16#30# range  0 .. 31;
      CCR1       at 16#34# range  0 .. 31; -- Capture/compare register 1
      CCR2       at 16#38# range  0 .. 31; -- Capture/compare register 2
      CCR3       at 16#3C# range  0 .. 31; -- Capture/compare register 3
      CCR4       at 16#40# range  0 .. 31; -- Capture/compare register 4
      Reserved_3 at 16#44# range  0 .. 31;
      DCR        at 16#48# range  0 .. 31; -- DMA control register
      DMAR       at 16#4C# range  0 .. 15; -- DMA address for full transfer
      Reserved_4 at 16#4C# range 16 .. 31;
   end record;

   --------------------
   -- Timer 9 and 12 --
   --------------------

   -- TIM_9_12_TIMx_CR1

   type TIM_9_12_TIMx_CR1 is record
      CEN        : Enabler;      -- Counter enable (read & write)
      UDIS       : UDIS_Options; -- Update disable (read & write)
      URS        : Bit;          -- Update request source (read & write)
      OPM        : Bit;          -- One pulse mode (read & write)
      Reserved_1 : Bits_3;
      ARPE       : Buffering;    -- Auto-reload preload enable (read & write)
      CKD        : CKD_Options;  -- Clock division (read & write)
      Reserved_2 : Bits_22;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_CR1 use record
      CEN        at 0 range  0 ..  0; -- (read & write)
      UDIS       at 0 range  1 ..  1; -- (read & write)
      URS        at 0 range  2 ..  2; -- (read & write)
      OPM        at 0 range  3 ..  3; -- (read & write)
      Reserved_1 at 0 range  4 ..  6;
      ARPE       at 0 range  7 ..  7; -- (read & write)
      CKD        at 0 range  8 ..  9; -- (read & write)
      Reserved_2 at 0 range 10 .. 31;
   end record;

   -- TIM_9_12_TIMx_SMCR

   type TIM_9_12_TIMx_SMCR is record
      SMS        : SMS_Options;  -- Slave mode selection (read & write)
      Reserved_1 : Bit;
      TS         : TS_Options;   -- Trigger selection (read & write)
      MSM        : Bit;          -- Master/slave mode (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_SMCR use record
      SMS        at 0 range  0 ..  2; -- Slave mode selection (read & write)
      Reserved_1 at 0 range  3 ..  3;
      TS         at 0 range  4 ..  6; -- Trigger selection (read & write)
      MSM        at 0 range  7 ..  7; -- Master/slave mode (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- TIM_9_12_TIMx_DIER

   type TIM_9_12_TIMx_DIER is record
      UIE        : Enabler; -- Update interrupt enable (read & write)
      CC1IE      : Enabler; -- Capture/Compare 1 enable (read & write)
      CC2IE      : Enabler; -- Capture/Compare 2 enable (read & write)
      Reserved_1 : Bits_3;
      TIE        : Enabler; -- Trigger interrupt enable (read & write)
      Reserved_2 : Bits_25;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_DIER use record
      UIE        at 0 range  0 ..  0; -- Update interrupt enable (read & write)
      CC1IE      at 0 range  1 ..  1; -- Capture/Compare 1 enable (read & write)
      CC2IE      at 0 range  2 ..  2; -- Capture/Compare 2 enable (read & write)
      Reserved_1 at 0 range  3 ..  5;
      TIE        at 0 range  6 ..  6; -- Trigger interrupt enable (read & write)
      Reserved_2 at 0 range  7 .. 31;
   end record;

   -- TIM_9_12_TIMx_SR

   type TIM_9_12_TIMx_SR is record
      UIF        : Occurance; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      : Occurance; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      : Occurance; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 : Bits_3;
      TIF        : Occurance; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      Reserved_2 : Bits_2;
      CC10F      : Occurance; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      : Occurance; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_3 : Bits_21;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_SR use record
      UIF        at 0 range  0 ..  0; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      at 0 range  1 ..  1; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      CC2IF      at 0 range  2 ..  2; -- Capture/Compare 2 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 at 0 range  3 ..  5;
      TIF        at 0 range  6 ..  6; -- Trigger interrupt flag (read & clear on write 'Not_Occured')
      Reserved_2 at 0 range  7 ..  8;
      CC10F      at 0 range  9 ..  9; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      CC20F      at 0 range 10 .. 10; -- Capture/Compare 2 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_3 at 0 range 11 .. 31;
   end record;

   -- TIM_9_12_TIMx_EGR

   type TIM_9_12_TIMx_EGR is record
      UG         : Generator; -- Update generation (write only)
      CC1G       : Generator; -- Capture/Compare 1 generation (write only)
      CC2G       : Generator; -- Capture/Compare 2 generation (write only)
      Reserved_1 : Bits_3;
      TG         : Generator; -- Trigger generation (write only)
      Reserved_2 : Bits_25;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_EGR use record
      UG         at 0 range  0 ..  0; -- Update generation (write only)
      CC1G       at 0 range  1 ..  1; -- Capture/Compare 1 generation (write only)
      CC2G       at 0 range  2 ..  2; -- Capture/Compare 2 generation (write only)
      Reserved_1 at 0 range  3 ..  5;
      TG         at 0 range  6 ..  6; -- Trigger generation (write only)
      Reserved_2 at 0 range  7 .. 31;
   end record;

   -- TIM_9_12_TIMx_CCMR1

   type TIM_9_12_TIMx_CCMR1_Capture is record
      CC1S        : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      IC1PSC      : IC1PSC_Options;  -- Input capture 1 prescaler (read & write)
      IC1F        : ETF_Options;     -- Input capture 1 filter (read & write)
      CC2S        : CC1S_Options;    -- Capture/Compare 2 Selection (read & write)
      IC2PSC      : IC1PSC_Options;  -- Input capture 2 prescaler (read & write)
      IC2F        : ETF_Options;     -- Input capture 2 filter (read & write)
      Reserved    : Bits_16;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_CCMR1_Capture use record
      CC1S        at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      IC1PSC      at 0 range  2 ..  3; -- Input capture 1 prescaler (read & write)
      IC1F        at 0 range  4 ..  7; -- Input capture 1 filter (read & write)
      CC2S        at 0 range  8 ..  9; -- Capture/Compare 2 Selection (read & write)
      IC2PSC      at 0 range 10 .. 11; -- Input capture 2 prescaler (read & write)
      IC2F        at 0 range 12 .. 15; -- Input capture 2 filter (read & write)
      Reserved    at 0 range 16 .. 31;
   end record;

   type TIM_9_12_TIMx_CCMR1_Compare is record
      CC1S        : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      OC1FE       : Enabler;         -- Output Compare 1 fast enable (read & write)
      OC1PE       : Enabler;         -- Output Compare 1 preload enable (read & write)
      OC1M        : OCM_Options;     -- Output Compare 1 mode (read & write)
      Reserved_1  : Bit;
      CC2S        : CC1S_Options;    -- Capture/Compare 2 Selection (read & write)
      OC2FE       : Enabler;         -- Output Compare 2 fast enable (read & write)
      OC2PE       : Enabler;         -- Output Compare 2 preload enable (read & write)
      OC2M        : OCM_Options;     -- Output Compare 2 mode (read & write)
      Reserved_2  : Bits_17;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_CCMR1_Compare use record
      CC1S        at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      OC1FE       at 0 range  2 ..  2; -- Output Compare 1 fast enable (read & write)
      OC1PE       at 0 range  3 ..  3; -- Output Compare 1 preload enable (read & write)
      OC1M        at 0 range  4 ..  6; -- Output Compare 1 mode (read & write)
      Reserved_1  at 0 range  7 ..  7;
      CC2S        at 0 range  8 ..  9; -- Capture/Compare 2 Selection (read & write)
      OC2FE       at 0 range 10 .. 10; -- Output Compare 2 fast enable (read & write)
      OC2PE       at 0 range 11 .. 11; -- Output Compare 2 preload enable (read & write)
      OC2M        at 0 range 12 .. 14; -- Output Compare 2 mode (read & write)
      Reserved_2  at 0 range 15 .. 31;
   end record;

   type TIM_9_12_TIMx_CCMR1 (State : Capture_Compare := Capture) is record
      case State is
         when Capture => CAP : TIM_9_12_TIMx_CCMR1_Capture;
         when Compare => CPR : TIM_9_12_TIMx_CCMR1_Compare;
      end case;
   end record with Volatile, Unchecked_Union, Size => Word'Size;

   -- TIM_9_12_TIMx_CCER

   type TIM_9_12_TIMx_CCER is record
      CC1E       : Enabler; -- Capture/Compare 1 output enable (read & write)
      CC1P       : Bit;     -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 : Bit;
      CC1NP      : Bit;     -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E       : Enabler; -- Capture/Compare 2 output enable (read & write)
      CC2P       : Bit;     -- Capture/Compare 2 output polarity (read & write)
      Reserved_2 : Bit;
      CC2NP      : Bit;     -- Capture/Compare 2 complementary output polarity (read & write)
      Reserved_3 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_9_12_TIMx_CCER use record
      CC1E       at 0 range  0 ..  0; -- Capture/Compare 1 output enable (read & write)
      CC1P       at 0 range  1 ..  1; -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 at 0 range  2 ..  2;
      CC1NP      at 0 range  3 ..  3; -- Capture/Compare 1 complementary output polarity (read & write)
      CC2E       at 0 range  4 ..  4; -- Capture/Compare 2 output enable (read & write)
      CC2P       at 0 range  5 ..  5; -- Capture/Compare 2 output polarity (read & write)
      Reserved_2 at 0 range  6 ..  6;
      CC2NP      at 0 range  7 ..  7; -- Capture/Compare 2 complementary output polarity (read & write)
      Reserved_3 at 0 range  8 .. 31;
   end record;

   -- TIM_9_12_Register

   type TIM_9_12_Register is record
      CR1        : TIM_9_12_TIMx_CR1;   -- Control register 1
      Reserved_1 : Bits_32;
      SMCR       : TIM_9_12_TIMx_SMCR;  -- Slave mode control register
      DIER       : TIM_9_12_TIMx_DIER;  -- DMA/Interrupt enable register
      SR         : TIM_9_12_TIMx_SR;    -- Status register
      EGR        : TIM_9_12_TIMx_EGR;   -- Event generation register
      CCMR1      : TIM_9_12_TIMx_CCMR1; -- Capture/compare mode register 1
      Reserved_2 : Bits_32;
      CCER       : TIM_9_12_TIMx_CCER;  -- Capture/compare enable register
      CNT        : Bits_16;             -- Counter
      Reserved_3 : Bits_16;
      PSC        : Bits_16;             -- Prescaler
      Reserved_4 : Bits_16;
      ARR        : Bits_16;             -- Auto-reload register
      Reserved_5 : Bits_16;
      Reserved_6 : Bits_32;
      CCR1       : Bits_16;             -- Capture/compare register 1
      Reserved_7 : Bits_16;
      CCR2       : Bits_16;             -- Capture/compare register 2
      Reserved_8 : Bits_16;
   end record with Volatile, Size => Byte'Size * 16#3C#;

   for TIM_9_12_Register use record
      CR1        at 16#00# range  0 .. 31; -- Control register 1
      Reserved_1 at 16#04# range  0 .. 31;
      SMCR       at 16#08# range  0 .. 31; -- Slave mode control register
      DIER       at 16#0C# range  0 .. 31; -- DMA/Interrupt enable register
      SR         at 16#10# range  0 .. 31; -- Status register
      EGR        at 16#14# range  0 .. 31; -- Event generation register
      CCMR1      at 16#18# range  0 .. 31; -- Capture/compare mode register 1
      Reserved_2 at 16#1C# range  0 .. 31;
      CCER       at 16#20# range  0 .. 31; -- Capture/compare enable register
      CNT        at 16#24# range  0 .. 15; -- Counter
      Reserved_3 at 16#24# range 16 .. 31;
      PSC        at 16#28# range  0 .. 15; -- Prescaler
      Reserved_4 at 16#28# range 16 .. 31;
      ARR        at 16#2C# range  0 .. 15; -- Auto-reload register
      Reserved_5 at 16#2C# range 16 .. 31;
      Reserved_6 at 16#30# range  0 .. 31;
      CCR1       at 16#34# range  0 .. 15; -- Capture/compare register 1
      Reserved_7 at 16#34# range 16 .. 31;
      CCR2       at 16#38# range  0 .. 15; -- Capture/compare register 2
      Reserved_8 at 16#38# range 16 .. 31;
   end record;

   ---------------------------
   -- Timers 10, 11, 13, 14 --
   ---------------------------

   -- TIM_10_11_13_14_TIMx_CR1

   type TIM_10_11_13_14_TIMx_CR1 is record
      CEN        : Enabler;      -- Counter enable (read & write)
      UDIS       : UDIS_Options; -- Update disable (read & write)
      URS        : Bit;          -- Update request source (read & write)
      Reserved_1 : Bits_4;
      ARPE       : Buffering;    -- Auto-reload preload enable (read & write)
      CKD        : CKD_Options;  -- Clock division (read & write)
      Reserved_2 : Bits_22;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_CR1 use record
      CEN        at 0 range  0 ..  0; -- (read & write)
      UDIS       at 0 range  1 ..  1; -- (read & write)
      URS        at 0 range  2 ..  2; -- (read & write)
      Reserved_1 at 0 range  3 ..  6;
      ARPE       at 0 range  7 ..  7; -- (read & write)
      CKD        at 0 range  8 ..  9; -- (read & write)
      Reserved_2 at 0 range 10 .. 31;
   end record;

   -- TIM_10_11_13_14_TIMx_DIER

   type TIM_10_11_13_14_TIMx_DIER is record
      UIE        : Enabler; -- Update interrupt enable (read & write)
      CC1IE      : Enabler; -- Capture/Compare 1 enable (read & write)
      Reserved_1 : Bits_30;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_DIER use record
      UIE        at 0 range  0 ..  0; -- Update interrupt enable (read & write)
      CC1IE      at 0 range  1 ..  1; -- Capture/Compare 1 enable (read & write)
      Reserved_1 at 0 range  2 .. 31;
   end record;

   -- TIM_10_11_13_14_TIMx_SR

   type TIM_10_11_13_14_TIMx_SR is record
      UIF        : Occurance; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      : Occurance; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 : Bits_7;
      CC10F      : Occurance; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_2 : Bits_22;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_SR use record
      UIF        at 0 range  0 ..  0; -- Update interrupt flag (read & clear on write 'Not_Occured')
      CC1IF      at 0 range  1 ..  1; -- Capture/Compare 1 interrupt flag (read & clear on write 'Not_Occured')
      Reserved_1 at 0 range  2 ..  8;
      CC10F      at 0 range  9 ..  9; -- Capture/Compare 1 overcapture flag (read & clear on write 'Not_Occured')
      Reserved_2 at 0 range 10 .. 31;
   end record;

   -- TIM_10_11_13_14_TIMx_EGR

   type TIM_10_11_13_14_TIMx_EGR is record
      UG         : Generator; -- Update generation (write only)
      CC1G       : Generator; -- Capture/Compare 1 generation (write only)
      Reserved   : Bits_30;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_EGR use record
      UG         at 0 range  0 ..  0; -- Update generation (write only)
      CC1G       at 0 range  1 ..  1; -- Capture/Compare 1 generation (write only)
      Reserved   at 0 range  2 .. 31;
   end record;

   -- TIM_10_11_13_14_TIMx_CCMR1

   type TIM_10_11_13_14_TIMx_CCMR1_Capture is record
      CC1S        : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      IC1PSC      : IC1PSC_Options;  -- Input capture 1 prescaler (read & write)
      IC1F        : ETF_Options;     -- Input capture 1 filter (read & write)
      Reserved    : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_CCMR1_Capture use record
      CC1S        at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      IC1PSC      at 0 range  2 ..  3; -- Input capture 1 prescaler (read & write)
      IC1F        at 0 range  4 ..  7; -- Input capture 1 filter (read & write)
      Reserved    at 0 range  8 .. 31;
   end record;

   type TIM_10_11_13_14_TIMx_CCMR1_Compare is record
      CC1S        : CC1S_Options;    -- Capture/Compare 1 Selection (read & write)
      OC1FE       : Enabler;         -- Output Compare 1 fast enable (read & write)
      OC1PE       : Enabler;         -- Output Compare 1 preload enable (read & write)
      OC1M        : OCM_Options;     -- Output Compare 1 mode (read & write)
      Reserved    : Bits_25;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_CCMR1_Compare use record
      CC1S        at 0 range  0 ..  1; -- Capture/Compare 1 Selection (read & write)
      OC1FE       at 0 range  2 ..  2; -- Output Compare 1 fast enable (read & write)
      OC1PE       at 0 range  3 ..  3; -- Output Compare 1 preload enable (read & write)
      OC1M        at 0 range  4 ..  6; -- Output Compare 1 mode (read & write)
      Reserved    at 0 range  7 .. 31;
   end record;

   type TIM_10_11_13_14_TIMx_CCMR1 (State : Capture_Compare := Capture) is record
      case State is
         when Capture => CAP : TIM_10_11_13_14_TIMx_CCMR1_Capture;
         when Compare => CPR : TIM_10_11_13_14_TIMx_CCMR1_Compare;
      end case;
   end record with Volatile, Unchecked_Union, Size => Word'Size;

   -- TIM_10_11_13_14_TIMx_CCER

   type TIM_10_11_13_14_TIMx_CCER is record
      CC1E       : Enabler; -- Capture/Compare 1 output enable (read & write)
      CC1P       : Bit;  -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 : Bit;
      CC1NP      : Bit;  -- Capture/Compare 1 complementary output polarity (read & write)
      Reserved_2 : Bits_28;
   end record with Volatile, Size => Word'Size;

   for TIM_10_11_13_14_TIMx_CCER use record
      CC1E       at 0 range  0 ..  0; -- Capture/Compare 1 output enable (read & write)
      CC1P       at 0 range  1 ..  1; -- Capture/Compare 1 output polarity (read & write)
      Reserved_1 at 0 range  2 ..  2;
      CC1NP      at 0 range  3 ..  3; -- Capture/Compare 1 complementary output polarity (read & write)
      Reserved_2 at 0 range  4 .. 31;
   end record;

   -- TIM_10_11_13_14_Register

   type TIM_10_11_13_14_Register is record
      CR1        : TIM_10_11_13_14_TIMx_CR1;   -- Control register 1
      Reserved_1 : Bits_32;
      Reserved_2 : Bits_32;
      DIER       : TIM_10_11_13_14_TIMx_DIER;  -- DMA/Interrupt enable register
      SR         : TIM_10_11_13_14_TIMx_SR;    -- Status register
      EGR        : TIM_10_11_13_14_TIMx_EGR;   -- Event generation register
      CCMR1      : TIM_10_11_13_14_TIMx_CCMR1; -- Capture/compare mode register 1
      Reserved_3 : Bits_32;
      CCER       : TIM_10_11_13_14_TIMx_CCER;  -- Capture/compare enable register
      CNT        : Bits_16;                    -- Counter
      Reserved_4 : Bits_16;
      PSC        : Bits_16;                    -- Prescaler
      Reserved_5 : Bits_16;
      ARR        : Bits_16;                    -- Auto-reload register
      Reserved_6 : Bits_16;
      Reserved_7 : Bits_32;
      CCR1       : Bits_16;                    -- Capture/compare register 1
      Reserved_8 : Bits_16;
   end record with Volatile, Size => Byte'Size * 16#38#;

   for TIM_10_11_13_14_Register use record
      CR1        at 16#00# range  0 .. 31; -- Control register 1
      Reserved_1 at 16#04# range  0 .. 31;
      Reserved_2 at 16#08# range  0 .. 31;
      DIER       at 16#0C# range  0 .. 31; -- DMA/Interrupt enable register
      SR         at 16#10# range  0 .. 31; -- Status register
      EGR        at 16#14# range  0 .. 31; -- Event generation register
      CCMR1      at 16#18# range  0 .. 31; -- Capture/compare mode register 1
      Reserved_3 at 16#1C# range  0 .. 31;
      CCER       at 16#20# range  0 .. 31; -- Capture/compare enable register
      CNT        at 16#24# range  0 .. 15; -- Counter
      Reserved_4 at 16#24# range 16 .. 31;
      PSC        at 16#28# range  0 .. 15; -- Prescaler
      Reserved_5 at 16#28# range 16 .. 31;
      ARR        at 16#2C# range  0 .. 15; -- Auto-reload register
      Reserved_6 at 16#2C# range 16 .. 31;
      Reserved_7 at 16#30# range  0 .. 31;
      CCR1       at 16#34# range  0 .. 15; -- Capture/compare register 1
      Reserved_8 at 16#34# range 16 .. 31;
   end record;

   -------------------
   -- Timer 6 and 7 --
   -------------------

   -- TIM_6_7_TIMx_CR1

   type TIM_6_7_TIMx_CR1 is record
      CEN        : Enabler;      -- Counter enable (read & write)
      UDIS       : UDIS_Options; -- Update disable (read & write)
      URS        : Bit;          -- Update request source (read & write)
      Reserved_1 : Bits_4;
      ARPE       : Buffering;    -- Auto-reload preload enable (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for TIM_6_7_TIMx_CR1 use record
      CEN        at 0 range  0 ..  0; -- (read & write)
      UDIS       at 0 range  1 ..  1; -- (read & write)
      URS        at 0 range  2 ..  2; -- (read & write)
      Reserved_1 at 0 range  3 ..  6;
      ARPE       at 0 range  7 ..  7; -- (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- TIM_6_7_TIMx_CR2

   type TIM_6_7_TIMx_CR2 is record
      Reserved_1 : Bits_4;
      MMS        : MMS_Options; -- Master mode selection (read & write)
      Reserved_2 : Bits_25;
   end record with Volatile, Size => Word'Size;

   for TIM_6_7_TIMx_CR2 use record
      Reserved_1 at 0 range  0 ..  3;
      MMS        at 0 range  4 ..  6; -- (read & write)
      Reserved_2 at 0 range  7 .. 31;
   end record;

   -- TIM_6_7_TIMx_DIER

   type TIM_6_7_TIMx_DIER is record
      UIE        : Enabler; -- Update interrupt enable (read & write)
      Reserved_1 : Bits_7;
      UDE        : Enabler; -- Update DMA request enable (read & write)
      Reserved_2 : Bits_23;
   end record with Volatile, Size => Word'Size;

   for TIM_6_7_TIMx_DIER use record
      UIE        at 0 range  0 ..  0; -- Update interrupt enable (read & write)
      Reserved_1 at 0 range  1 ..  7;
      UDE        at 0 range  8 ..  8; -- Update DMA request enable (read & write)
      Reserved_2 at 0 range  9 .. 31;
   end record;

   -- TIM_6_7_TIMx_SR

   type TIM_6_7_TIMx_SR is record
      UIF        : Occurance; -- Update interrupt flag (read & clear on write 'Not_Occured')
      Reserved   : Bits_31;
   end record with Volatile, Size => Word'Size;

   for TIM_6_7_TIMx_SR use record
      UIF        at 0 range  0 ..  0; -- Update interrupt flag (read & clear on write 'Not_Occured')
      Reserved   at 0 range  1 .. 31;
   end record;

   -- TIM_6_7_TIMx_EGR

   type TIM_6_7_TIMx_EGR is record
      UG         : Generator; -- Update generation (write only)
      Reserved   : Bits_31;
   end record with Volatile, Size => Word'Size;

   for TIM_6_7_TIMx_EGR use record
      UG         at 0 range  0 ..  0; -- Update generation (write only)
      Reserved   at 0 range  1 .. 31;
   end record;

   -- TIM_6_7_Register

   type TIM_6_7_Register is record
      CR1        : TIM_6_7_TIMx_CR1;   -- Control register 1
      CR2        : TIM_6_7_TIMx_CR2;   -- Control register 1
      Reserved_1 : Bits_32;
      DIER       : TIM_6_7_TIMx_DIER;  -- DMA/Interrupt enable register
      SR         : TIM_6_7_TIMx_SR;    -- Status register
      EGR        : TIM_6_7_TIMx_EGR;   -- Event generation register
      Reserved_2 : Bits_32;
      Reserved_3 : Bits_32;
      Reserved_4 : Bits_32;
      CNT        : Bits_16;            -- Counter
      Reserved_5 : Bits_16;
      PSC        : Bits_16;            -- Prescaler
      Reserved_6 : Bits_16;
      ARR        : Bits_16;            -- Auto-reload register
   end record with Volatile, Size => Byte'Size * 16#30#;

   for TIM_6_7_Register use record
      CR1        at 16#00# range  0 .. 31; -- Control register 1
      CR2        at 16#04# range  0 .. 31; -- Control register 1
      Reserved_1 at 16#08# range  0 .. 31;
      DIER       at 16#0C# range  0 .. 31; -- DMA/Interrupt enable register
      SR         at 16#10# range  0 .. 31; -- Status register
      EGR        at 16#14# range  0 .. 31; -- Event generation register
      Reserved_2 at 16#18# range  0 .. 31;
      Reserved_3 at 16#1C# range  0 .. 31;
      Reserved_4 at 16#20# range  0 .. 31;
      CNT        at 16#24# range  0 .. 15; -- Counter
      Reserved_5 at 16#24# range 16 .. 31;
      PSC        at 16#28# range  0 .. 15; -- Prescaler
      Reserved_6 at 16#28# range 16 .. 31;
      ARR        at 16#2C# range  0 .. 15; -- Auto-reload register
   end record;

   -- Set register addresses

   TIM1 : aliased TIM_1_8_Register with
     Volatile, Address => System'To_Address (Base_TIM1), Import;

   TIM2 : aliased TIM_2_to_5_Register with
     Volatile, Address => System'To_Address (Base_TIM2), Import;

   TIM3 : aliased TIM_2_to_5_Register with
     Volatile, Address => System'To_Address (Base_TIM3), Import;

   TIM4 : aliased TIM_2_to_5_Register with
     Volatile, Address => System'To_Address (Base_TIM4), Import;

   TIM5 : aliased TIM_2_to_5_Register with
     Volatile, Address => System'To_Address (Base_TIM5), Import;

   TIM6 : aliased TIM_6_7_Register with
     Volatile, Address => System'To_Address (Base_TIM6), Import;

   TIM7 : aliased TIM_6_7_Register with
     Volatile, Address => System'To_Address (Base_TIM7), Import;

   TIM8 : aliased TIM_1_8_Register with
     Volatile, Address => System'To_Address (Base_TIM8), Import;

   TIM9 : aliased TIM_9_12_Register with
     Volatile, Address => System'To_Address (Base_TIM9), Import;

   TIM10 : aliased TIM_10_11_13_14_Register with
     Volatile, Address => System'To_Address (Base_TIM10), Import;

   TIM11 : aliased TIM_10_11_13_14_Register with
     Volatile, Address => System'To_Address (Base_TIM11), Import;

   TIM12 : aliased TIM_9_12_Register with
     Volatile, Address => System'To_Address (Base_TIM12), Import;

   TIM13 : aliased TIM_10_11_13_14_Register with
     Volatile, Address => System'To_Address (Base_TIM13), Import;

   TIM14 : aliased TIM_10_11_13_14_Register with
     Volatile, Address => System'To_Address (Base_TIM14), Import;

   -- Mapping timers to registers

   type TIM_1_8_Access         is access all TIM_1_8_Register;
   type TIM_2_to_5_Access      is access all TIM_2_to_5_Register;
   type TIM_6_7_Access         is access all TIM_6_7_Register;
   type TIM_9_12_Access        is access all TIM_9_12_Register;
   type TIM_10_11_13_14_Access is access all TIM_10_11_13_14_Register;

   function TIM_1_8 (No : Timer_No) return TIM_1_8_Access is

     (case No is
         when  1     => TIM1'Access,
         when  8     => TIM8'Access,
         when others => null);

   function TIM_2_to_5 (No : Timer_No) return TIM_2_to_5_Access is

     (case No is
         when  2     => TIM2'Access,
         when  3     => TIM3'Access,
         when  4     => TIM4'Access,
         when  5     => TIM5'Access,
         when others => null);

   function TIM_6_7 (No : Timer_No) return TIM_6_7_Access is

     (case No is
         when  6     => TIM6'Access,
         when  7     => TIM7'Access,
         when others => null);

   function TIM_9_12 (No : Timer_No) return TIM_9_12_Access is

     (case No is
         when  9     => TIM9'Access,
         when 12     => TIM12'Access,
         when others => null);

   function TIM_10_11_13_14 (No : Timer_No) return TIM_10_11_13_14_Access is

     (case No is
         when 10     => TIM10'Access,
         when 11     => TIM11'Access,
         when 13     => TIM13'Access,
         when 14     => TIM14'Access,
         when others => null);

end STM32F4.Timers;

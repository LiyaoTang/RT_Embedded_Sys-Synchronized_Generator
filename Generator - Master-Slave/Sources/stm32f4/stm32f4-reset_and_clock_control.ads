--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F405xx/07xx, STM32F415xx Reset and clock control

package STM32F4.Reset_and_clock_control is

   -- RCC_PLLCFGR

   type PLLM_Range is range   2 ..  63 with Size => 6;
   type PLLN_Range is range 192 .. 432 with Size => 9;
   type PLLQ_Range is range   2 ..  15 with Size => 4;

   type PLLP_Options is (Divided_by_2,
                         Divided_by_4,
                         Divided_by_6,
                         Divided_by_8) with Size => 2;

   type PLLSRC_Options is (HSI, HSE)  with Size => 1;

   -- RCC_CFGR

   type SW_Options is (HSI, HSE, PLL) with Size => 2;

   type HPRE_Options is (Not_Divided,
                         Divided_by_2,
                         Divided_by_4,
                         Divided_by_8,
                         Divided_by_16,
                         Divided_by_64,
                         Divided_by_128,
                         Divided_by_256,
                         Divided_by_521) with Size => 4;

   type PPRE_Options is (Not_Divided,
                         Divided_by_2,
                         Divided_by_4,
                         Divided_by_8,
                         Divided_by_16) with Size => 3;

   type MCO1_Options is (HSI, LSE, HSE, PLL) with Size => 2;

   type MCO2_Options is (SYSCLK, PLLI2S, HSE, PLL) with Size => 2;

   type I2SSCR_Options is (PLLI2S, External) with Size => 1;

   type MCOPRE_Options is (Not_Divided,
                           Divided_by_2,
                           Divided_by_3,
                           Divided_by_4,
                           Divided_by_5) with Size => 3;

   -- RCC_BDCR

   type RTCSEL_Options is (No_Clock, LSE, LSI, HSE) with Size => 2;

   -- RCC_SSCGR

   type SPREADSEL_Options is (Center, Down) with Size => 1;

   -- RCC_PLLI2SCFGR

   type PLLI2S_Range is range 2 .. 7 with Size => 3;

private

   -- RCC_CR

   type RCC_CR is record
      HSION      : Switch;    -- Internal high-speed clock enable (read & write)
      HSIRDY     : Readiness; -- Internal high-speed clock ready flag (read only)
      Reserved_1 : Bit;
      HSITRIM    : Bits_5;    -- Internal high-speed clock trimming (read & write)
      HSICAL     : Bits_8;    -- Internal high-speed clock calibration (read only)
      HSEON      : Switch;    -- HSE clock enable (read & write)
      HSERDY     : Readiness; -- HSE clock ready flag (read only)
      HSEBYP     : Bypass;    -- HSE clock bypass (read & write)
      CSSON      : Switch;    -- Clock security system enable (read & write)
      Reserved_2 : Bits_4;
      PLLON      : Switch;    -- Main PLL (PLL) enable (read & write)
      PLLRDY     : Lock;      -- Main PLL (PLL) clock ready flag (read only)
      PLLI2SON   : Switch;    -- PLLI2S enable (read & write)
      PLLI2SRDY  : Lock;      -- PLLI2S clock ready flag (read only)
      Reserved_3 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for RCC_CR use record
      HSION      at 0 range  0 ..  0; -- Internal high-speed clock enable (read & write)
      HSIRDY     at 0 range  1 ..  1; -- Internal high-speed clock ready flag (read only)
      Reserved_1 at 0 range  2 ..  2;
      HSITRIM    at 0 range  3 ..  7; -- Internal high-speed clock trimming (read & write)
      HSICAL     at 0 range  8 .. 15; -- Internal high-speed clock calibration (read only)
      HSEON      at 0 range 16 .. 16; -- HSE clock enable (read & write)
      HSERDY     at 0 range 17 .. 17; -- HSE clock ready flag (read only)
      HSEBYP     at 0 range 18 .. 18; -- HSE clock bypass (read & write)
      CSSON      at 0 range 19 .. 19; -- Clock security system enable (read & write)
      Reserved_2 at 0 range 20 .. 23;
      PLLON      at 0 range 24 .. 24; -- Main PLL (PLL) enable (read & write)
      PLLRDY     at 0 range 25 .. 25; -- Main PLL (PLL) clock ready flag (read only)
      PLLI2SON   at 0 range 26 .. 26; -- PLLI2S enable (read & write)
      PLLI2SRDY  at 0 range 27 .. 27; -- PLLI2S clock ready flag (read only)
      Reserved_3 at 0 range 28 .. 31;
   end record;

   -- RCC_PLLCFGR

   for PLLP_Options use (Divided_by_2 => 0,
                         Divided_by_4 => 1,
                         Divided_by_6 => 2,
                         Divided_by_8 => 3);

   for PLLSRC_Options use (HSI => 0, HSE => 1);

   type RCC_PLLCFGR is record
      PLLM       : PLLM_Range;     -- Division factor for the main PLL (PLL) (read & write)
      PLLN       : PLLN_Range;     -- Main PLL (PLL) multiplication factor for VCO (read & write)
      Reserved_1 : Bit;
      PLLP       : PLLP_Options;   -- Main PLL (PLL) division factor (read & write)
      Reserved_2 : Bits_4;
      PLLSRC     : PLLSRC_Options; -- Main PLL(PLL) and audio PLL (PLLI2S) entry clock source (read & write)
      Reserved_3 : Bit;
      PLLQ       : PLLQ_Range;     -- Main PLL (PLL) division factor (read & write)
      Reserved_4 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for RCC_PLLCFGR use record
      PLLM       at 0 range  0 ..  5; -- Division factor for the main PLL (PLL) (read & write)
      PLLN       at 0 range  6 .. 14; -- Main PLL (PLL) multiplication factor for VCO (read & write)
      Reserved_1 at 0 range 15 .. 15;
      PLLP       at 0 range 16 .. 17; -- Main PLL (PLL) division factor (read & write)
      Reserved_2 at 0 range 18 .. 21;
      PLLSRC     at 0 range 22 .. 22; -- Main PLL(PLL) and audio PLL (PLLI2S) entry clock source (read & write)
      Reserved_3 at 0 range 23 .. 23;
      PLLQ       at 0 range 24 .. 27; -- Main PLL (PLL) division factor (read & write)
      Reserved_4 at 0 range 28 .. 31;
   end record;

   -- RCC_CFGR

   for SW_Options use (HSI => 0,
                       HSE => 1,
                       PLL => 2);

   for HPRE_Options use (Not_Divided    => 2#0000#,
                         Divided_by_2   => 2#1000#,
                         Divided_by_4   => 2#1001#,
                         Divided_by_8   => 2#1010#,
                         Divided_by_16  => 2#1011#,
                         Divided_by_64  => 2#1100#,
                         Divided_by_128 => 2#1101#,
                         Divided_by_256 => 2#1110#,
                         Divided_by_521 => 2#1111#);

   for PPRE_Options use (Not_Divided   => 2#000#,
                         Divided_by_2  => 2#100#,
                         Divided_by_4  => 2#101#,
                         Divided_by_8  => 2#110#,
                         Divided_by_16 => 2#111#);

   for MCO1_Options use (HSI => 0,
                         LSE => 1,
                         HSE => 2,
                         PLL => 3);

   for MCO2_Options use (SYSCLK => 0,
                         PLLI2S => 1,
                         HSE    => 2,
                         PLL    => 3);

   for I2SSCR_Options use (PLLI2S => 0, External => 1);

   for MCOPRE_Options use (Not_Divided  => 2#000#,
                           Divided_by_2 => 2#100#,
                           Divided_by_3 => 2#101#,
                           Divided_by_4 => 2#110#,
                           Divided_by_5 => 2#111#);

   type RCC_CFGR is record
      SW         : SW_Options;     -- System clock switch (read & write)
      SWS        : SW_Options;     -- System clock switch status (read only)
      HPRE       : HPRE_Options;   -- AHB prescaler (read & write)
      Reserved   : Bits_2;
      PPRE1      : PPRE_Options;   -- APB Low speed prescaler (APB1) (read & write)
      PPRE2      : PPRE_Options;   -- APB high-speed prescaler (APB2) (read & write)
      RTCPRE     : Bits_5;         -- HSE division factor for RTC clock (read & write)
      MCO1       : MCO1_Options;   -- Microcontroller clock output 1 (read & write)
      I2SSCR     : I2SSCR_Options; -- I2S clock selection (read & write)
      MCO1PRE    : MCOPRE_Options; -- MCO1 prescaler (read & write)
      MCO2PRE    : MCOPRE_Options; -- MCO2 prescaler (read & write)
      MCO2       : MCO2_Options;   -- Microcontroller clock output 2 (read & write)
   end record with Volatile, Size => Word'Size;

   for RCC_CFGR use record
      SW         at 0 range  0 ..  1; -- System clock switch (read & write)
      SWS        at 0 range  2 ..  3; -- System clock switch status (read only)
      HPRE       at 0 range  4 ..  7; -- AHB prescaler (read & write)
      Reserved   at 0 range  8 ..  9;
      PPRE1      at 0 range 10 .. 12; -- APB Low speed prescaler (APB1) (read & write)
      PPRE2      at 0 range 13 .. 15; -- APB high-speed prescaler (APB2) (read & write)
      RTCPRE     at 0 range 16 .. 20; -- HSE division factor for RTC clock (read & write)
      MCO1       at 0 range 21 .. 22; -- Microcontroller clock output 1 (read & write)
      I2SSCR     at 0 range 23 .. 23; -- I2S clock selection (read & write)
      MCO1PRE    at 0 range 24 .. 26; -- MCO1 prescaler (read & write)
      MCO2PRE    at 0 range 27 .. 29; -- MCO2 prescaler (read & write)
      MCO2       at 0 range 30 .. 31;  -- Microcontroller clock output 2 (read & write)
   end record;

   -- RCC_CIR

   type RCC_CIR is record
      LSIRDYF     : Flagging; -- LSI ready interrupt flag (read only)
      LSERDYF     : Flagging; -- LSE ready interrupt flag (read only)
      HSIRDYF     : Flagging; -- HSI ready interrupt flag (read only)
      HSERDYF     : Flagging; -- HSE ready interrupt flag (read only)
      PLLRDYF     : Flagging; -- Main PLL (PLL) ready interrupt flag (read only)
      PLLI2SRDYF  : Flagging; -- PLLI2S ready interrupt flag (read only)
      PLLSAIRDYF  : Flagging; -- PLLSAI Ready Interrupt flag (read only)
      CSSF        : Flagging; -- Clock security system interrupt flag (read only)
      LSIRDYIE    : Enabler;  -- LSI ready interrupt enable (read & write)
      LSERDYIE    : Enabler;  -- LSE ready interrupt enable (read & write)
      HSIRDYIE    : Enabler;  -- HSI ready interrupt enable (read & write)
      HSERDYIE    : Enabler;  -- HSE ready interrupt enable (read & write)
      PLLRDYIE    : Enabler;  -- Main PLL (PLL) ready interrupt enable (read & write)
      PLLI2SRDYIE : Enabler;  -- PLLI2S ready interrupt enable (read & write)
      PLLSAIRDYIE : Enabler;  -- PLLSAI Ready Interrupt Enable (read & write)
      Reserved_1  : Bit;
      LSIRDYC     : Clearing; -- LSI ready interrupt clear (write only)
      LSERDYC     : Clearing; -- LSE ready interrupt clear (write only)
      HSIRDYC     : Clearing; -- HSI ready interrupt clear (write only)
      HSERDYC     : Clearing; -- HSE ready interrupt clear (write only)
      PLLRDYC     : Clearing; -- Main PLL(PLL) ready interrupt clear (write only)
      PLLI2SRDYC  : Clearing; -- PLLI2S ready interrupt clear (write only)
      PLLSAIRDYC  : Clearing; -- PLLSAI Ready Interrupt Clear (write only)
      CSSC        : Clearing; -- Clock security system interrupt clear (write only)
      Reserved_2  : Bits_8;
   end record with Volatile, Size => Word'Size;

   for RCC_CIR use record
      LSIRDYF     at 0 range  0 ..  0; -- LSI ready interrupt flag (read only)
      LSERDYF     at 0 range  1 ..  1; -- LSE ready interrupt flag (read only)
      HSIRDYF     at 0 range  2 ..  2; -- HSI ready interrupt flag (read only)
      HSERDYF     at 0 range  3 ..  3; -- HSE ready interrupt flag (read only)
      PLLRDYF     at 0 range  4 ..  4; -- Main PLL (PLL) ready interrupt flag (read only)
      PLLI2SRDYF  at 0 range  5 ..  5; -- PLLI2S ready interrupt flag (read only)
      PLLSAIRDYF  at 0 range  6 ..  6; -- PLLSAI Ready Interrupt flag (read only)
      CSSF        at 0 range  7 ..  7; -- Clock security system interrupt flag (read only)
      LSIRDYIE    at 0 range  8 ..  8;  -- LSI ready interrupt enable (read & write)
      LSERDYIE    at 0 range  9 ..  9;  -- LSE ready interrupt enable (read & write)
      HSIRDYIE    at 0 range 10 .. 10;  -- HSI ready interrupt enable (read & write)
      HSERDYIE    at 0 range 11 .. 11;  -- HSE ready interrupt enable (read & write)
      PLLRDYIE    at 0 range 12 .. 12;  -- Main PLL (PLL) ready interrupt enable (read & write)
      PLLI2SRDYIE at 0 range 13 .. 13;  -- PLLI2S ready interrupt enable (read & write)
      PLLSAIRDYIE at 0 range 14 .. 14;  -- PLLSAI Ready Interrupt Enable (read & write)
      Reserved_1  at 0 range 15 .. 15;
      LSIRDYC     at 0 range 16 .. 16; -- LSI ready interrupt clear (write only)
      LSERDYC     at 0 range 17 .. 17; -- LSE ready interrupt clear (write only)
      HSIRDYC     at 0 range 18 .. 18; -- HSI ready interrupt clear (write only)
      HSERDYC     at 0 range 19 .. 19; -- HSE ready interrupt clear (write only)
      PLLRDYC     at 0 range 20 .. 20; -- Main PLL(PLL) ready interrupt clear (write only)
      PLLI2SRDYC  at 0 range 21 .. 21; -- PLLI2S ready interrupt clear (write only)
      PLLSAIRDYC  at 0 range 22 .. 22; -- PLLSAI Ready Interrupt Clear (write only)
      CSSC        at 0 range 23 .. 23; -- Clock security system interrupt clear (write only)
      Reserved_2  at 0 range 24 .. 31;
   end record;

   -- RCC_AHB1RSTR

   type RCC_AHB1RSTR is record
      GPIOA,
      GPIOB,
      GPIOC,
      GPIOD,
      GPIOE,
      GPIOF,
      GPIOG,
      GPIOH,
      GPIOI        : Resetting; -- IO port x reset (read & write)
      Reserved_1   : Bits_3;
      CRC          : Resetting; -- CRC reset (read & write)
      Reserved_2   : Bits_8;
      DMA1         : Resetting; -- DMA1 reset (read & write)
      DMA2         : Resetting; -- DMA2 reset (read & write)
      Reserved_3   : Bits_2;
      ETHMAC       : Resetting; -- Ethernet MAC reset (read & write)
      Reserved_4   : Bits_3;
      OTGHS        : Resetting; -- USB OTG HS module reset (read & write)
      Reserved_5   : Bits_2;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB1RSTR use record
      GPIOA       at 0 range  0 ..  0; -- IO port A reset (read & write)
      GPIOB       at 0 range  1 ..  1; -- IO port B reset (read & write)
      GPIOC       at 0 range  2 ..  2; -- IO port C reset (read & write)
      GPIOD       at 0 range  3 ..  3; -- IO port D reset (read & write)
      GPIOE       at 0 range  4 ..  4; -- IO port E reset (read & write)
      GPIOF       at 0 range  5 ..  5; -- IO port F reset (read & write)
      GPIOG       at 0 range  6 ..  6; -- IO port G reset (read & write)
      GPIOH       at 0 range  7 ..  7; -- IO port H reset (read & write)
      GPIOI       at 0 range  8 ..  8; -- IO port I reset (read & write)
      Reserved_1  at 0 range  9 .. 11;
      CRC         at 0 range 12 .. 12; -- CRC reset (read & write)
      Reserved_2  at 0 range 13 .. 20;
      DMA1        at 0 range 21 .. 21; -- DMA1 reset (read & write)
      DMA2        at 0 range 22 .. 22; -- DMA2 reset (read & write)
      Reserved_3  at 0 range 23 .. 24;
      ETHMAC      at 0 range 25 .. 25; -- Ethernet MAC reset (read & write)
      Reserved_4  at 0 range 26 .. 28;
      OTGHS       at 0 range 29 .. 29; -- USB OTG HS module reset (read & write)
      Reserved_5  at 0 range 30 .. 31;
   end record;

   -- RCC_AHB2RSTR

   type RCC_AHB2RSTR is record
      DCMI       : Resetting; -- Camera interface reset (read & write)
      Reserved_1 : Bits_3;
      CRYP       : Resetting; -- Cryptographic module reset (read & write)
      HASH       : Resetting; -- Hash module reset (read & write)
      RNG        : Resetting; -- Random number generator module reset (read & write)
      OTGFS      : Resetting; -- USB OTG FS module reset (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB2RSTR use record
      DCMI       at 0 range  0 ..  0; -- Camera interface reset (read & write)
      Reserved_1 at 0 range  1 ..  3;
      CRYP       at 0 range  4 ..  4; -- Cryptographic module reset (read & write)
      HASH       at 0 range  5 ..  5; -- Hash module reset (read & write)
      RNG        at 0 range  6 ..  6; -- Random number generator module reset (read & write)
      OTGFS      at 0 range  7 ..  7; -- USB OTG FS module reset (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- RCC_AHB3RSTR

   type RCC_AHB3RSTR is record
      FSMCEN   : Resetting; -- Flexible static memory controller module reset (read & write)
      Reserved : Bits_31;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB3RSTR use record
      FSMCEN   at 0 range  0 ..  0; -- Flexible static memory controller module reset (read & write)
      Reserved at 0 range  1 .. 31;
   end record;

   -- RCC_APB1RSTR

   type RCC_APB1RSTR is record
      TIM2       : Resetting; -- TIM2 reset (read & write)
      TIM3       : Resetting; -- TIM3 reset (read & write)
      TIM4       : Resetting; -- TIM4 reset (read & write)
      TIM5       : Resetting; -- TIM5 reset (read & write)
      TIM6       : Resetting; -- TIM6 reset (read & write)
      TIM7       : Resetting; -- TIM7 reset (read & write)
      TIM12      : Resetting; -- TIM12 reset (read & write)
      TIM13      : Resetting; -- TIM13 reset (read & write)
      TIM14      : Resetting; -- TIM14 reset (read & write)
      Reserved_1 : Bits_2;
      WWDG       : Resetting; -- Window watchdog reset (read & write)
      Reserved_2 : Bits_2;
      SPI2       : Resetting; -- SPI2 reset (read & write)
      SPI3       : Resetting; -- SPI3 reset (read & write)
      Reserved_3 : Bit;
      USART2     : Resetting; -- UART2 reset (read & write)
      USART3     : Resetting; -- UART3 reset (read & write)
      USART4     : Resetting; -- UART4 reset (read & write)
      USART5     : Resetting; -- UART5 reset (read & write)
      I2C1       : Resetting; -- I2C1 reset (read & write)
      I2C2       : Resetting; -- I2C2 reset (read & write)
      I2C3       : Resetting; -- I2C3 reset (read & write)
      Reserved_4 : Bit;
      CAN1       : Resetting; -- CAN1 reset (read & write)
      CAN2       : Resetting; -- CAN2 reset (read & write)
      Reserved_5 : Bit;
      PWR        : Resetting; -- Power interface reset (read & write)
      DAC        : Resetting; -- DAC reset (read & write)
      Reserved_6 : Bits_2;
   end record with Volatile, Size => Word'Size;

   for RCC_APB1RSTR use record
      TIM2       at 0 range  0 ..  0; -- TIM2 reset (read & write)
      TIM3       at 0 range  1 ..  1; -- TIM3 reset (read & write)
      TIM4       at 0 range  2 ..  2; -- TIM4 reset (read & write)
      TIM5       at 0 range  3 ..  3; -- TIM5 reset (read & write)
      TIM6       at 0 range  4 ..  4; -- TIM6 reset (read & write)
      TIM7       at 0 range  5 ..  5; -- TIM7 reset (read & write)
      TIM12      at 0 range  6 ..  6; -- TIM12 reset (read & write)
      TIM13      at 0 range  7 ..  7; -- TIM13 reset (read & write)
      TIM14      at 0 range  8 ..  8; -- TIM14 reset (read & write)
      Reserved_1 at 0 range  9 .. 10;
      WWDG       at 0 range 11 .. 11; -- Window watchdog reset (read & write)
      Reserved_2 at 0 range 12 .. 13;
      SPI2       at 0 range 14 .. 14; -- SPI2 reset (read & write)
      SPI3       at 0 range 15 .. 15; -- SPI3 reset (read & write)
      Reserved_3 at 0 range 16 .. 16;
      USART2     at 0 range 17 .. 17; -- USART2 reset (read & write)
      USART3     at 0 range 18 .. 18; -- USART3 reset (read & write)
      USART4     at 0 range 19 .. 19; -- USART4 reset (read & write)
      USART5     at 0 range 20 .. 20; -- USART5 reset (read & write)
      I2C1       at 0 range 21 .. 21; -- I2C1 reset (read & write)
      I2C2       at 0 range 22 .. 22; -- I2C2 reset (read & write)
      I2C3       at 0 range 23 .. 23; -- I2C3 reset (read & write)
      Reserved_4 at 0 range 24 .. 24;
      CAN1       at 0 range 25 .. 25; -- CAN 1 reset (read & write)
      CAN2       at 0 range 26 .. 26; -- CAN 2 reset (read & write)
      Reserved_5 at 0 range 27 .. 27;
      PWR        at 0 range 28 .. 28; -- Power interface reset (read & write)
      DAC        at 0 range 29 .. 29; -- DAC interface reset (read & write)
      Reserved_6 at 0 range 30 .. 31;
   end record;

   -- RCC_APB2RSTR

   type RCC_APB2RSTR is record
      TIM1       : Resetting; -- TIM1 reset (read & write)
      TIM8       : Resetting; -- TIM8 reset (read & write)
      Reserved_1 : Bits_2;
      USART1     : Resetting; -- USART1 reset (read & write)
      USART6     : Resetting; -- USART6 reset (read & write)
      Reserved_2 : Bits_2;
      ADC        : Resetting; -- ADC1 reset (read & write)
      Reserved_3 : Bits_2;
      SDIO       : Resetting; -- SDIO reset (read & write)
      SPI1       : Resetting; -- SPI1 reset (read & write)
      Reserved_4 : Bit;
      SYSCFG     : Resetting; -- System configuration controller reset (read & write)
      Reserved_5 : Bit;
      TIM9       : Resetting; -- TIM9 reset (read & write)
      TIM10      : Resetting; -- TIM10 reset (read & write)
      TIM11      : Resetting; -- TIM11 reset (read & write)
      Reserved_6 : Bits_13;
   end record with Volatile, Size => Word'Size;

   for RCC_APB2RSTR use record
      TIM1       at 0 range  0 ..  0; -- TIM1 reset (read & write)
      TIM8       at 0 range  1 ..  1; -- TIM8 reset (read & write)
      Reserved_1 at 0 range  2 ..  3;
      USART1     at 0 range  4 ..  4; -- USART1 reset (read & write)
      USART6     at 0 range  5 ..  5; -- USART6 reset (read & write)
      Reserved_2 at 0 range  6 ..  7;
      ADC        at 0 range  8 ..  8; -- ADC1 reset (read & write)
      Reserved_3 at 0 range  9 .. 10;
      SDIO       at 0 range 11 .. 11; -- SDIO reset (read & write)
      SPI1       at 0 range 12 .. 12; -- SPI1 reset (read & write)
      Reserved_4 at 0 range 13 .. 13;
      SYSCFG     at 0 range 14 .. 14; -- System configuration controller reset (read & write)
      Reserved_5 at 0 range 15 .. 15;
      TIM9       at 0 range 16 .. 16; -- TIM9 reset (read & write)
      TIM10      at 0 range 17 .. 17; -- TIM10 reset (read & write)
      TIM11      at 0 range 18 .. 18; -- TIM11 reset (read & write)
      Reserved_6 at 0 range 19 .. 31;
   end record;

   -- RCC_AHB1ENR

   type RCC_AHB1ENR is record
      GPIOA,
      GPIOB,
      GPIOC,
      GPIOD,
      GPIOE,
      GPIOF,
      GPIOG,
      GPIOH,
      GPIOI        : Enabler; -- IO port x clock enable (read & write)
      Reserved_1   : Bits_3;
      CRC          : Enabler; -- CRC clock enable (read & write)
      Reserved_2   : Bits_5;
      BKPSRAM      : Enabler; -- Backup SRAM interface clock enable (read & write)
      Reserved_3   : Bit;
      CCMDATARAM   : Enabler; -- CCM data RAM clock enable (read & write)
      DMA1         : Enabler; -- DMA1 clock enable (read & write)
      DMA2         : Enabler; -- DMA2 clock enable (read & write)
      Reserved_4   : Bits_2;
      ETHMAC       : Enabler; -- Ethernet MAC clock enable (read & write)
      ETHMACTX     : Enabler; -- Ethernet Transmission clock enable (read & write)
      ETHMACRX     : Enabler; -- Ethernet Reception clock enable (read & write)
      ETHMACPTP    : Enabler; -- Ethernet PTP clock enable (read & write)
      OTGHS        : Enabler; -- USB OTG HS clock enable (read & write)
      OTGHSULPI    : Enabler; -- USB OTG HSULPI clock enable (read & write)
      Reserved_5   : Bit;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB1ENR use record
      GPIOA         at 0 range  0 ..  0; -- IO port A clock enable (read & write)
      GPIOB         at 0 range  1 ..  1; -- IO port B clock enable (read & write)
      GPIOC         at 0 range  2 ..  2; -- IO port C clock enable (read & write)
      GPIOD         at 0 range  3 ..  3; -- IO port D clock enable (read & write)
      GPIOE         at 0 range  4 ..  4; -- IO port E clock enable (read & write)
      GPIOF         at 0 range  5 ..  5; -- IO port F clock enable (read & write)
      GPIOG         at 0 range  6 ..  6; -- IO port G clock enable (read & write)
      GPIOH         at 0 range  7 ..  7; -- IO port H clock enable (read & write)
      GPIOI         at 0 range  8 ..  8; -- IO port I clock enable (read & write)
      Reserved_1    at 0 range  9 .. 11;
      CRC           at 0 range 12 .. 12; -- CRC clock enable (read & write)
      Reserved_2    at 0 range 13 .. 17;
      BKPSRAM       at 0 range 18 .. 18; -- Backup SRAM interface clock enable (read & write)
      Reserved_3    at 0 range 19 .. 19;
      CCMDATARAM    at 0 range 20 .. 20; -- CCM data RAM clock enable (read & write)
      DMA1          at 0 range 21 .. 21; -- DMA1 clock enable (read & write)
      DMA2          at 0 range 22 .. 22; -- DMA2 clock enable (read & write)
      Reserved_4    at 0 range 23 .. 24;
      ETHMAC        at 0 range 25 .. 25; -- Ethernet MAC clock enable (read & write)
      ETHMACTX      at 0 range 26 .. 26; -- Ethernet Transmission clock enable (read & write)
      ETHMACRX      at 0 range 27 .. 27; -- Ethernet Reception clock enable (read & write)
      ETHMACPTP     at 0 range 28 .. 28; -- Ethernet PTP clock enable (read & write)
      OTGHS         at 0 range 29 .. 29; -- USB OTG HS clock enable (read & write)
      OTGHSULPI     at 0 range 30 .. 30; -- USB OTG HSULPI clock enable (read & write)
      Reserved_5    at 0 range 31 .. 31;
   end record;

   -- RCC_AHB2ENR

   type RCC_AHB2ENR is record
      DCMI       : Enabler; -- Camera interface enable (read & write)
      Reserved_1 : Bits_3;
      CRYP       : Enabler; -- Cryptographic modules clock enable (read & write)
      HASH       : Enabler; -- Hash modules clock enable (read & write)
      RNG        : Enabler; -- Random number generator clock enable (read & write)
      OTGFS      : Enabler; -- USB OTG FS clock enable (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB2ENR use record
      DCMI       at 0 range  0 ..  0; -- Camera interface enable (read & write)
      Reserved_1 at 0 range  1 ..  3;
      CRYP       at 0 range  4 ..  4; -- Cryptographic modules clock enable (read & write)
      HASH       at 0 range  5 ..  5; -- Hash modules clock enable (read & write)
      RNG        at 0 range  6 ..  6; -- Random number generator clock enable (read & write)
      OTGFS      at 0 range  7 ..  7; -- USB OTG FS clock enable (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- RCC_AHB3ENR

   type RCC_AHB3ENR is record
      FSMC     : Enabler; -- Flexible static memory controller module clock enable (read & write)
      Reserved : Bits_31;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB3ENR use record
      FSMC     at 0 range  0 ..  0; -- Flexible static memory controller module clock enable (read & write)
      Reserved at 0 range  1 .. 31;
   end record;

   -- RCC_APB1ENR

   type RCC_APB1ENR is record
      TIM2       : Enabler; -- TIM2 clock enable (read & write)
      TIM3       : Enabler; -- TIM3 clock enable (read & write)
      TIM4       : Enabler; -- TIM4 clock enable (read & write)
      TIM5       : Enabler; -- TIM5 clock enable (read & write)
      TIM6       : Enabler; -- TIM6 clock enable (read & write)
      TIM7       : Enabler; -- TIM7 clock enable (read & write)
      TIM12      : Enabler; -- TIM12 clock enable (read & write)
      TIM13      : Enabler; -- TIM13 clock enable (read & write)
      TIM14      : Enabler; -- TIM14 clock enable (read & write)
      Reserved_1 : Bits_2;
      WWDG       : Enabler; -- Window watchdog clock enable (read & write)
      Reserved_2 : Bits_2;
      SPI2       : Enabler; -- SPI2 clock enable (read & write)
      SPI3       : Enabler; -- SPI3 clock enable (read & write)
      Reserved_3 : Bit;
      USART2     : Enabler; -- USART2 clock enable (read & write)
      USART3     : Enabler; -- USART3 clock enable (read & write)
      USART4     : Enabler; -- USART4 clock enable (read & write)
      USART5     : Enabler; -- USART5 clock enable (read & write)
      I2C1       : Enabler; -- I2C1 clock enable (read & write)
      I2C2       : Enabler; -- I2C2 clock enable (read & write)
      I2C3       : Enabler; -- I2C3 clock enable (read & write)
      Reserved_4 : Bit;
      CAN1       : Enabler; -- CAN 1 clock enable (read & write)
      CAN2       : Enabler; -- CAN 2 clock enable (read & write)
      Reserved_5 : Bit;
      PWR        : Enabler; -- Power interface clock enable (read & write)
      DAC        : Enabler; -- DAC interface clock enable (read & write)
      Reserved_6 : Bits_2;
   end record with Volatile, Size => Word'Size;

   for RCC_APB1ENR use record
      TIM2       at 0 range  0 ..  0; -- TIM2 clock enable (read & write)
      TIM3       at 0 range  1 ..  1; -- TIM3 clock enable (read & write)
      TIM4       at 0 range  2 ..  2; -- TIM4 clock enable (read & write)
      TIM5       at 0 range  3 ..  3; -- TIM5 clock enable (read & write)
      TIM6       at 0 range  4 ..  4; -- TIM6 clock enable (read & write)
      TIM7       at 0 range  5 ..  5; -- TIM7 clock enable (read & write)
      TIM12      at 0 range  6 ..  6; -- TIM12 clock enable (read & write)
      TIM13      at 0 range  7 ..  7; -- TIM13 clock enable (read & write)
      TIM14      at 0 range  8 ..  8; -- TIM14 clock enable (read & write)
      Reserved_1 at 0 range  9 .. 10;
      WWDG       at 0 range 11 .. 11; -- Window watchdog clock enable (read & write)
      Reserved_2 at 0 range 12 .. 13;
      SPI2       at 0 range 14 .. 14; -- SPI2 clock enable (read & write)
      SPI3       at 0 range 15 .. 15; -- SPI3 clock enable (read & write)
      Reserved_3 at 0 range 16 .. 16;
      USART2     at 0 range 17 .. 17; -- USART2 clock enable (read & write)
      USART3     at 0 range 18 .. 18; -- USART3 clock enable (read & write)
      USART4     at 0 range 19 .. 19; -- USART4 clock enable (read & write)
      USART5     at 0 range 20 .. 20; -- USART5 clock enable (read & write)
      I2C1       at 0 range 21 .. 21; -- I2C1 clock enable (read & write)
      I2C2       at 0 range 22 .. 22; -- I2C2 clock enable (read & write)
      I2C3       at 0 range 23 .. 23; -- I2C3 clock enable (read & write)
      Reserved_4 at 0 range 24 .. 24;
      CAN1       at 0 range 25 .. 25; -- CAN 1 clock enable (read & write)
      CAN2       at 0 range 26 .. 26; -- CAN 2 clock enable (read & write)
      Reserved_5 at 0 range 27 .. 27;
      PWR        at 0 range 28 .. 28; -- Power interface clock enable (read & write)
      DAC        at 0 range 29 .. 29; -- DAC interface clock enable (read & write)
      Reserved_6 at 0 range 30 .. 31;
   end record;

   -- RCC_APB2ENR

   type RCC_APB2ENR is record
      TIM1       : Enabler; -- TIM1 clock enable (read & write)
      TIM8       : Enabler; -- TIM8 clock enable (read & write)
      Reserved_1 : Bits_2;
      USART1     : Enabler; -- USART1 clock enable (read & write)
      USART6     : Enabler; -- USART6 clock enable (read & write)
      Reserved_2 : Bits_2;
      ADC1       : Enabler; -- ADC1 clock enable (read & write)
      ADC2       : Enabler; -- ADC2 clock enable (read & write)
      ADC3       : Enabler; -- ADC3 clock enable (read & write)
      SDIO       : Enabler; -- SDIO clock enable (read & write)
      SPI1       : Enabler; -- SPI1 clock enable (read & write)
      Reserved_3 : Bit;
      SYSCFG     : Enabler; -- System configuration controller clock enable (read & write)
      Reserved_4 : Bit;
      TIM9       : Enabler; -- TIM9 clock enable (read & write)
      TIM10      : Enabler; -- TIM10 clock enable (read & write)
      TIM11      : Enabler; -- TIM11 clock enable (read & write)
      Reserved_5 : Bits_13;
   end record with Volatile, Size => Word'Size;

   for RCC_APB2ENR use record
      TIM1       at 0 range  0 ..  0; -- TIM1 clock enable (read & write)
      TIM8       at 0 range  1 ..  1; -- TIM8 clock enable (read & write)
      Reserved_1 at 0 range  2 ..  3;
      USART1     at 0 range  4 ..  4; -- USART1 clock enable (read & write)
      USART6     at 0 range  5 ..  5; -- USART6 clock enable (read & write)
      Reserved_2 at 0 range  6 ..  7;
      ADC1       at 0 range  8 ..  8; -- ADC1 clock enable (read & write)
      ADC2       at 0 range  9 ..  9; -- ADC2 clock enable (read & write)
      ADC3       at 0 range 10 .. 10; -- ADC3 clock enable (read & write)
      SDIO       at 0 range 11 .. 11; -- SDIO clock enable (read & write)
      SPI1       at 0 range 12 .. 12; -- SPI1 clock enable (read & write)
      Reserved_3 at 0 range 13 .. 13;
      SYSCFG     at 0 range 14 .. 14; -- System configuration controller clock enable (read & write)
      Reserved_4 at 0 range 15 .. 15;
      TIM9       at 0 range 16 .. 16; -- TIM9 clock enable (read & write)
      TIM10      at 0 range 17 .. 17; -- TIM10 clock enable (read & write)
      TIM11      at 0 range 18 .. 18; -- TIM11 clock enable (read & write)
      Reserved_5 at 0 range 19 .. 31;
   end record;

   -- RCC_AHB1LPENR

   type RCC_AHB1LPENR is record
      GPIOA,
      GPIOB,
      GPIOC,
      GPIOD,
      GPIOE,
      GPIOF,
      GPIOG,
      GPIOH,
      GPIOI        : Enabler; -- IO port x clock enable during Speep mode (read & write)
      Reserved_1   : Bits_3;
      CRC          : Enabler; -- CRC clock enable during Sleep mode (read & write)
      Reserved_2   : Bits_2;
      FLITF        : Enabler; -- Flash interface clock enable during Sleep mode (read & write)
      SRAM1        : Enabler; -- SRAM 1 interface clock enable during Sleep mode (read & write)
      SRAM2        : Enabler; -- SRAM 2 interface clock enable during Sleep mode (read & write)
      BKPSRAM      : Enabler; -- Backup SRAM interface clock enable during Sleep mode (read & write)
      Reserved_3   : Bit;
      CCMDATARAM   : Enabler; -- CCM data RAM clock enable during Sleep mode (read & write)
      DMA1         : Enabler; -- DMA1 clock enable during Sleep mode (read & write)
      DMA2         : Enabler; -- DMA2 clock enable during Sleep mode (read & write)
      Reserved_4   : Bits_2;
      ETHMAC       : Enabler; -- Ethernet MAC clock enable during Sleep mode (read & write)
      ETHMACTX     : Enabler; -- Ethernet Transmission clock enable during Sleep mode (read & write)
      ETHMACRX     : Enabler; -- Ethernet Reception clock enable during Sleep mode (read & write)
      ETHMACPTP    : Enabler; -- Ethernet PTP clock enable during Sleep mode (read & write)
      OTGHS        : Enabler; -- USB OTG HS clock enable during Sleep mode (read & write)
      OTGHSULPI    : Enabler; -- USB OTG HSULPI clock enable during Sleep mode (read & write)
      Reserved_5   : Bit;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB1LPENR use record
      GPIOA         at 0 range  0 ..  0; -- IO port A clock enable during Sleep mode (read & write)
      GPIOB         at 0 range  1 ..  1; -- IO port B clock enable during Sleep mode (read & write)
      GPIOC         at 0 range  2 ..  2; -- IO port C clock enable during Sleep mode (read & write)
      GPIOD         at 0 range  3 ..  3; -- IO port D clock enable during Sleep mode (read & write)
      GPIOE         at 0 range  4 ..  4; -- IO port E clock enable during Sleep mode (read & write)
      GPIOF         at 0 range  5 ..  5; -- IO port F clock enable during Sleep mode (read & write)
      GPIOG         at 0 range  6 ..  6; -- IO port G clock enable during Sleep mode (read & write)
      GPIOH         at 0 range  7 ..  7; -- IO port H clock enable during Sleep mode (read & write)
      GPIOI         at 0 range  8 ..  8; -- IO port I clock enable during Sleep mode (read & write)
      Reserved_1    at 0 range  9 .. 11;
      CRC           at 0 range 12 .. 12; -- CRC clock enable during Sleep mode (read & write)
      Reserved_2    at 0 range 13 .. 14;
      FLITF         at 0 range 15 .. 15; -- Flash interface clock enable during Sleep mode (read & write)
      SRAM1         at 0 range 16 .. 16; -- SRAM 1 interface clock enable during Sleep mode (read & write)
      SRAM2         at 0 range 17 .. 17; -- SRAM 2 interface clock enable during Sleep mode (read & write)
      BKPSRAM       at 0 range 18 .. 18; -- Backup SRAM interface clock enable during Sleep mode (read & write)
      Reserved_3    at 0 range 19 .. 19;
      CCMDATARAM    at 0 range 20 .. 20; -- CCM data RAM clock enable during Sleep mode (read & write)
      DMA1          at 0 range 21 .. 21; -- DMA1 clock enable during Sleep mode (read & write)
      DMA2          at 0 range 22 .. 22; -- DMA2 clock enable during Sleep mode (read & write)
      Reserved_4    at 0 range 23 .. 24;
      ETHMAC        at 0 range 25 .. 25; -- Ethernet MAC clock enable during Sleep mode (read & write)
      ETHMACTX      at 0 range 26 .. 26; -- Ethernet Transmission clock enable during Sleep mode (read & write)
      ETHMACRX      at 0 range 27 .. 27; -- Ethernet Reception clock enable during Sleep mode (read & write)
      ETHMACPTP     at 0 range 28 .. 28; -- Ethernet PTP clock enable during Sleep mode (read & write)
      OTGHS         at 0 range 29 .. 29; -- USB OTG HS clock enable during Sleep mode (read & write)
      OTGHSULPI     at 0 range 30 .. 30; -- USB OTG HSULPI clock enable during Sleep mode (read & write)
      Reserved_5    at 0 range 31 .. 31;
   end record;

   -- RCC_AHB2LPENR

   type RCC_AHB2LPENR is record
      DCMI       : Enabler; -- Camera interface enable during Sleep mode (read & write)
      Reserved_1 : Bits_3;
      CRYP       : Enabler; -- Cryptographic modules clock enable during Sleep mode (read & write)
      HASH       : Enabler; -- Hash modules clock enable during Sleep mode (read & write)
      RNG        : Enabler; -- Random number generator clock enable during Sleep mode (read & write)
      OTGFS      : Enabler; -- USB OTG FS clock enable during Sleep mode (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB2LPENR use record
      DCMI       at 0 range  0 ..  0; -- Camera interface enable during Sleep mode (read & write)
      Reserved_1 at 0 range  1 ..  3;
      CRYP       at 0 range  4 ..  4; -- Cryptographic modules clock enable during Sleep mode (read & write)
      HASH       at 0 range  5 ..  5; -- Hash modules clock enable during Sleep mode (read & write)
      RNG        at 0 range  6 ..  6; -- Random number generator clock enable during Sleep mode (read & write)
      OTGFS      at 0 range  7 ..  7; -- USB OTG FS clock enable during Sleep mode (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- RCC_AHB3LPENR

   type RCC_AHB3LPENR is record
      FSMC     : Enabler; -- Flexible static memory controller module clock enable during Sleep mode (read & write)
      Reserved : Bits_31;
   end record with Volatile, Size => Word'Size;

   for RCC_AHB3LPENR use record
      FSMC     at 0 range  0 ..  0; -- Flexible static memory controller module clock enable during Sleep mode (read & write)
      Reserved at 0 range  1 .. 31;
   end record;

   -- RCC_APB1LPENR

   type RCC_APB1LPENR is record
      TIM2       : Enabler; -- TIM2 clock enable during Sleep mode  (read & write)
      TIM3       : Enabler; -- TIM3 clock enable during Sleep mode (read & write)
      TIM4       : Enabler; -- TIM4 clock enable during Sleep mode (read & write)
      TIM5       : Enabler; -- TIM5 clock enable during Sleep mode (read & write)
      TIM6       : Enabler; -- TIM6 clock enable during Sleep mode (read & write)
      TIM7       : Enabler; -- TIM7 clock enable during Sleep mode (read & write)
      TIM12      : Enabler; -- TIM12 clock enable during Sleep mode (read & write)
      TIM13      : Enabler; -- TIM13 clock enable during Sleep mode (read & write)
      TIM14      : Enabler; -- TIM14 clock enable during Sleep mode (read & write)
      Reserved_1 : Bits_2;
      WWDG       : Enabler; -- Window watchdog clock enable during Sleep mode (read & write)
      Reserved_2 : Bits_2;
      SPI2       : Enabler; -- SPI2 clock enable during Sleep mode (read & write)
      SPI3       : Enabler; -- SPI3 clock enable during Sleep mode (read & write)
      Reserved_3 : Bit;
      USART2     : Enabler; -- USART2 clock enable during Sleep mode (read & write)
      USART3     : Enabler; -- USART3 clock enable during Sleep mode (read & write)
      USART4     : Enabler; -- USART4 clock enable during Sleep mode (read & write)
      USART5     : Enabler; -- USART5 clock enable during Sleep mode (read & write)
      I2C1       : Enabler; -- I2C1 clock enable during Sleep mode (read & write)
      I2C2       : Enabler; -- I2C2 clock enable during Sleep mode (read & write)
      I2C3       : Enabler; -- I2C3 clock enable during Sleep mode (read & write)
      Reserved_4 : Bit;
      CAN1       : Enabler; -- CAN 1 clock enable during Sleep mode (read & write)
      CAN2       : Enabler; -- CAN 2 clock enable during Sleep mode (read & write)
      Reserved_5 : Bit;
      PWR        : Enabler; -- Power interface clock enable during Sleep mode (read & write)
      DAC        : Enabler; -- DAC interface clock enable during Sleep mode (read & write)
      Reserved_6 : Bits_2;
   end record with Volatile, Size => Word'Size;

   for RCC_APB1LPENR use record
      TIM2       at 0 range  0 ..  0; -- TIM2 clock enable during Sleep mode (read & write)
      TIM3       at 0 range  1 ..  1; -- TIM3 clock enable during Sleep mode (read & write)
      TIM4       at 0 range  2 ..  2; -- TIM4 clock enable during Sleep mode (read & write)
      TIM5       at 0 range  3 ..  3; -- TIM5 clock enable during Sleep mode (read & write)
      TIM6       at 0 range  4 ..  4; -- TIM6 clock enable during Sleep mode (read & write)
      TIM7       at 0 range  5 ..  5; -- TIM7 clock enable during Sleep mode (read & write)
      TIM12      at 0 range  6 ..  6; -- TIM12 clock enable during Sleep mode (read & write)
      TIM13      at 0 range  7 ..  7; -- TIM13 clock enable during Sleep mode (read & write)
      TIM14      at 0 range  8 ..  8; -- TIM14 clock enable during Sleep mode (read & write)
      Reserved_1 at 0 range  9 .. 10;
      WWDG       at 0 range 11 .. 11; -- Window watchdog clock enable during Sleep mode (read & write)
      Reserved_2 at 0 range 12 .. 13;
      SPI2       at 0 range 14 .. 14; -- SPI2 clock enable during Sleep mode (read & write)
      SPI3       at 0 range 15 .. 15; -- SPI3 clock enable during Sleep mode (read & write)
      Reserved_3 at 0 range 16 .. 16;
      USART2     at 0 range 17 .. 17; -- USART2 clock enable during Sleep mode (read & write)
      USART3     at 0 range 18 .. 18; -- USART3 clock enable during Sleep mode (read & write)
      USART4     at 0 range 19 .. 19; -- USART4 clock enable during Sleep mode (read & write)
      USART5     at 0 range 20 .. 20; -- USART5 clock enable during Sleep mode (read & write)
      I2C1       at 0 range 21 .. 21; -- I2C1 clock enable during Sleep mode (read & write)
      I2C2       at 0 range 22 .. 22; -- I2C2 clock enable during Sleep mode (read & write)
      I2C3       at 0 range 23 .. 23; -- I2C3 clock enable during Sleep mode (read & write)
      Reserved_4 at 0 range 24 .. 24;
      CAN1       at 0 range 25 .. 25; -- CAN 1 clock enable during Sleep mode (read & write)
      CAN2       at 0 range 26 .. 26; -- CAN 2 clock enable during Sleep mode (read & write)
      Reserved_5 at 0 range 27 .. 27;
      PWR        at 0 range 28 .. 28; -- Power interface clock enable during Sleep mode (read & write)
      DAC        at 0 range 29 .. 29; -- DAC interface clock enable during Sleep mode (read & write)
      Reserved_6 at 0 range 30 .. 31;
   end record;

   -- RCC_APB2LPENR

   type RCC_APB2LPENR is record
      TIM1       : Enabler; -- TIM1 clock enable during Sleep mode (read & write)
      TIM8       : Enabler; -- TIM8 clock enable during Sleep mode (read & write)
      Reserved_1 : Bits_2;
      USART1     : Enabler; -- USART1 clock enable during Sleep mode (read & write)
      USART6     : Enabler; -- USART6 clock enable during Sleep mode (read & write)
      Reserved_2 : Bits_2;
      ADC1       : Enabler; -- ADC1 clock enable during Sleep mode (read & write)
      ADC2       : Enabler; -- ADC2 clock enable during Sleep mode (read & write)
      ADC3       : Enabler; -- ADC3 clock enable during Sleep mode (read & write)
      SDIO       : Enabler; -- SDIO clock enable during Sleep mode (read & write)
      SPI1       : Enabler; -- SPI1 clock enable during Sleep mode (read & write)
      Reserved_3 : Bit;
      SYSCFG     : Enabler; -- System configuration controller clock enable during Sleep mode (read & write)
      Reserved_4 : Bit;
      TIM9       : Enabler; -- TIM9 clock enable during Sleep mode (read & write)
      TIM10      : Enabler; -- TIM10 clock enable during Sleep mode (read & write)
      TIM11      : Enabler; -- TIM11 clock enable during Sleep mode (read & write)
      Reserved_5 : Bits_13;
   end record with Volatile, Size => Word'Size;

   for RCC_APB2LPENR use record
      TIM1       at 0 range  0 ..  0; -- TIM1 clock enable during Sleep mode (read & write)
      TIM8       at 0 range  1 ..  1; -- TIM8 clock enable during Sleep mode (read & write)
      Reserved_1 at 0 range  2 ..  3;
      USART1     at 0 range  4 ..  4; -- USART1 clock enable during Sleep mode (read & write)
      USART6     at 0 range  5 ..  5; -- USART6 clock enable during Sleep mode (read & write)
      Reserved_2 at 0 range  6 ..  7;
      ADC1       at 0 range  8 ..  8; -- ADC1 clock enable during Sleep mode (read & write)
      ADC2       at 0 range  9 ..  9; -- ADC2 clock enable during Sleep mode (read & write)
      ADC3       at 0 range 10 .. 10; -- ADC3 clock enable during Sleep mode (read & write)
      SDIO       at 0 range 11 .. 11; -- SDIO clock enable during Sleep mode (read & write)
      SPI1       at 0 range 12 .. 12; -- SPI1 clock enable during Sleep mode (read & write)
      Reserved_3 at 0 range 13 .. 13;
      SYSCFG     at 0 range 14 .. 14; -- System configuration controller clock enable during Sleep mode (read & write)
      Reserved_4 at 0 range 15 .. 15;
      TIM9       at 0 range 16 .. 16; -- TIM9 clock enable during Sleep mode (read & write)
      TIM10      at 0 range 17 .. 17; -- TIM10 clock enable during Sleep mode (read & write)
      TIM11      at 0 range 18 .. 18; -- TIM11 clock enable during Sleep mode (read & write)
      Reserved_5 at 0 range 19 .. 31;
   end record;

   -- RCC_BDCR

   for RTCSEL_Options use (No_Clock => 0,
                           LSE      => 1,
                           LSI      => 2,
                           HSE      => 3);

   type RCC_BDCR is record
      LSEON      : Enabler;        -- External low-speed oscillator enable (read & write)
      LSERDY     : Readiness;      -- External low-speed oscillator ready (read only)
      LSEBYP     : Bypass;         -- External low-speed oscillator bypass (read & write)
      Reserved_1 : Bits_5;
      RTCSEL     : RTCSEL_Options; -- RTC clock source selection (read & write)
      Reserved_2 : Bits_5;
      RTCEN      : Enabler;        -- RTC clock enable (read & write)
      BDRST      : Resetting;      -- Backup domain software reset (read & write)
      Reserved_3 : Bits_15;
   end record with Volatile, Size => Word'Size;

   for RCC_BDCR use record
      LSEON      at 0 range  0 ..  0; -- External low-speed oscillator enable (read & write)
      LSERDY     at 0 range  1 ..  1; -- External low-speed oscillator ready (read only)
      LSEBYP     at 0 range  2 ..  2; -- External low-speed oscillator bypass (read & write)
      Reserved_1 at 0 range  3 ..  7;
      RTCSEL     at 0 range  8 ..  9; -- RTC clock source selection (read & write)
      Reserved_2 at 0 range 10 .. 14;
      RTCEN      at 0 range 15 .. 15; -- RTC clock enable (read & write)
      BDRST      at 0 range 16 .. 16; -- Backup domain software reset (read & write)
      Reserved_3 at 0 range 17 .. 31;
   end record;

   -- RCC_CSR

   type RCC_CSR is record
      LSION    : Enabler;   -- Internal low-speed oscillator enable (read & write)
      LSIRDY   : Readiness; -- Internal low-speed oscillator ready (read only)
      Reserved : Bits_22;
      RMVF     : Clearing;  -- Remove reset flag (read & clear on write)
      BORRSTF  : Occurance; -- BOR reset flag (read only)
      PINRSTF  : Occurance; -- PIN reset flag (read only)
      PORRSTF  : Occurance; -- POR/PDR reset flag (read only)
      SFTRSTF  : Occurance; -- Software reset flag (read only)
      IWDGRSTF : Occurance; -- Independent watchdog reset flag (read only)
      WWDGRSTF : Occurance; -- Window watchdog reset flag (read only)
      LPWRRSTF : Occurance; -- Low-power reset flag (read only)
   end record with Volatile, Size => Word'Size;

   for RCC_CSR use record
      LSION    at 0 range  0 ..  0; -- Internal low-speed oscillator enable (read & write)
      LSIRDY   at 0 range  1 ..  1; -- Internal low-speed oscillator ready (read only)
      Reserved at 0 range  2 .. 23;
      RMVF     at 0 range 24 .. 24; -- Remove reset flag (read & clear on write)
      BORRSTF  at 0 range 25 .. 25; -- BOR reset flag (read only)
      PINRSTF  at 0 range 26 .. 26; -- PIN reset flag (read only)
      PORRSTF  at 0 range 27 .. 27; -- POR/PDR reset flag (read only)
      SFTRSTF  at 0 range 28 .. 28; -- Software reset flag (read only)
      IWDGRSTF at 0 range 29 .. 29; -- Independent watchdog reset flag (read only)
      WWDGRSTF at 0 range 30 .. 30; -- Window watchdog reset flag (read only)
      LPWRRSTF at 0 range 31 .. 31; -- Low-power reset flag (read only)
   end record;

   -- RCC_SSCGR

   for SPREADSEL_Options use (Center => 0, Down => 1);

   type RCC_SSCGR is record
      MODPER    : Bits_13;           -- Modulation period (read & write)
      INCSTEP   : Bits_15;           -- Incrementation step (read & write)
      Reserved  : Bits_2;
      SPREADSEL : SPREADSEL_Options; -- Spread Select (read & write)
      SSCGEN    : Enabler;           -- Spread spectrum modulation enable (read & write)
   end record with Volatile, Size => Word'Size;

   for RCC_SSCGR use record
      MODPER    at 0 range  0 .. 12; -- Modulation period (read & write)
      INCSTEP   at 0 range 13 .. 27; -- Incrementation step (read & write)
      Reserved  at 0 range 28 .. 29;
      SPREADSEL at 0 range 30 .. 30; -- Spread Select (read & write)
      SSCGEN    at 0 range 31 .. 31; -- Spread spectrum modulation enable (read & write)
   end record;

   -- RCC_PLLI2SCFGR

   type RCC_PLLI2SCFGR is record
      Reserved_1 : Bits_6;
      PLLI2SN    : PLLN_Range;   -- PLLI2S multiplication factor for VCO (read & write)
      Reserved_2 : Bits_13;
      PLLI2SR    : PLLI2S_Range; -- PLLI2S division factor for I2S clocks (read & write)
      Reserved_3 : Bit;
   end record with Volatile, Size => Word'Size;

   for RCC_PLLI2SCFGR use record
      Reserved_1 at 0 range  0 ..  5;
      PLLI2SN    at 0 range  6 .. 14; -- PLLI2S multiplication factor for VCO (read & write)
      Reserved_2 at 0 range 15 .. 27;
      PLLI2SR    at 0 range 28 .. 30; -- PLLI2S division factor for I2S clocks (read & write)
      Reserved_3 at 0 range 31 .. 31;
   end record;

   -- RCC_Register

   type RCC_Register is record
      CR          : RCC_CR;         --  Clock control
      PLLCFGR     : RCC_PLLCFGR;    --  PLL configuration
      CFGR        : RCC_CFGR;       --  Clock configuration
      CIR         : RCC_CIR;        --  Clock interrupt
      AHB1RSTR    : RCC_AHB1RSTR;   --  AHB1 peripheral reset
      AHB2RSTR    : RCC_AHB2RSTR;   --  AHB2 peripheral reset
      AHB3RSTR    : RCC_AHB3RSTR;   --  AHB3 peripheral reset
      Reserved_1  : Word;
      APB1RSTR    : RCC_APB1RSTR;   --  APB1 peripheral reset
      APB2RSTR    : RCC_APB2RSTR;   --  APB2 peripheral reset
      Reserved_2  : Word;
      Reserved_3  : Word;
      AHB1ENR     : RCC_AHB1ENR;    --  AHB1 peripheral clock enable
      AHB2ENR     : RCC_AHB2ENR;    --  AHB2 peripheral clock enable
      AHB3ENR     : RCC_AHB3ENR;    --  AHB3 peripheral clock enable
      Reserved_4  : Word;
      APB1ENR     : RCC_APB1ENR;    --  APB1 peripheral clock enable
      APB2ENR     : RCC_APB2ENR;    --  APB2 peripheral clock enable
      Reserved_5  : Word;
      Reserved_6  : Word;
      AHB1LPENR   : RCC_AHB1LPENR;  --  AHB1 peripheral clock enable in low power mode
      AHB2LPENR   : RCC_AHB2LPENR;  --  AHB2 peripheral clock enable in low power mode
      AHB3LPENR   : RCC_AHB3LPENR;  --  AHB3 peripheral clock enable in low power mode
      Reserved_7  : Word;
      APB1LPENR   : RCC_APB1LPENR;  --  APB1 peripheral clock enable in low power mode
      APB2LPENR   : RCC_APB2LPENR;  --  APB2 peripheral clock enable in low power mode
      Reserved_8  : Word;
      Reserved_9  : Word;
      BDCR        : RCC_BDCR;       --  Backup domain control
      CSR         : RCC_CSR;        --  Clock control & status
      Reserved_10 : Word;
      Reserved_11 : Word;
      SSCGR       : RCC_SSCGR;      --  Spread spectrum clock generation register
      PLLI2SCFGR  : RCC_PLLI2SCFGR; --  PLLI2S configuration
   end record with Volatile, Size => Byte'Size * 16#88#;

   for RCC_Register use record
      CR          at 16#00# range 0 .. 31;
      PLLCFGR     at 16#04# range 0 .. 31;
      CFGR        at 16#08# range 0 .. 31;
      CIR         at 16#0C# range 0 .. 31;
      AHB1RSTR    at 16#10# range 0 .. 31;
      AHB2RSTR    at 16#14# range 0 .. 31;
      AHB3RSTR    at 16#18# range 0 .. 31;
      Reserved_1  at 16#1C# range 0 .. 31;
      APB1RSTR    at 16#20# range 0 .. 31;
      APB2RSTR    at 16#24# range 0 .. 31;
      Reserved_2  at 16#28# range 0 .. 31;
      Reserved_3  at 16#2C# range 0 .. 31;
      AHB1ENR     at 16#30# range 0 .. 31;
      AHB2ENR     at 16#34# range 0 .. 31;
      AHB3ENR     at 16#38# range 0 .. 31;
      Reserved_4  at 16#3C# range 0 .. 31;
      APB1ENR     at 16#40# range 0 .. 31;
      APB2ENR     at 16#44# range 0 .. 31;
      Reserved_5  at 16#48# range 0 .. 31;
      Reserved_6  at 16#4C# range 0 .. 31;
      AHB1LPENR   at 16#50# range 0 .. 31;
      AHB2LPENR   at 16#54# range 0 .. 31;
      AHB3LPENR   at 16#58# range 0 .. 31;
      Reserved_7  at 16#5C# range 0 .. 31;
      APB1LPENR   at 16#60# range 0 .. 31;
      APB2LPENR   at 16#64# range 0 .. 31;
      Reserved_8  at 16#68# range 0 .. 31;
      Reserved_9  at 16#6C# range 0 .. 31;
      BDCR        at 16#70# range 0 .. 31;
      CSR         at 16#74# range 0 .. 31;
      Reserved_10 at 16#78# range 0 .. 31;
      Reserved_11 at 16#7C# range 0 .. 31;
      SSCGR       at 16#80# range 0 .. 31;
      PLLI2SCFGR  at 16#84# range 0 .. 31;
   end record;

   -- Set register address

   RCC : RCC_Register with
     Volatile, Address => System'To_Address (Base_RCC), Import;

end STM32F4.Reset_and_clock_control;

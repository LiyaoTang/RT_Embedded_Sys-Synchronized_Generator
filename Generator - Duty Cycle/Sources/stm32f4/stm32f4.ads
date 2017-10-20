--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

-- STM32F4xx - Target controller: STM32F407VGT6

package STM32F4 is

   -- Bits are mostly used as fillers for data structures,
   -- yet sometimes also as actual values - especially Bit.

   type Bit     is mod 2 **  1 with Size =>  1;
   type Bits_2  is mod 2 **  2 with Size =>  2;
   type Bits_3  is mod 2 **  3 with Size =>  3;
   type Bits_4  is mod 2 **  4 with Size =>  4;
   type Bits_5  is mod 2 **  5 with Size =>  5;
   type Bits_6  is mod 2 **  6 with Size =>  6;
   type Bits_7  is mod 2 **  7 with Size =>  7;
   type Bits_8  is mod 2 **  8 with Size =>  8;
   type Bits_9  is mod 2 **  9 with Size =>  9;
   type Bits_10 is mod 2 ** 10 with Size => 10;
   type Bits_11 is mod 2 ** 11 with Size => 11;
   type Bits_12 is mod 2 ** 12 with Size => 12;
   type Bits_13 is mod 2 ** 13 with Size => 13;
   type Bits_14 is mod 2 ** 14 with Size => 14;
   type Bits_15 is mod 2 ** 15 with Size => 15;
   type Bits_16 is mod 2 ** 16 with Size => 16;
   type Bits_17 is mod 2 ** 17 with Size => 17;
   type Bits_18 is mod 2 ** 18 with Size => 18;
   type Bits_19 is mod 2 ** 19 with Size => 19;
   type Bits_20 is mod 2 ** 20 with Size => 20;
   type Bits_21 is mod 2 ** 21 with Size => 21;
   type Bits_22 is mod 2 ** 22 with Size => 22;
   type Bits_23 is mod 2 ** 23 with Size => 23;
   type Bits_24 is mod 2 ** 24 with Size => 24;
   type Bits_25 is mod 2 ** 25 with Size => 25;
   type Bits_26 is mod 2 ** 26 with Size => 26;
   type Bits_27 is mod 2 ** 27 with Size => 27;
   type Bits_28 is mod 2 ** 28 with Size => 28;
   type Bits_29 is mod 2 ** 29 with Size => 29;
   type Bits_30 is mod 2 ** 30 with Size => 30;
   type Bits_31 is mod 2 ** 31 with Size => 31;
   type Bits_32 is mod 2 ** 32 with Size => 32;
   type Bits_64 is mod 2 ** 64 with Size => 64;

   -- Bit groups.

   subtype Word      is Bits_32;
   subtype Half_Word is Bits_16;
   subtype Byte      is Bits_8;

   -- Bit_Ranges are used when subtype compatibility is required.

   subtype Bit_Range is Natural;

   -- Arrange as array of Bits.

   type Bit_Array is array (Bit_Range range <>) of Bit with Pack;

   -- Two value types and arrays for all occasions.

   type Enabler   is (Disable,      Enable)        with Size => 1;
   type Disabler  is (Enable,       Disable)       with Size => 1;
   type Switch    is (Off,          On)            with Size => 1;
   type Readiness is (Not_Ready,    Ready)         with Size => 1;
   type Bypass    is (Not_Bypassed, Bypassed)      with Size => 1;
   type Lock      is (Unlocked,     Locked)        with Size => 1;
   type Mask      is (Masked,       Unmasked)      with Size => 1;
   type Event     is (Clear,        Trigger)       with Size => 1;
   type Pending   is (None,         Occured_Clear) with Size => 1;
   type Direction is (Up,           Down)          with Size => 1;
   type Buffering is (Not_Buffered, Buffered)      with Size => 1;
   type Inverter  is (Not_Inverted, Inverted)      with Size => 1;
   type Occurance is (Not_Occured,  Occured)       with Size => 1;
   type Generator is (No_Action,    Generate)      with Size => 1;
   type Polarity  is (Active_Low,   Active_High)   with Size => 1;
   type Resetting is (Do_Nothing,   Reset)         with Size => 1;
   type Setting   is (Do_Nothing,   Set)           with Size => 1;
   type Flagging  is (No_Flag,      Flag)          with Size => 1;
   type Clearing  is (Do_Nothing,   Clear)         with Size => 1;
   type Status    is (Ok,           Faulty)        with Size => 1;
   type Detection is (Not_Detected, Detected)      with Size => 1;
   type Sending   is (Do_Nothing,   Send)          with Size => 1;
   type Muting    is (Active,       Muted)         with Size => 1;
   type Parity    is (Even,         Odd)           with Size => 1;
   type Completed is (Not_Complete, Complete)      with Size => 1;
   type Starter   is (Not_Started,  Started)       with Size => 1;

   type Enabler_Array   is array (Bit_Range range <>) of Enabler   with Pack;
   type Disabler_Array  is array (Bit_Range range <>) of Disabler  with Pack;
   type Switch_Array    is array (Bit_Range range <>) of Switch    with Pack;
   type Readiness_Array is array (Bit_Range range <>) of Readiness with Pack;
   type Bypass_Array    is array (Bit_Range range <>) of Bypass    with Pack;
   type Lock_Array      is array (Bit_Range range <>) of Lock      with Pack;
   type Mask_Array      is array (Bit_Range range <>) of Mask      with Pack;
   type Event_Array     is array (Bit_Range range <>) of Event     with Pack;
   type Pending_Array   is array (Bit_Range range <>) of Pending   with Pack;
   type Direction_Array is array (Bit_Range range <>) of Direction with Pack;
   type Buffering_Array is array (Bit_Range range <>) of Buffering with Pack;
   type Inverter_Array  is array (Bit_Range range <>) of Inverter  with Pack;
   type Occurance_Array is array (Bit_Range range <>) of Occurance with Pack;
   type Generator_Array is array (Bit_Range range <>) of Generator with Pack;
   type Polarity_Array  is array (Bit_Range range <>) of Polarity  with Pack;
   type Resetting_Array is array (Bit_Range range <>) of Resetting with Pack;
   type Setting_Array   is array (Bit_Range range <>) of Setting   with Pack;
   type Flagging_Array  is array (Bit_Range range <>) of Flagging  with Pack;
   type Clearing_Array  is array (Bit_Range range <>) of Clearing  with Pack;
   type Status_Array    is array (Bit_Range range <>) of Status    with Pack;
   type Detection_Array is array (Bit_Range range <>) of Detection with Pack;
   type Sending_Array   is array (Bit_Range range <>) of Sending   with Pack;
   type Muting_Array    is array (Bit_Range range <>) of Muting    with Pack;
   type Parity_Array    is array (Bit_Range range <>) of Parity    with Pack;
   type Completed_Array is array (Bit_Range range <>) of Completed with Pack;
   type Starter_Array   is array (Bit_Range range <>) of Starter   with Pack;

private

   for Enabler   use (Disable      => 0, Enable        => 1);
   for Disabler  use (Enable       => 0, Disable       => 1);
   for Switch    use (Off          => 0, On            => 1);
   for Readiness use (Not_Ready    => 0, Ready         => 1);
   for Bypass    use (Not_Bypassed => 0, Bypassed      => 1);
   for Lock      use (Unlocked     => 0, Locked        => 1);
   for Mask      use (Masked       => 0, Unmasked      => 1);
   for Event     use (Clear        => 0, Trigger       => 1);
   for Pending   use (None         => 0, Occured_Clear => 1);
   for Direction use (Up           => 0, Down          => 1);
   for Buffering use (Not_Buffered => 0, Buffered      => 1);
   for Inverter  use (Not_Inverted => 0, Inverted      => 1);
   for Occurance use (Not_Occured  => 0, Occured       => 1);
   for Generator use (No_Action    => 0, Generate      => 1);
   for Polarity  use (Active_Low   => 0, Active_High   => 1);
   for Resetting use (Do_Nothing   => 0, Reset         => 1);
   for Setting   use (Do_Nothing   => 0, Set           => 1);
   for Flagging  use (No_Flag      => 0, Flag          => 1);
   for Clearing  use (Do_Nothing   => 0, Clear         => 1);
   for Status    use (Ok           => 0, Faulty        => 1);
   for Detection use (Not_Detected => 0, Detected      => 1);
   for Sending   use (Do_Nothing   => 0, Send          => 1);
   for Muting    use (Active       => 0, Muted         => 1);
   for Parity    use (Even         => 0, Odd           => 1);
   for Completed use (Not_Complete => 0, Complete      => 1);
   for Starter   use (Not_Started  => 0, Started       => 1);

   --  Register base addresses for STM32F40x

   Base_Peripheral : constant := 16#4000_0000#;

   -- Advanced Peripheral Bus

   Base_APB1 : constant := Base_Peripheral;
   Base_APB2 : constant := Base_Peripheral + 16#0001_0000#;

   -- Advanced High-performance Bus

   Base_AHB1      : constant := Base_Peripheral + 16#0002_0000#;
   Base_AHB2      : constant := Base_Peripheral + 16#1000_0000#;
   Base_AHB3      : constant := Base_Peripheral + 16#2000_0000#;
   Base_Cortex_M4 : constant := Base_Peripheral + 16#A000_0000#;

   -- APB1 Advanced Peripheral Bus 1

   Base_TIM2            : constant := Base_APB1 + 16#0000#;
   Base_TIM3            : constant := Base_APB1 + 16#0400#;
   Base_TIM4            : constant := Base_APB1 + 16#0800#;
   Base_TIM5            : constant := Base_APB1 + 16#0C00#;
   Base_TIM6            : constant := Base_APB1 + 16#1000#;
   Base_TIM7            : constant := Base_APB1 + 16#1400#;
   Base_TIM12           : constant := Base_APB1 + 16#1800#;
   Base_TIM13           : constant := Base_APB1 + 16#1C00#;
   Base_TIM14           : constant := Base_APB1 + 16#2000#;
   Base_RTC_BKP         : constant := Base_APB1 + 16#2800#;
   Base_WWDG            : constant := Base_APB1 + 16#2C00#;
   Base_IWDG            : constant := Base_APB1 + 16#3000#;
   Base_I2S2ext         : constant := Base_APB1 + 16#3400#;
   Base_SPI2_I2S2       : constant := Base_APB1 + 16#3800#;
   Base_SPI3_I2S3       : constant := Base_APB1 + 16#3C00#;
   Base_I2S3ext         : constant := Base_APB1 + 16#4000#;
   Base_USART2          : constant := Base_APB1 + 16#4400#;
   Base_USART3          : constant := Base_APB1 + 16#4800#;
   Base_UART4           : constant := Base_APB1 + 16#4C00#;
   Base_UART5           : constant := Base_APB1 + 16#5000#;
   Base_I2C1            : constant := Base_APB1 + 16#5400#;
   Base_I2C2            : constant := Base_APB1 + 16#5800#;
   Base_I2C3            : constant := Base_APB1 + 16#5C00#;
   Base_CAN1            : constant := Base_APB1 + 16#6400#;
   Base_CAN2            : constant := Base_APB1 + 16#6800#;
   Base_PWR             : constant := Base_APB1 + 16#7000#;
   Base_DAC             : constant := Base_APB1 + 16#7400#;

   -- APB2 Advanced Peripheral Bus 2

   Base_TIM1            : constant := Base_APB2 + 16#0000#;
   Base_TIM8            : constant := Base_APB2 + 16#0400#;
   Base_USART1          : constant := Base_APB2 + 16#1000#;
   Base_USART6          : constant := Base_APB2 + 16#1400#;
   Base_ADC123          : constant := Base_APB2 + 16#2000#;
   Base_SDIO            : constant := Base_APB2 + 16#2C00#;
   Base_SPI1            : constant := Base_APB2 + 16#3000#;
   Base_SYSCFG          : constant := Base_APB2 + 16#3800#;
   Base_EXTI            : constant := Base_APB2 + 16#3C00#;
   Base_TIM9            : constant := Base_APB2 + 16#4000#;
   Base_TIM10           : constant := Base_APB2 + 16#4400#;
   Base_TIM11           : constant := Base_APB2 + 16#4800#;

   -- AHB1 Advanced High-performance Bus 1

   Base_GPIOA           : constant := Base_AHB1 + 16#0000#;
   Base_GPIOB           : constant := Base_AHB1 + 16#0400#;
   Base_GPIOC           : constant := Base_AHB1 + 16#0800#;
   Base_GPIOD           : constant := Base_AHB1 + 16#0C00#;
   Base_GPIOE           : constant := Base_AHB1 + 16#1000#;
   Base_GPIOF           : constant := Base_AHB1 + 16#1400#;
   Base_GPIOG           : constant := Base_AHB1 + 16#1800#;
   Base_GPIOH           : constant := Base_AHB1 + 16#1C00#;
   Base_GPIOI           : constant := Base_AHB1 + 16#2000#;
   Base_CRC             : constant := Base_AHB1 + 16#3000#;
   Base_RCC             : constant := Base_AHB1 + 16#3800#;
   Base_FLASH           : constant := Base_AHB1 + 16#3C00#;
   Base_BKPSRAM         : constant := Base_AHB1 + 16#4000#;
   Base_DMA1            : constant := Base_AHB1 + 16#6000#;
   Base_DMA2            : constant := Base_AHB1 + 16#6400#;
   Base_ETHERNET_MAC    : constant := Base_AHB1 + 16#8000#;
   Base_USB_OTG_HS      : constant := Base_AHB1 + 16#0002_0000#;

   -- AHB2 Advanced High-performance Bus 2

   Base_USB_OTG_FS      : constant := Base_AHB2 + 16#0000_0000#;
   Base_DCMI            : constant := Base_AHB2 + 16#0005_0000#;
   Base_CRYP            : constant := Base_AHB2 + 16#0006_0000#;
   Base_HASH            : constant := Base_AHB2 + 16#0006_0400#;
   Base_RNG             : constant := Base_AHB2 + 16#0006_0800#;

   -- AHB3 Advanced High-performance Bus 3

   Base_FSMC_bank_1     : constant := Base_AHB3 + 16#0000_0000#;
   Base_FSMC_bank_2     : constant := Base_AHB3 + 16#1000_0000#;
   Base_FSMC_bank_3     : constant := Base_AHB3 + 16#2000_0000#;
   Base_FSMC_bank_4     : constant := Base_AHB3 + 16#3000_0000#;
   Base_FSMC_control    : constant := Base_AHB3 + 16#4000_0000#;

end STM32F4;

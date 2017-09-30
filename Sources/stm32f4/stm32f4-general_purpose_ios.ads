--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx General purpose IOs

package STM32F4.General_purpose_IOs is

   -- General purpose IO enumerations and port/pin structures

   type    GPIO_Ports is (A, B, C, D, E, F, G, H, I) with Size => 4;
   subtype GPIO_Pins  is Natural range 0 .. 15;

   for GPIO_Ports use (A => 0,
                       B => 1,
                       C => 2,
                       D => 3,
                       E => 4,
                       F => 5,
                       G => 6,
                       H => 7,
                       I => 8);

   type Port_Pin is record
      Port : GPIO_Ports;
      Pin  : GPIO_Pins;
   end record;

   type Port_Pins is array (Natural range <>) of Port_Pin;

   -- Mode options

   type MODER_Options is (Input,                  -- Floating or pull-up or pull-down
                          General_purpose_output, -- Open-drain or push-pull with potential pull-up or pull-down
                          Alternate_function,     -- Open-drain or push-pull with potential pull-up or pull-down
                          Analog)                 -- Analog (see ADCs, DACs)
     with Size => 2;

   -- Output type options

   type OTYPER_Options is (Push_Pull,  -- P-MOS and N-MOS transistors active
                           Open_Drain) -- P-MOS and N-MOS transistors inactive
     with Size => 1;

   -- Output speed options (indivative speeds are about half in low-power mode)

   type OSPEEDR_Options is (Speed_Low,    --   4 MHz, 100 ns on 50 pF,   8 MHz, 100 ns on 10 pF
                            Speed_Medium, --  25 MHz,  10 ns on 50 pF,  50 MHz,   6 ns on 10 pF
                            Speed_Fast,   --  50 MHz,   6 ns on 40 pF, 100 MHz,   4 ns on 10 pF
                            Speed_High)   -- 100 MHz,   4 ns on 30 pF, 180 MHz, 2.5 ns on 10 pF
     with Size => 2;

   -- Pull-up/pull-down options

   type PUPDR_Options is (No_Pull,   -- P-MOS and N-MOS transistors inactive
                          Pull_Up,   -- P-MOS permanently active
                          Pull_Down) -- N-MOS permanently active
     with Size => 2;

private

   -- Mode options

   for MODER_Options use (Input                  => 0,  -- Floating or pull-up or pull-down
                          General_purpose_output => 1,  -- Open-drain or push-pull with potential pull-up or pull-down
                          Alternate_function     => 2,  -- Open-drain or push-pull with potential pull-up or pull-down
                          Analog                 => 3); -- Analog (see ADCs, DACs)

   type MODER_Array is array (GPIO_Pins) of MODER_Options with Pack, Size => Word'Size;

   GPIO_MODER_Resets : constant array (GPIO_Ports) of MODER_Array :=
     (A => (11     => Alternate_function,
            12     => Alternate_function,
            13     => Alternate_function,
            others => Input),
      B => (3      => Alternate_function,
            4      => Alternate_function,
            others => Input),
      others => (others => Input));

   -- Output type options

   for OTYPER_Options use (Push_Pull  => 0,  -- P-MOS and N-MOS transistors active
                           Open_Drain => 1); -- P-MOS and N-MOS transistors inactive

   type OTYPER_Array is array (GPIO_Pins) of OTYPER_Options with Pack, Size => Half_Word'Size;

   GPIO_OTYPER_Reset : constant OTYPER_Array := (others => Push_Pull);

   -- Output speed options (indivative speeds are about half in low-power mode)

   for OSPEEDR_Options use (Speed_Low    => 0,  --   4 MHz, 100 ns on 50 pF,   8 MHz, 100 ns on 10 pF
                            Speed_Medium => 1,  --  25 MHz,  10 ns on 50 pF,  50 MHz,   6 ns on 10 pF
                            Speed_Fast   => 2,  --  50 MHz,   6 ns on 40 pF, 100 MHz,   4 ns on 10 pF
                            Speed_High   => 3); -- 100 MHz,   4 ns on 30 pF, 180 MHz, 2.5 ns on 10 pF

   type OSPEEDR_Array is array (GPIO_Pins) of OSPEEDR_Options with Pack, Size => Word'Size;

   GPIO_OSPEEDR_Resets : constant array (GPIO_Ports) of OSPEEDR_Array :=
        (A     => (13 => Speed_High, others => Speed_Low),
         B     =>  (3 => Speed_High, others => Speed_Low),
        others =>                   (others => Speed_Low));

   -- Pull-up/pull-down options

   for PUPDR_Options use (No_Pull   => 0,  -- P-MOS and N-MOS transistors inactive
                          Pull_Up   => 1,  -- P-MOS permanently active
                          Pull_Down => 2); -- N-MOS permanently active

   type PUPDR_Array is array (GPIO_Pins) of PUPDR_Options with Pack, Size => Word'Size;

   GPIO_PUPDR_Resets : constant array (GPIO_Ports) of PUPDR_Array :=
     (A      => (13 => Pull_Up,
                 14 => Pull_Down,
                 15 => Pull_Up,
                 others => No_Pull),
      B      => (4 => Pull_Up,
                 others => No_Pull),
      others => (others => No_Pull));

   -- GPIOx_BSRR Port bit set/reset

   type GPIOx_BSRR is record
      Set,                                -- Port x set   bit y (write only)
      Reset : Setting_Array (GPIO_Pins);  -- Port x reset bit y (write only)
   end record with Volatile, Size => Word'Size;

   for GPIOx_BSRR use record
      Set   at 0 range  0 .. 15; -- Port x set   bit y (write only)
      Reset at 0 range 16 .. 31; -- Port x reset bit y (write only)
   end record;

   -- GPIOx_LCKR Port configuration lock

   type GPIOx_LCKR is record
      Locked_Pins : Lock_Array (GPIO_Pins); -- Port x lock bit y (read & write) (until locked)
      Lock_Key    : Lock;                   -- Lock key          (read & write) (until locked)
      Reserved    : Bits_15;
   end record with Volatile, Size => Word'Size;

   for GPIOx_LCKR use record
      Locked_Pins at 0 range  0 .. 15; -- Port x lock bit y (read & write) (until locked)
      Lock_Key    at 0 range 16 .. 16; -- Lock key          (read & write) (until locked)
      Reserved    at 0 range 17 .. 31;
   end record;

   -- GPIOx_AFRL, GPIOx_AFRH

   type AFRL_Array is array (0 ..  7) of Bits_4 with Pack, Size => Word'Size;
   type AFRH_Array is array (8 .. 15) of Bits_4 with Pack, Size => Word'Size;

   -- GPIOx

   type GPIOx_Register is record
      MODER      : MODER_Array;           -- Port mode
      OTYPER     : OTYPER_Array;          -- Port output type
      Reserved_1 : Bits_16;
      OSPEEDR    : OSPEEDR_Array;         -- Port output speed
      PUPDR      : PUPDR_Array;           -- Port pull-up/pull-down
      IDR        : Bit_Array (GPIO_Pins); -- Port input data
      Reserved_2 : Bits_16;
      ODR        : Bit_Array (GPIO_Pins); -- Port output data
      Reserved_3 : Bits_16;
      BSRR       : GPIOx_BSRR;            -- Port bit set/reset
      LCKR       : GPIOx_LCKR;            -- Port configuration lock
      AFRL       : AFRL_Array;            -- Alternate function low
      AFRH       : AFRH_Array;            -- Alternate function high
   end record with Volatile, Size => Byte'Size * 16#28#;

   for GPIOx_Register use record
      MODER      at 16#00# range  0 .. 31;
      OTYPER     at 16#04# range  0 .. 15;
      Reserved_1 at 16#04# range 16 .. 31;
      OSPEEDR    at 16#08# range  0 .. 31;
      PUPDR      at 16#0C# range  0 .. 31;
      IDR        at 16#10# range  0 .. 15;
      Reserved_2 at 16#10# range 16 .. 31;
      ODR        at 16#14# range  0 .. 15;
      Reserved_3 at 16#14# range 16 .. 31;
      BSRR       at 16#18# range  0 .. 31;
      LCKR       at 16#1C# range  0 .. 31;
      AFRL       at 16#20# range  0 .. 31;
      AFRH       at 16#24# range  0 .. 31;
   end record;

   -- Set register addresses

   GPIOA : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOA), Import;

   GPIOB : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOB), Import;

   GPIOC : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOC), Import;

   GPIOD : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOD), Import;

   GPIOE : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOE), Import;

   GPIOF : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOF), Import;

   GPIOG : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOG), Import;

   GPIOH : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOH), Import;

   GPIOI : aliased GPIOx_Register with
     Volatile, Address => System'To_Address (Base_GPIOI), Import;

   -- Mapping ports to registers

   type GPIO_Register_Access is access all GPIOx_Register;

   function GPIOx (Port : GPIO_Ports) return GPIO_Register_Access is

     (case Port is
         when A => GPIOA'Access,
         when B => GPIOB'Access,
         when C => GPIOC'Access,
         when D => GPIOD'Access,
         when E => GPIOE'Access,
         when F => GPIOF'Access,
         when G => GPIOG'Access,
         when H => GPIOH'Access,
         when I => GPIOI'Access
     );

end STM32F4.General_purpose_IOs;

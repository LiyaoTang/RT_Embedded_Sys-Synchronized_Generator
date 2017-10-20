--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;
with System;                      use System;

-- STM32F405xx/07xx, STM32F415xx/17xx System configuration controller

package STM32F4.System_configuration_controller is

   -- SYSCFG_MEMRMP

   type MEM_MODE is (Main_Flash,
                     System_Flash,
                     FSMC_Bank1,
                     Embedded_SRAM) with Size => 2;

   -- SYSCFG_PMC

   type MII_RMII_SEL_Options is (MII_interface,
                                 RMII_PHY_interface) with Size => 1;

private

   -- SYSCFG_MEMRMP

   for MEM_MODE use (Main_Flash    => 0,
                     System_Flash  => 1,
                     FSMC_Bank1    => 2,
                     Embedded_SRAM => 3);

   type SYSCFG_MEMRMP is record
      MEMRMP   : MEM_MODE; -- read & write
      Reserved : Bits_30;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_MEMRMP use record
      MEMRMP   at 0 range 0 ..  1; -- read & write
      Reserved at 0 range 2 .. 31;
   end record;

   -- SYSCFG_PMC

   for MII_RMII_SEL_Options use (MII_interface      => 0,
                                 RMII_PHY_interface => 1);

   type SYSCFG_PMC is record
      Reserved_1   : Bits_23;
      MII_RMII_SEL : MII_RMII_SEL_Options; -- read & write
      Reserved_2   : Bits_8;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_PMC use record
      Reserved_1   at 0 range  0 .. 22;
      MII_RMII_SEL at 0 range 23 .. 23; -- read & write
      Reserved_2   at 0 range 24 .. 31;
   end record;

   -- SYSCFG_EXTICR1

   type SYSCFG_EXTICR1 is record
      EXTI0    : GPIO_Ports; -- read & write
      EXTI1    : GPIO_Ports; -- read & write
      EXTI2    : GPIO_Ports; -- read & write
      EXTI3    : GPIO_Ports; -- read & write
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_EXTICR1 use record
      EXTI0    at 0 range  0 ..  3; -- read & write
      EXTI1    at 0 range  4 ..  7; -- read & write
      EXTI2    at 0 range  8 .. 11; -- read & write
      EXTI3    at 0 range 12 .. 15; -- read & write
      Reserved at 0 range 16 .. 31;
   end record;

   -- SYSCFG_EXTICR2

   type SYSCFG_EXTICR2 is record
      EXTI4    : GPIO_Ports; -- read & write
      EXTI5    : GPIO_Ports; -- read & write
      EXTI6    : GPIO_Ports; -- read & write
      EXTI7    : GPIO_Ports; -- read & write
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_EXTICR2 use record
      EXTI4    at 0 range  0 ..  3; -- read & write
      EXTI5    at 0 range  4 ..  7; -- read & write
      EXTI6    at 0 range  8 .. 11; -- read & write
      EXTI7    at 0 range 12 .. 15; -- read & write
      Reserved at 0 range 16 .. 31;
   end record;

   -- SYSCFG_EXTICR3

   type SYSCFG_EXTICR3 is record
      EXTI8    : GPIO_Ports; -- read & write
      EXTI9    : GPIO_Ports; -- read & write
      EXTI10   : GPIO_Ports; -- read & write
      EXTI11   : GPIO_Ports; -- read & write
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_EXTICR3 use record
      EXTI8    at 0 range  0 ..  3; -- read & write
      EXTI9    at 0 range  4 ..  7; -- read & write
      EXTI10   at 0 range  8 .. 11; -- read & write
      EXTI11   at 0 range 12 .. 15; -- read & write
      Reserved at 0 range 16 .. 31;
   end record;

   -- SYSCFG_EXTICR4

   type SYSCFG_EXTICR4 is record
      EXTI12   : GPIO_Ports; -- read & write
      EXTI13   : GPIO_Ports; -- read & write
      EXTI14   : GPIO_Ports; -- read & write
      EXTI15   : GPIO_Ports; -- read & write
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_EXTICR4 use record
      EXTI12   at 0 range  0 ..  3; -- read & write
      EXTI13   at 0 range  4 ..  7; -- read & write
      EXTI14   at 0 range  8 .. 11; -- read & write
      EXTI15   at 0 range 12 .. 15; -- read & write
      Reserved at 0 range 16 .. 31;
   end record;

   -- SYSCFG_CMPCR

   type SYSCFG_CMPCR is record
      CMP_PD     : Enabler;   -- read & write
      Reserved_1 : Bits_7;
      READY      : Readiness; -- read only
      Reserved_2 : Bits_23;
   end record with Volatile, Size => Word'Size;

   for SYSCFG_CMPCR use record
      CMP_PD     at 0 range  0 ..  0; -- read & write
      Reserved_1 at 0 range  1 ..  7;
      READY      at 0 range  8 ..  8; -- read only
      Reserved_2 at 0 range  9 .. 31;
   end record;

   -- SYSCFG_Register

   type SYSCFG_Register is record
      MEMRM    : SYSCFG_MEMRMP;  -- Memory remap
      PMC      : SYSCFG_PMC;     -- Peripheral mode configuration
      EXTICR1  : SYSCFG_EXTICR1; -- External interrupt configuration 1
      EXTICR2  : SYSCFG_EXTICR2; -- External interrupt configuration 2
      EXTICR3  : SYSCFG_EXTICR3; -- External interrupt configuration 3
      EXTICR4  : SYSCFG_EXTICR4; -- External interrupt configuration 4
      Reserved : Bits_64;
      CMPCR    : SYSCFG_CMPCR;   -- Compensation cell control
   end record with Volatile, Size => Byte'Size * 16#24#;

   for SYSCFG_Register use record
      MEMRM    at 16#00# range 0 .. 31;
      PMC      at 16#04# range 0 .. 31;
      EXTICR1  at 16#08# range 0 .. 31;
      EXTICR2  at 16#0C# range 0 .. 31;
      EXTICR3  at 16#10# range 0 .. 31;
      EXTICR4  at 16#14# range 0 .. 31;
      Reserved at 16#18# range 0 .. 63;
      CMPCR    at 16#20# range 0 .. 31;
   end record;

   -- Set register address

   SYSCFG : SYSCFG_Register with
     Volatile, Address => System'To_Address (Base_SYSCFG), Import;

end STM32F4.System_configuration_controller;

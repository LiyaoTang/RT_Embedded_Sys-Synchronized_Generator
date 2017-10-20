--
-- Uwe R. Zimmer, Australia 2015
--

with STM32F4.General_purpose_IOs; use STM32F4.General_purpose_IOs;
with STM32F4.Timers;              use STM32F4.Timers;

-- STM32F405xx/07xx, STM32F415xx Reset and clock control Operations

package STM32F4.Reset_and_clock_control.Ops is

   type Peripheral_Enable is
     (CRC,
      Backup_SRAM,                 -- cannot be reset
      CCM_data_RAM,                -- cannot be reset
      DMA1,
      DMA2,
      Ethernet_MAC,
      Ethernet_Transmission,       -- cannot be reset
      Ethernet_Reception,          -- cannot be reset
      Ethernet_PTP,                -- cannot be reset
      USB_OTG_HS,
      USB_OTG_HSULPI,              -- cannot be reset
      USB_OTG_FS,
      Camera,
      Cryptographic_modules,
      Hash_modules,
      Random_number_generator,
      Flexible_static_memory,      -- cannot be reset
      Window_watchdog,
      SPI1,
      SPI2,
      SPI3,
      USART1,
      USART2,
      USART3,
      UART4,
      UART5,
      USART6,
      I2C1,
      I2C2,
      I2C3,
      CAN1,
      CAN2,
      Power_interface,
      DAC_interface,
      ADC1,                        -- can only be reset with all ADCs
      ADC2,                        -- can only be reset with all ADCs
      ADC3,                        -- can only be reset with all ADCs
      SDIO,
      System_Configuration_Contr);

   type Peripheral_Reset is
     (CRC,
      DMA1,
      DMA2,
      Ethernet_MAC,
      USB_OTG_HS,
      USB_OTG_FS,
      Camera,
      Cryptographic_modules,
      Hash_modules,
      Random_number_generator,
      Window_watchdog,
      SPI1,
      SPI2,
      SPI3,
      USART1,
      USART2,
      USART3,
      UART4,
      UART5,
      USART6,
      I2C1,
      I2C2,
      I2C3,
      CAN1,
      CAN2,
      Power_interface,
      DAC_interface,
      ADC,
      SDIO,
      System_Configuration_Contr);

   procedure Enable  (Device : Peripheral_Enable; Low_Power : Boolean := False) with Inline;
   procedure Disable (Device : Peripheral_Enable; Low_Power : Boolean := False) with Inline;
   procedure Reset   (Device : Peripheral_Reset) with Inline;
   procedure Release (Device : Peripheral_Reset) with Inline;

   procedure Enable  (Port : GPIO_Ports; Low_Power : Boolean := False) with Inline;
   procedure Disable (Port : GPIO_Ports; Low_Power : Boolean := False) with Inline;
   procedure Reset   (Port : GPIO_Ports) with Inline;
   procedure Release (Port : GPIO_Ports) with Inline;

   procedure Enable  (No : Timer_No; Low_Power : Boolean := False) with Inline;
   procedure Disable (No : Timer_No; Low_Power : Boolean := False) with Inline;
   procedure Reset   (No : Timer_No) with Inline;
   procedure Release (No : Timer_No) with Inline;

end STM32F4.Reset_and_clock_control.Ops;

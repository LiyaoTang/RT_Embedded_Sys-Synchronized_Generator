--
-- Uwe R. Zimmer, Australia 2015
--

package body STM32F4.Reset_and_clock_control.Ops is

   procedure Enable_Disable (Device : Peripheral_Enable; State : Enabler) with Inline is

   begin
      case Device is
         when CRC                             => RCC.AHB1ENR.CRC        := State;
         when Backup_SRAM                     => RCC.AHB1ENR.BKPSRAM    := State;
         when CCM_data_RAM                    => RCC.AHB1ENR.CCMDATARAM := State;
         when DMA1                            => RCC.AHB1ENR.DMA1       := State;
         when DMA2                            => RCC.AHB1ENR.DMA2       := State;
         when Ethernet_MAC                    => RCC.AHB1ENR.ETHMAC     := State;
         when Ethernet_Transmission           => RCC.AHB1ENR.ETHMACTX   := State;
         when Ethernet_Reception              => RCC.AHB1ENR.ETHMACRX   := State;
         when Ethernet_PTP                    => RCC.AHB1ENR.ETHMACPTP  := State;
         when USB_OTG_HS                      => RCC.AHB1ENR.OTGHS      := State;
         when USB_OTG_HSULPI                  => RCC.AHB1ENR.OTGHSULPI  := State;
         when USB_OTG_FS                      => RCC.AHB2ENR.OTGFS      := State;
         when Camera                          => RCC.AHB2ENR.DCMI       := State;
         when Cryptographic_modules           => RCC.AHB2ENR.CRYP       := State;
         when Hash_modules                    => RCC.AHB2ENR.HASH       := State;
         when Random_number_generator         => RCC.AHB2ENR.RNG        := State;
         when Flexible_static_memory          => RCC.AHB3ENR.FSMC       := State;
         when Window_watchdog                 => RCC.APB1ENR.WWDG       := State;
         when SPI1                            => RCC.APB2ENR.SPI1       := State;
         when SPI2                            => RCC.APB1ENR.SPI2       := State;
         when SPI3                            => RCC.APB1ENR.SPI3       := State;
         when USART1                          => RCC.APB2ENR.USART1     := State;
         when USART2                          => RCC.APB1ENR.USART2     := State;
         when USART3                          => RCC.APB1ENR.USART3     := State;
         when UART4                           => RCC.APB1ENR.USART4     := State;
         when UART5                           => RCC.APB1ENR.USART5     := State;
         when USART6                          => RCC.APB2ENR.USART6     := State;
         when I2C1                            => RCC.APB1ENR.I2C1       := State;
         when I2C2                            => RCC.APB1ENR.I2C2       := State;
         when I2C3                            => RCC.APB1ENR.I2C3       := State;
         when CAN1                            => RCC.APB1ENR.CAN1       := State;
         when CAN2                            => RCC.APB1ENR.CAN2       := State;
         when Power_interface                 => RCC.APB1ENR.PWR        := State;
         when DAC_interface                   => RCC.APB1ENR.DAC        := State;
         when ADC1                            => RCC.APB2ENR.ADC1       := State;
         when ADC2                            => RCC.APB2ENR.ADC2       := State;
         when ADC3                            => RCC.APB2ENR.ADC3       := State;
         when SDIO                            => RCC.APB2ENR.SDIO       := State;
         when System_Configuration_Contr      => RCC.APB2ENR.SYSCFG     := State;
      end case;
   end Enable_Disable;

   procedure Enable_Disable_Low_Power (Device : Peripheral_Enable; State : Enabler) with Inline is

   begin
      case Device is
         when CRC                             => RCC.AHB1LPENR.CRC        := State;
         when Backup_SRAM                     => RCC.AHB1LPENR.BKPSRAM    := State;
         when CCM_data_RAM                    => RCC.AHB1LPENR.CCMDATARAM := State;
         when DMA1                            => RCC.AHB1LPENR.DMA1       := State;
         when DMA2                            => RCC.AHB1LPENR.DMA2       := State;
         when Ethernet_MAC                    => RCC.AHB1LPENR.ETHMAC     := State;
         when Ethernet_Transmission           => RCC.AHB1LPENR.ETHMACTX   := State;
         when Ethernet_Reception              => RCC.AHB1LPENR.ETHMACRX   := State;
         when Ethernet_PTP                    => RCC.AHB1LPENR.ETHMACPTP  := State;
         when USB_OTG_HS                      => RCC.AHB1LPENR.OTGHS      := State;
         when USB_OTG_HSULPI                  => RCC.AHB1LPENR.OTGHSULPI  := State;
         when USB_OTG_FS                      => RCC.AHB2LPENR.OTGFS      := State;
         when Camera                          => RCC.AHB2LPENR.DCMI       := State;
         when Cryptographic_modules           => RCC.AHB2LPENR.CRYP       := State;
         when Hash_modules                    => RCC.AHB2LPENR.HASH       := State;
         when Random_number_generator         => RCC.AHB2LPENR.RNG        := State;
         when Flexible_static_memory          => RCC.AHB3LPENR.FSMC       := State;
         when Window_watchdog                 => RCC.APB1LPENR.WWDG       := State;
         when SPI1                            => RCC.APB2LPENR.SPI1       := State;
         when SPI2                            => RCC.APB1LPENR.SPI2       := State;
         when SPI3                            => RCC.APB1LPENR.SPI3       := State;
         when USART1                          => RCC.APB2LPENR.USART1     := State;
         when USART2                          => RCC.APB1LPENR.USART2     := State;
         when USART3                          => RCC.APB1LPENR.USART3     := State;
         when UART4                           => RCC.APB1LPENR.USART4     := State;
         when UART5                           => RCC.APB1LPENR.USART5     := State;
         when USART6                          => RCC.APB2LPENR.USART6     := State;
         when I2C1                            => RCC.APB1LPENR.I2C1       := State;
         when I2C2                            => RCC.APB1LPENR.I2C2       := State;
         when I2C3                            => RCC.APB1LPENR.I2C3       := State;
         when CAN1                            => RCC.APB1LPENR.CAN1       := State;
         when CAN2                            => RCC.APB1LPENR.CAN2       := State;
         when Power_interface                 => RCC.APB1LPENR.PWR        := State;
         when DAC_interface                   => RCC.APB1LPENR.DAC        := State;
         when ADC1                            => RCC.APB2LPENR.ADC1       := State;
         when ADC2                            => RCC.APB2LPENR.ADC2       := State;
         when ADC3                            => RCC.APB2LPENR.ADC3       := State;
         when SDIO                            => RCC.APB2LPENR.SDIO       := State;
         when System_Configuration_Contr      => RCC.APB2LPENR.SYSCFG     := State;
      end case;
   end Enable_Disable_Low_Power;

   procedure Enable  (Device : Peripheral_Enable; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (Device, Enable);
         when False => Enable_Disable           (Device, Enable);
      end case;
   end Enable;

   procedure Disable (Device : Peripheral_Enable; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (Device, Disable);
         when False => Enable_Disable           (Device, Disable);
      end case;
   end Disable;

   procedure Reset (Device : Peripheral_Reset) is

   begin
      case Device is
         when CRC                             => RCC.AHB1RSTR.CRC        := Reset;
         when DMA1                            => RCC.AHB1RSTR.DMA1       := Reset;
         when DMA2                            => RCC.AHB1RSTR.DMA2       := Reset;
         when Ethernet_MAC                    => RCC.AHB1RSTR.ETHMAC     := Reset;
         when USB_OTG_HS                      => RCC.AHB1RSTR.OTGHS      := Reset;
         when USB_OTG_FS                      => RCC.AHB2RSTR.OTGFS      := Reset;
         when Camera                          => RCC.AHB2RSTR.DCMI       := Reset;
         when Cryptographic_modules           => RCC.AHB2RSTR.CRYP       := Reset;
         when Hash_modules                    => RCC.AHB2RSTR.HASH       := Reset;
         when Random_number_generator         => RCC.AHB2RSTR.RNG        := Reset;
         when Window_watchdog                 => RCC.APB1RSTR.WWDG       := Reset;
         when SPI1                            => RCC.APB2RSTR.SPI1       := Reset;
         when SPI2                            => RCC.APB1RSTR.SPI2       := Reset;
         when SPI3                            => RCC.APB1RSTR.SPI3       := Reset;
         when USART1                          => RCC.APB2RSTR.USART1     := Reset;
         when USART2                          => RCC.APB1RSTR.USART2     := Reset;
         when USART3                          => RCC.APB1RSTR.USART3     := Reset;
         when UART4                           => RCC.APB1RSTR.USART4     := Reset;
         when UART5                           => RCC.APB1RSTR.USART5     := Reset;
         when USART6                          => RCC.APB2RSTR.USART6     := Reset;
         when I2C1                            => RCC.APB1RSTR.I2C1       := Reset;
         when I2C2                            => RCC.APB1RSTR.I2C2       := Reset;
         when I2C3                            => RCC.APB1RSTR.I2C3       := Reset;
         when CAN1                            => RCC.APB1RSTR.CAN1       := Reset;
         when CAN2                            => RCC.APB1RSTR.CAN2       := Reset;
         when Power_interface                 => RCC.APB1RSTR.PWR        := Reset;
         when DAC_interface                   => RCC.APB1RSTR.DAC        := Reset;
         when ADC                             => RCC.APB2RSTR.ADC        := Reset;
         when SDIO                            => RCC.APB2RSTR.SDIO       := Reset;
         when System_Configuration_Contr      => RCC.APB2RSTR.SYSCFG     := Reset;
      end case;
   end Reset;

   procedure Release (Device : Peripheral_Reset) is

   begin
      case Device is
         when CRC                             => RCC.AHB1RSTR.CRC        := Do_Nothing;
         when DMA1                            => RCC.AHB1RSTR.DMA1       := Do_Nothing;
         when DMA2                            => RCC.AHB1RSTR.DMA2       := Do_Nothing;
         when Ethernet_MAC                    => RCC.AHB1RSTR.ETHMAC     := Do_Nothing;
         when USB_OTG_HS                      => RCC.AHB1RSTR.OTGHS      := Do_Nothing;
         when USB_OTG_FS                      => RCC.AHB2RSTR.OTGFS      := Do_Nothing;
         when Camera                          => RCC.AHB2RSTR.DCMI       := Do_Nothing;
         when Cryptographic_modules           => RCC.AHB2RSTR.CRYP       := Do_Nothing;
         when Hash_modules                    => RCC.AHB2RSTR.HASH       := Do_Nothing;
         when Random_number_generator         => RCC.AHB2RSTR.RNG        := Do_Nothing;
         when Window_watchdog                 => RCC.APB1RSTR.WWDG       := Do_Nothing;
         when SPI1                            => RCC.APB2RSTR.SPI1       := Do_Nothing;
         when SPI2                            => RCC.APB1RSTR.SPI2       := Do_Nothing;
         when SPI3                            => RCC.APB1RSTR.SPI3       := Do_Nothing;
         when USART1                          => RCC.APB2RSTR.USART1     := Do_Nothing;
         when USART2                          => RCC.APB1RSTR.USART2     := Do_Nothing;
         when USART3                          => RCC.APB1RSTR.USART3     := Do_Nothing;
         when UART4                           => RCC.APB1RSTR.USART4     := Do_Nothing;
         when UART5                           => RCC.APB1RSTR.USART5     := Do_Nothing;
         when USART6                          => RCC.APB2RSTR.USART6     := Do_Nothing;
         when I2C1                            => RCC.APB1RSTR.I2C1       := Do_Nothing;
         when I2C2                            => RCC.APB1RSTR.I2C2       := Do_Nothing;
         when I2C3                            => RCC.APB1RSTR.I2C3       := Do_Nothing;
         when CAN1                            => RCC.APB1RSTR.CAN1       := Do_Nothing;
         when CAN2                            => RCC.APB1RSTR.CAN2       := Do_Nothing;
         when Power_interface                 => RCC.APB1RSTR.PWR        := Do_Nothing;
         when DAC_interface                   => RCC.APB1RSTR.DAC        := Do_Nothing;
         when ADC                             => RCC.APB2RSTR.ADC        := Do_Nothing;
         when SDIO                            => RCC.APB2RSTR.SDIO       := Do_Nothing;
         when System_Configuration_Contr      => RCC.APB2RSTR.SYSCFG     := Do_Nothing;
      end case;
   end Release;

   procedure Enable_Disable (Port : GPIO_Ports; State : Enabler) with Inline is

   begin
      case Port is
         when A => RCC.AHB1ENR.GPIOA := State;
         when B => RCC.AHB1ENR.GPIOB := State;
         when C => RCC.AHB1ENR.GPIOC := State;
         when D => RCC.AHB1ENR.GPIOD := State;
         when E => RCC.AHB1ENR.GPIOE := State;
         when F => RCC.AHB1ENR.GPIOF := State;
         when G => RCC.AHB1ENR.GPIOG := State;
         when H => RCC.AHB1ENR.GPIOH := State;
         when I => RCC.AHB1ENR.GPIOI := State;
      end case;
   end Enable_Disable;

   procedure Enable_Disable_Low_Power (Port : GPIO_Ports; State : Enabler) with Inline is

   begin
      case Port is
         when A => RCC.AHB1LPENR.GPIOA := State;
         when B => RCC.AHB1LPENR.GPIOB := State;
         when C => RCC.AHB1LPENR.GPIOC := State;
         when D => RCC.AHB1LPENR.GPIOD := State;
         when E => RCC.AHB1LPENR.GPIOE := State;
         when F => RCC.AHB1LPENR.GPIOF := State;
         when G => RCC.AHB1LPENR.GPIOG := State;
         when H => RCC.AHB1LPENR.GPIOH := State;
         when I => RCC.AHB1LPENR.GPIOI := State;
      end case;
   end Enable_Disable_Low_Power;

   procedure Enable (Port : GPIO_Ports; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (Port, Enable);
         when False => Enable_Disable           (Port, Enable);
      end case;
   end Enable;

   procedure Disable (Port : GPIO_Ports; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (Port, Disable);
         when False => Enable_Disable           (Port, Disable);
      end case;
   end Disable;

   procedure Reset (Port : GPIO_Ports) is

   begin
      case Port is
         when A => RCC.AHB1RSTR.GPIOA := Reset;
         when B => RCC.AHB1RSTR.GPIOB := Reset;
         when C => RCC.AHB1RSTR.GPIOC := Reset;
         when D => RCC.AHB1RSTR.GPIOD := Reset;
         when E => RCC.AHB1RSTR.GPIOE := Reset;
         when F => RCC.AHB1RSTR.GPIOF := Reset;
         when G => RCC.AHB1RSTR.GPIOG := Reset;
         when H => RCC.AHB1RSTR.GPIOH := Reset;
         when I => RCC.AHB1RSTR.GPIOI := Reset;
      end case;
   end Reset;

   procedure Release (Port : GPIO_Ports) is

   begin
      case Port is
         when A => RCC.AHB1RSTR.GPIOA := Do_Nothing;
         when B => RCC.AHB1RSTR.GPIOB := Do_Nothing;
         when C => RCC.AHB1RSTR.GPIOC := Do_Nothing;
         when D => RCC.AHB1RSTR.GPIOD := Do_Nothing;
         when E => RCC.AHB1RSTR.GPIOE := Do_Nothing;
         when F => RCC.AHB1RSTR.GPIOF := Do_Nothing;
         when G => RCC.AHB1RSTR.GPIOG := Do_Nothing;
         when H => RCC.AHB1RSTR.GPIOH := Do_Nothing;
         when I => RCC.AHB1RSTR.GPIOI := Do_Nothing;
      end case;
   end Release;

   procedure Enable_Disable (No : Timer_No; State : Enabler) with Inline is

   begin
      case No is
         when  1 => RCC.APB2ENR.TIM1  := State;
         when  2 => RCC.APB1ENR.TIM2  := State;
         when  3 => RCC.APB1ENR.TIM3  := State;
         when  4 => RCC.APB1ENR.TIM4  := State;
         when  5 => RCC.APB1ENR.TIM5  := State;
         when  6 => RCC.APB1ENR.TIM6  := State;
         when  7 => RCC.APB1ENR.TIM7  := State;
         when  8 => RCC.APB2ENR.TIM8  := State;
         when  9 => RCC.APB2ENR.TIM9  := State;
         when 10 => RCC.APB2ENR.TIM10 := State;
         when 11 => RCC.APB2ENR.TIM11 := State;
         when 12 => RCC.APB1ENR.TIM12 := State;
         when 13 => RCC.APB1ENR.TIM13 := State;
         when 14 => RCC.APB1ENR.TIM14 := State;
      end case;
   end Enable_Disable;

   procedure Enable_Disable_Low_Power (No : Timer_No; State : Enabler) with Inline is

   begin
      case No is
         when  1 => RCC.APB2LPENR.TIM1  := State;
         when  2 => RCC.APB1LPENR.TIM2  := State;
         when  3 => RCC.APB1LPENR.TIM3  := State;
         when  4 => RCC.APB1LPENR.TIM4  := State;
         when  5 => RCC.APB1LPENR.TIM5  := State;
         when  6 => RCC.APB1LPENR.TIM6  := State;
         when  7 => RCC.APB1LPENR.TIM7  := State;
         when  8 => RCC.APB2LPENR.TIM8  := State;
         when  9 => RCC.APB2LPENR.TIM9  := State;
         when 10 => RCC.APB2LPENR.TIM10 := State;
         when 11 => RCC.APB2LPENR.TIM11 := State;
         when 12 => RCC.APB1LPENR.TIM12 := State;
         when 13 => RCC.APB1LPENR.TIM13 := State;
         when 14 => RCC.APB1LPENR.TIM14 := State;
      end case;
   end Enable_Disable_Low_Power;

   procedure Enable  (No : Timer_No; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (No, Enable);
         when False => Enable_Disable           (No, Enable);
      end case;
   end Enable;

   procedure Disable (No : Timer_No; Low_Power : Boolean := False) is

   begin
      case Low_Power is
         when True  => Enable_Disable_Low_Power (No, Disable);
         when False => Enable_Disable           (No, Disable);
      end case;
   end Disable;

   procedure Reset (No : Timer_No) is

   begin
      case No is
         when  1 => RCC.APB2RSTR.TIM1  := Reset;
         when  2 => RCC.APB1RSTR.TIM2  := Reset;
         when  3 => RCC.APB1RSTR.TIM3  := Reset;
         when  4 => RCC.APB1RSTR.TIM4  := Reset;
         when  5 => RCC.APB1RSTR.TIM5  := Reset;
         when  6 => RCC.APB1RSTR.TIM6  := Reset;
         when  7 => RCC.APB1RSTR.TIM7  := Reset;
         when  8 => RCC.APB2RSTR.TIM8  := Reset;
         when  9 => RCC.APB2RSTR.TIM9  := Reset;
         when 10 => RCC.APB2RSTR.TIM10 := Reset;
         when 11 => RCC.APB2RSTR.TIM11 := Reset;
         when 12 => RCC.APB1RSTR.TIM12 := Reset;
         when 13 => RCC.APB1RSTR.TIM13 := Reset;
         when 14 => RCC.APB1RSTR.TIM14 := Reset;
      end case;
   end Reset;

   procedure Release (No : Timer_No) is

   begin
      case No is
         when  1 => RCC.APB2RSTR.TIM1  := Do_Nothing;
         when  2 => RCC.APB1RSTR.TIM2  := Do_Nothing;
         when  3 => RCC.APB1RSTR.TIM3  := Do_Nothing;
         when  4 => RCC.APB1RSTR.TIM4  := Do_Nothing;
         when  5 => RCC.APB1RSTR.TIM5  := Do_Nothing;
         when  6 => RCC.APB1RSTR.TIM6  := Do_Nothing;
         when  7 => RCC.APB1RSTR.TIM7  := Do_Nothing;
         when  8 => RCC.APB2RSTR.TIM8  := Do_Nothing;
         when  9 => RCC.APB2RSTR.TIM9  := Do_Nothing;
         when 10 => RCC.APB2RSTR.TIM10 := Do_Nothing;
         when 11 => RCC.APB2RSTR.TIM11 := Do_Nothing;
         when 12 => RCC.APB1RSTR.TIM12 := Do_Nothing;
         when 13 => RCC.APB1RSTR.TIM13 := Do_Nothing;
         when 14 => RCC.APB1RSTR.TIM14 := Do_Nothing;
      end case;
   end Release;

end STM32F4.Reset_and_clock_control.Ops;

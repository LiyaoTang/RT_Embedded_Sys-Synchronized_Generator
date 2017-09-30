--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Universal synchronous asynchronous receiver transmitter Operations

package STM32F4.USART.Ops is

   type USART_Status_Flags is (Parity_error,
                               Framing_error,
                               Noise_detected,
                               Overrun_error,
                               Idle_line_detected,
                               Read_data_register_not_empty,
                               Transmission_complete,
                               Transmit_data_register_empty,
                               LIN_break_detection,
                               CTS                           -- not available on UARTs
                              );

   type USART_Status_Array is array (USART_Status_Flags) of Boolean;

   function USART_Status (No : USARTs) return USART_Status_Array;

   procedure Clear_Read_data_register_not_empty (No : USARTs) with Inline;
   procedure Clear_Transmission_complete        (No : USARTs) with Inline;
   procedure Clear_LIN_break_detection          (No : USARTs) with Inline;
   procedure Clear_CTS                          (No : USARTs) with Inline;

   procedure Transmit_Data (No : USARTs; Data : Bits_9) with Inline;
   function  Receive_Data  (No : USARTs) return Bits_9  with Inline;

   procedure Set_Baud_Rate_Divider (No           : USARTs;
                                    Mantissa     : Bits_12;
                                    Fraction     : Bits_4;
                                    Oversampling : OVER8_Options := Oversampling_by_16) with Inline;

   procedure Enable  (No : USARTs; TxEnable,  RxEnable  : Enabler := Enable)  with Inline;
   procedure Disable (No : USARTs; TxEnable,  RxEnable  : Enabler := Disable) with Inline;

   Not_Available_on_UARTs : exception;

   procedure Enable_Interrupts  (No : USARTs; Intr : USART_Status_Flags) with Inline;
   procedure Disable_Interrupts (No : USARTs; Intr : USART_Status_Flags) with Inline;

   procedure Send_Break (No : USARTs) with Inline;

   procedure Configure_USART_Format
     (No               : USARTs_only;
      Word_Length      : M_Options     := Data_Bits_8;
      Parity_Control   : Enabler       := Disable;
      Parity_Selection : Parity        := Even;
      Stop_Bits        : SSTOP_Options := Stop_One;
      One_Bit_Sampling : Enabler       := Disable;
      Half_Duplex      : Enabler       := Disable) with Inline;

   procedure Configure_UART_Format
     (No               : UARTs;
      Word_Length      : M_Options     := Data_Bits_8;
      Parity_Control   : Enabler       := Disable;
      Parity_Selection : Parity        := Even;
      Stop_Bits        : ASTOP_Options := Stop_One;
      One_Bit_Sampling : Enabler       := Disable;
      Half_Duplex      : Enabler       := Disable) with Inline;

   procedure Set_USART_Address
     (No               : USARTs;
      USART_Address    : Bits_4        := 0) with Inline;

   procedure Configure_Local_Interconnection_Network
     (No               : USARTs;
      LIN_mode         : Enabler       := Disable;
      LIN_Length       : LBDL_Options  := Break_Detection_10bit) with Inline;

   procedure Configure_IrDA
     (No               : USARTs;
      IrDA_Mode        : Enabler       := Disable;
      IrDA_Low_Power   : IRLP_Options  := Normal;
      Prescaler        : Bits_8        := 0) with Inline; -- Prescaler has no effect for UARTs 4 and 5

   procedure Configure_Smartcard
     (No               : USARTs_only;
      SC_Mode,
      SC_NACK          : Enabler       := Disable;
      Guard_Time,
      Prescaler        : Bits_8        := 0) with Inline;

   procedure Configure_Wakeup
     (No               : USARTs;
      Wakeup           : WAKE_Options  := Idle_Line;
      Receiver_wakeup  : Muting        := Active) with Inline;

   procedure Configure_Sync
     (No               : USARTs_only;
      Clock_Pin        : Enabler       := Disable;
      Clock_Polarity   : CPOL_Options  := Low;
      Clock_Phase      : CPHA_Options  := First;
      Clock_Pulse      : Enabler       := Disable;
      CTS_Enable       : Enabler       := Disable;
      RTS_Enable       : Enabler       := Disable) with Inline;

   procedure Configure_DMA
     (No               : USARTs;
      DMA_Transmitter,
      DMA_Receiver     : Enabler) with Inline;

end STM32F4.USART.Ops;

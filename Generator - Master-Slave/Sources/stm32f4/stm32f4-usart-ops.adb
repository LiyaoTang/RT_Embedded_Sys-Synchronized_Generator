--
-- Uwe R. Zimmer, Australia 2015
--

-- STM32F4xx Universal synchronous asynchronous receiver transmitter Operations

package body STM32F4.USART.Ops is

   function Read_SR (No : USARTs) return USART_SR is

     (case USART_Mode (No) is
         when USART_Type => USARTx (No).all.SR,
         when UART_Type  => UARTx  (No).all.SR) with Inline;

   function Read_DR (No : USARTs) return USART_DR is

     (case USART_Mode (No) is
         when USART_Type => USARTx (No).all.DR,
         when UART_Type  => UARTx  (No).all.DR) with Inline;

   function Read_BRR (No : USARTs) return USART_BRR is

     (case USART_Mode (No) is
         when USART_Type => USARTx (No).all.BRR,
         when UART_Type  => UARTx  (No).all.BRR) with Inline;

   function Read_CR1 (No : USARTs) return USART_CR1 is

     (case USART_Mode (No) is
         when USART_Type => USARTx (No).all.CR1,
         when UART_Type  => UARTx  (No).all.CR1) with Inline;

   procedure Write_SR (No : USARTs; SR_Copy : USART_SR) with Inline;
   procedure Write_SR (No : USARTs; SR_Copy : USART_SR) is

   begin
      case USART_Mode (No) is
         when USART_Type => USARTx (No).all.SR := SR_Copy;
         when UART_Type  => UARTx  (No).all.SR := SR_Copy;
      end case;
   end Write_SR;

   procedure Write_DR (No : USARTs; DR_Copy : USART_DR) with Inline;
   procedure Write_DR (No : USARTs; DR_Copy : USART_DR) is

   begin
      case USART_Mode (No) is
         when USART_Type => USARTx (No).all.DR := DR_Copy;
         when UART_Type  => UARTx  (No).all.DR := DR_Copy;
      end case;
   end Write_DR;

   procedure Write_BRR (No : USARTs; BRR_Copy : USART_BRR) with Inline;
   procedure Write_BRR (No : USARTs; BRR_Copy : USART_BRR) is

   begin
      case USART_Mode (No) is
         when USART_Type => USARTx (No).all.BRR := BRR_Copy;
         when UART_Type  => UARTx  (No).all.BRR := BRR_Copy;
      end case;
   end Write_BRR;

   procedure Write_CR1 (No : USARTs; CR1_Copy : USART_CR1) with Inline;
   procedure Write_CR1 (No : USARTs; CR1_Copy : USART_CR1) is

   begin
      case USART_Mode (No) is
         when USART_Type => USARTx (No).all.CR1 := CR1_Copy;
         when UART_Type  => UARTx  (No).all.CR1 := CR1_Copy;
      end case;
   end Write_CR1;

   function USART_Status (No : USARTs) return USART_Status_Array is

      SR_Copy : constant USART_SR := Read_SR (No);

   begin
      return (Parity_error                 => SR_Copy.PE   = Occured,
              Framing_error                => SR_Copy.FE   = Occured,
              Noise_detected               => SR_Copy.NF   = Occured,
              Overrun_error                => SR_Copy.ORE  = Occured,
              Idle_line_detected           => SR_Copy.IDLE = Detected,
              Read_data_register_not_empty => SR_Copy.RXNE = Ready,
              Transmission_complete        => SR_Copy.TC   = Ready,
              Transmit_data_register_empty => SR_Copy.TXE  = Ready,
              LIN_break_detection          => SR_Copy.LBD  = Detected,
              CTS                          => SR_Copy.CTS  = Occured);
   end USART_Status;

   procedure Clear_Read_data_register_not_empty (No : USARTs) is

      SR_Copy : USART_SR := Read_SR (No);

   begin
      SR_Copy.RXNE := Not_Ready;
      Write_SR (No, SR_Copy);
   end Clear_Read_data_register_not_empty;

   procedure Clear_Transmission_complete (No : USARTs) is

      SR_Copy : USART_SR := Read_SR (No);

   begin
      SR_Copy.TC := Not_Ready;
      Write_SR (No, SR_Copy);
   end Clear_Transmission_complete;

   procedure Clear_LIN_break_detection (No : USARTs) is

      SR_Copy : USART_SR := Read_SR (No);

   begin
      SR_Copy.LBD := Not_Detected;
      Write_SR (No, SR_Copy);
   end Clear_LIN_break_detection;

   procedure Clear_CTS (No : USARTs) is

      SR_Copy : USART_SR := Read_SR (No);

   begin
      SR_Copy.CTS := Not_Occured;
      Write_SR (No, SR_Copy);
   end Clear_CTS;

   procedure Transmit_Data (No : USARTs; Data : Bits_9) is

      DR_Copy : USART_DR := Read_DR (No);

   begin
      DR_Copy.Data := Data;
      Write_DR (No, DR_Copy);
   end Transmit_Data;

   function Receive_Data (No : USARTs) return Bits_9 is

      DR_Copy : constant USART_DR := Read_DR (No);

   begin
      return DR_Copy.Data;
   end Receive_Data;

   procedure Set_Baud_Rate_Divider (No           : USARTs;
                                    Mantissa     : Bits_12;
                                    Fraction     : Bits_4;
                                    Oversampling : OVER8_Options := Oversampling_by_16) is

      BRR_Copy : USART_BRR := Read_BRR (No);
      CR1_Copy : USART_CR1 := Read_CR1 (No);

   begin
      BRR_Copy := (DIV_Fraction => Fraction,
                   DIV_Mantissa => Mantissa,
                   Reserved     => BRR_Copy.Reserved);
      CR1_Copy.OVER8 := Oversampling;
      Write_BRR (No, BRR_Copy);
      Write_CR1 (No, CR1_Copy);
   end Set_Baud_Rate_Divider;

   procedure Enable (No : USARTs; TxEnable,  RxEnable  : Enabler := Enable) is

      CR1_Copy : USART_CR1 := Read_CR1 (No);

   begin
      CR1_Copy.UE := Enable;
      CR1_Copy.TE := TxEnable;
      CR1_Copy.RE := RxEnable;
      Write_CR1 (No, CR1_Copy);
   end Enable;

   procedure Disable (No : USARTs; TxEnable,  RxEnable  : Enabler := Disable) is

      CR1_Copy : USART_CR1 := Read_CR1 (No);

   begin
      CR1_Copy.UE := Disable;
      CR1_Copy.TE := TxEnable;
      CR1_Copy.RE := RxEnable;
      Write_CR1 (No, CR1_Copy);
   end Disable;

   procedure Enable_Disable_Interrupts (No : USARTs; Intr : USART_Status_Flags; State : Enabler) is

      CR1_Copy : USART_CR1 := Read_CR1 (No);

      procedure Enable_Disable_Error with Inline;
      procedure Enable_Disable_Error is

      begin
         case USART_Mode (No) is
            when USART_Type => USARTx (No).all.CR3.EIE := State;
            when UART_Type  => UARTx  (No).all.CR3.EIE := State;
         end case;
      end Enable_Disable_Error;

   begin
      case Intr is
         when Parity_error                 => CR1_Copy.PEIE   := State;
         when Framing_error                => CR1_Copy.RXNEIE := State; Enable_Disable_Error;
         when Noise_detected               => CR1_Copy.RXNEIE := State; Enable_Disable_Error;
         when Overrun_error                => CR1_Copy.RXNEIE := State; Enable_Disable_Error;
         when Idle_line_detected           => CR1_Copy.IDLEIE := State;
         when Read_data_register_not_empty => CR1_Copy.RXNEIE := State;
         when Transmission_complete        => CR1_Copy.TCIE   := State;
         when Transmit_data_register_empty => CR1_Copy.TXEIE  := State;
         when LIN_break_detection          =>
            case USART_Mode (No) is
               when USART_Type => USARTx (No).all.CR2.LBDIE   := State;
               when UART_Type  => UARTx  (No).all.CR2.LBDIE   := State;
            end case;
         when CTS                          =>
            case USART_Mode (No) is
               when USART_Type => USARTx (No).all.CR3.CTSIE   := State;
               when UART_Type  => raise Not_Available_on_UARTs;
            end case;

      end case;
      Write_CR1 (No, CR1_Copy);
   end Enable_Disable_Interrupts;

   procedure Enable_Interrupts  (No : USARTs; Intr : USART_Status_Flags) is

   begin
      Enable_Disable_Interrupts (No, Intr, Enable);
   end Enable_Interrupts;

   procedure Disable_Interrupts (No : USARTs; Intr : USART_Status_Flags) is

   begin
      Enable_Disable_Interrupts (No, Intr, Disable);
   end Disable_Interrupts;

   procedure Send_Break (No : USARTs) is

      CR1_Copy : USART_CR1 := Read_CR1 (No);

   begin
      CR1_Copy.SBK := Send;
      Write_CR1 (No, CR1_Copy);
   end Send_Break;

   procedure Configure_USART_Format
     (No               : USARTs_only;
      Word_Length      : M_Options     := Data_Bits_8;
      Parity_Control   : Enabler       := Disable;
      Parity_Selection : Parity        := Even;
      Stop_Bits        : SSTOP_Options := Stop_One;
      One_Bit_Sampling : Enabler       := Disable;
      Half_Duplex      : Enabler       := Disable) is

   begin
      USARTx (No).all.CR1.M      := Word_Length;
      USARTx (No).all.CR1.PCE    := Parity_Control;
      USARTx (No).all.CR1.PS     := Parity_Selection;
      USARTx (No).all.CR2.STOP   := Stop_Bits;
      USARTx (No).all.CR3.ONEBIT := One_Bit_Sampling;
      USARTx (No).all.CR3.HDSEL  := Half_Duplex;
   end Configure_USART_Format;

   procedure Configure_UART_Format
     (No               : UARTs;
      Word_Length      : M_Options     := Data_Bits_8;
      Parity_Control   : Enabler       := Disable;
      Parity_Selection : Parity        := Even;
      Stop_Bits        : ASTOP_Options := Stop_One;
      One_Bit_Sampling : Enabler       := Disable;
      Half_Duplex      : Enabler       := Disable) is

   begin
      UARTx (No).all.CR1.M      := Word_Length;
      UARTx (No).all.CR1.PCE    := Parity_Control;
      UARTx (No).all.CR1.PS     := Parity_Selection;
      UARTx (No).all.CR2.STOP   := Stop_Bits;
      UARTx (No).all.CR3.ONEBIT := One_Bit_Sampling;
      UARTx (No).all.CR3.HDSEL  := Half_Duplex;
   end Configure_UART_Format;

   procedure Set_USART_Address
     (No               : USARTs;
      USART_Address    : Bits_4        := 0) is

   begin
      case USART_Mode (No) is
         when USART_Type => USARTx (No).all.CR2.ADD := USART_Address;
         when UART_Type  => UARTx  (No).all.CR2.ADD := USART_Address;
      end case;
   end Set_USART_Address;

   procedure Configure_Local_Interconnection_Network
     (No               : USARTs;
      LIN_mode         : Enabler       := Disable;
      LIN_Length       : LBDL_Options  := Break_Detection_10bit) is

   begin
      case USART_Mode (No) is
         when USART_Type =>
            USARTx (No).all.CR2.LINEN := LIN_mode;
            USARTx (No).all.CR2.LBDL  := LIN_Length;
         when UART_Type  =>
            UARTx  (No).all.CR2.LINEN := LIN_mode;
            UARTx  (No).all.CR2.LBDL  := LIN_Length;
      end case;
   end Configure_Local_Interconnection_Network;

   procedure Configure_IrDA
     (No               : USARTs;
      IrDA_Mode        : Enabler       := Disable;
      IrDA_Low_Power   : IRLP_Options  := Normal;
      Prescaler        : Bits_8        := 0) is -- Prescaler has no effect for UARTs 4 and 5

   begin
      case USART_Mode (No) is
         when USART_Type =>
            USARTx (No).all.CR3.IREN := IrDA_Mode;
            USARTx (No).all.CR3.IRLP := IrDA_Low_Power;
            USARTx (No).all.GTPR.PSC := Prescaler;
         when UART_Type  =>
            UARTx  (No).all.CR3.IREN := IrDA_Mode;
            UARTx  (No).all.CR3.IRLP := IrDA_Low_Power;
            -- Omitting Prescaler setting for UARTs
      end case;
   end Configure_IrDA;

   procedure Configure_Smartcard
     (No               : USARTs_only;
      SC_Mode,
      SC_NACK          : Enabler       := Disable;
      Guard_Time,
      Prescaler        : Bits_8        := 0) is

   begin
      USARTx (No).all.CR3.SCEN := SC_Mode;
      USARTx (No).all.CR3.NACK := SC_NACK;
      USARTx (No).all.GTPR.GT  := Guard_Time;
      USARTx (No).all.GTPR.PSC := Prescaler;
   end Configure_Smartcard;

   procedure Configure_Wakeup
     (No               : USARTs;
      Wakeup           : WAKE_Options  := Idle_Line;
      Receiver_wakeup  : Muting        := Active) is

   begin
      case USART_Mode (No) is
         when USART_Type =>
            USARTx (No).all.CR1.WAKE := Wakeup;
            USARTx (No).all.CR1.RWU  := Receiver_wakeup;
         when UART_Type  =>
            UARTx  (No).all.CR1.WAKE := Wakeup;
            UARTx  (No).all.CR1.RWU  := Receiver_wakeup;
      end case;
   end Configure_Wakeup;

   procedure Configure_Sync
     (No               : USARTs_only;
      Clock_Pin        : Enabler       := Disable;
      Clock_Polarity   : CPOL_Options  := Low;
      Clock_Phase      : CPHA_Options  := First;
      Clock_Pulse      : Enabler       := Disable;
      CTS_Enable       : Enabler       := Disable;
      RTS_Enable       : Enabler       := Disable) is

   begin
      USARTx (No).all.CR2.CLKEN := Clock_Pin;
      USARTx (No).all.CR2.CPOL  := Clock_Polarity;
      USARTx (No).all.CR2.CPHA  := Clock_Phase;
      USARTx (No).all.CR2.LBCL  := Clock_Pulse;
      USARTx (No).all.CR3.CTSE  := CTS_Enable;
      USARTx (No).all.CR3.RTSE  := RTS_Enable;
   end Configure_Sync;

   procedure Configure_DMA
     (No               : USARTs;
      DMA_Transmitter,
      DMA_Receiver     : Enabler) is

   begin
      case USART_Mode (No) is
         when USART_Type =>
            USARTx (No).all.CR3.DMAT := DMA_Transmitter;
            USARTx (No).all.CR3.DMAR := DMA_Receiver;
         when UART_Type  =>
            UARTx  (No).all.CR3.DMAT := DMA_Transmitter;
            UARTx  (No).all.CR3.DMAR := DMA_Receiver;
      end case;
   end Configure_DMA;

end STM32F4.USART.Ops;

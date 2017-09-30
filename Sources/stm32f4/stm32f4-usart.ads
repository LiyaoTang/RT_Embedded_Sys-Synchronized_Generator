--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx Universal synchronous asynchronous receiver transmitter

package STM32F4.USART is

   -- General purpose types to identify connections to the U(S)ARTs

   type    USARTs      is        range 1 .. 6;
   subtype UARTs       is USARTs range 4 .. 5;
   subtype USARTs_only is USARTs range 1 .. 6
     with Static_Predicate => (USARTs_only <= 3 or else USARTs_only = 6);

   type USART_Modes is (UART_Type, USART_Type);

   USART_Mode : constant array (USARTs) of USART_Modes :=
     (1 => USART_Type,
      2 => USART_Type,
      3 => USART_Type,
      4 => UART_Type,
      5 => UART_Type,
      6 => USART_Type);

   type    USART_Pins is (UART_Tx, UART_Rx, SW_Rx, SClk, nRTS, nCTS);
   subtype UART_Pins  is USART_Pins range UART_Tx .. SW_Rx;

   type UART_Connection (No : USARTs) is record
      case No is
         when UARTs  => UART_Pin  : UART_Pins;
         when others => USART_Pin : USART_Pins;
      end case;
   end record;

   type UART_Connection_Access is access all UART_Connection;

   -- USART_CR1 Control 1

   type WAKE_Options is (Idle_Line,
                         Address_Mark)      with Size => 1;

   type M_Options is (Data_Bits_8,
                      Data_Bits_9)      with Size => 1;

   type OVER8_Options is (Oversampling_by_16,
                          Oversampling_by_8)      with Size => 1;

   -- USART_CR2 Control 2

   type LBDL_Options is (Break_Detection_10bit,
                         Break_Detection_11bit)      with Size => 1;

   type CPHA_Options is (First,
                         Second)      with Size => 1;

   type CPOL_Options is (Low,
                         High)      with Size => 1;

   type SSTOP_Options is (Stop_One,
                          Stop_Half,
                          Stop_Two,
                          Stop_One_and_a_half) with Size => 2;

   -- UART_CR2 Control 2

   type ASTOP_Options is (Stop_One,
                          Stop_Two) with Size => 2;

   -- USART_C3 Control 3

   type IRLP_Options is (Normal,
                         Low_Power)      with Size => 1;
private

   -- USART_SR - Status

   type USART_SR is record
      PE       : Occurance; -- Parity error (read only)
      FE       : Occurance; -- Framing error (read only)
      NF       : Occurance; -- Noise detected flag (read only)
      ORE      : Occurance; -- Overrun error (read only)
      IDLE     : Detection; -- IDLE line detected (read only)
      RXNE     : Readiness; -- Read data register not empty (read & clear on write)
      TC       : Readiness; -- Transmission complete (read & clear on write
      TXE      : Readiness; -- Transmit data register empty (read only)
      LBD      : Detection; -- LIN break detection flag (read & clear on write
      CTS      : Occurance; -- CTS flag (read & clear on write
      Reserved : Bits_22;
   end record with Volatile, Size => Word'Size;

   for USART_SR use record
      PE       at 0 range  0 ..  0; -- Parity error (read only)
      FE       at 0 range  1 ..  1; -- Framing error (read only)
      NF       at 0 range  2 ..  2; -- Noise detected flag (read only)
      ORE      at 0 range  3 ..  3; -- Overrun error (read only)
      IDLE     at 0 range  4 ..  4; -- IDLE line detected (read only)
      RXNE     at 0 range  5 ..  5; -- Read data register not empty (read & clear on write)
      TC       at 0 range  6 ..  6; -- Transmission complete (read & clear on write
      TXE      at 0 range  7 ..  7; -- Transmit data register empty (read only)
      LBD      at 0 range  8 ..  8; -- LIN break detection flag (read & clear on write
      CTS      at 0 range  9 ..  9; -- CTS flag (read & clear on write
      Reserved at 0 range 10 .. 31;
   end record;

   USART_SR_Reset : constant USART_SR :=
     (PE       => Not_Occured,
      FE       => Not_Occured,
      NF       => Not_Occured,
      ORE      => Not_Occured,
      IDLE     => Not_Detected,
      RXNE     => Not_Ready,
      TC       => Not_Ready,
      TXE      => Not_Ready,
      LBD      => Not_Detected,
      CTS      => Not_Occured,
      Reserved => 2#0000000011000000000000#);

   -- USART_DR Data

   type USART_DR is record
      Data     : Bits_9;  -- Data (read & write)
      Reserved : Bits_23;
   end record with Volatile, Size => Word'Size;

   for USART_DR use record
      Data     at 0 range  0 ..  8; -- Data (read & write)
      Reserved at 0 range  9 .. 31;
   end record;

   -- USART_BRR Baud rate

   type USART_BRR is record
      DIV_Fraction : Bits_4;  -- Fraction of USARTDIV (read & write)
      DIV_Mantissa : Bits_12; -- Mantissa of USARTDIV (read & write)
      Reserved     : Bits_16;
   end record with Volatile, Size => Word'Size;

   for USART_BRR use record
      DIV_Fraction at 0 range  0 ..  3; -- Fraction of USARTDIV (read & write)
      DIV_Mantissa at 0 range  4 .. 15; -- Mantissa of USARTDIV (read & write)
      Reserved     at 0 range 16 .. 31;
   end record;

   USART_BRR_Reset : constant USART_BRR :=
     (DIV_Fraction => 0,
      DIV_Mantissa => 0,
      Reserved     => 0);

   -- USART_CR1 Control 1

   for WAKE_Options use (Idle_Line => 0, Address_Mark => 1);

   for M_Options use (Data_Bits_8 => 0, Data_Bits_9 => 1);

   for OVER8_Options use (Oversampling_by_16 => 0, Oversampling_by_8 => 1);

   type USART_CR1 is record
      SBK        : Sending;       -- Send break (read & write)
      RWU        : Muting;        -- Receiver wakeup (read & write)
      RE         : Enabler;       -- Receiver enable (read & write)
      TE         : Enabler;       -- Transmitter enable (read & write)
      IDLEIE     : Enabler;       -- IDLE interrupt enable (read & write)
      RXNEIE     : Enabler;       -- RXNE interrupt enable (read & write)
      TCIE       : Enabler;       -- Transmission complete interrupt enable (read & write)
      TXEIE      : Enabler;       -- TXE interrupt enable (read & write)
      PEIE       : Enabler;       -- PE interrupt enable (read & write)
      PS         : Parity;        -- Parity selection (read & write)
      PCE        : Enabler;       -- Parity control enable (read & write)
      WAKE       : WAKE_Options;  -- Wakeup method (read & write)
      M          : M_Options;     -- Word length (read & write)
      UE         : Enabler;       -- USART enable (read & write)
      Reserved_1 : Bit;
      OVER8      : OVER8_Options; -- Oversampling mode (read & write)
      Reserved_2 : Bits_16;
   end record with Volatile, Size => Word'Size;

   for USART_CR1 use record
      SBK        at 0 range  0 ..  0; -- Send break (read & write)
      RWU        at 0 range  1 ..  1; -- Receiver wakeup (read & write)
      RE         at 0 range  2 ..  2; -- Receiver enable (read & write)
      TE         at 0 range  3 ..  3; -- Transmitter enable (read & write)
      IDLEIE     at 0 range  4 ..  4; -- IDLE interrupt enable (read & write)
      RXNEIE     at 0 range  5 ..  5; -- RXNE interrupt enable (read & write)
      TCIE       at 0 range  6 ..  6; -- Transmission complete interrupt enable (read & write)
      TXEIE      at 0 range  7 ..  7; -- TXE interrupt enable (read & write)
      PEIE       at 0 range  8 ..  8; -- PE interrupt enable (read & write)
      PS         at 0 range  9 ..  9; -- Parity selection (read & write)
      PCE        at 0 range 10 .. 10; -- Parity control enable (read & write)
      WAKE       at 0 range 11 .. 11; -- Wakeup method (read & write)
      M          at 0 range 12 .. 12; -- Word length (read & write)
      UE         at 0 range 13 .. 13; -- USART enable (read & write)
      Reserved_1 at 0 range 14 .. 14;
      OVER8      at 0 range 15 .. 15; -- Oversampling mode (read & write)
      Reserved_2 at 0 range 16 .. 31;
   end record;

   USART_CR1_Reset : constant USART_CR1 :=
     (SBK        => Do_Nothing,         -- Send break (read & write)
      RWU        => Active,             -- Receiver wakeup (read & write)
      RE         => Disable,            -- Receiver enable (read & write)
      TE         => Disable,            -- Transmitter enable (read & write)
      IDLEIE     => Disable,            -- IDLE interrupt enable (read & write)
      RXNEIE     => Disable,            -- RXNE interrupt enable (read & write)
      TCIE       => Disable,            -- Transmission complete interrupt enable (read & write)
      TXEIE      => Disable,            -- TXE interrupt enable (read & write)
      PEIE       => Disable,            -- PE interrupt enable (read & write)
      PS         => Even,               -- Parity selection (read & write)
      PCE        => Disable,            -- Parity control enable (read & write)
      WAKE       => Idle_Line,          -- Wakeup method (read & write)
      M          => Data_Bits_8,        -- Word length (read & write)
      UE         => Disable,            -- USART enable (read & write)
      Reserved_1 => 0,
      OVER8      => Oversampling_by_16, -- Oversampling mode (read & write)
      Reserved_2 => 0);

   -- USART_CR2 Control 2

   for LBDL_Options use (Break_Detection_10bit => 0, Break_Detection_11bit => 1);

   for CPHA_Options use (First => 0, Second => 1);

   for CPOL_Options use (Low => 0, High => 1);

   for SSTOP_Options use (Stop_One            => 0,
                          Stop_Half           => 1,
                          Stop_Two            => 2,
                          Stop_One_and_a_half => 3);

   type USART_CR2 is record
      ADD        : Bits_4;        -- Address of the USART node (read & write)
      Reserved_1 : Bit;
      LBDL       : LBDL_Options;  -- Lin break detection length (read & write)
      LBDIE      : Enabler;       -- LIN break detection interrupt enable (read & write)
      Reserved_2 : Bit;
      LBCL       : Enabler;       -- Last bit clock pulse (read & write)
      CPHA       : CPHA_Options;  -- Clock phase (read & write)
      CPOL       : CPOL_Options;  -- Clock polarity (read & write)
      CLKEN      : Enabler;       -- Clock enable (read & write)
      STOP       : SSTOP_Options; -- STOP bits (read & write)
      LINEN      : Enabler;       -- LIN mode enable (read & write)
      Reserved_3 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for USART_CR2 use record
      ADD        at 0 range  0 ..  3; -- Address of the USART node (read & write)
      Reserved_1 at 0 range  4 ..  4;
      LBDL       at 0 range  5 ..  5; -- Lin break detection length (read & write)
      LBDIE      at 0 range  6 ..  6; -- LIN break detection interrupt enable (read & write)
      Reserved_2 at 0 range  7 ..  7;
      LBCL       at 0 range  8 ..  8; -- Last bit clock pulse (read & write)
      CPHA       at 0 range  9 ..  9; -- Clock phase (read & write)
      CPOL       at 0 range 10 .. 10; -- Clock polarity (read & write)
      CLKEN      at 0 range 11 .. 11; -- Clock enable (read & write)
      STOP       at 0 range 12 .. 13; -- STOP bits (read & write)
      LINEN      at 0 range 14 .. 14; -- LIN mode enable (read & write)
      Reserved_3 at 0 range 15 .. 31;
   end record;

   USART_CR2_Reset : constant USART_CR2 :=
     (ADD        => 0,                     -- Address of the USART node (read & write)
      Reserved_1 => 0,
      LBDL       => Break_Detection_10bit, -- Lin break detection length (read & write)
      LBDIE      => Disable,               -- LIN break detection interrupt enable (read & write)
      Reserved_2 => 0,
      LBCL       => Disable,               -- Last bit clock pulse (read & write)
      CPHA       => First,                 -- Clock phase (read & write)
      CPOL       => Low,                   -- Clock polarity (read & write)
      CLKEN      => Disable,               -- Clock enable (read & write)
      STOP       => Stop_One,              -- STOP bits (read & write)
      LINEN      => Disable,               -- LIN mode enable (read & write)
      Reserved_3 => 0);

   -- UART_CR2 Control 2

   for ASTOP_Options use (Stop_One => 0,
                          Stop_Two => 2);

   type UART_CR2 is record
      ADD        : Bits_4;        -- Address of the USART node (read & write)
      Reserved_1 : Bit;
      LBDL       : LBDL_Options;  -- Lin break detection length (read & write)
      LBDIE      : Enabler;       -- LIN break detection interrupt enable (read & write)
      Reserved_2 : Bits_5;
      STOP       : ASTOP_Options; -- STOP bits (read & write)
      LINEN      : Enabler;       -- LIN mode enable (read & write)
      Reserved_3 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for UART_CR2 use record
      ADD        at 0 range  0 ..  3; -- Address of the USART node (read & write)
      Reserved_1 at 0 range  4 ..  4;
      LBDL       at 0 range  5 ..  5; -- Lin break detection length (read & write)
      LBDIE      at 0 range  6 ..  6; -- LIN break detection interrupt enable (read & write)
      Reserved_2 at 0 range  7 .. 11;
      STOP       at 0 range 12 .. 13; -- STOP bits (read & write)
      LINEN      at 0 range 14 .. 14; -- LIN mode enable (read & write)
      Reserved_3 at 0 range 15 .. 31;
   end record;

   UART_CR2_Reset : constant UART_CR2 :=
     (ADD        => 0,                     -- Address of the USART node (read & write)
      Reserved_1 => 0,
      LBDL       => Break_Detection_10bit, -- Lin break detection length (read & write)
      LBDIE      => Disable,               -- LIN break detection interrupt enable (read & write)
      Reserved_2 => 0,
      STOP       => Stop_One,              -- STOP bits (read & write)
      LINEN      => Disable,               -- LIN mode enable (read & write)
      Reserved_3 => 0);

   -- USART_C3 Control 3

   for IRLP_Options use (Normal => 0, Low_Power => 1);

   type USART_CR3 is record
      EIE      : Enabler;      -- Error interrupt enable (read & write)
      IREN     : Enabler;      -- IrDA mode enable (read & write)
      IRLP     : IRLP_Options; -- IrDA low-power (read & write)
      HDSEL    : Enabler;      -- Half-duplex selection (read & write)
      NACK     : Enabler;      -- Smartcard NACK enable (read & write)
      SCEN     : Enabler;      -- Smartcard mode enable (read & write)
      DMAR     : Enabler;      -- DMA enable receiver (read & write)
      DMAT     : Enabler;      -- DMA enable transmitter (read & write)
      RTSE     : Enabler;      -- RTS enable (read & write)
      CTSE     : Enabler;      -- CTS enable (read & write)
      CTSIE    : Enabler;      -- CTS interrupt enable (read & write)
      ONEBIT   : Enabler;      -- One sample bit method enable (read & write)
      Reserved : Bits_20;
   end record with Volatile, Size => Word'Size;

   for USART_CR3 use record
      EIE      at 0 range  0 ..  0; -- Error interrupt enable (read & write)
      IREN     at 0 range  1 ..  1; -- IrDA mode enable (read & write)
      IRLP     at 0 range  2 ..  2; -- IrDA low-power (read & write)
      HDSEL    at 0 range  3 ..  3; -- Half-duplex selection (read & write)
      NACK     at 0 range  4 ..  4; -- Smartcard NACK enable (read & write)
      SCEN     at 0 range  5 ..  5; -- Smartcard mode enable (read & write)
      DMAR     at 0 range  6 ..  6; -- DMA enable receiver (read & write)
      DMAT     at 0 range  7 ..  7; -- DMA enable transmitter (read & write)
      RTSE     at 0 range  8 ..  8; -- RTS enable (read & write)
      CTSE     at 0 range  9 ..  9; -- CTS enable (read & write)
      CTSIE    at 0 range 10 .. 10; -- CTS interrupt enable (read & write)
      ONEBIT   at 0 range 11 .. 11; -- One sample bit method enable (read & write)
      Reserved at 0 range 12 .. 31;
   end record;

   USART_CR3_Reset : constant USART_CR3 :=
     (EIE      => Disable, -- Error interrupt enable (read & write)
      IREN     => Disable, -- IrDA mode enable (read & write)
      IRLP     => Normal,  -- IrDA low-power (read & write)
      HDSEL    => Disable, -- Half-duplex selection (read & write)
      NACK     => Disable, -- Smartcard NACK enable (read & write)
      SCEN     => Disable, -- Smartcard mode enable (read & write)
      DMAR     => Disable, -- DMA enable receiver (read & write)
      DMAT     => Disable, -- DMA enable transmitter (read & write)
      RTSE     => Disable, -- RTS enable (read & write)
      CTSE     => Disable, -- CTS enable (read & write)
      CTSIE    => Disable, -- CTS interrupt enable (read & write)
      ONEBIT   => Disable, -- One sample bit method enable (read & write)
      Reserved => 0);

   -- UART_C3 Control 3

   type UART_CR3 is record
      EIE        : Enabler;      -- Error interrupt enable (read & write)
      IREN       : Enabler;      -- IrDA mode enable (read & write)
      IRLP       : IRLP_Options; -- IrDA low-power (read & write)
      HDSEL      : Enabler;      -- Half-duplex selection (read & write)
      Reserved_1 : Bits_2;
      DMAR       : Enabler;      -- DMA enable receiver (read & write)
      DMAT       : Enabler;      -- DMA enable transmitter (read & write)
      Reserved_2 : Bits_3;
      ONEBIT     : Enabler;      -- One sample bit method enable (read & write)
      Reserved_3 : Bits_20;
   end record with Volatile, Size => Word'Size;

   for UART_CR3 use record
      EIE        at 0 range  0 ..  0; -- Error interrupt enable (read & write)
      IREN       at 0 range  1 ..  1; -- IrDA mode enable (read & write)
      IRLP       at 0 range  2 ..  2; -- IrDA low-power (read & write)
      HDSEL      at 0 range  3 ..  3; -- Half-duplex selection (read & write)
      Reserved_1 at 0 range  4 ..  5;
      DMAR       at 0 range  6 ..  6; -- DMA enable receiver (read & write)
      DMAT       at 0 range  7 ..  7; -- DMA enable transmitter (read & write)
      Reserved_2 at 0 range  8 .. 10;
      ONEBIT     at 0 range 11 .. 11; -- One sample bit method enable (read & write)
      Reserved_3 at 0 range 12 .. 31;
   end record;

   UART_CR3_Reset : constant UART_CR3 :=
     (EIE        => Disable, -- Error interrupt enable (read & write)
      IREN       => Disable, -- IrDA mode enable (read & write)
      IRLP       => Normal,  -- IrDA low-power (read & write)
      HDSEL      => Disable, -- Half-duplex selection (read & write)
      Reserved_1 => 0,
      DMAR       => Disable, -- DMA enable receiver (read & write)
      DMAT       => Disable, -- DMA enable transmitter (read & write)
      Reserved_2 => 0,
      ONEBIT     => Disable, -- One sample bit method enable (read & write)
      Reserved_3 => 0);

   -- USART_GTPR Guard time and prescaler

   type USART_GTPR is record
      PSC      : Bits_8; -- Prescaler value (read & write)
      GT       : Bits_8; -- Guard time value (read & write)
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for USART_GTPR use record
      PSC      at 0 range  0 ..  7; -- Prescaler value (read & write)
      GT       at 0 range  8 .. 15; -- Guard time value (read & write)
      Reserved at 0 range 16 .. 31;
   end record;

   USART_GTPR_Reset : constant USART_GTPR :=
     (PSC      => 0, -- Prescaler value (read & write)
      GT       => 0, -- Guard time value (read & write)
      Reserved => 0);

   -- USART Register

   type USART_Register is record
      SR         : USART_SR;   -- Status
      DR         : USART_DR;   -- Data
      BRR        : USART_BRR;  -- Baud rate
      CR1        : USART_CR1;  -- Control 1
      CR2        : USART_CR2;  -- Control 2
      CR3        : USART_CR3;  -- Control 3
      GTPR       : USART_GTPR; -- Guard time and prescaler
   end record with Volatile, Size => Byte'Size * 16#1C#;

   for USART_Register use record
      SR         at 16#00# range  0 .. 31; -- Status
      DR         at 16#04# range  0 .. 31; -- Data
      BRR        at 16#08# range  0 .. 31; -- Baud rate
      CR1        at 16#0C# range  0 .. 31; -- Control 1
      CR2        at 16#10# range  0 .. 31; -- Control 2
      CR3        at 16#14# range  0 .. 31; -- Control 3
      GTPR       at 16#18# range  0 .. 31; -- Guard time and prescaler
   end record;

   -- UART Register

   type UART_Register is record
      SR         : USART_SR;  -- Status
      DR         : USART_DR;  -- Data
      BRR        : USART_BRR; -- Baud rate
      CR1        : USART_CR1; -- Control 1
      CR2        : UART_CR2;  -- Control 2
      CR3        : UART_CR3;  -- Control 3
   end record with Volatile, Size => Byte'Size * 16#18#;

    for UART_Register use record
      SR         at 16#00# range  0 .. 31; -- Status
      DR         at 16#04# range  0 .. 31; -- Data
      BRR        at 16#08# range  0 .. 31; -- Baud rate
      CR1        at 16#0C# range  0 .. 31; -- Control 1
      CR2        at 16#10# range  0 .. 31; -- Control 2
      CR3        at 16#14# range  0 .. 31; -- Control 3
   end record;

   -- Set register addresses

   USART1 : aliased USART_Register with
     Volatile, Address => System'To_Address (Base_USART1), Import;

   USART2 : aliased USART_Register with
     Volatile, Address => System'To_Address (Base_USART2), Import;

   USART3 : aliased USART_Register with
     Volatile, Address => System'To_Address (Base_USART3), Import;

   UART4 : aliased UART_Register with
     Volatile, Address => System'To_Address (Base_UART4), Import;

   UART5 : aliased UART_Register with
     Volatile, Address => System'To_Address (Base_UART5), Import;

   USART6 : aliased USART_Register with
     Volatile, Address => System'To_Address (Base_USART6), Import;

   -- Mapping U(S)ARTS to registers

   type USART_Access is access all USART_Register;
   type UART_Access  is access all UART_Register;

   function USARTx (No : USARTs) return USART_Access is

     (case No is
         when  1     => USART1'Access,
         when  2     => USART2'Access,
         when  3     => USART3'Access,
         when  6     => USART6'Access,
         when others => null);

   function UARTx (No : UARTs) return UART_Access is

     (case No is
         when 4 => UART4'Access,
         when 5 => UART5'Access);

end STM32F4.USART;

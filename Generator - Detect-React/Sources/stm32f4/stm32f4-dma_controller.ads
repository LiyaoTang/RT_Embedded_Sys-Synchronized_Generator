--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- STM32F4xx DMA controller

package STM32F4.DMA_controller is

   type DMAs    is range 1 .. 2;
   type Streams is range 0 .. 7;

   -- DMA_SxCR - Stream x configuration

   type PFCTRL_Options is (DMA_Flowcontrol,
                           Peripheral_Flowcontrol) with Size => 1;

   type DIR_Options is (Peripheral_to_Memory,
                        Memory_to_Peripheral,
                        Memory_to_Memory) with Size => 2;

   type INC_Options is (Address_Fixed,
                        Address_Incremented) with Size => 1;

   type SIZE_Options is (Byte_Size,
                         Half_Word_Size,
                         Word_Size) with Size => 2;

   type PINCOS_Options is (Linked_to_PSIZE,
                           Fixed_to_4)    with Size => 1;

   type PL_Options is (Low,
                       Medium,
                       High,
                       Very_high) with Size => 2;

   type CT_Options is (Memory_0,
                       Memory_1)      with Size => 1;

   type BURST_Options is (Single,
                          INCR4,
                          INCR8,
                          INCR16) with Size => 2;

   -- DMA_SxFCR - Stream x FIFO control

   type FIFO_Options is (Less_than_quarter,
                         Less_than_half,
                         Less_than_three_quarters,
                         Less_than_full,
                         Empty,
                         Full) with Size => 3;

   type FTH_Options is (Quarter_full,
                        Half_full,
                        Three_quarters_full,
                        Full) with Size => 2;

private

   -- DMA_LISR - Low interrupt status

   type Status_Module is record
      FE       : Occurance; -- Stream x FIFO error interrupt flag (read only)
      Reserved : Bit;
      DME      : Occurance; -- Stream x direct mode error interrupt flag (read only)
      TE       : Occurance; -- Stream x transfer error interrupt flag (read only)
      HT       : Occurance; -- Stream x half transfer interrupt flag (read only)
      TC       : Occurance; -- Stream x transfer complete interrupt flag (read only)
   end record with Size => 6;

   for Status_Module use record
      FE       at 0 range  0 ..  0; -- Stream x FIFO error interrupt flag (read only)
      Reserved at 0 range  1 ..  1;
      DME      at 0 range  2 ..  2; -- Stream x direct mode error interrupt flag (read only)
      TE       at 0 range  3 ..  3; -- Stream x transfer error interrupt flag (read only)
      HT       at 0 range  4 ..  4; -- Stream x half transfer interrupt flag (read only)
      TC       at 0 range  5 ..  5; -- Stream x transfer complete interrupt flag (read only)
   end record;

   type DMA_LISR is record
      IF0,
      IF1        : Status_Module;
      Reserved_1 : Bits_4;
      IF2,
      IF3        : Status_Module;
      Reserved_2 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for DMA_LISR use record
      IF0        at 0 range  0 ..  5;
      IF1        at 0 range  6 .. 11;
      Reserved_1 at 0 range 12 .. 15;
      IF2        at 0 range 16 .. 21;
      IF3        at 0 range 22 .. 27;
      Reserved_2 at 0 range 28 .. 31;
   end record;

   -- DMA_HISR - High interrupt status

   type DMA_HISR is record
      IF4,
      IF5        : Status_Module;
      Reserved_1 : Bits_4;
      IF6,
      IF7        : Status_Module;
      Reserved_2 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for DMA_HISR use record
      IF4        at 0 range  0 ..  5;
      IF5        at 0 range  6 .. 11;
      Reserved_1 at 0 range 12 .. 15;
      IF6        at 0 range 16 .. 21;
      IF7        at 0 range 22 .. 27;
      Reserved_2 at 0 range 28 .. 31;
   end record;

   -- DMA_LIFCR - Low interrupt flag clear

   type Clear_Module is record
      CFE      : Clearing; -- Stream x clear FIFO error interrupt flag (write only)
      Reserved : Bit;
      CDME     : Clearing; -- Stream x clear direct mode error interrupt flag (write only)
      CTE      : Clearing; -- Stream x clear transfer error interrupt flag (write only)
      CHT      : Clearing; -- Stream x clear half transfer interrupt flag (write only)
      CTC      : Clearing; -- Stream x clear transfer complete interrupt flag (write only)
   end record with Size => 6;

   for Clear_Module use record
      CFE      at 0 range  0 ..  0; -- Stream x clear FIFO error interrupt flag (write only)
      Reserved at 0 range  1 ..  1;
      CDME     at 0 range  2 ..  2; -- Stream x clear direct mode error interrupt flag (write only)
      CTE      at 0 range  3 ..  3; -- Stream x clear transfer error interrupt flag (write only)
      CHT      at 0 range  4 ..  4; -- Stream x clear half transfer interrupt flag (write only)
      CTC      at 0 range  5 ..  5; -- Stream x clear transfer complete interrupt flag (write only)
   end record;

   Clear_Module_Reset : constant Clear_Module :=
     (CFE      => Do_Nothing,
      Reserved => 0,
      CDME     => Do_Nothing,
      CTE      => Do_Nothing,
      CHT      => Do_Nothing,
      CTC      => Do_Nothing);

   type DMA_LIFCR is record
      IF0,
      IF1        : Clear_Module;
      Reserved_1 : Bits_4;
      IF2,
      IF3        : Clear_Module;
      Reserved_2 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for DMA_LIFCR use record
      IF0        at 0 range  0 ..  5;
      IF1        at 0 range  6 .. 11;
      Reserved_1 at 0 range 12 .. 15;
      IF2        at 0 range 16 .. 21;
      IF3        at 0 range 22 .. 27;
      Reserved_2 at 0 range 28 .. 31;
   end record;

   DMA_LIFCR_Reset : constant DMA_LIFCR :=
     (IF0        => Clear_Module_Reset,
      IF1        => Clear_Module_Reset,
      Reserved_1 => 0,
      IF2        => Clear_Module_Reset,
      IF3        => Clear_Module_Reset,
      Reserved_2 => 0);

   -- DMA_HIFCR - High interrupt flag clear

   type DMA_HIFCR is record
      IF4,
      IF5        : Clear_Module;
      Reserved_1 : Bits_4;
      IF6,
      IF7        : Clear_Module;
      Reserved_2 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for DMA_HIFCR use record
      IF4        at 0 range  0 ..  5;
      IF5        at 0 range  6 .. 11;
      Reserved_1 at 0 range 12 .. 15;
      IF6        at 0 range 16 .. 21;
      IF7        at 0 range 22 .. 27;
      Reserved_2 at 0 range 28 .. 31;
   end record;

   DMA_HIFCR_Reset : constant DMA_HIFCR :=
     (IF4        => Clear_Module_Reset,
      IF5        => Clear_Module_Reset,
      Reserved_1 => 0,
      IF6        => Clear_Module_Reset,
      IF7        => Clear_Module_Reset,
      Reserved_2 => 0);
   -- DMA_SxCR - Stream x configuration

   for PFCTRL_Options use (DMA_Flowcontrol        => 0,
                           Peripheral_Flowcontrol => 1);

   for DIR_Options use (Peripheral_to_Memory => 0,
                        Memory_to_Peripheral => 1,
                        Memory_to_Memory     => 2);

   for INC_Options use (Address_Fixed       => 0,
                        Address_Incremented => 1);

   for SIZE_Options use (Byte_Size => 0, Half_Word_Size => 1, Word_Size => 2);

   for PINCOS_Options use (Linked_to_PSIZE => 0, Fixed_to_4 => 1);

   for PL_Options use (Low       => 0,
                       Medium    => 1,
                       High      => 2,
                       Very_high => 3);

   for CT_Options use (Memory_0 => 0, Memory_1 => 1);

   for BURST_Options use (Single => 0, INCR4 => 1, INCR8 => 2, INCR16 => 3);

   type DMA_SxCR is record
      EN         : Enabler;        -- Stream enable / flag stream ready when read low (read & write)
      DMEIE      : Enabler;        -- Direct mode error interrupt enable (read & write)
      TEIE       : Enabler;        -- Transfer error interrupt enable (read & write)
      HTIE       : Enabler;        -- Half transfer interrupt enable (read & write)
      TCIE       : Enabler;        -- Transfer complete interrupt enable (read & write)
      PFCTRL     : PFCTRL_Options; -- Peripheral flow controller (read & write)
      DIR        : DIR_Options;    -- Data transfer direction (read & write)
      CIRC       : Enabler;        -- Circular mode (read & write)
      PINC       : INC_Options;    -- Peripheral increment mode (read & write)
      MINC       : INC_Options;    -- Memory increment mode (read & write)
      PSIZE      : SIZE_Options;   -- Peripheral data size (read & write)
      MSIZE      : SIZE_Options;   -- Memory data size (read & write)
      PINCOS     : PINCOS_Options; -- Peripheral increment offset size (read & write)
      PL         : PL_Options;     -- Priority level (read & write)
      DBM        : Enabler;        -- Double buffer mode (read & write)
      CT         : CT_Options;     -- Current target (only in double buffer mode) (read & write)
      Reserved_1 : Bit;
      PBURST     : BURST_Options;  -- Peripheral burst transfer configuration (read & write)
      MBURST     : BURST_Options;  -- Memory burst transfer configuration (read & write)
      CHSEL      : Bits_3;         -- Channel selection (read & write)
      Reserved_2 : Bits_4;
   end record with Volatile, Size => Word'Size;

   for DMA_SxCR use record
      EN         at 0 range  0 ..  0; -- Stream enable / flag stream ready when read low (read & write)
      DMEIE      at 0 range  1 ..  1; -- Direct mode error interrupt enable (read & write)
      TEIE       at 0 range  2 ..  2; -- Transfer error interrupt enable (read & write)
      HTIE       at 0 range  3 ..  3; -- Half transfer interrupt enable (read & write)
      TCIE       at 0 range  4 ..  4; -- Transfer complete interrupt enable (read & write)
      PFCTRL     at 0 range  5 ..  5; -- Peripheral flow controller (read & write)
      DIR        at 0 range  6 ..  7; -- Data transfer direction (read & write)
      CIRC       at 0 range  8 ..  8; -- Circular mode (read & write)
      PINC       at 0 range  9 ..  9; -- Peripheral increment mode (read & write)
      MINC       at 0 range 10 .. 10; -- Memory increment mode (read & write)
      PSIZE      at 0 range 11 .. 12; -- Peripheral data size (read & write)
      MSIZE      at 0 range 13 .. 14; -- Memory data size (read & write)
      PINCOS     at 0 range 15 .. 15; -- Peripheral increment offset size (read & write)
      PL         at 0 range 16 .. 17; -- Priority level (read & write)
      DBM        at 0 range 18 .. 18; -- Double buffer mode (read & write)
      CT         at 0 range 19 .. 19; -- Current target (only in double buffer mode) (read & write)
      Reserved_1 at 0 range 20 .. 20;
      PBURST     at 0 range 21 .. 22; -- Peripheral burst transfer configuration (read & write)
      MBURST     at 0 range 23 .. 24; -- Memory burst transfer configuration (read & write)
      CHSEL      at 0 range 25 .. 27; -- Channel selection (read & write)
      Reserved_2 at 0 range 28 .. 31;
   end record;

   -- DMA_SxNDTR - Stream x Number of data

   type DMA_SxNDTR is record
      NDT      : Bits_16; -- Number of data items to transfer (read & write)
      Reserved : Bits_16;
   end record with Volatile, Size => Word'Size;

   for DMA_SxNDTR use record
      NDT      at 0 range  0 .. 15; -- Number of data items to transfer (read & write)
      reserved at 0 range 16 .. 31;
   end record;

   -- DMA_SxFCR - Stream x FIFO control

   for FIFO_Options use (Less_than_quarter        => 0,
                         Less_than_half           => 1,
                         Less_than_three_quarters => 2,
                         Less_than_full           => 3,
                         Empty                    => 4,
                         Full                     => 5);

   for FTH_Options use (Quarter_full        => 0,
                        Half_full           => 1,
                        Three_quarters_full => 2,
                        Full                => 3);

   type DMA_SxFCR is record
      FTH        : FTH_Options;  -- FIFO threshold selection (read & write)
      DMDIS      : Disabler;     -- Direct mode disable (read & write)
      FS         : FIFO_Options; -- FIFO status (read only)
      Reserved_1 : Bit;
      FEIE       : Enabler;      -- FIFO error interrupt enable (read & write)
      Reserved_2 : Bits_24;
   end record with Volatile, Size => Word'Size;

   for DMA_SxFCR use record
      FTH        at 0 range  0 ..  1; -- FIFO threshold selection (read & write)
      DMDIS      at 0 range  2 ..  2; -- Direct mode disable (read & write)
      FS         at 0 range  3 ..  5; -- FIFO status (read only)
      Reserved_1 at 0 range  6 ..  6;
      FEIE       at 0 range  7 ..  7; -- FIFO error interrupt enable (read & write)
      Reserved_2 at 0 range  8 .. 31;
   end record;

   -- DMA stream

   type DMA_S_Module is record
      CR         : DMA_SxCR;   -- Stream x configuration register
      NDTR       : DMA_SxNDTR; -- Number of data items to transfer (read & write)
      PAR        : Bits_32;    -- Peripheral address (read & write)
      M0AR       : Bits_32;    -- Memory 0 address (read & write)
      M1AR       : Bits_32;    -- Memory 1 address (read & write)
      FCR        : DMA_SxFCR;  -- Stream x FIFO control register
   end record with Volatile, Size => Byte'Size * 16#18#;

   for DMA_S_Module use record
      CR         at 16#00# range  0 .. 31; -- Stream x configuration register
      NDTR       at 16#04# range  0 .. 31; -- Number of data items to transfer (read & write)
      PAR        at 16#08# range  0 .. 31; -- Peripheral address (read & write)
      M0AR       at 16#0C# range  0 .. 31; -- Memory 0 address (read & write)
      M1AR       at 16#10# range  0 .. 31; -- Memory 1 address (read & write)
      FCR        at 16#14# range  0 .. 31; -- Stream x FIFO control register
   end record;

   type DMA_S_Pack is array (Streams) of DMA_S_Module with Pack, Size => 8 * Byte'Size * 16#18#;

   -- DMA_Register

   type DMA_Register is record
      LISR  : DMA_LISR;  -- Low interrupt status register
      HISR  : DMA_HISR;  -- High interrupt status register
      LIFCR : DMA_LIFCR; -- Low interrupt flag clear register
      HIFCR : DMA_HIFCR; -- High interrupt flag clear register
      DMA_S : DMA_S_Pack;
   end record with Volatile, Size => Byte'Size * (16#10# + 8 * 16#18#);

   for DMA_Register use record
      LISR  at 16#00# range  0 ..   31;  -- Low interrupt status register
      HISR  at 16#04# range  0 ..   31;  -- High interrupt status register
      LIFCR at 16#08# range  0 ..   31; -- Low interrupt flag clear register
      HIFCR at 16#0C# range  0 ..   31; -- High interrupt flag clear register
      DMA_S at 16#10# range  0 .. 1535; -- DMA streams
   end record;

   -- Set register addresses

   DMA1 : aliased DMA_Register with
     Volatile, Address => System'To_Address (Base_DMA1), Import;

   DMA2 : aliased DMA_Register with
     Volatile, Address => System'To_Address (Base_DMA2), Import;

   -- Mapping DMAs to registers

   type DMA_Register_Access is access all DMA_Register;

   function DMAx (DMA : DMAs) return DMA_Register_Access is

     (case DMA is
         when 1 => DMA1'Access,
         when 2 => DMA2'Access
     );

end STM32F4.DMA_controller;

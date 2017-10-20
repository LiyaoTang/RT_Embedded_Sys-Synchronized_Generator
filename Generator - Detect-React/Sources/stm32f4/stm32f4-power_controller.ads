--
-- Uwe R. Zimmer, Australia 2015
--

pragma Restrictions (No_Elaboration_Code);

with System; use System;

-- -- STM32F405xx/07xx, STM32F415xx/17xx Power controller

package STM32F4.Power_controller is

   -- PWR_CR Control

   type LPDS_Options is (Regulator_on,
                         Regulator_low_power)      with Size => 1;

   type PDDS_Options is (Stop_mode,
                         Standby_mode)      with Size => 1;

   type PLS_Options is (V_2_0,
                        V_2_1,
                        V_2_3,
                        V_2_5,
                        V_2_6,
                        V_2_7,
                        V_2_8,
                        V_2_9) with Size => 3;

   type FPDS_Options is (Not_power_down,
                         Power_down)      with Size => 1;

   type VOS_Options is (Scale_2,
                        Scale_1)      with Size => 1;

   -- PWR_CSR Power control/status

   type PVDO_Options is (Higher,
                         Lower)      with Size => 1;

private

   -- PWR_CR Control

   for LPDS_Options use (Regulator_on => 0, Regulator_low_power => 1);

   for PDDS_Options use (Stop_mode => 0, Standby_mode => 1);

   for PLS_Options use (V_2_0 => 0,
                        V_2_1 => 1,
                        V_2_3 => 2,
                        V_2_5 => 3,
                        V_2_6 => 4,
                        V_2_7 => 5,
                        V_2_8 => 6,
                        V_2_9 => 7);

   for FPDS_Options use (Not_power_down => 0, Power_down => 1);

   for VOS_Options use (Scale_2 => 0, Scale_1 => 1);

   type PWR_CR is record
      LPDS       : LPDS_Options; -- Low-power deepsleep (read & write)
      PDDS       : PDDS_Options; -- Power-down deepsleep (read & write)
      CWUF       : Clearing;     -- Clear wakeup flag (write only)
      CSBF       : Clearing;     -- Clear standby flag (write only)
      PVDE       : Enabler;      -- Power voltage detector enable (read & write)
      PLS        : PLS_Options;  -- PVD level selection (read & write)
      DBP        : Enabler;      -- Disable backup domain write protection (read & write)
      FPDS       : FPDS_Options; -- Flash power-down in Stop mode (read & write)
      Reserved_1 : Bits_4;
      VOS        : VOS_Options;  -- Regulator voltage scaling output selection; (read & write)
      Reserved_2 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for PWR_CR use record
      LPDS       at 0 range  0 ..  0; -- Low-power deepsleep (read & write)
      PDDS       at 0 range  1 ..  1; -- Power-down deepsleep (read & write)
      CWUF       at 0 range  2 ..  2; -- Clear wakeup flag (write only)
      CSBF       at 0 range  3 ..  3; -- Clear standby flag (write only)
      PVDE       at 0 range  4 ..  4; -- Power voltage detector enable (read & write)
      PLS        at 0 range  5 ..  7; -- PVD level selection (read & write)
      DBP        at 0 range  8 ..  8; -- Disable backup domain write protection (read & write)
      FPDS       at 0 range  9 ..  9; -- Flash power-down in Stop mode (read & write)
      Reserved_1 at 0 range 10 .. 13;
      VOS        at 0 range 14 .. 14; -- Regulator voltage scaling output selection; (read & write)
      Reserved_2 at 0 range 15 .. 31;
   end record;

   PWR_CR_Reset : constant PWR_CR :=
     (LPDS       => Regulator_on,   -- Low-power deepsleep (read & write)
      PDDS       => Stop_mode,      -- Power-down deepsleep (read & write)
      CWUF       => Do_Nothing,     -- Clear wakeup flag (write only)
      CSBF       => Do_Nothing,     -- Clear standby flag (write only)
      PVDE       => Disable,        -- Power voltage detector enable (read & write)
      PLS        => V_2_0,          -- PVD level selection (read & write)
      DBP        => Disable,        -- Disable backup domain write protection (read & write)
      FPDS       => Not_power_down, -- Flash power-down in Stop mode (read & write)
      Reserved_1 => 0,
      VOS        => Scale_1,        -- Regulator voltage scaling output selection; (read & write)
      Reserved_2 => 0);

   -- PWR_CSR Power control/status

   for PVDO_Options use (Higher => 0, Lower => 1);

   type PWR_CSR is record
      WUF        : Flagging;     -- Wakeup flag (read only)
      SBF        : Flagging;     -- Standby flag (read only)
      PVDO       : PVDO_Options; -- PVD output (read only)
      BRR        : Readiness;    -- Backup regulator ready (read only)
      Reserved_1 : Bits_4;
      EWUP       : Enabler;      -- Enable WKUP pin (read & write)
      BRE        : Enabler;      -- Backup regulator enable (read & write)
      Reserved_2 : Bits_4;
      VOSRDY     : Readiness;    -- Regulator voltage scaling output selection ready bit (read only)
      Reserved_3 : Bits_17;
   end record with Volatile, Size => Word'Size;

   for PWR_CSR use record
      WUF        at 0 range  0 ..  0; -- Wakeup flag (read only)
      SBF        at 0 range  1 ..  1; -- Standby flag (read only)
      PVDO       at 0 range  2 ..  2; -- PVD output (read only)
      BRR        at 0 range  3 ..  3; -- Backup regulator ready (read only)
      Reserved_1 at 0 range  4 ..  7;
      EWUP       at 0 range  8 ..  8; -- Enable WKUP pin (read & write)
      BRE        at 0 range  9 ..  9; -- Backup regulator enable (read & write)
      Reserved_2 at 0 range 10 .. 13;
      VOSRDY     at 0 range 14 .. 14; -- Regulator voltage scaling output selection ready bit (read only)
      Reserved_3 at 0 range 15 .. 31;
   end record;

   PWR_CSR_Reset : constant PWR_CSR :=
     (WUF        => No_Flag,   -- Wakeup flag (read only)
      SBF        => No_Flag,   -- Standby flag (read only)
      PVDO       => Higher,    -- PVD output (read only)
      BRR        => Not_Ready, -- Backup regulator ready (read only)
      Reserved_1 => 0,
      EWUP       => Disable,   -- Enable WKUP pin (read & write)
      BRE        => Disable,   -- Backup regulator enable (read & write)
      Reserved_2 => 0,
      VOSRDY     => Not_Ready, -- Regulator voltage scaling output selection ready bit (read only)
      Reserved_3 => 0);

   -- CRC_Register

   type PWR_Register is record
      CR  : PWR_CR;  -- Power control
      CSR : PWR_CSR; -- Power control/status
   end record with Volatile, Size => 16#08# * Byte'Size;

   for PWR_Register use record
      CR         at 16#00# range  0 .. 31; -- Power control
      CSR        at 16#04# range  0 .. 31; -- Power control/status
   end record;

   -- Set register addresse

   PWR : PWR_Register with
     Volatile, Address => System'To_Address (Base_PWR), Import;

end STM32F4.Power_controller;

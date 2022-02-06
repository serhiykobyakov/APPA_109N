unit APPA_109N;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dialogs, synaser,
  StdCtrls, Controls, Forms,
  FileUtil,
  Unix,
  Math,
  DateUtils;


type

   { APPA_109N_device }

   APPA_109N_device = object
   private
     {private declarations}
     ser: TBlockSerial;
     theLastReadTime: TDateTime;      // the timestamp of the last communication with the device
     Answer : Array[0..18] of Byte;
     theModeStr, theRange, theUnits, theUnitsSI, theUnitsSi2: string;
     theValueSI, theValueSI2, theValue, theRandomUncSI, theSystematicUncSI: Real;
     theRangeCode: byte;  // 0.. 7
     theTestStr: string;  // testing feature, will dissapear in the future

     procedure RmLockFile(ComPort: string);
     function RoundUnc(val: Real): Real;

   const
     theDeviceName = 'APPA 109N'; // just some device name
     TimeOutDelay = 600;     // MAx timeout delay for reading, must be larger than 450 ms according to the documentation
     askStr : Array[0..4] of Byte = ($55, $55, $00, $00, $aa); // the bytes which invoke the device to give out the data
     AnswerBits = 18;         // number of bits (incl. 0 bit) which we expect to get from the device

     DenominatorTable : Array[0..4] of Integer = (1, 10, 100, 1000, 10000);

     ModeTable: array[0..2,0..8] of String =
       (('AC V',    'AC mV',    'Ohm',     'Diode',  'AC mA',    'AC A',    'Cap.', 'Hz',          'T, °C'),
        ('DC V',    'DC mV',    'Low Ohm', 'Beeper', 'DC mA',    'DC A',    'NA',   'Duty factor', 'T, °F'),
        ('AC+DC V', 'AC+DC mV', 'NA',      'NA',     'AC+DC mA', 'AC+DC A', 'NA',   'NA',          'NA'));

     ModeCode: array[0..2,0..8] of byte =
       ((0, 3, 6,  18, 8,  11, 14, 15, 16),
        (1, 4, 7,  18, 9,  12, 18, 18, 17),
        (2, 5, 18, 18, 10, 13, 18, 18, 18));

     ACModeCode: array[0..2,0..8] of byte =
       ((2, 1, 0, 0, 5, 6, 0, 0, 0),
        (0, 0, 0, 0, 0, 0, 0, 0, 0),
        (4, 3, 0, 0, 7, 8, 0, 0, 0));

     MultiplicatorSITable : Array[0..29] of Real =
       (1, 1, 1e-3, 1, 1e-3, 1, 1, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1,
        1, 1e3, 1e6, 1, 1, 1, 1e-3, 1e-9, 1, 1e-3, 1, 1e-3, 1, 1e3, 1e-3);

     UnitsTable : Array[0..29] of String =
       ('NA', 'V', 'mV', 'A', 'mA', 'dB', 'dBm', 'nF', 'uF', 'mF', 'Ohm', 'kOhm', 'MOhm', 'GOhm', '%',
        'Hz', 'kHz', 'MHz', '°C', '°F', 's', 'ms', 'ns', 'V', 'mV', 'A', 'mA', 'Ohm', 'kOhm', 'mOhm');

     UnitsSITable : Array[0..29] of String =
       ('NA', 'V', 'V', 'A', 'A', 'dB', 'dBm', 'F', 'F', 'F', 'Ohm', 'Ohm', 'Ohm', 'Ohm', '%',
        'Hz', 'Hz', 'Hz', '°C', '°F', 's', 's', 's', 'V', 'V', 'A', 'A', 'Ohm', 'Ohm', 'Ohm');

     RangeTable: Array[0..7,0..18] of string =
       (('2V',   '2V',    '2V',   '20mV',  '20mV',  '20mV',  '200Ohm',  '2kOhm',   '20mA',  '20mA',  '20mA',  '2A',  '2A',  '2A',  '4nF',   '20Hz',   '400°C',  '400°F',  'NA'),
        ('20V',  '20V',   '20V',  '200mV', '200mV', '200mV', '2kOhm',   '20kOhm',  '200mA', '200mA', '200mA', '10A', '10A', '10A', '40nF',  '200Hz',  '1200°C', '2192°F', 'NA'),
        ('200V', '200V',  '200V', '',      '',      '',      '20kOhm',  '200kOhm', '',      '',      '',      '',    '',    '',    '400nF', '2kHz',   '',       '',       'NA'),
        ('750V', '1000V', '750V', '',      '',      '',      '200kOhm', '2MOhm',   '',      '',      '',      '',    '',    '',    '4uF',   '20kHz',  '',       '',       'NA'),
        ('',     '',      '',     '',      '',      '',      '2MOhm',   '20MOhm',  '',      '',      '',      '',    '',    '',    '40uF',  '200kHz', '',       '',       'NA'),
        ('',     '',      '',     '',      '',      '',      '20MOhm',  '200MOhm', '',      '',      '',      '',    '',    '',    '400uF', '1MHz',   '',       '',       'NA'),
        ('',     '',      '',     '',      '',      '',      '200MOhm', '2GOhm',   '',      '',      '',      '',    '',    '',    '4mF',   '',       '',       '',       'NA'),
        ('',     '',      '',     '',      '',      '',      '2GOhm',   '',        '',      '',      '',      '',    '',    '',    '40mF',  '',       '',       '',       'NA'));

     ResolutionTable: Array[0..7,0..18] of Real =
       ((1e-4, 1e-4, 1e-4, 1e-3, 1e-3, 1e-3, 1e-2, 1e-4, 1e-3, 1e-3, 1e-3, 1e-4, 1e-4, 1e-4, 1e-3, 1e-3, 0.1, 0.1, 0),
        (1e-3, 1e-3, 1e-3, 1e-2, 1e-2, 1e-2, 1e-4, 1e-3, 1e-2, 1e-2, 1e-2, 1e-3, 1e-3, 1e-3, 1e-2, 1e-2, 1,   1,   0),
        (1e-2, 1e-2, 1e-2, 0,    0,    0,    1e-3, 1e-2, 0,    0,    0,    0,    0,    0,    1e-1, 1e-4, 0,   0,   0),
        (0.1,  0.1,  0.1,  0,    0,    0,    1e-2, 1e-4, 0,    0,    0,    0,    0,    0,    1e-3, 1e-3, 0,   0,   0),
        (0,    0,    0,    0,    0,    0,    1e-4, 1e-3, 0,    0,    0,    0,    0,    0,    1e-2, 1e-2, 0,   0,   0),
        (0,    0,    0,    0,    0,    0,    1e-3, 1,    0,    0,    0,    0,    0,    0,    1e-1, 1e-4, 0,   0,   0),
        (0,    0,    0,    0,    0,    0,    1,    1e-4, 0,    0,    0,    0,    0,    0,    1e-3, 0,    0,   0,   0),
        (0,    0,    0,    0,    0,    0,    1e-4, 0,    0,    0,    0,    0,    0,    0,    1e-2, 0,    0,   0,   0));


     RandErrPercTable: Array[0..7,0..18] of Real =
       ((0,  0.06, 0,  0, 0.06, 0, 0.3, 0.6, 1.2,  0.2, 1.2, 2,    0.2, 2.2,  1.5, 1e-2, 0.1, 0.1, 0),
        (0,  0.06, 0,  0, 0.06, 0, 0.3, 0.6, 1.2,  0.2, 1.2, 2,    0.2, 2.2,  1.5, 1e-2, 0.1, 0.1, 0),
        (0,  0.06, 0,  0, 0,    0, 0.3, 0.6, 0,    0,   0,   0,    0,   0,    0.9, 1e-2, 0,   0,   0),
        (0,  0.06, 0,  0, 0,    0, 0.3, 0.6, 0,    0,   0,   0,    0,   0,    0.9, 1e-2, 0,   0,   0),
        (0,  0,    0,  0, 0,    0, 0.3, 7,   0,    0,   0,   0,    0,   0,    1.2, 1e-2, 0,   0,   0),
        (0,  0,    0,  0, 0,    0, 5,   7,   0,    0,   0,   0,    0,   0,    1.2, 1e-2, 0,   0,   0),
        (0,  0,    0,  0, 0,    0, 5,   7,   0,    0,   0,   0,    0,   0,    1.5, 0,    0,   0,   0),
        (0,  0,    0,  0, 0,    0, 5,   0,   0,    0,   0,   0,    0,   0,    1.5, 0,    0,   0,   0));


     SystErrTable: Array[0..7,0..18] of Real =
       ((0,    10, 0,   0,  60, 0,   30, 30, 80,   40, 120, 80,   40, 160,  10, 50, 3, 6,  0),
        (0,    10, 0,   0,  20, 0,   30, 30, 80,   40, 120, 80,   40, 160,  10, 10, 6, 12, 0),
        (0,    10, 0,   0,  0,  0,   30, 30, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
        (0,    10, 0,   0,  0,  0,   30, 50, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
        (0,    0,  0,   0,  0,  0,   50, 50, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
        (0,    0,  0,   0,  0,  0,   50, 20, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
        (0,    0,  0,   0,  0,  0,   20, 20, 0,    0,  0,   0,    0,  0,    5,  0,  0, 0,  0),
        (0,    0,  0,   0,  0,  0,   8, 0,   0,    0,  0,   0,    0,  0,    5,  0,  0, 0,  0));


     ACRandErrTable: Array[1..8, 0..6, 0..7] of Real =
     (((0,   0,   0, 0, 0, 0, 0, 0),
       (0.7, 0.7, 0, 0, 0, 0, 0, 0),
       (1,   1,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),       // AC mV  ACModeCode = 1
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0,   0,   0,   0,   0, 0, 0, 0),
       (0.7, 0.7, 0.7, 0.7, 0, 0, 0, 0),
       (1,   1,   1,   1,   0, 0, 0, 0),
       (2,   2,   2,   0,   0, 0, 0, 0),
       (3,   3,   3,   0,   0, 0, 0, 0),       // AC V  ACModeCode = 2
       (5,   5,   5,   0,   0, 0, 0, 0),
       (10,  10,  0,   0,   0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (1.7, 1.7, 0, 0, 0, 0, 0, 0),
       (2,   2,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),       // AC+DC mV   ACModeCode = 3
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0,   0,   0,   0,   0, 0, 0, 0),
       (1.7, 1.7, 1.7, 1.7, 0, 0, 0, 0),
       (2,   2,   2,   2,   0, 0, 0, 0),
       (3,   3,   3,   0,   0, 0, 0, 0),
       (4,   4,   4,   0,   0, 0, 0, 0),       // AC+DC V   ACModeCode = 4
       (6,   6,   6,   0,   0, 0, 0, 0),
       (11,  11,  0,   0,   0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (0.8, 0.8, 0, 0, 0, 0, 0, 0),
       (1.2, 1.2, 0, 0, 0, 0, 0, 0),
       (0,   2,   0, 0, 0, 0, 0, 0),       // AC mA   ACModeCode = 5
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (0.8, 0.8, 0, 0, 0, 0, 0, 0),
       (1.2, 1.2, 0, 0, 0, 0, 0, 0),
       (0,   2,   0, 0, 0, 0, 0, 0),       // AC A   ACModeCode = 6
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),       // AC+DC mA   ACModeCode = 7
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0)),
      ((0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),       // AC+DC A    ACModeCode = 8
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0),
       (0, 0, 0, 0, 0, 0, 0, 0)));


     ACSystErrTable: Array[1..8, 0..6, 0..7] of Real =
     (((0,  0,  0, 0, 0, 0, 0, 0),
       (80, 80, 0, 0, 0, 0, 0, 0),
       (80, 80, 0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0),       // AC mV  ACModeCode = 1
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (50,  50,  50, 50, 0, 0, 0, 0),
       (50,  50,  50, 50, 0, 0, 0, 0),
       (60,  60,  60, 0,  0, 0, 0, 0),
       (70,  70,  70, 0,  0, 0, 0, 0),       // AC V  ACModeCode = 2
       (80,  80,  80, 0,  0, 0, 0, 0),
       (100, 100, 0,  0,  0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (160, 160, 0, 0, 0, 0, 0, 0),
       (160, 160, 0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),       // AC+DC mV   ACModeCode = 3
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0,    0,    0,   0,   0, 0, 0, 0),
       (130,  130,  130, 130, 0, 0, 0, 0),
       (130,  130,  130, 130, 0, 0, 0, 0),
       (140,  140,  140, 0,   0, 0, 0, 0),
       (150,  150,  150, 0,   0, 0, 0, 0),       // AC+DC V   ACModeCode = 4
       (160,  160,  160, 0,   0, 0, 0, 0),
       (180,  180,  0,   0,   0, 0, 0, 0)),
      ((0,  0,  0, 0, 0, 0, 0, 0),
       (50, 50, 0, 0, 0, 0, 0, 0),
       (80, 80, 0, 0, 0, 0, 0, 0),
       (0,  80, 0, 0, 0, 0, 0, 0),       // AC mA   ACModeCode = 5
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0)),
      ((0,  0,  0, 0, 0, 0, 0, 0),
       (50, 50, 0, 0, 0, 0, 0, 0),
       (80, 80, 0, 0, 0, 0, 0, 0),
       (0,  80, 0, 0, 0, 0, 0, 0),       // AC A   ACModeCode = 6
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0),
       (0,  0,  0, 0, 0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (130, 130, 0, 0, 0, 0, 0, 0),
       (160, 160, 0, 0, 0, 0, 0, 0),
       (0,   160, 0, 0, 0, 0, 0, 0),       // AC+DC mA   ACModeCode = 7
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)),
      ((0,   0,   0, 0, 0, 0, 0, 0),
       (130, 130, 0, 0, 0, 0, 0, 0),
       (160, 160, 0, 0, 0, 0, 0, 0),       // AC+DC A    ACModeCode = 8
       (0,   160, 0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0),
       (0,   0,   0, 0, 0, 0, 0, 0)));


     function FreqRangeI(freq: Real): Byte;
     function FreqRangeU(freq: Real): Byte;


   public
     constructor Init(ComPort: string);
     destructor Done;

     procedure GetData();                   // let the unit comunicate with the device and aquire the measurement data

     function GetMode: string;              // get the mode of the measurement
     function GetRange(): string;

     function GetStrVal(): string;          // get value string - just like on the multimeter screen
     function GetStrUnits(): string;        // get units string - just like on the multimeter screen

     function GetValueSI(): Real;           // get value in SI units
     function GetUnitsSI(): string;         // get SI units of the value
     function GetUncertaintySI(): Real;     // get the measurement uncertainty in SI units
     function GetRandUncertaintySI(): Real; // get the only random part of the measurement uncertainty in SI units

     function GetValue2SI(): Real;          // get the 2nd value in SI units
     function GetUnits2SI(): string;        // get SI units of the 2nd value

     function GetTestStr(): string;         // testing feature, will dissapear in the future
   end;


  procedure SleepFor(thetime: LongInt);
  function APPA_109N_on(ComPort: string): Boolean;


//var


//const


implementation

function APPA_109N_on(ComPort: string): Boolean;
var
  ser: TBlockSerial;
  counter: Integer;
  res: Boolean;
  Answer : Array[0..20] of Byte;
begin
  ser := TBlockSerial.Create;
    try
      ser.RaiseExcept:=true;
      ser.Connect(ComPort);
      ser.config(9600, 8, 'N', SB1, False, False);
      SleepFor(100);
      counter := 0;
      ser.SendByte($55); ser.SendByte($55); ser.SendByte($00); ser.SendByte($00); ser.SendByte($aa);
      while ser.canread(600) and (counter < 20) do
        begin
          Answer[counter] := ser.RecvByte(100);
          counter := counter + 1;
        end;
      if (counter = 19) and (Answer[0] = $55) then res := True  // if we got 19 bytes and the first one is $55
                                                                // most probably this is APPA 109 (or 107)
      else res := False;

    finally
      ser.Free;
    end;
  Result := res;
end;

procedure SleepFor(thetime: LongInt);
var
  tstartwait: TDateTime;
begin
  tstartwait := Now;
  repeat
    Application.ProcessMessages;         // do something useful while waiting
  until MillisecondsBetween(Now, tstartwait) > thetime;
end;

procedure APPA_109N_device.RmLockFile(ComPort: string);   // Remove lock-file
var
  FindFiles: TStringList;
begin
  {$IFDEF Linux}
  FindFiles := TStringList.Create;
  try
    FindAllFiles(FindFiles, '/var/lock', '*' + ExtractFileName(ComPort) + '*', true);
    if (FindFiles.Count = 1) then DeleteFile(FindFiles.Strings[0]);
  finally
    FindFiles.Free;
  end;
  {$ENDIF}
end;

function APPA_109N_device.RoundUnc(val: Real): Real;
var
  factor: Real;
begin
  if (val = 0) then Result := 0
  else
    begin
      val := abs(val);
      factor := Power(10, 2 - ceil(log10(val)));
      Result := Round(val*factor)/factor;
    end;
end;

function APPA_109N_device.FreqRangeI(freq: Real): Byte;
var
  thefRange: Byte;
begin
  thefRange := 0;

  if InRange(freq, 0, 40) then thefRange := 0
  else if InRange(freq, 40.001, 500) then thefRange := 1
  else if InRange(freq, 500.001, 1000) then thefRange := 2
  else if InRange(freq, 1000.001, 3000) then thefRange := 3
  else if (freq > 3000.001) then thefRange := 0;

  Result := thefRange;
end;

function APPA_109N_device.FreqRangeU(freq: Real): Byte;
var
  thefRange: Byte;
begin
  thefRange := 0;

  if InRange(freq, 0, 40) then thefRange := 0
  else if InRange(freq, 40.001, 100) then thefRange := 1
  else if InRange(freq, 100.001, 1000) then thefRange := 2
  else if InRange(freq, 1000.001, 10000) then thefRange := 3
  else if InRange(freq, 10000.001, 20000) then thefRange := 4
  else if InRange(freq, 20000.001, 50000) then thefRange := 5
  else if InRange(freq, 50000.001, 100000) then thefRange := 6
  else if (freq > 100000.001) then thefRange := 0;

  Result := thefRange;
end;

constructor APPA_109N_device.Init(ComPort: string);
var
  MyForm: TForm;
  MyLabel: TLabel;
  i: Integer;
begin

// -- make the splash screen ---
  MyForm := TForm.Create(nil);
  with MyForm do
   begin
      SetBounds(0, 0, 450, 70);
      Position:=poDesktopCenter;
      BorderStyle := bsNone;
   end;
  MyForm.Color := $00EEEEEE;

  MyLabel := TLabel.Create(MyForm);
  with MyLabel do
   begin
     Align := alClient; Alignment := taCenter;
     Parent := MyForm;  Visible := True;
   end;

  MyForm.Show;
  MyForm.BringToFront;

  MyLabel.Caption:= LineEnding + 'Initializing ' + theDeviceName + LineEnding +
                    'Connecting to ' + ComPort + '...';
  SleepFor(700);

  RmLockFile(ComPort);

  ser := TBlockSerial.Create;
  try
    ser.RaiseExcept:=true;
    ser.Connect(ComPort);
    ser.config(9600, 8, 'N', SB1, False, False);

    SleepFor(100);
    if ser.lastError<>0 then showmessage(theDeviceName + ':' + LineEnding +
                                         'Error in communication after ser.config');

    for i := 0 to 4 do ser.SendByte(askStr[i]);
    if ser.lastError<>0 then showmessage(theDeviceName + ':' + LineEnding +
                                         'Error in communication after ser.SendString');
    if ser.canread(TimeOutDelay) then
      for i := 0 to AnswerBits do
        Answer[i] := ser.RecvByte(100);

    if ser.lastError<>0 then showmessage(theDeviceName + ':' + LineEnding +
                                         'Error in communication after ser.Recvstring');
    if i<>AnswerBits then
      begin
        showmessage(theDeviceName + ':' + LineEnding +
                    'Please connect the device and switch it on');
        ser.Free;
        halt(0);
      end;
    theLastReadTime := Now;
  finally

  end;

  SleepFor(150);
  MyForm.Close;
  FreeAndNil(MyForm);
end;

destructor APPA_109N_device.Done;
var
  MyForm: TForm;
  MyLabel: TLabel;
begin

  MyForm := TForm.Create(nil);
  with MyForm do
   begin
      SetBounds(0, 0, 450, 65);
      Position:=poDesktopCenter;
      BorderStyle := bsNone;
   end;
  MyForm.Color := $00EEEEEE;

  MyLabel := TLabel.Create(MyForm);
  with MyLabel do
   begin
     Align := alClient; Alignment := taCenter; Parent := MyForm;
     Caption:= LineEnding +'Shutdown ' + theDeviceName + ':' + LineEnding +
                    'Close communication...';
     Visible := True;
   end;

  MyForm.Show; MyForm.BringToFront;
  Application.ProcessMessages;  // actually show the window

  ser.Free;

  SleepFor(500);
  MyForm.Close;
  FreeAndNil(MyForm);
end;


procedure APPA_109N_device.GetData();
var
  i, counter, theModecode, theDenominator, theSum, theval: integer;
  tstartwait: TDateTime;
  theMultiplicatorSI: Real;
  posval, negval, str: string;
  theFreqRange: byte;
begin
// clear all the variables before obtaining new data
// it maybe useless but I am testing now
// so it may be useful in order to catch an error in code eventually...
  theMultiplicatorSI := 0;  theDenominator := 1;       theRange := 'NA';
  theUnits := 'NA';         theUnitsSI := 'NA';        theUnitsSI2 := 'NA';
  theValue := 0;            theValueSI := 0;           theValueSI2 := 0;
  theRandomUncSI := 0;      theSystematicUncSI := 0;

// let's get the answer drom the device!
  counter := 0;
  Repeat
    tstartwait := Now;
    if counter > 0 then   // wait up to 0,3 s if we have to repeat the data acquisition too soon
      repeat
        Application.ProcessMessages;     // do something useful while waiting
      until MillisecondsBetween(Now, tstartwait) > 300;

    for i := 0 to AnswerBits do Answer[i] := 0;

    try
      for i := 0 to 4 do ser.SendByte(askStr[i]);
      if ser.canread(TimeOutDelay) then ser.RecvBufferEx(@Answer, AnswerBits + 1, TimeOutDelay);
    finally
    end;

    counter := counter + 1;      // counting the number of attempts to read the data
    theTestStr := 'Read attempt: ' + IntToStr(counter) + LineEnding;
    if counter > 3 then   // if there is no answer for the 4th time - most probably the device is off
      begin
        showmessage(theDeviceName + ':' + LineEnding +
                        'Switch on the multimeter and then click "OK"!' + LineEnding + LineEnding +
                        'Hold blue button while switching the multimeter on'+ LineEnding +
                        'to disable auto switch off'); counter := 0;
        Application.ProcessMessages;
        ser.Purge;  // clear all the buffers before the next attempt
      end;

    theSum := 0;
    for i := 0 to AnswerBits - 1 do
      theSum := theSum + Answer[i];  // count the sum of bits except the last one

  Until (theSum <> 0) and (StrToInt('$'+copy(IntToHex(theSum,2),2,2)) = StrToInt('$' + HexStr(Answer[AnswerBits],2))); // repeat until the checksum is correct


// So we got the answer from the device
// let's encode the data
  theModeStr := ModeTable[Answer[5], Answer[4]-1];
  theModecode := ModeCode[Answer[5], Answer[4]-1];
  if (Answer[7] > 7) then theRangeCode := Answer[7]-128
  else theRangeCode := Answer[7];
  theRange := RangeTable[theRangeCode,theModeCode];

// encode the main reading
  negval := '$' + 'FF' + HexStr(Answer[10],2) + HexStr(Answer[9],2) + HexStr(Answer[8],2);
  posval := '$' + '00' + HexStr(Answer[10],2) + HexStr(Answer[9],2) + HexStr(Answer[8],2);
  theval := StrToInt(posval);
  if theval>$7FFFFF then theval := StrToInt(negval);

  theMultiplicatorSI := MultiplicatorSITable[Round((Answer[11] and 248)/8)];
  theDenominator := DenominatorTable[Answer[11] and 7];
  theValue := theval/theDenominator;
  theValueSI := theval*theMultiplicatorSI/theDenominator;
  theRandomUncSI := 0.01*RandErrPercTable[theRangeCode,theModeCode]*theValueSI;
  theSystematicUncSI := SystErrTable[theRangeCode,theModeCode]*ResolutionTable[theRangeCode,theModeCode];
  theUnits := UnitsTable[Round((Answer[11] and 248)/8)];
  theUnitsSI := UnitsSITable[Round((Answer[11] and 248)/8)];

// encode the second reading
  negval := '$' + 'FF' + HexStr(Answer[15],2) + HexStr(Answer[14],2) + HexStr(Answer[13],2);
  posval := '$' + '00' + HexStr(Answer[15],2) + HexStr(Answer[14],2) + HexStr(Answer[13],2);
  theval := StrToInt(posval);
  if theval>$7FFFFF then theval := StrToInt(negval);

  theMultiplicatorSI := MultiplicatorSITable[Round((Answer[16] and 248)/8)];
  theDenominator := DenominatorTable[Answer[16] and 7];
  theValueSI2 := theval*theMultiplicatorSI/theDenominator;
  theUnitsSI2 := UnitsSITable[Round((Answer[16] and 248)/8)];

  if (theValueSI2 <> 0) then
    begin
      if InRange(ACModeCode[Answer[5], Answer[4]-1], 1, 4) then theFreqRange := FreqRangeU(theValueSI2)
      else theFreqRange := FreqRangeI(theValueSI2);
      theRandomUncSI := 0.01*ACRandErrTable[ACModeCode[Answer[5], Answer[4]-1], theFreqRange, theRangeCode]*abs(theValueSI);
      theSystematicUncSI := ACSystErrTable[ACModeCode[Answer[5], Answer[4]-1], theFreqRange, theRangeCode]*ResolutionTable[theRangeCode,theModeCode];
    end;


  theTestStr := theTestStr + LineEnding;


  theTestStr := theTestStr + 'theRangeCode:  ' + IntToStr(theRangeCode) + LineEnding;

  theTestStr := theTestStr + LineEnding;

  theTestStr := theTestStr + 'Resolution:  ' + FloatToStr(ResolutionTable[theRangeCode,theModeCode]) + LineEnding;
  theTestStr := theTestStr + 'Rand err:    ' + FloatToStr(RandErrPercTable[theRangeCode,theModeCode]) + LineEnding;
  theTestStr := theTestStr + 'Syst err:    ' + FloatToStr(SystErrTable[theRangeCode,theModeCode]) + LineEnding;

  theTestStr := theTestStr + LineEnding;
  theTestStr := theTestStr + LineEnding;


  theTestStr := theTestStr + 'ACModeCode:  ' + IntToStr(ACModeCode[Answer[5], Answer[4]-1]) + LineEnding;
  theTestStr := theTestStr + 'AC U range:  ' + IntToStr(FreqRangeU(theValueSI2)) + LineEnding;
  theTestStr := theTestStr + 'AC I range:  ' + IntToStr(FreqRangeI(theValueSI2)) + LineEnding;
  theTestStr := theTestStr + 'AC Rand err: ' + FloatToStr(ACRandErrTable[ACModeCode[Answer[5], Answer[4]-1], theFreqRange, theRangeCode]) + LineEnding;
  theTestStr := theTestStr + 'AC Syst err: ' + FloatToStr(ACSystErrTable[ACModeCode[Answer[5], Answer[4]-1], theFreqRange, theRangeCode]) + LineEnding;

end;

function APPA_109N_device.GetStrVal(): string;
begin
  Result := FloatToStr(theValue);
end;

function APPA_109N_device.GetStrUnits(): string;
begin
  Result := theUnits;
end;


function APPA_109N_device.GetUnitsSI(): string;
begin
  Result := theUnitsSI;
end;

function APPA_109N_device.GetValueSI(): Real;
begin
  Result := theValueSI;
end;

function APPA_109N_device.GetUncertaintySI(): Real;
begin
  Result := RoundUnc(theRandomUncSI + theSystematicUncSI);
end;

function APPA_109N_device.GetRandUncertaintySI(): Real;
begin
  Result := RoundUnc(theRandomUncSI);
end;


function APPA_109N_device.GetUnits2SI(): string;
begin
  Result := theUnitsSI2;
end;

function APPA_109N_device.GetTestStr(): string;
begin
  Result := theTestStr
end;

function APPA_109N_device.GetValue2SI(): Real;
begin
  Result := theValueSI2;
end;

function APPA_109N_device.GetRange(): string;
begin
  Result := theRange;
end;

function APPA_109N_device.GetMode: string;
begin
  Result :=theModeStr;
end;


{ APPA_109N_device }


end.


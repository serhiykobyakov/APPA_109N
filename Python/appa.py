""" Appa 109N multimeter module

This module implements a communication interface to Appa 109N multimeter,
allowing to get the measured values along with their uncertainties.
"""


__version__ = '27.09.2022'
__author__ = 'Serhiy Kobyakov'


#from cmath import nan
import time
import math
import serial
import tkinter as tk


class Model109N:
    "Appa 109N multimeter class"

    COMPORTSPEED = 9600
    COMPORTPARITY = serial.PARITY_NONE
    COMPORTSTOPBITS = serial.STOPBITS_ONE
    COMPORTBITS = serial.EIGHTBITS
    COMPORTTIMEOUT = 0.4
    COMPORTWRITETIMEOUT = 0.1
    SHORTESTTIMEBETWEENREADS = 0.46
    
    __ModeTable = (('AC V',    'AC mV',    'R Ohm',   'Diode',  'AC mA',    'AC A',    'Cap.', 'Hz',          'T, °C'),
                   ('DC V',    'DC mV',    'Low Ohm', 'Beeper', 'DC mA',    'DC A',    'NA',   'Duty factor', 'T, °F'),
                   ('AC+DC V', 'AC+DC mV',  '',        '',      'AC+DC mA', 'AC+DC A', '',     '',            ''))
    
    __ModeCodeTable = ((0, 3, 6,  18, 8,  11, 14, 15, 16),
                       (1, 4, 7,  18, 9,  12, 18, 18, 17),
                       (2, 5, 18, 18, 10, 13, 18, 18, 18))

    # [self.__rangecode][self.__modecode]
    __RangeTable = (('2V',   '2V',    '2V',   '20mV',  '20mV',  '20mV',  '200Ohm',  '2kOhm',   '20mA',  '20mA',  '20mA',  '2A',  '2A',  '2A',  '4nF',   '20Hz',   '400°C',  '400°F',  'NA'),
                    ('20V',  '20V',   '20V',  '200mV', '200mV', '200mV', '2kOhm',   '20kOhm',  '200mA', '200mA', '200mA', '10A', '10A', '10A', '40nF',  '200Hz',  '1200°C', '2192°F', 'NA'),
                    ('200V', '200V',  '200V', '',      '',      '',      '20kOhm',  '200kOhm', '',      '',      '',      '',    '',    '',    '400nF', '2kHz',   '',       '',       'NA'),
                    ('750V', '1000V', '750V', '',      '',      '',      '200kOhm', '2MOhm',   '',      '',      '',      '',    '',    '',    '4uF',   '20kHz',  '',       '',       'NA'),
                    ('',     '',      '',     '',      '',      '',      '2MOhm',   '20MOhm',  '',      '',      '',      '',    '',    '',    '40uF',  '200kHz', '',       '',       'NA'),
                    ('',     '',      '',     '',      '',      '',      '20MOhm',  '200MOhm', '',      '',      '',      '',    '',    '',    '400uF', '1MHz',   '',       '',       'NA'),
                    ('',     '',      '',     '',      '',      '',      '200MOhm', '2GOhm',   '',      '',      '',      '',    '',    '',    '4mF',   '',       '',       '',       'NA'),
                    ('',     '',      '',     '',      '',      '',      '2GOhm',   '',        '',      '',      '',      '',    '',    '',    '40mF',  '',       '',       '',       'NA'))

    # [self.__rangecode][self.__modecode]
    __ResolutionTable = ((1e-4, 1e-4, 1e-4, 1e-3, 1e-3, 1e-3, 1e-2, 1e-4, 1e-3, 1e-3, 1e-3, 1e-4, 1e-4, 1e-4, 1e-3, 1e-3, 0.1, 0.1, 0),
                         (1e-3, 1e-3, 1e-3, 1e-2, 1e-2, 1e-2, 1e-4, 1e-3, 1e-2, 1e-2, 1e-2, 1e-3, 1e-3, 1e-3, 1e-2, 1e-2, 1,   1,   0),
                         (1e-2, 1e-2, 1e-2, 0,    0,    0,    1e-3, 1e-2, 0,    0,    0,    0,    0,    0,    1e-1, 1e-4, 0,   0,   0),
                         (0.1,  0.1,  0.1,  0,    0,    0,    1e-2, 1e-4, 0,    0,    0,    0,    0,    0,    1e-3, 1e-3, 0,   0,   0),
                         (0,    0,    0,    0,    0,    0,    1e-4, 1e-3, 0,    0,    0,    0,    0,    0,    1e-2, 1e-2, 0,   0,   0),
                         (0,    0,    0,    0,    0,    0,    1e-3, 1,    0,    0,    0,    0,    0,    0,    1e-1, 1e-4, 0,   0,   0),
                         (0,    0,    0,    0,    0,    0,    1,    1e-4, 0,    0,    0,    0,    0,    0,    1e-3, 0,    0,   0,   0),
                         (0,    0,    0,    0,    0,    0,    1e-4, 0,    0,    0,    0,    0,    0,    0,    1e-2, 0,    0,   0,   0))

    # [self.__rangecode][self.__modecode]
    __RandErrPercTable = ((0,  0.06, 0,  0, 0.06, 0, 0.3, 0.6, 1.2,  0.2, 1.2, 2,    0.2, 2.2,  1.5, 1e-2, 0.1, 0.1, 0),
                          (0,  0.06, 0,  0, 0.06, 0, 0.3, 0.6, 1.2,  0.2, 1.2, 2,    0.2, 2.2,  1.5, 1e-2, 0.1, 0.1, 0),
                          (0,  0.06, 0,  0, 0,    0, 0.3, 0.6, 0,    0,   0,   0,    0,   0,    0.9, 1e-2, 0,   0,   0),
                          (0,  0.06, 0,  0, 0,    0, 0.3, 0.6, 0,    0,   0,   0,    0,   0,    0.9, 1e-2, 0,   0,   0),
                          (0,  0,    0,  0, 0,    0, 0.3, 7,   0,    0,   0,   0,    0,   0,    1.2, 1e-2, 0,   0,   0),
                          (0,  0,    0,  0, 0,    0, 5,   7,   0,    0,   0,   0,    0,   0,    1.2, 1e-2, 0,   0,   0),
                          (0,  0,    0,  0, 0,    0, 5,   7,   0,    0,   0,   0,    0,   0,    1.5, 0,    0,   0,   0),
                          (0,  0,    0,  0, 0,    0, 5,   0,   0,    0,   0,   0,    0,   0,    1.5, 0,    0,   0,   0))

    # [self.__rangecode][self.__modecode]
    __SystErrTable = ((0,    10, 0,   0,  60, 0,   30, 30, 80,   40, 120, 80,   40, 160,  10, 50, 3, 6,  0),
                      (0,    10, 0,   0,  20, 0,   30, 30, 80,   40, 120, 80,   40, 160,  10, 10, 6, 12, 0),
                      (0,    10, 0,   0,  0,  0,   30, 30, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
                      (0,    10, 0,   0,  0,  0,   30, 50, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
                      (0,    0,  0,   0,  0,  0,   50, 50, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
                      (0,    0,  0,   0,  0,  0,   50, 20, 0,    0,  0,   0,    0,  0,    5,  10, 0, 0,  0),
                      (0,    0,  0,   0,  0,  0,   20, 20, 0,    0,  0,   0,    0,  0,    5,  0,  0, 0,  0),
                      (0,    0,  0,   0,  0,  0,   8, 0,   0,    0,  0,   0,    0,  0,    5,  0,  0, 0,  0))

    # [self.__rangecode][self.__frangecode][self.__acmodecode]
    __ACRandErrTable = (((0.7, 0.7, 0, 0, 0, 0, 0, 0),
                         (1,   1,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),       # AC mV  ACModeCode = 0
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((0.7, 0.7, 0.7, 0.7, 0, 0, 0, 0),
                         (1,   1,   1,   1,   0, 0, 0, 0),
                         (2,   2,   2,   0,   0, 0, 0, 0),
                         (3,   3,   3,   0,   0, 0, 0, 0),       # AC V  ACModeCode = 1
                         (5,   5,   5,   0,   0, 0, 0, 0),
                         (10,  10,  0,   0,   0, 0, 0, 0)),
                        ((1.7, 1.7, 0, 0, 0, 0, 0, 0),
                         (2,   2,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),       # AC+DC mV   ACModeCode = 2
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((1.7, 1.7, 1.7, 1.7, 0, 0, 0, 0),
                         (2,   2,   2,   2,   0, 0, 0, 0),
                         (3,   3,   3,   0,   0, 0, 0, 0),
                         (4,   4,   4,   0,   0, 0, 0, 0),       # AC+DC V   ACModeCode = 3
                         (6,   6,   6,   0,   0, 0, 0, 0),
                         (11,  11,  0,   0,   0, 0, 0, 0)),
                        ((0.8, 0.8, 0, 0, 0, 0, 0, 0),
                         (1.2, 1.2, 0, 0, 0, 0, 0, 0),
                         (0,   2,   0, 0, 0, 0, 0, 0),       # AC mA   ACModeCode = 4
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((0.8, 0.8, 0, 0, 0, 0, 0, 0),
                         (1.2, 1.2, 0, 0, 0, 0, 0, 0),
                         (0,   2,   0, 0, 0, 0, 0, 0),       # AC A   ACModeCode = 5
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),       # AC+DC mA   ACModeCode = 6
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0)),
                        ((0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),       # AC+DC A    ACModeCode = 7
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0),
                         (0, 0, 0, 0, 0, 0, 0, 0)))

    # [self.__rangecode][self.__frangecode][self.__acmodecode]
    __ACSystErrTable = (((80, 80, 0, 0, 0, 0, 0, 0),
                         (80, 80, 0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0),            # AC mV  ACModeCode = 0
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0)),
                        ((50,  50,  50, 50, 0, 0, 0, 0),
                         (50,  50,  50, 50, 0, 0, 0, 0),
                         (60,  60,  60, 0,  0, 0, 0, 0),
                         (70,  70,  70, 0,  0, 0, 0, 0),       # AC V  ACModeCode = 1
                         (80,  80,  80, 0,  0, 0, 0, 0),
                         (100, 100, 0,  0,  0, 0, 0, 0)),
                        ((160, 160, 0, 0, 0, 0, 0, 0),
                         (160, 160, 0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),       # AC+DC mV   ACModeCode = 2
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((130,  130,  130, 130, 0, 0, 0, 0),
                         (130,  130,  130, 130, 0, 0, 0, 0),
                         (140,  140,  140, 0,   0, 0, 0, 0),
                         (150,  150,  150, 0,   0, 0, 0, 0),       # AC+DC V   ACModeCode = 3
                         (160,  160,  160, 0,   0, 0, 0, 0),
                         (180,  180,  0,   0,   0, 0, 0, 0)),
                        ((50, 50, 0, 0, 0, 0, 0, 0),
                         (80, 80, 0, 0, 0, 0, 0, 0),
                         (0,  80, 0, 0, 0, 0, 0, 0),       # AC mA   ACModeCode = 4
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0)),
                        ((50, 50, 0, 0, 0, 0, 0, 0),
                         (80, 80, 0, 0, 0, 0, 0, 0),
                         (0,  80, 0, 0, 0, 0, 0, 0),       # AC A   ACModeCode = 5
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0),
                         (0,  0,  0, 0, 0, 0, 0, 0)),
                        ((130, 130, 0, 0, 0, 0, 0, 0),
                         (160, 160, 0, 0, 0, 0, 0, 0),
                         (0,   160, 0, 0, 0, 0, 0, 0),       # AC+DC mA   ACModeCode = 6
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)),
                        ((130, 130, 0, 0, 0, 0, 0, 0),
                         (160, 160, 0, 0, 0, 0, 0, 0),       # AC+DC A    ACModeCode = 7
                         (0,   160, 0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0),
                         (0,   0,   0, 0, 0, 0, 0, 0)))

    __DenominatorTable = (1, 10, 100, 1000, 10000)

    __MultiplicatorSITable = (1,    1, 1e-3,   1, 1e-3, 1,    1, 1e-9, 1e-6, 1e-3,
                              1,  1e3,  1e6, 1e9,    1, 1,  1e3,  1e6,    1,    1,
                              1, 1e-3, 1e-9,   1, 1e-3, 1, 1e-3,    1,  1e3,  1e6)

    __UnitsSITable = ('NA', 'V', 'V', 'A', 'A', 'dB', 'dBm', 'F', 'F', 'F',
                     'Ohm', 'Ohm', 'Ohm', 'Ohm', '%', 'Hz', 'Hz', 'Hz', '°C', '°F',
                     's', 's', 's', 'V', 'V', 'A', 'A', 'Ohm', 'Ohm', 'Ohm')

    __UnitsTable =   ('NA', 'V', 'mV', 'A', 'mA', 'dB', 'dBm', 'nF', 'uF', 'mF',
                     'Ohm', 'kOhm', 'MOhm', 'GOhm', '%', 'Hz', 'kHz', 'MHz', '°C', '°F',
                     's', 'ms', 'ns', 'V', 'mV', 'A', 'mA', 'Ohm', 'kOhm', 'MOhm')

    __ser = None
    __comport = None
    __company = 'Appa'
    __model = '109N'
    __device = 'multimeter'

    __lastcomtimestamp = 0.0
    __dataline = ''

    __rangecode = 0
    __modecode = 0
    __acmodecode = 0
    __intvalue = 0
    __multiplicator_SI = 1
    __denominator = 1

    __mode = ''
    __range = ''
    __2intvalue = 0
    __freq = 0.0
    __the_value = 0.0
    __the_value_SI = 0.0
    __the_rand_unc = 0.0
    __the_rand_unc_SI = 0.0
    __the_syst_unc = 0.0
    __the_syst_unc_SI = 0.0
    __units_SI = ''
    __units = ''


    @classmethod
    def atport(cls, comport):
        "Returns True if an Appa 109N multimeter is connected at COM port \"comport\""
        res = False
        if not isinstance(comport, str):
            raise TypeError("comport: string value expected, got", type(str), "instead")
        ser = serial.Serial(port = comport,
                            baudrate = cls.COMPORTSPEED,
                            writeTimeout = cls.COMPORTWRITETIMEOUT,
                            timeout = cls.COMPORTTIMEOUT,
                            parity = cls.COMPORTPARITY,
                            stopbits = cls.COMPORTSTOPBITS,
                            bytesize = cls.COMPORTBITS)
        ser.write(bytearray(b"\x55\x55\x00\x00\xaa"))
        try:
            if len(ser.readline()) == 19:
                res = True
        finally:
            ser.close()
        return res


    def __repr__(self) -> str:
        return f'{self.__company} {self.__model} {self.__device} at {self.__comport}'


    def __str__(self) -> str:
        return f'{self.__company} {self.__model} {self.__device} at {self.__comport}'


    # def __bool__(self):
    #     return self.atport(self.__comport)


    def __init__(self, comport):
        self.__ser = serial.Serial(port = comport,
                            baudrate = self.COMPORTSPEED,
                            write_timeout = self.COMPORTWRITETIMEOUT,
                            timeout = self.COMPORTTIMEOUT,
                            parity = self.COMPORTPARITY,
                            stopbits = self.COMPORTSTOPBITS,
                            bytesize = self.COMPORTBITS)
        self.__comport = comport
        self.__getdata()


    def __del__(self):
        self.__ser.close()


    def __showmsg(self, msg: str):
        """ function shows separate small window at the center of the screen\
        with the message 'msg' """
        root = tk.Tk()
        root.title('APPA 109N message')

        window_width = 400
        window_height = 200

        # get the screen dimension
        screen_width = root.winfo_screenwidth()
        screen_height = root.winfo_screenheight()

        # find the center point
        center_x = int(screen_width/2 - window_width / 2)
        center_y = int(screen_height/2 - window_height / 2)

        # set the position of the window to the center of the screen
        root.geometry(f'{window_width}x{window_height}+{center_x}+{center_y}')

        root.resizable(False, False)
        root.attributes('-topmost', 1)

        tk.Label(root, text="\n"+msg).pack()

        def exit_this_function():
            root.destroy()

        button = tk.Button(root, text='OK!', command=exit_this_function)
        button.pack(side=tk.BOTTOM)
        button.focus_set()

        root.mainloop()


    def __check_sum(self):
        "Calculate checksum (0-17 bytes of the obtained string) and compare it with the 18-th byte"
        if len(self.__dataline) == 19:
            read_sum = self.__dataline[18]
            thesum = 0
            for i in range(18):
                thesum += self.__dataline[i]
            thesum = thesum % 256
            return read_sum == thesum
        else:
            return False


    def __getdata(self):
        "Obtain data from the device"
        if time.time() - self.__lastcomtimestamp > self.SHORTESTTIMEBETWEENREADS:
            read_problem_counter = 0
            self.__dataline = ''
            checksumOK = False
            while len(self.__dataline) != 19 and not checksumOK:
                self.__ser.write(bytearray(b"\x55\x55\x00\x00\xaa"))
                self.__dataline = self.__ser.readline()
                checksumOK = self.__check_sum()
                if len(self.__dataline) != 19:
                    read_problem_counter += 1
                    if read_problem_counter > 3:
                        self.__showmsg("""Switch the multimeter on and then click 'OK'!
                        \n\nHold blue button while switching the multimeter on
                        \nto disable auto switch off""")
                    time.sleep(self.SHORTESTTIMEBETWEENREADS)

            # let's process the data:
            self.__lastcomtimestamp = time.time()
            self.__modecode = self.__ModeCodeTable[self.__dataline[5]][self.__dataline[4]-1]
            match self.__modecode:
                case 0:
                    self.__acmodecode = 1
                case 3:
                    self.__acmodecode = 0
                case 8:
                    self.__acmodecode = 4
                case 11:
                    self.__acmodecode = 5
                case 2:
                    self.__acmodecode = 3
                case 5:
                    self.__acmodecode = 2
                case 10:
                    self.__acmodecode = 6
                case 13:
                    self.__acmodecode = 7
                case _:
                    self.__acmodecode = 10

            self.__rangecode =\
                (self.__dataline[7] - 128) if self.__dataline[7] > 7 else self.__dataline[7]
            self.__mode = self.__ModeTable[self.__dataline[5]][self.__dataline[4]-1]
            self.__range = self.__RangeTable[self.__rangecode][self.__modecode]

            self.__intvalue = int.from_bytes(bytearray([self.__dataline[10],
                                                        self.__dataline[9],
                                                        self.__dataline[8]]),
                                             byteorder='big', signed=True)
            self.__denominator = self.__DenominatorTable[self.__dataline[11] & 7]
            self.__multiplicator_SI =\
                self.__MultiplicatorSITable[round((self.__dataline[11] & 248)/8)]
            self.__units = self.__UnitsTable[round((self.__dataline[11] & 248)/8)]
            self.__units_SI = self.__UnitsSITable[round((self.__dataline[11] & 248)/8)]
            self.__the_value = self.__intvalue / self.__denominator
            self.__the_value_SI =\
                round(self.__intvalue * self.__multiplicator_SI/ self.__denominator,
                                        round(math.log10(self.__denominator) + 3))

            self.__2intvalue = int.from_bytes(bytearray([self.__dataline[15],
                                                        self.__dataline[14],
                                                        self.__dataline[13]]),
                                             byteorder='big', signed=True)
            self.__2denominator = self.__DenominatorTable[self.__dataline[16] & 7]

            if self.__acmodecode < 8:
                self.__freq = self.__2intvalue / self.__2denominator
                self.__frangecode = self.__freq_to_code(self.__freq)
                self.__the_rand_unc = 0.01 *\
                    self.__ACRandErrTable[self.__rangecode][self.__frangecode][self.__acmodecode] *\
                    abs(self.__the_value)
                self.__the_rand_unc_SI = 0.01 *\
                    self.__ACRandErrTable[self.__rangecode][self.__frangecode][self.__acmodecode] *\
                    abs(self.__the_value_SI)
                self.__the_syst_unc =\
                    self.__ACSystErrTable[self.__rangecode][self.__frangecode][self.__acmodecode] *\
                    self.__ResolutionTable[self.__rangecode][self.__modecode]
                self.__the_syst_unc_SI =\
                    self.__ACSystErrTable[self.__rangecode][self.__frangecode][self.__acmodecode] *\
                    self.__ResolutionTable[self.__rangecode][self.__modecode] *\
                    self.__multiplicator_SI
            else:
                self.__freq = 0.0
                self.__the_rand_unc =\
                    0.01 * self.__RandErrPercTable[self.__rangecode][self.__modecode] *\
                    abs(self.__the_value)
                self.__the_rand_unc_SI = 0.01 *\
                    self.__RandErrPercTable[self.__rangecode][self.__modecode] *\
                    abs(self.__the_value_SI)
                self.__the_syst_unc =\
                    self.__SystErrTable[self.__rangecode][self.__modecode] *\
                    self.__ResolutionTable[self.__rangecode][self.__modecode]
                self.__the_syst_unc_SI =\
                    self.__SystErrTable[self.__rangecode][self.__modecode] *\
                    self.__ResolutionTable[self.__rangecode][self.__modecode] *\
                    self.__multiplicator_SI


    def __freq_to_code(self, freq: float) -> int:
        "return range code from frequency value"
        if freq <= 100.0:
            thecode = 0
        elif 100.0 < freq <= 1000.0:
            thecode = 1
        elif 1000.0 < freq <= 10000.0:
            thecode = 2
        elif 10000.0 < freq <= 20000.0:
            thecode = 3
        elif 20000.0 < freq <= 50000.0:
            thecode = 4
        elif 50000.0 < freq:
            thecode = 5
        return thecode


    def __roundunc(self, unc) -> float:
        "round uncertainty value to two significant digits"
        if unc == 0.0:
            return 1
        else:
            decPlace = 1 - round(math.log10(unc))
            return round(unc, decPlace)


    @property
    def mode(self) -> str:
        "returns actual mode"
        self.__getdata()
        return self.__mode

    @property
    def range(self) -> str:
        "returns actual range"
        self.__getdata()
        return self.__range + ' (' + str(self.__rangecode) + ')'


    @property
    def value(self) -> float:
        "returns actual value"
        self.__getdata()
        return self.__the_value

    @property
    def uncertainty(self) -> float:
        "returns actual value in SI units"
        #self.__getdata()
        return self.__roundunc(self.__the_rand_unc + self.__the_syst_unc)

    @property
    def units(self) -> str:
        "returns actual units"
        return self.__units

    @property
    def value_str(self) -> str:
        "returns actual value along with the uncertainty and units"
        self.__getdata()
        return f"{self.__the_value} ±\
            {self.__roundunc(self.__the_rand_unc + self.__the_syst_unc)} {self.__units}"


    @property
    def value_SI(self) -> float:
        "returns actual value in SI units"
        self.__getdata()
        return self.__the_value_SI

    @property
    def uncertainty_SI(self) -> float:
        "returns actual value in SI units"
        #self.__getdata()
        return self.__roundunc(self.__the_rand_unc_SI + self.__the_syst_unc_SI)

    @property
    def units_SI(self) -> str:
        "returns actual SI units"
        return self.__units_SI

    @property
    def value_SI_str(self) -> str:
        "returns actual value in SI units along with the uncertainty and units"
        self.__getdata()
        return f"{self.__the_value_SI} ±\
            {self.__roundunc(self.__the_rand_unc_SI + self.__the_syst_unc_SI)} {self.__units_SI}"


    @property
    def freq(self) -> float:
        "returns sub-reading value"
        return self.__2intvalue / self.__2denominator

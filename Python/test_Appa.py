#!/usr/bin/python3
""" sample program to use few Appa 109N multimeters simultaneously  """

import sys
import time
import tty
import termios
from datetime import datetime
import serial
import serial.tools.list_ports
import appa


__version__ = '02.10.2022'
__author__ = 'Serhiy Kobyakov'


# max number of multimeters that can be simultaneously procesed:
MAXNMETERS = 4
NUMDIGITS = 5

mmeter = []
n_mmeters = 0
data_file_header = ""
the_data = ""


def getch():
    "read single character from keyboard"
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)
    try:
        tty.setraw(sys.stdin.fileno())
        thech = sys.stdin.read(1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)
    return thech


def clearstr():
    "clear string and put carret at the first character"
    print("", end="\r")
    print (' ' * 100, end="\r")


def savedata():
    "save the obtained data to file"
    clearstr()
    if len(the_data) > 0:
        now = datetime.now()
        fname = now.strftime("%Y-%m-%d_%H-%M-%S") + ".dat"
        f = open(fname, "w", encoding="utf_8")
        f.write(data_file_header)
        f.write(the_data)
        f.close()
        print("the data has been saved into file:", fname)
    print()


def print_header():
    "print header of the table"
    global data_file_header
    data_file_header = ""
    global the_data
    the_data = ""
    for n in range(n_mmeters):
        print("#".rjust(NUMDIGITS + 2) + str(n).ljust(NUMDIGITS + 3), "", end="")
    print()
    for n in range(n_mmeters):
        thestr = " (" + mmeter[n].units + ")"
        the_mode = mmeter[n].mode
        print(the_mode.rjust(NUMDIGITS + 2) + thestr.ljust(NUMDIGITS + 3), "", end="")
        data_file_header = data_file_header + the_mode + thestr +\
                    "\tΔ" + the_mode + thestr +"\t"
    data_file_header = data_file_header[:-1] + "\n"
    print()


if __name__ == "__main__":
    print("\nLooking for devices at serial ports...", end="\r")
    ports = serial.tools.list_ports.comports()

    for port in ports:
        if appa.Model109N.atport(port.device):
            print(f"Found Appa 109N at {port.device}, conected as device #{n_mmeters}")
            mmeter.append(appa.Model109N(port.device))
            if n_mmeters == MAXNMETERS:
                break
            n_mmeters += 1

    if n_mmeters == 0:
        print("\n Please attach at least one Appa 109N multimeter\n")
        sys.exit(1)

    print()
    print_header()

    while True:
        time.sleep(0.1)
        thechar = 0
        print("   ---> [Esc], [q] - quit, [r] - start again, [Space] -\
 take measurement(s) <---   ", end="", flush=True)
        thechar = getch()

        if ord(thechar) == 0:
            pass
        elif ord(thechar) == 27:   # Esc
            savedata()
            sys.exit(0)
        elif ord(thechar) == 113:  # q
            savedata()
            sys.exit(0)
        elif ord(thechar) == 114:  # r - restart
            clearstr()
            print("\n")
            print_header()
        elif ord(thechar) == 32 or ord(thechar) == 13: # Space, Enter
            clearstr()
            for i in range(n_mmeters):
                value_1, uncertainty_1 = mmeter[i].value
                print(str(value_1).rjust(NUMDIGITS + 2) + " ± " +\
                    str(uncertainty_1).ljust(NUMDIGITS), "", end="")
                the_data = the_data + str(value_1) + "\t" +\
                    str(uncertainty_1) + "\t"
            the_data = the_data[:-1] + "\n"
            print()
        else:
            pass

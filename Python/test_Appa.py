#!/usr/bin/python3
""" sample programto use few Appa 109N multimeters simultaneously  """

import sys
import time
import tty
import termios
sys.path.append('/home/serg/Projects/0_new_python')
import appa
import serial
import serial.tools.list_ports
from datetime import datetime


__version__ = '27.09.2022'
__author__ = 'Serhiy Kobyakov'


# max number of multimeters that can be simultaneously procesed:
MAXNMETERS = 4

mmeter = []
n_mmeters = 0
theheader = ""
thedata = ""


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
    if len(thedata) > 0:
        now = datetime.now()
        fname = now.strftime("%Y-%m-%d_%H-%M-%S") + ".dat"
        f = open(fname, "w", encoding="utf_8")
        f.write(theheader)
        f.write(thedata)
        f.close()
        print("the data has been saved into file:", fname)
    print()


def printheader():
    "restart measurement"
    global theheader
    theheader = ""
    for n in range(n_mmeters):
        print("#".rjust(8) + str(n).ljust(8), "", end="")
    print()
    for n in range(n_mmeters):
        thestr = " (" + mmeter[n].units + ")"
        print(mmeter[n].mode.rjust(7) + thestr.ljust(9), "", end="")
        theheader = theheader + mmeter[n].mode + thestr +\
                    "\tΔ" + mmeter[n].mode + thestr +"\t"
    theheader = theheader[:-1] + "\n"
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
    printheader()

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
            printheader()
        elif ord(thechar) == 32: # Space
            clearstr()
            for i in range(n_mmeters):
                print(str(mmeter[i].value).rjust(7) + " ± " + str(mmeter[i].uncertainty).ljust(6),\
                    "", end="")
                thedata = thedata + str(mmeter[i].value) + "\t" + str(mmeter[i].uncertainty) + "\t"
                #print("\t\t", end="")
            thedata = thedata[:-1] + "\n"
            print()
        else:
            pass
